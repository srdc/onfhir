package io.onfhir.db

import java.time.Instant

import akka.http.scaladsl.model.{DateTime, StatusCode, StatusCodes}
import io.onfhir.Onfhir
import io.onfhir.api._
import io.onfhir.api.model.Parameter
import io.onfhir.api.parsers.FHIRResultParameterResolver
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.{FhirConfigurationManager, OnfhirConfig, SearchParameterConf}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.conversions.Bson
import io.onfhir.db.BsonTransformer._
import io.onfhir.event.{FhirEventBus, ResourceCreated, ResourceDeleted, ResourceUpdated}
import io.onfhir.exception.{BadRequestException, InternalServerException, UnsupportedParameterException}
import io.onfhir.util.DateTimeUtil
import org.json4s.JsonAST.JValue
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}


/**
  * FHIR Resource Persistency Manager (Mapping FHIR operations to Mongo queries/commands)
  * //TODO Handle resolution of Logical references for chaining and includes (also reverse)
  */
object ResourceManager {
  private implicit val logger: Logger = LoggerFactory.getLogger("ResourceManager")

  implicit val executionContext:ExecutionContext = Onfhir.actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")

  /**
    * FHIR Search Operation
    * @param rtype Resource Type to search
    * @param parameters Parsed FHIR parameters
    * @param excludeExtraParams If true, the extra params set by Onfhir are excluded from the returned resources
    * @return Num of Total Matched Resources, Matched Resources (with Paging) and Included Resources (based on matched ones)
    */
  def searchResources(rtype:String, parameters:List[Parameter] = List.empty, excludeExtraParams:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, Seq[Resource], Seq[Resource])] = {
    //Extract FHIR result parameters
    val resultParameters = parameters.filter(_.paramCategory == FHIR_PARAMETER_CATEGORIES.RESULT)
    //Check _page and _count
    val (page, count) = FHIRResultParameterResolver.resolveCountPageParameters(resultParameters)
    //Check _summary param to identify what to include or exclude
    val summaryIncludesOrExcludes =  FHIRResultParameterResolver.resolveSummaryParameter(rtype, resultParameters)
    //Check _elements param to include further
    val elementsIncludes = FHIRResultParameterResolver.resolveElementsParameter(resultParameters)
    //Decide on final includes and excludes
    val finalIncludesOrExcludes = if(elementsIncludes.nonEmpty) Some(true -> elementsIncludes) else summaryIncludesOrExcludes

    //Find sorting details
    val sortingFields = FHIRResultParameterResolver.resolveSortingParameters(rtype, resultParameters)
    //_total parameter, if total number of matched resource is needed or not
    val needTotal = resultParameters.find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.TOTAL).forall(_.valuePrefixList.head._2 != "none")
    //Find out include and revinclude params
    val includeParams = resultParameters.filter(_.name == FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE)
    val revIncludeParams = resultParameters.filter(_.name == FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE)

    //Now others are query parameters
    val queryParams = parameters.filter(_.paramCategory !=  FHIR_PARAMETER_CATEGORIES.RESULT)

    //If _summary parameter is count, just count the documents
    if(summaryIncludesOrExcludes.exists(s => s._1 && s._2.isEmpty)){
      countResources(rtype, queryParams)
        .map(total => (total, Seq.empty, Seq.empty))
    } else { //Otherwise normal search
      //Execute the actual query to find matched resources
      queryResources(rtype, queryParams, count, page, sortingFields, finalIncludesOrExcludes, excludeExtraParams, needTotal)
        .flatMap(totalAndMatchedResources =>
          //Handle _include and _revinclude params
          (includeParams, revIncludeParams) match {
            //No _include or _revinclude
            case (Nil, Nil) => Future.apply((totalAndMatchedResources._1, totalAndMatchedResources._2, Seq.empty))
            //Only _revinclude
            case (Nil, _) =>
              handleRevIncludes(rtype, totalAndMatchedResources._2, revIncludeParams)
                .map(revIncludedResources => (totalAndMatchedResources._1, totalAndMatchedResources._2, revIncludedResources))
            //Only _include
            case (_, Nil) =>
              handleIncludes(rtype, totalAndMatchedResources._2, includeParams)
                .map(includedResources => (totalAndMatchedResources._1, totalAndMatchedResources._2, includedResources))
            //Both
            case (_, _) =>
              for {
                includedResources <- handleIncludes(rtype, totalAndMatchedResources._2, includeParams)
                revIncludedResources <- handleRevIncludes(rtype, totalAndMatchedResources._2, revIncludeParams)
              } yield (totalAndMatchedResources._1, totalAndMatchedResources._2, includedResources ++ revIncludedResources)
          }
        )
    }
  }

  /**
    * Search FHIR resources of a specific Resource Type with given query
    * @param rtype Resource type
    * @param queryParams Parsed FHIR parameters
    * @param count FHIR _count
    * @param page FHIR _page
    * @param sortingFields Sorting params their sort direction and field paths and target types e.g. date, 1, Seq((effectiveDateTime, DateTime), (effectiveInstant, Instant))
    * @param elementsIncludedOrExcluded Element paths to specifically include or exclude within the resource; first element true -> include, false -> exclude
    * @param excludeExtraFields If true extra onFhir elements are excluded from the resource
    * @param needTotal If false, the total number of matched resources is not returned, instead -1 is returned
    * @return Number of Total resources and the resulting resources (due to paging only a subset can be returned)
    */
  def queryResources(rtype:String,
                     queryParams:List[Parameter] = List.empty,
                     count:Int = -1, page:Int = 1,
                     sortingFields:Seq[(String, Int, Seq[(String, String)])] = Seq.empty,
                     elementsIncludedOrExcluded:Option[(Boolean, Set[String])] = None,
                     excludeExtraFields:Boolean = false,
                     needTotal:Boolean = true
                     )(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, Seq[Resource])] = {

    //Run the search
    constructQuery(rtype, queryParams).flatMap {
      //If there is no query, although there are parameters
      case None if queryParams.nonEmpty => Future.apply((0L, Nil))
      //Otherwise run it
      case finalQuery =>
        queryResourcesDirectly(rtype, finalQuery, count, page, sortingFields, elementsIncludedOrExcluded, excludeExtraFields, needTotal)
    }
  }

  /**
    * Search FHIR resources of a specific Resource Type with given query
    * @param rtype                        Resource type
    * @param query                        Mongo query
    * @param count                        Number of resources to return
    * @param page                         Page to return
    * @param sortingFields                Sorting fields (param name, sorting direction, path and target resource type
    * @param elementsIncludedOrExcluded   Elements to include or exclude
    * @param excludeExtraFields           If true, extra fields are cleared
    * @param needTotal                    If total number of results is needed at the response
    * @return
    */
  def queryResourcesDirectly(rtype:String,
                     query:Option[Bson] = None,
                     count:Int = -1, page:Int = 1,
                     sortingFields:Seq[(String, Int, Seq[(String, String)])] = Seq.empty,
                     elementsIncludedOrExcluded:Option[(Boolean, Set[String])] = None,
                     excludeExtraFields:Boolean = false,
                     needTotal:Boolean = true)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, Seq[Resource])] = {

    val sortingPaths = constructFinalSortingPaths(sortingFields)

    for {
      total <- if(needTotal) DocumentManager.countDocuments(rtype, query) else Future.apply(-1L) //If they don't need total number, just return -1 for it
      resultResources <-
        DocumentManager
          .searchDocuments(rtype, query, count, page, sortingPaths, elementsIncludedOrExcluded, excludeExtraFields)
          //.searchDocumentsByAggregation(rtype, query, count, page, sortingPaths, elementsIncludedOrExcluded, excludeExtraFields)
          .map(_.map(_.fromBson))
    } yield total -> resultResources
  }

  /**
   * Execute the given query and return last or first n resources (based on sorting params) for each group determined by groupByParams
   * e.g. Query the last 3 observations for each observation code given in the query of a given patient
   *
   * @param rtype              Resource type to query
   * @param parameters         FHIR Search parameters e.g. ?subject=Patient/...&code=1235-4,35864-7,65668-8
   * @param sortingParams      Sorting FHIR search parameters to sort the results for last or first n selection e.g. date
   * @param groupByParams      FHIR search parameters to indicate the group by expression e.g. code
   * @param lastOrFirstN       Number of last/first results to return for each group;
   *                            - negative value means last e.g -2 --> last 2
   *                            - positive value means first e.g. 3 --> first 3
   * @param excludeExtraFields If true, extra fields are cleared
   * @param transactionSession Session if this is part of a transaction
   * @return                   Bucket keys and results for each group , all included or revincluded resources
   */
  def searchLastOrFirstNResources(rtype:String,
                                 parameters:List[Parameter] = List.empty,
                                 sortingParams:List[String],
                                 groupByParams:List[String],
                                 lastOrFirstN:Int = -1,
                                 excludeExtraFields:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[(Seq[(Map[String, JValue], Seq[Resource])], Seq[Resource])] = {

    if(sortingParams.isEmpty || groupByParams.isEmpty)
      throw new RuntimeException(s"Parameters  sortingFields or groupByParams is empty! They are required for this method 'queryLastOrFirstNResources'")

    //Extract FHIR result parameters
    val resultParameters = parameters.filter(_.paramCategory == FHIR_PARAMETER_CATEGORIES.RESULT)
    //Check _summary param to identify what to include or exclude
    val summaryIncludesOrExcludes =  FHIRResultParameterResolver.resolveSummaryParameter(rtype, resultParameters)
    //Check _elements param to include further
    val elementsIncludes = FHIRResultParameterResolver.resolveElementsParameter(resultParameters)
    //Decide on final includes and excludes
    val finalIncludesOrExcludes = if(elementsIncludes.nonEmpty) Some(true -> elementsIncludes) else summaryIncludesOrExcludes

    //Find out include and revinclude params
    val includeParams = resultParameters.filter(_.name == FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE)
    val revIncludeParams = resultParameters.filter(_.name == FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE)

    //other result parameters are ignored as they are not relevant


    //Now others are query parameters
    val queryParams = parameters.filter(_.paramCategory !=  FHIR_PARAMETER_CATEGORIES.RESULT)

    //Run the search
    constructQuery(rtype, queryParams).flatMap {
      //If there is no query, although there are parameters
      case None if queryParams.nonEmpty => Future.apply(Nil-> Nil)
      //Otherwise process groupBy and sorting parameters and execute the query
      case finalQuery =>
        //Find out paths for each sorting parameter
        val finalSortingPaths:Seq[(String, Seq[String])] =
          sortingParams
            .map(sp => fhirConfig.findSupportedSearchParameter(rtype, sp) match {
              case None =>
                throw new UnsupportedParameterException(s"Search parameter ${sp} is not supported for resource type $rtype, or you can not use it for sorting! Check conformance statement of server!")
              case Some(spConf) =>
                spConf.pname ->
                  spConf
                    .extractElementPathsAndTargetTypes()
                    .flatMap { case (path, ttype) =>
                      SORTING_SUBPATHS
                        .getOrElse(ttype, Seq("")) //Get the subpaths for sorting for the target element type
                        .map(subpath => path + subpath)
                    }
            })

        //Construct expressions for groupBy params
        val groupByParamConfs =
          groupByParams
          .map(gbyp =>
            fhirConfig.findSupportedSearchParameter(rtype, gbyp) match {
              case None =>
                throw new UnsupportedParameterException(s"Search parameter ${gbyp} is not supported for resource type $rtype, or you can not use it for grouping! Check conformance statement of server!")
              case Some(gbypConf) => gbypConf
            }
          )

      val groupByExpressions = AggregationHandler.constructGroupByExpression(groupByParamConfs, parameters)

      //Execute the query and find the matched results
      val fResults =
        DocumentManager
        .searchLastOrFirstNByAggregation(rtype, finalQuery, lastOrFirstN,finalSortingPaths, groupByExpressions)
        .map(results =>
          results.map(r => {
            val keys =
              groupByParams.map(p =>
                p -> r._1.get(p).get.fromBson
              ).toMap
            keys -> r._2.map(_.fromBson)
          })
        )

       fResults.flatMap(matchedResources =>
       //Handle _include and _revinclude params
        (includeParams, revIncludeParams) match {
          //No _include or _revinclude
          case (Nil, Nil) => Future.apply(matchedResources, Seq.empty)
          //Only _revinclude
          case (Nil, _) =>
            handleRevIncludes(rtype, matchedResources.flatMap(_._2), revIncludeParams)
              .map(revIncludedResources => (matchedResources, revIncludedResources))
          //Only _include
          case (_, Nil) =>
            handleIncludes(rtype, matchedResources.flatMap(_._2), includeParams)
              .map(includedResources => (matchedResources, includedResources))
          //Both
          case (_, _) =>
            val allMatchedResources = matchedResources.flatMap(_._2)
            for {
              includedResources <- handleIncludes(rtype, allMatchedResources, includeParams)
              revIncludedResources <- handleRevIncludes(rtype, allMatchedResources, revIncludeParams)
            } yield (matchedResources, includedResources ++ revIncludedResources)
        }
       )
    }
  }

  /**
    * Construct alternative paths according to FHIR type of the target element
    * @param sortingFields  Sorting fields (param name, sorting direction, path and target resource type
    * @return
    */
  private def constructFinalSortingPaths(sortingFields:Seq[(String, Int, Seq[(String, String)])]):Seq[(String, Int, Seq[String])] = {
    sortingFields.map { case (pname, sorder, pathsAndTypes) =>
      (
        pname,
        sorder,
        // Indicate alternative subpaths to sort for the field path
        pathsAndTypes.flatMap { case (path, ttype) =>
          SORTING_SUBPATHS
            .getOrElse(ttype, Seq("")) //Get the subpaths for sorting for the target element type
            .map(subpath => path + subpath)
        }
      )
    }
  }

  /**
    * Count the resources that matched the given query
    * @param rtype        Resource type
    * @param queryParams  Parsed FHIR query parameter
    * @return
    */
  def countResources(rtype:String,
                     queryParams:List[Parameter] = List.empty)(implicit transactionSession: Option[TransactionSession] = None):Future[Long] = {
    //Construct query
    constructQuery(rtype, queryParams).flatMap {
      //If there is no query, although there are parameters
      case None if queryParams.nonEmpty => Future.apply(0L)
      //Otherwise run it
      case finalQuery => DocumentManager.countDocuments(rtype, finalQuery)
    }
  }

  /**
    * Construct the final Mongo query from FHIR parameters
    * @param rtype        Resource type
    * @param queryParams  Parsed FHIR query parameter
    * @return None if the query fails (no matching resources) otherwise final Mongo query
    */
  def constructQuery(rtype:String, queryParams:List[Parameter] = List.empty)(implicit transactionSession: Option[TransactionSession] = None):Future[Option[Bson]] = {
    //Handle Special params
    val specialParamQueries:Future[Seq[Option[Bson]]] = handleSpecialParams(rtype, queryParams.filter(_.paramCategory == FHIR_PARAMETER_CATEGORIES.SPECIAL))
    //Handle Chained Params
    val chainedParamQueries:Future[Seq[Option[Bson]]] = Future.sequence(
      queryParams
        .filter(_.paramCategory == FHIR_PARAMETER_CATEGORIES.CHAINED)
        .map(p => handleChainParam(rtype, p))
    )
    //Handle Reverse Chained Params
    val revchainedParamQueries:Future[Seq[Option[Bson]]] = Future.sequence(
      queryParams
        .filter(_.paramCategory == FHIR_PARAMETER_CATEGORIES.REVCHAINED)
        .map(p => handleReverseChainParam(rtype, p))
    )

    //Merge all special query handlings
    val fotherQueries =
      for {
        r1 <- specialParamQueries
        r2 <- chainedParamQueries
        r3 <- revchainedParamQueries
      } yield r1 ++ r2 ++ r3

    //Get valid query parameters for the resource type
    val validQueryParams = fhirConfig.getSupportedParameters(rtype)
    //Construct final query by anding all of them
    fotherQueries.map( otherQueries => {
      //If there are other queries, but some empty it means rejection
      if(otherQueries.exists(_.isEmpty))
       None
      else {
        val normalQueries = queryParams
          .filter(p => p.paramCategory == FHIR_PARAMETER_CATEGORIES.NORMAL || p.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT)
          .map(p => {
            p.paramCategory match {
              case FHIR_PARAMETER_CATEGORIES.NORMAL =>
                ResourceQueryBuilder.constructQueryForNormal(p, validQueryParams.apply(p.name), validQueryParams)
              case FHIR_PARAMETER_CATEGORIES.COMPARTMENT =>
                ResourceQueryBuilder.constructQueryForCompartment(rtype, p, validQueryParams)
            }
          })
        DocumentManager.andQueries(normalQueries ++ otherQueries.flatten)
      }
    })
  }

  /**
    * Handle FHIR _revinclude to return revinclude resources
    * @param rtype                Resource Type
    * @param matchedResources     Matched Resources
    * @param revIncludeParams     Parsed FHIR _revinclude parameters
    * @return Linked resources
    */
  private def handleRevIncludes(rtype:String, matchedResources:Seq[Resource],revIncludeParams:List[Parameter])(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Resource]] = {
    val matchedResourceReferences = matchedResources.map(mr => rtype + "/"+FHIRUtil.extractIdFromResource(mr))
    Future.sequence(
      revIncludeParams.map( p => {
        val (linkedResourceType, linkParam) = p.valuePrefixList.head
        val searchParameterConf = fhirConfig.findSupportedSearchParameter(linkedResourceType, linkParam).get
        val query = ResourceQueryBuilder.constructQueryForRevInclude(matchedResourceReferences, searchParameterConf)
        DocumentManager
          .searchDocuments(linkedResourceType, Some(query))
          .map(_.map(_.fromBson))
      })
    ).map(_.flatten)
  }


  /**
    * Handle FHIR _include to return to be included resources
    * @param rtype Resource Type
    * @param matchedResources Matched Resources
    * @param includeParams parsed _include params
    * @return Included resources
    */
  private def handleIncludes(rtype:String, matchedResources:Seq[Resource], includeParams:List[Parameter])(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Resource]] = {
    val matchedResourceMap =
      matchedResources
        .map(mr => (rtype -> FHIRUtil.extractIdFromResource(mr)) -> mr).toMap

    //Execute iterations
    executeIncludeIteration(rtype, matchedResourceMap.keySet, matchedResources, includeParams)
  }

  /**
    * Recuresivly handle _include iterates
    * @param rtype          Resource type
    * @param allResources   All resources compiled until now
    * @param newResources   new resources returned at the last iteration
    * @param includeParams  Include parameters
    * @return
    */
  private def executeIncludeIteration(rtype:String, allResources:Set[(String,String)], newResources:Seq[Resource], includeParams:List[Parameter])(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Resource]] = {
    val newResourceMap =
       newResources
        .map(mr => (rtype -> FHIRUtil.extractIdFromResource(mr)) -> mr).toMap
    //First execute single iteration includes (include without :iterate
    val includes:Map[String, Set[String]] =   //ResourceType -> Set(Rids) to include
      includeParams
        .filter(_.valuePrefixList.head._1 == rtype) //Filter related include params
        .map(p => findIncludeResources(allResources, newResourceMap, p))
        .reduce((s1,s2) => s1 ++ s2)
        .groupBy(_._1).map(g => g._1 -> g._2.map(_._2))
    //If there is nothing to include, return
    if(includes.isEmpty)
      return Future.apply(Seq.empty)
    //Retrieve the resources
    val fincludedResources =
      Future.sequence(
        includes
          .map(includeSet =>
            DocumentManager.searchDocuments(includeSet._1, Some(DocumentManager.ridsQuery(includeSet._2)))
              .map(r => includeSet._1 -> r) //Query each resource type with given ids
          )
      ).map(_.map(r => r._1 -> r._2.map(_.fromBson))) //Convert them to resources

    //Now we go on iterated include params
    val iteratedIncludeParams = includeParams.filter(_.suffix == ":iterate")
    fincludedResources flatMap (includedResources => {
      //If there is no include, or no parameter with iterate
      if(includedResources.isEmpty || iteratedIncludeParams.isEmpty)
        Future.apply(includedResources.toSeq.flatMap(_._2))
      else {
        val newAllResources = allResources ++ includedResources.flatMap(ir => ir._2.map(r => ir._1 -> FHIRUtil.extractIdFromResource(r))).toSet
        Future.sequence(
          //Run iteration recursively for each ResourceType, run iterations only on new resources
          includedResources.map(ir => executeIncludeIteration(ir._1, newAllResources, ir._2, iteratedIncludeParams))
        ).map(includedResources.toSeq.flatMap(_._2) ++ _.flatten) //And merge all included resources
      }
    })
  }


  /**
    * Find to be included resources based on the matched resources and the _include parameter
    * @param matchedResources Matched Resources (Resource Type, Rid) -> Resource content
    * @param parameter Parsed _include parameter
    * @return Set of included resources as ResourceType and Rid
    */
  private def findIncludeResources(allResources:Set[(String,String)], matchedResources:Map[(String, String), Resource], parameter: Parameter):Set[(String, String)] = {
    //Target resource type to include
    val targetResourceType = if(parameter.paramType == "") None else Some(parameter.paramType)

    //For all inclusions in this parameter
    parameter.valuePrefixList.flatMap {
      case (includeResourceType, includeParam) =>
        //Find parameter configuration
        val refParamConf = fhirConfig.findSupportedSearchParameter(includeResourceType, includeParam)
        //Find the path to the FHIR Reference elements
        val refPath:Option[String] =
          refParamConf.flatMap(pconf => {
            if (targetResourceType.forall(t => pconf.targets.contains(t) || pconf.targets.contains(FHIR_DATA_TYPES.RESOURCE)))
              Some(pconf.extractElementPaths().head)
            else
              None
          })

        if(refPath.isDefined) {
          //Extract type and ids
          val resourcesToInclude =
            matchedResources
              .filter(_._1._1 == includeResourceType) //Only evaluate the resources with the specified type
              .flatMap(mresource => FHIRUtil.extractReferences(refPath.get, mresource._2)) //Extract Reference values
              .toSet
              .map(refValue => FHIRUtil.parseReferenceValue(refValue)) //Parse the reference values
              .filter(_._1.forall(_ == OnfhirConfig.fhirRootUrl)) //Only get the ones that are persistent inside our repository
              .filter(pref => targetResourceType.forall(_ == pref._2)) //Only get the ones that are compliant with expected target type
              .map(pref => pref._2 -> pref._3) //only get ResourceType and Rid
              .diff(allResources) //take difference with existent ones

          resourcesToInclude
        } else Seq.empty
    }.toSet
  }


  /**
    * Handle special parameters
    * @param rtype              Resource Type
    * @param specialParameters  Parsed FHIR special parameters
    * @return
    */
  private def handleSpecialParams(rtype:String, specialParameters: List[Parameter])(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Option[Bson]]] = {
    Future.sequence(
      specialParameters.map(p => p.name match {
        //Search with ids
        case FHIR_SEARCH_SPECIAL_PARAMETERS.ID =>
          Future.apply(Some(ResourceQueryBuilder.constructQueryForIds(p)))
        //FHIR _list query
        case FHIR_SEARCH_SPECIAL_PARAMETERS.LIST =>
          handleListSearch(rtype, p)
        case FHIR_SEARCH_SPECIAL_PARAMETERS.TEXT | FHIR_SEARCH_SPECIAL_PARAMETERS.CONTENT | FHIR_SEARCH_SPECIAL_PARAMETERS.FILTER =>
          throw new UnsupportedParameterException(s"Parameter ${p.name} is not supported in onFhir.io!")
      })
    )
  }

  /**
    * FHIR _list search
    * @param rtype Resource Type
    * @param parameter Parsed parameter
    * @return
    */
  private def handleListSearch(rtype:String, parameter: Parameter)(implicit transactionSession: Option[TransactionSession] = None):Future[Option[Bson]] = {
    val listId = parameter.valuePrefixList.map(_._2).head
    listId match {
      case  currentInd if currentInd.startsWith("$") => throw new UnsupportedParameterException("Parameter _list is not supported for $current-* like queries!")
      case lid =>
        //Try to retrieve the list
        DocumentManager
          .getDocument("List", lid, includingOrExcludingFields = Some(true -> Set("entry.item"))) //Retrieve the List document with given id (only item reference elements)
          .map(_.map( _.fromBson))
          .map(r => r.map(FHIRUtil.extractReferences("entry.item", _))) //extract the references
          .map(_.map(_.map(FHIRUtil.parseReferenceValue))) //Parse the references
          .map(_.map(parsedRefs =>
            parsedRefs
              .filter(_._1.forall(_ == OnfhirConfig.fhirRootUrl)) //Only the references in our server
              .filter(_._2 == rtype) //Only the ones refering the given resource type)
              .map(_._3) //Get resource id
              .toSet
          ))
          .map(_.map(DocumentManager.ridsQuery)) // Construct the query
    }
  }

  /**
    * Handle FHIR _has search (Reverse chain)
    * @param rtype Resource Type to search
    * @param parameter Parsed parameter
    * @return
    */
  private def handleReverseChainParam(rtype:String, parameter: Parameter)(implicit transactionSession: Option[TransactionSession] = None):Future[Option[Bson]] = {
    //Get the resource type to query (the end of revchain)
    //e.g. Patient?_has:Observation:patient:_has:AuditEvent:entity:agent=MyUserId
    val rtypeToQuery = parameter.chain.last._1
    val searchParameterConf = fhirConfig.findSupportedSearchParameter(rtypeToQuery, parameter.name)
    if(searchParameterConf.isEmpty)
      throw new UnsupportedParameterException(s"Parameter ${parameter.name} is not supported on $rtypeToQuery within '_has' query!")

    //Construct the Query on the leaf of chain to find the resource references
    val query = ResourceQueryBuilder.constructQueryForNormal(parameter, searchParameterConf.get, fhirConfig.getSupportedParameters(rtypeToQuery))
    //Find the path of reference
    val chainParameterConf = fhirConfig.findSupportedSearchParameter(rtypeToQuery, parameter.chain.last._2)
    if(chainParameterConf.isEmpty)
      throw new UnsupportedParameterException(s"Parameter ${parameter.chain.last._2} is not supported on $rtypeToQuery within '_has' query!")

    //Find the paths of reference element to return
    val referencePaths = chainParameterConf.get.extractElementPaths().toSet
    //Run query but only return the references on chain parameter
    var fresourceReferences = DocumentManager
      .searchDocuments(rtypeToQuery, Some(query), includingOrExcludingFields = Some(true -> referencePaths))
      .map(_.map(_.fromBson))
      .map(
        _.flatMap(r =>
          referencePaths.flatMap(rpath => FHIRUtil.extractReferences(rpath, r))
        )
      )

    // Come from deep in revchain by evaluating from right
    fresourceReferences =
      parameter.chain
        .dropRight(1) // We already evaluate the last one
        .foldRight(fresourceReferences)(findResourceReferencesInResources)

    fresourceReferences.map {
      case Nil => None //No such resource
      case references =>
        val rids = references
          .map(FHIRUtil.parseReferenceValue) //Parse the reference value
          .filter(_._1.forall(_ == OnfhirConfig.fhirRootUrl)) //Only the references in our server
          .filter(_._2 == rtype) //Only the ones refering the given resource type
          .map(_._3)
          .toSet
        //Return the query
        Some(DocumentManager.ridsQuery(rids))
    }
  }


  /***
    * Within the given resources (by references)  find resource references within the given parameter
    * @param rtypeAndChainParamName Resource Type and Chain Parameter Name
    * @param freferences References to the resources that search will be on
    * @return
    */
  private def findResourceReferencesInResources(rtypeAndChainParamName:(String,String), freferences:Future[Seq[String]])(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[String]] = {
    freferences.flatMap(references => {
      val rids =
        references
          .map(FHIRUtil.parseReferenceValue) //Parse the reference value
          .filter(_._1.forall(_ == OnfhirConfig.fhirRootUrl)) //Only the references in our server
          .filter(_._2 == rtypeAndChainParamName._1) //Only the ones refering the given resource type
          .map(_._3)
          .toSet

      //Find the path of reference
      val chainParameterConf = fhirConfig.findSupportedSearchParameter(rtypeAndChainParamName._1,rtypeAndChainParamName._2)
      if(chainParameterConf.isEmpty)
        throw new UnsupportedParameterException(s"Parameter ${rtypeAndChainParamName._2} is not supported on ${rtypeAndChainParamName._1} within '_has' query!")

      //Find the paths of reference element to return
      val referencePaths = chainParameterConf.get.extractElementPaths().toSet
      //Go and get the references inside the given resources
      getResourcesWithIds(rtypeAndChainParamName._1, rids, Some(true -> referencePaths), excludeExtraFields = true)
        .map(
          _.flatMap(r =>
            referencePaths.flatMap(rpath => FHIRUtil.extractReferences(rpath, r))
          )
        )
    })
  }


  /**
    * Handle a chain parameter and returns a query indicating this chained search
    * @param rtype Resource Type to search
    * @param parameter Parsed Chained Parameter definition
    * @return
    */
  private def handleChainParam(rtype:String, parameter:Parameter)(implicit transactionSession: Option[TransactionSession] = None):Future[Option[Bson]] = {
    //Get the resource type to query (the end of chain)
    // e.g. /DiagnosticReport?subject:Patient.name=peter --> chain=List(Patient, subject) --> Patient
    // e.g. /DiagnosticReport?subject:Patient.general-practitioner.name=peter --> chain=List(Patient -> subject, Practitioner -> general-practitioner)
    val rtypeToQuery = parameter.chain.last._1
    val searchParameterConf = fhirConfig.findSupportedSearchParameter(rtypeToQuery, parameter.name)
    if(searchParameterConf.isEmpty)
      throw new UnsupportedParameterException(s"Parameter ${parameter.name} is not supported on $rtypeToQuery within chained query!")

    //Run the Query on the leaf of chain to find the resource references
    val query = ResourceQueryBuilder.constructQueryForNormal(parameter, searchParameterConf.get, fhirConfig.getSupportedParameters(rtypeToQuery))
    var fresourceReferences =
      DocumentManager.searchDocumentsReturnIds(rtypeToQuery, query)
        .map(rids => rids.map(rid => s"$rtypeToQuery/$rid") ) //Only get ids and convert them to references

    //Resource Type chains by references e.g. DiagnosticReport or DiagnosticReport, Patient
    val rtypeChain = rtype +: parameter.chain.map(_._1).dropRight(1)
    //Reference param names chain e.g. subject or subject, general-practitioner
    val refParamChain = parameter.chain.map(_._2)
    //Zip these
    val chain = rtypeChain.zip(refParamChain)
    // Come from deep in chain by evaluating from right
    fresourceReferences = chain.tail.foldRight(fresourceReferences)( findResourceIdsReferingChained)
    //Construct last query
    fresourceReferences.map {
      case Nil => None  //If the chain search result is empty, we don't have exra query
      case refs => Some(constructQueryForChained(chain.head._1, chain.head._2, refs))
    }
  }

  /**
    * Construct query for chained searches
    * @param rtype Resource type
    * @param pname Parameter name
    * @param references All Resource references to search
    * @return
    */
  private def constructQueryForChained(rtype:String, pname:String, references:Seq[String]):Bson = {
    val parameter = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.REFERENCE, pname, references.map(r => "" -> r))
    val searchParamConf = fhirConfig.findSupportedSearchParameter(rtype, pname)
    if(searchParamConf.isEmpty)
      throw new UnsupportedParameterException(s"Parameter $pname is not supported on $rtype within chained query!")
    ResourceQueryBuilder.constructQueryForSimpleParameter(parameter, searchParamConf.get)
  }

  /**
    * Find resource ids that refer the given resources
    * @param rtypeAndPName Resource Type and reference parameter name to search
    * @param fcids given resource ids to refer
    * @return
    */
  private def findResourceIdsReferingChained(rtypeAndPName:(String,String), fcids:Future[Seq[String]])(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[String]] = {
    fcids.flatMap(cids => {
      val query = constructQueryForChained(rtypeAndPName._1, rtypeAndPName._2, cids)
      DocumentManager.searchDocumentsReturnIds(rtypeAndPName._1, query)
        .map(rids =>
          rids.map(rid => s"${rtypeAndPName._1}/$rid")
        )
    })
  }

  /**
    * Returns a specific version of a resource. If version id is not provided then the current version is return
    *
    * @param rtype type of the resource
    * @param id id of the resource
    * @param vid version id of the resource
    * @param includingOrExcludingFields Elements to include or exclude
    * @param excludeExtraFields If true exclude extra onfhir appended fields
    * @return a future document for the given resource
    */
  def getResource(rtype:String, id:String, vid:Option[String] = None, includingOrExcludingFields:Option[(Boolean, Set[String])] = None, excludeExtraFields:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None) : Future[Option[Resource]] = {
    //Retrieve the document
    DocumentManager
      .getDocument(rtype, id, vid, includingOrExcludingFields, excludeExtraFields)
      .map(
        _.map(_.fromBson)
      )
  }

  /**
    * Check if given resource exist and active (not deleted)
    * @param rtype Resource type
    * @param id Resource id
    * @return
    */
  def isResourceExist(rtype:String, id:String):Future[Boolean] = {
    DocumentManager
      .getDocument(rtype, id, None, Some(true -> Set.empty), excludeExtraFields = false)
      .map(document =>
        document.map(_.fromBson)
      ).map(resource =>
        resource.exists(r => !FHIRUtil.isDeleted(r))
      )
  }

  /**
    * Get resources with given Id set
    * @param rtype Resource type
    * @param rids Resource ids
    * @param includingOrExcludingFields Elements to include or exclude
    * @param excludeExtraFields If true exclude extra fields
    * @return
    */
  def getResourcesWithIds(rtype:String, rids:Set[String], includingOrExcludingFields:Option[(Boolean, Set[String])] = None, excludeExtraFields:Boolean = false) (implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Resource]] = {
    DocumentManager
      .searchDocuments(rtype, Some(DocumentManager.ridsQuery(rids)), includingOrExcludingFields =  includingOrExcludingFields, excludeExtraFields = excludeExtraFields)
      .map(_.map(_.fromBson))
  }

  /**
    * FHIR _since query on history
    * @param since FHIR time to query history
    * @return
    */
  private def sinceQuery(since:String):Bson =
    PrefixModifierHandler.dateRangePrefixHandler(
      s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}",
      since,
      FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL)

  /**
    * FHIR _at query on history
    * @param atTime Time query
    * @return
    */
  private def atQuery(atTime:String):Bson =
    PrefixModifierHandler.dateRangePrefixHandler(
      s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}",
      atTime,
      FHIR_PREFIXES_MODIFIERS.LESS_THAN)


  /**
    * Get history of a resource type or resource instance (FHIR History interactions)
    * @param rtype Resource Type
    * @param rid Resource id
    * @param searchParameters FHIR search parameters for history search
    * @return
    */
  def getResourceHistory(rtype:String, rid:Option[String], searchParameters:List[Parameter])(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, Seq[Resource])] = {
    //Resolve history parameters
    val since = searchParameters.find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.SINCE).map(_.valuePrefixList.head._2)
    val at = searchParameters.find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.AT).map(_.valuePrefixList.head._2)

    val (page, count) = FHIRResultParameterResolver.resolveCountPageParameters(searchParameters)

    val listQueryFuture = searchParameters.find(_.name == FHIR_SEARCH_SPECIAL_PARAMETERS.LIST) match {
      case None => Future.apply(None)
      case Some(p) => handleListSearch(rtype, p)
    }

    val result = listQueryFuture flatMap(listQuery => {
      if(at.isDefined){
        val finalQuery = DocumentManager.andQueries(listQuery.toSeq :+ atQuery(at.get))
        DocumentManager.searchHistoricDocumentsWithAt(rtype, rid, finalQuery, count, page)
      } else {
        val finalQuery = DocumentManager.andQueries(since.map(sinceQuery).toSeq ++ listQuery.toSeq)
        DocumentManager.searchHistoricDocuments(rtype, rid, finalQuery, count, page)
      }
    })

    result
      .map {
        case (total, docs) => total -> docs.map(_.fromBson)
      }
  }

  /**
    * Get the latest status of a resource
    * @param rtype Resource type
    * @param id Resource id
    * @return Latest Version id, Timestamp, and isDeleted
    */
  def getResourceStatus(rtype:String, id:String):Future[Option[(Long, DateTime, Boolean)]] = {
    DocumentManager
      .getDocument(rtype, id, includingOrExcludingFields = Some(true -> Set.empty))
      .map(
        _.map(_.fromBson)
          .map(resource =>
          (
            FHIRUtil.extractVersionFromResource(resource),
            FHIRUtil.extractLastUpdatedFromResource(resource),
            FHIRUtil.isDeleted(resource)
          )
        )
      )
  }

  /**
    * Creates the given FHIR resource into database
    * @param rtype Resource type
    * @param resource FHIR resource
    * @param generatedId If given resource is created with this id, otherwise a uuid is generated
    * @param withUpdate Indicates if this creation is done by an update operation
    * @return a future indicating the assigned resource id and version and modified time and the resource itself
    */
  def createResource(rtype:String, resource:Resource, generatedId:Option[String] = None, withUpdate:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[(String, Long, DateTime, Resource)] = {
    //1) set resource version and last update time
    val newVersion   = 1L                                                     //new version is always 1 for create operation
    val lastModified = Instant.now()                                          //last modified will be "now"
    val newId        = generatedId.getOrElse(FHIRUtil.generateResourceId())   //generate an identifier for the new resource
    val resourceWithMeta = FHIRUtil.populateResourceWithMeta(resource, Some(newId), newVersion, lastModified)

    //2) add "current" field with value true (after serializing to json)
    val populatedResource =
      FHIRUtil.populateResourceWithExtraFields(
        resourceWithMeta,
        if(withUpdate) FHIR_METHOD_NAMES.METHOD_PUT else FHIR_METHOD_NAMES.METHOD_POST, //how it is created
        StatusCodes.Created)

    //3) persist the resource
    DocumentManager
      .insertDocument(rtype, Document(populatedResource.toBson))
      .map( c => resourceCreated(rtype, newId, resourceWithMeta)) //trigger the event
      .map( _ =>
        (newId, newVersion, DateTimeUtil.instantToDateTime(lastModified), resourceWithMeta)
      )
  }

  /**
    * Create the given resources
    * @param rtype    Resource type
    * @param resources  Resources to create
    * @return
    */
  def createResources(rtype:String, resources:Map[String, Resource]):Future[Seq[Resource]] = {
    val newVersion   = 1L                                                     //new version is always 1 for create operation
    val lastModified = Instant.now()                                          //last modified will be "now"
    val resourcesWithMeta = resources.map {
      case (rid, resource) => FHIRUtil.populateResourceWithMeta(resource, Some(rid), newVersion, lastModified)
    }.toSeq

    val populatedDocuments =
      resourcesWithMeta
        .map(r => FHIRUtil.populateResourceWithExtraFields(r, FHIR_METHOD_NAMES.METHOD_POST, StatusCodes.Created))
        .map(r => Document(r.toBson))

    DocumentManager
      .insertDocuments(rtype, populatedDocuments)
      .map(_ => resourcesWithMeta)
  }
/*
  /**
    * Updates a given FHIR resource
    * @param rtype Resource type
    * @param rid Resource id to update
    * @param resource Updated resource
    * @param previousVersion Previous version id of the resource
    * @param wasDeleted If previously, this was deleted resource
    * @return Latest version, modified time, and the final resource content
    */
  def updateResource(rtype:String, rid:String, resource:Resource, previousVersion:Long = 0L, wasDeleted :Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, DateTime, Resource)] = {
    //1) Update the resource version and last update time
    val newVersion   = previousVersion + 1L   //new version always be 1 incremented of current version
    val lastModified = DateTime.now           //last modified will be "now"
    val resourceWithMeta = FHIRUtil.populateResourceWithMeta(resource, Some(rid), newVersion, lastModified)

    //2) Add "current" field with value. (after serializing to json)
    val populatedResource = FHIRUtil.populateResourceWithExtraFields(resourceWithMeta, FHIR_METHOD_NAMES.METHOD_PUT, if(previousVersion > 0 && !wasDeleted) StatusCodes.OK else StatusCodes.Created)

    //3) persist the resource
    DocumentManager
      .insertNewVersion(rtype, rid, Document(populatedResource.toBson))
      .map( c => resourceUpdated(rtype, rid, resource)) //trigger the event
      .map( _ =>
        (newVersion, lastModified, resourceWithMeta)
      )
  }*/

  /**
    * Updates a given FHIR resource
    * @param rtype Resource type
    * @param rid Resource id to update
    * @param resource Updated resource
    * @param previousVersion Previous version of the document together with version id
    * @param wasDeleted If previously, this was deleted resource
    * @param silentEvent  If true, ResourceUpdate event is not triggered (used internally)
    * @param transactionSession
    * @return
    */
  def updateResource(rtype:String, rid:String, resource:Resource, previousVersion:(Long, Resource), wasDeleted :Boolean = false, silentEvent:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, DateTime, Resource)] = {
    //1) Update the resource version and last update time
    val newVersion   = previousVersion._1 + 1L   //new version always be 1 incremented of current version
    val lastModified = Instant.now()             //last modified will be "now"
    val resourceWithMeta = FHIRUtil.populateResourceWithMeta(resource, Some(rid), newVersion, lastModified)

    //2) Add "current" field with value. (after serializing to json)
    val populatedResource = FHIRUtil.populateResourceWithExtraFields(resourceWithMeta, FHIR_METHOD_NAMES.METHOD_PUT, if(previousVersion._1 > 0 && !wasDeleted) StatusCodes.OK else StatusCodes.Created)

    //3) Construct shard query if sharding is enabled and on a field other than id
    val shardQueryOpt = ResourceQueryBuilder.constructShardingQuery(rtype, resource)

    //4) Remove mongo id from old version
    val oldBsonDocument = previousVersion._2.toBson
    oldBsonDocument.remove(FHIR_COMMON_FIELDS.MONGO_ID)

    val fop =
      //If the resource type is versioned, insert new version and move the old version to history
      if(fhirConfig.isResourceTypeVersioned(rtype))
        DocumentManager
          .insertNewVersion(rtype, rid, Left(Document(populatedResource.toBson)), previousVersion._1 -> Document(oldBsonDocument), shardQueryOpt)
      else
        //Otherwise, replace current version
        DocumentManager
          .replaceCurrent(rtype, rid, Document(populatedResource.toBson), shardQueryOpt)

    if(silentEvent)
      fop.map(_ =>
        (newVersion, DateTimeUtil.instantToDateTime(lastModified), resourceWithMeta)
      )
    else
      fop
        .map( c => resourceUpdated(rtype, rid, resource)) //trigger the event
        .map( _ =>
          (newVersion, DateTimeUtil.instantToDateTime(lastModified), resourceWithMeta)
        )
  }


  /*/***
    * Deletes a given FHIR resource
    * @param rtype Resource type
    * @param rid Resource id to delete
    * @param previousVersion  Previous version id of the resource
    * @param statusCode Http Status to set for deletion result
    * @return
    */
  def deleteResource(rtype:String, rid:String, previousVersion:Long, statusCode:StatusCode = StatusCodes.NoContent)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, DateTime)]  = {
    //1) Create a empty document to represent deletion
    val newVersion     = previousVersion + 1L   //new version is 1 incremented
    val lastModified = DateTime.now
    val resource  = FHIRUtil.createEmptyResourceForDelete(rtype, rid, newVersion, lastModified, statusCode)  //create an empty resource

    //2) insert this new version
    DocumentManager
      .insertNewVersion(rtype, rid, Document(resource.toBson))
      .map( _ => resourceDeleted(rtype, rid)) //trigger the event
      .map( _ =>
        (newVersion, lastModified)
      )
  }*/

  /***
    * Deletes a given FHIR resource
    * @param rtype Resource type
    * @param rid Resource id to delete
    * @param previousVersion  Previous version id of the resource
    * @param statusCode Http Status to set for deletion result
    * @return
    */
  def deleteResource(rtype:String, rid:String, previousVersion:(Long, Resource), statusCode:StatusCode = StatusCodes.NoContent)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long, DateTime)]  = {
    //1) Create a empty document to represent deletion
    val newVersion     = previousVersion._1 + 1L   //new version is 1 incremented
    val lastModified = Instant.now()

    //2) Construct shard query if sharding is enabled and on a field other than id
    val shardQueryOpt = ResourceQueryBuilder.constructShardingQuery(rtype, previousVersion._2)

    //3) Remove mongo id from old version
    val oldBsonDocument = previousVersion._2.toBson
    oldBsonDocument.remove(FHIR_COMMON_FIELDS.MONGO_ID)

    val fop =
      // 4) If resource type is versioned, insert this deleted version, and move old version to history
      if(fhirConfig.isResourceTypeVersioned(rtype))
        DocumentManager
          .insertNewVersion(rtype, rid, Right(statusCode.intValue().toString -> lastModified), previousVersion._1 -> Document(oldBsonDocument), shardQueryOpt)
      else
        //Otherwise, mark the current as deleted
        DocumentManager.deleteCurrent(rtype, rid, statusCode.intValue().toString, newVersion, lastModified, shardQueryOpt)

     fop
      .map( _ => resourceDeleted(rtype, rid)) //trigger the event
      .map( _ =>
        (newVersion, DateTimeUtil.instantToDateTime(lastModified))
      )
  }

  /**
    * Replace a current resource with the given content (Used by onFHIR administrative parts) without adding old version to the history
    * @param rtype    Resource Type
    * @param rid      Resource id
    * @param resource Replaced resource
    * @return
    */
  def replaceResource(rtype:String, rid:String, resource:Resource):Future[Boolean] = {
    //2) Construct shard query if sharding is enabled and on a field other than id
    val shardQueryOpt = ResourceQueryBuilder.constructShardingQuery(rtype, resource)

    DocumentManager.replaceCurrent(rtype, rid, Document(resource.toBson), shardQueryOpt)
  }

  /**
    * Event triggered when a resource is created
    * @param rtype    Resource type
    * @param rid      Resource id
    * @param resource Created resource
    * @return
    */
  private def resourceCreated(rtype:String, rid:String, resource:Resource): Unit = {
    FhirEventBus.publish(ResourceCreated(rtype, rid, resource))
    /*
    if(OnfhirConfig.isKafkaEnabled(rtype)) {
      logger.debug(s"Sending resource to Kafka...")
      val kafkaKey   = rtype.toLowerCase + ":" + rid //e.g. observation:23132
      val kafkaValue = resource.toJson
      //KafkaClient.sendString(OnfhirConfig.kafkaTopic, kafkaKey, kafkaValue)
      Future.apply()
    } else
      Future.apply(Unit)*/
  }

  /**
    * Event triggered when a resource is updated
    * @param rtype       Resource type
    * @param rid         Resource id
    * @param resource    Last version of updated resource
    * @return
    */
  private def resourceUpdated(rtype:String, rid:String, resource:Resource): Unit = {
    FhirEventBus.publish(ResourceUpdated(rtype, rid, resource))
    /*if (OnfhirConfig.isKafkaEnabled(rtype)) {
      logger.debug(s"Sending resource to Kafka...")
      val kafkaKey = rtype.toLowerCase + ":" + rid //e.g. observation:23132
      val kafkaValue = resource.toJson
      //KafkaClient.sendString(OnfhirConfig.kafkaTopic, kafkaKey, kafkaValue)
      Future.apply()
    } else Future.apply(Unit)*/
  }

  /**
    * Event triggered when a resource is deleted
    * @param rtype  Resource type
    * @param rid    Deleted resource id
    * @return
    */
  private def resourceDeleted(rtype:String, rid:String):Unit = {
    FhirEventBus.publish(ResourceDeleted(rtype, rid))
    /*if(OnfhirConfig.isKafkaEnabled(rtype)) {
      logger.debug(s"Sending resource deletion to Kafka...")
      val kafkaKey   = rtype.toLowerCase + ":" + rid + ":" + "delete" //e.g. observation:23132:delete
      val kafkaValue = ""//FHIRUtil.clearExtraFields(document).fromBson.toJson //TODO
      //KafkaClient.sendString(OnfhirConfig.kafkaTopic, kafkaKey, kafkaValue)
      Future.apply()
    }
    else Future.apply(Unit)*/
  }
}

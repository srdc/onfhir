package io.onfhir.db

import io.onfhir.api._
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.{OnfhirConfig, ResourceConf, SearchParameterConf}
import io.onfhir.exception.{BadRequestException, InvalidParameterException}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters._
import org.slf4j.{Logger, LoggerFactory}

/**
 * MongoDB query builder for FHIR search mechanisms for the given configuration of resource type
 * @param resourceConf REST Configuration for research type
 */
class ResourceQueryBuilder(resourceConf: ResourceConf) extends IFhirQueryBuilder {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Construct query for normal search parameters (token, reference, quantity, etc)
   *
   * @param parameter           Parsed search parameter
   * @param searchParameterConf Configuration for the search parameter
   * @return
   */
  def constructQueryForSimpleParameter(parameter:Parameter, searchParameterConf:SearchParameterConf):Bson = {
    parameter.suffix match {
      //Missing modifier is common, so handle it here
      case FHIR_PREFIXES_MODIFIERS.MISSING =>
        if (searchParameterConf.ptype == FHIR_PARAMETER_TYPES.COMPOSITE)
          throw new InvalidParameterException(s"Missing modifier cannot be used with composite parameters!")
        if(parameter.valuePrefixList.length != 1)
          throw new InvalidParameterException("Invalid parameter value for :missing modifier, either true or false should be provided!")
        val paths = searchParameterConf.extractElementPaths(withArrayIndicators = true)
        constructQueryForMissingModifier(paths.map(FHIRUtil.normalizeElementPath), parameter.valuePrefixList.head._2)
      //Any other modifier or no modifier
      case _ =>
        //For each possible path, construct queries
        val queries:Seq[Bson] =
          searchParameterConf
            .extractElementPathsTargetTypesAndRestrictions(withArrayIndicators = true)
            .map {
              case (path, targetType, Nil) =>
                constructQueryForSimpleWithoutRestriction(parameter, searchParameterConf, path, targetType)
              //If there is a restriction on the search or search on extension we assume it is a direct field match e.g phone parameter on Patient
              //e.g. f:PlanDefinition/f:relatedArtifact[f:type/@value='depends-on']/f:resource -->  path = relatedArtifact[i].resource, restriction = @.type -->  (relatedArtifact[i], resource, type)
              //e.g. f:OrganizationAffiliation/f:telecom[system/@value='email']  --> path => telecom[i] , restriction = system --> (telecom[i], "", system)
              case (path, targetType, restrictions) =>
                //Find the index in the path for given restrictions
                val pathParts = path.split('.').toIndexedSeq
                val indexOfRestrictions = FHIRUtil.findIndexOfRestrictionsOnPath(pathParts, restrictions)
                constructQueryForSimpleWithRestrictions(parameter, searchParameterConf, pathParts, targetType, indexOfRestrictions)
            }
        //merge queries
        mergeQueriesFromDifferentPaths(parameter.suffix, queries)
    }
  }

  /**
   * Handles missing modifier
   *
   * @param pathList absolute path of the parameter
   * @param bool     boolean value (:missing=  true | false)
   * @return         BsonDocument for the query
   */
  def constructQueryForMissingModifier(pathList: Seq[String], bool: String): Bson = {
    bool match {
      case MISSING_MODIFIER_VALUES.STRING_TRUE =>
        //All paths should be missing
        pathList.map(path => Filters.exists(path, exists = false)) match {
          case Seq(single) => single
          case multiple => Filters.and(multiple:_*)
        }
      case MISSING_MODIFIER_VALUES.STRING_FALSE =>
        //One of the path should exist
        pathList.map(path => Filters.exists(path, exists = true)) match {
          case Seq(single) => single
          case multiple => Filters.or(multiple: _*)
        }
      case _ =>
        throw new InvalidParameterException("Invalid parameter value for :missing modifier, either true or false should be provided!")
    }
  }

  /**
   * Construct MongoDB query for simple parameter
   * @param parameter             Parsed parameter details
   * @param searchParameterConf   Corresponding search parameter configuration
   * @param path                  Path to the element to run the search
   * @param targetType            Data type of target element
   * @return
   */
  private def constructQueryForSimpleWithoutRestriction(parameter:Parameter, searchParameterConf:SearchParameterConf, path:String, targetType:String):Bson = {
    parameter.paramType match {
      case FHIR_PARAMETER_TYPES.NUMBER =>
        NumberQueryBuilder.getQuery(parameter.valuePrefixList, path, targetType)
      case FHIR_PARAMETER_TYPES.QUANTITY =>
        QuantityQueryBuilder.getQuery(parameter.valuePrefixList, path, targetType)
      case FHIR_PARAMETER_TYPES.DATE =>
        DateQueryBuilder.getQuery(parameter.valuePrefixList, path, targetType)
      case FHIR_PARAMETER_TYPES.TOKEN =>
        TokenQueryBuilder.getQuery(parameter.valuePrefixList.map(_._2), parameter.suffix, path, targetType)
      case FHIR_PARAMETER_TYPES.STRING =>
        StringQueryBuilder.getQuery(parameter.valuePrefixList.map(_._2), parameter.suffix, path, targetType)
      case FHIR_PARAMETER_TYPES.URI =>
        UriQueryBuilder.getQuery(parameter.valuePrefixList.map(_._2), parameter.suffix, path, targetType)
      case FHIR_PARAMETER_TYPES.REFERENCE =>
        getReferenceQueryBuilder()
          .getQuery(parameter.valuePrefixList.map(_._2), parameter.suffix, path, targetType, searchParameterConf.targets)
    }
  }

  /**
   * Construct MongoDB query for simple parameter with extra restrictions
   * e.g. Search parameters on extensions
   * e.g. Search parameters like phone on Patient type --> Target path : Patient.telecom.where(system='phone')
   * @param parameter               Parsed parameter details
   * @param searchParameterConf     Corresponding search parameter configuration
   * @param pathParts               Parsed path to the element to run the search
   * @param targetType              Data type of target element
   * @param indexOfRestrictions     Parsed and indexed restrictions e.g. 0 -> Seq(system -> phone)
   * @return
   */
  def constructQueryForSimpleWithRestrictions(parameter:Parameter,
                                              searchParameterConf:SearchParameterConf,
                                              pathParts:Seq[String],
                                              targetType:String,
                                              indexOfRestrictions:Seq[(Int, Seq[(String, String)])]
                                             ): Bson = {

    //Get the next restriction in the path
    val restriction = indexOfRestrictions.head
    //Find the parent path to this element e.g. telecom[i]
    val parentPath = pathParts.slice(0, restriction._1 + 1).mkString(".")
    //Find child paths
    val childPathParts = pathParts.drop(restriction._1 + 1)
    //Split the parent path
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(parentPath)
    //If this is the last restriction, return the query by merging the actual query and query coming from restriction
    if (indexOfRestrictions.tail.isEmpty) {
      val childPath = childPathParts.mkString(".")
      val mainQuery =
        Filters.and(
          (
            //Restriction queries e.g. extension.where(url='')
            restriction._2.map(r => Filters.eq(FHIRUtil.mergeElementPath(queryPath, r._1), r._2)) :+
              //Actual query
              constructQueryForSimpleWithoutRestriction(parameter, searchParameterConf,  FHIRUtil.mergeElementPath(queryPath, childPath), targetType)
            ): _*
        )
      getFinalQuery(elemMatchPath, mainQuery)
    }
    //If there are still restrictions on child paths, continue applying restrictions
    else {
      val mainQuery =
        and(
          (
            //Restriction queries
            restriction._2.map(r => Filters.eq(FHIRUtil.mergeElementPath(queryPath, r._1), r._2)) :+
              constructQueryForSimpleWithRestrictions(parameter, searchParameterConf, childPathParts, targetType, indexOfRestrictions.tail)
            ): _*
        )
      elemMatch(elemMatchPath.get, mainQuery)
    }
  }

  /**
    * Builds query for composite parameters wrt to provided
    * list of paths and parameter object
    * @param parameter Parameter object
    * @param searchParamConf Configuration of search parameter corresponding to Composite parameter
    * @param validQueryParameters All query parameters supported for this resource
    */
  private def constructQueryForCompositeParameter(parameter:Parameter, searchParamConf:SearchParameterConf, validQueryParameters:Map[String, SearchParameterConf]):Bson = {
    //Extract the values split by $
    val valuesArr = parameter.valuePrefixList.map(_._2.split('$'))
    // name of the combined parameters
    val compositeParams = searchParamConf.targets
    //If for any value, there is a mismatching
    if(!valuesArr.forall(_.length  == compositeParams.size))
      throw new InvalidParameterException(s"Invalid query value supplied for composite parameter ${parameter.name}, it needs ${compositeParams.size} values to query seperated by dollar sign!")

    //Root paths for them to search on
    val commonPathsAndTargetTypes = searchParamConf.extractElementPathsAndTargetTypes(withArrayIndicators = true)
    val normalCommonPaths = commonPathsAndTargetTypes.filter(_._1 != "").map(_._1)

    val queries = valuesArr.flatMap(value =>
      //For each common alternative path
      commonPathsAndTargetTypes.map { case (commonPath, targetType) =>
        //Find the path to the last array field
        val finalCommonPath =
          //If the path goes over an array element, but the last path item of common path is not array, find this last array item and make it common path (mongodb $elemMatch works that way)
          if(!commonPath.endsWith("[i]") && commonPath.contains("[i]")){
            val i = commonPath.split(".").indexWhere(_.endsWith("[i]"))
            commonPath.slice(0, i+1).mkString(".")
          } else
            commonPath

        //Construct query for each composite
        val queriesForEachCombParam =
          compositeParams.zipWithIndex.map { case (compParamName, i) =>
            //Get the definition of combined search parameter
            val compParamConf = validQueryParameters(compParamName)
            //Parse value again as it may indicate a prefix (we don't parse prefixes in Composite at the beginning)
            val (queryPartPrefix, queryPartValue) = FHIRSearchParameterValueParser.parseSimpleValue(value.apply(i), compParamConf.ptype).head

            //Find out the subpaths of this param after the common path
            val subpathsAfterCommonPathAndTargetTypes = commonPath match {
              //If the search is on root Resource, find the paths that does not belong to any common path listed
              case "" if targetType == "Resource" =>
                compParamConf
                  .extractElementPathsAndTargetTypes(withArrayIndicators = true)
                  .filter(p => !normalCommonPaths.exists(p._1.startsWith))
              //Otherwise, find the paths that has this commonPath as prefix
              case _ =>
                compParamConf
                  .extractElementPathsAndTargetTypes(withArrayIndicators = true)
                  .filter(p => p._1.startsWith(finalCommonPath))
                  .map(p =>
                    //Get rid of from common path
                    (p._1.drop(finalCommonPath.length) match {
                      case startWithDot if startWithDot.headOption.contains('.') => startWithDot.tail
                      case oth => oth
                    }) -> p._2
                  )
            }
            //Construct query for this param
            val queriesForCombParam =
              subpathsAfterCommonPathAndTargetTypes.map {
                case (path, spTargetType) =>
                  val childParam = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, compParamConf.ptype, searchParamConf.pname, Seq(queryPartPrefix->queryPartValue), parameter.suffix)
                  constructQueryForSimpleWithoutRestriction(childParam, searchParamConf, path, spTargetType)
              }
            orQueries(queriesForCombParam)
          }
        //Queries on all combined components should hold
        val mainQuery = and(queriesForEachCombParam:_*)

        if(finalCommonPath.endsWith("[i]"))
          elemMatch(FHIRUtil.normalizeElementPath(commonPath), mainQuery)
        else
          mainQuery
      }
    )

    //OR the queries for multiple values and multiple common paths
    orQueries(queries)
  }

  /**
    * Construct query for compartment search
    * @param resourceType Resource type
    * @param compartmentParam Compartment search parameter
    * @param validQueryParameters All validquery parameters for the resource type
    * @return
    */
  def constructQueryForCompartment(resourceType:String, compartmentParam:Parameter, validQueryParameters:Map[String, SearchParameterConf]):Bson = {
    //Resource type and value
    val compartmentType = compartmentParam.valuePrefixList.head._1
    val compartmentId = compartmentParam.valuePrefixList.head._2
    val reference =  compartmentType + "/" + compartmentId
    val params = compartmentParam.chain.map(_._2)
    val queries = params.map{
      case "_id" =>
        val parameter = Parameter(FHIR_PARAMETER_CATEGORIES.SPECIAL, FHIR_PARAMETER_TYPES.TOKEN, FHIR_SEARCH_SPECIAL_PARAMETERS.ID, Seq("" -> compartmentId))
        constructQueryForIds(parameter)
      case p =>
        val parameter = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.REFERENCE, p, Seq("" -> reference))
        constructQueryForSimpleParameter(parameter, validQueryParameters.apply(p))
    }
    //OR the queries for multiple values
    if(queries.length > 1) or(queries:_*) else queries.head
  }

  /**
    * Construct query for Normal category parameters
    * @param param Parsed search parameter
    * @param searchParamConf Corresponding search parameter configuration
    * @param validQueryParameters Valid query parameters for the resource type
    */
  def constructQueryForNormal(param:Parameter, searchParamConf:SearchParameterConf, validQueryParameters:Map[String, SearchParameterConf]): Bson ={
    param.paramType match {
      case FHIR_PARAMETER_TYPES.COMPOSITE => constructQueryForCompositeParameter(param, searchParamConf, validQueryParameters)
      case _ => constructQueryForSimpleParameter(param, searchParamConf)
    }
  }

  /**
    * Construct query for FHIR _id query
    * @param param Parsed search parameter
    * @return
    */
  def constructQueryForIds(param:Parameter):Bson = {
    val rids = param.valuePrefixList.map(_._2).toSet
    DocumentManager.ridsQuery(rids)
  }


  /**
    * Build a revInclude query
    * @param revIncludeReferences References of the resources, and optionally canonical url and versions that are the result of main query e.g. Goal/234234324 or (Questionnaire/5, http://example.com/Questionnaire/cgs, v1.0)
    * @param parameterConf Parameter configuration for the revInclude parameter
    * @return
    */
  def constructQueryForRevInclude(revIncludeReferences:Seq[(String, Option[String], Option[String])], parameterConf: SearchParameterConf):Bson = {
    val queries =
      parameterConf.targetTypes.head match {
        case FHIR_DATA_TYPES.REFERENCE =>
          parameterConf.paths.map {
            case normalPath: String =>
              getReferenceQueryBuilder().getQuery(revIncludeReferences.map(_._1), "", normalPath, FHIR_DATA_TYPES.REFERENCE, parameterConf.targets)
          }
        case FHIR_DATA_TYPES.CANONICAL =>
          parameterConf.paths.map {
            case normalPath: String =>
              val canonicalRefs =
                revIncludeReferences
                  .filter(r => r._2.isDefined)
                  .map(r => s"${r._2.get}${r._3.map(v => s"|$v").getOrElse("")}")

              ReferenceQueryBuilder.getQueryOnCanonicals(canonicalRefs, "", normalPath)
          }
      }
    //Merge queries with or (for multiple paths parameters)
    orQueries(queries)
  }

  /**
    * Construct query part on shard field for sharded clusters
    * @param rtype      Resource type
    * @param resource   Resource content
    * @return
    */
  def constructShardingQuery(resource: Resource):Option[Bson] = {
    if(!OnfhirConfig.mongoShardingEnabled)
      None
    else
      fhirConfig.shardKeys
        .get(resourceConf.resource) //get shard key, if exist
        .flatMap(_.headOption) //Only take the first, as we support single shard key
        .filterNot(_ == FHIR_SEARCH_SPECIAL_PARAMETERS.ID) // if shard is on id, we don't need this query, id is already used
        .flatMap(shardParamName =>
          fhirConfig.getSupportedParameters(resourceConf.resource).get(shardParamName)//Try to find the param configuration
        )
        .flatMap(shardParam =>
          shardParam.ptype match {
            //For we only support sharding on reference search parameters apart from Resource.id
            case FHIR_PARAMETER_TYPES.REFERENCE =>
              //This is actual path for the element
              val elementPath = s"${shardParam.extractElementPaths().head}.${FHIR_COMMON_FIELDS.REFERENCE}"
              //This is what we keep in database
              val shardElementPath =  s"$elementPath.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}"
              FHIRUtil.extractValueOptionByPath[String](resource, elementPath) match {
                case None => throw new BadRequestException(Seq(
                  OutcomeIssue(
                    FHIRResponse.SEVERITY_CODES.FATAL,
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some(s"Collection for the resource type ${resourceConf.resource} is sharded on path $elementPath! Therefore it is required, but the resource does not include the field! Please consult with the maintainer of the OnFhir repository."),
                    Seq(elementPath)
                  )
                ))
                case Some(refValue) =>
                  val (_,rrtype,rid,version) = FHIRUtil.parseReferenceValue(refValue)
                  Some(
                    and(
                      equal(s"$elementPath.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}", rid),
                      equal(s"$elementPath.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE}", rrtype)
                    )
                  )
              }
            case _ => None
          }
       )
  }

  /**
   * Construct ReferenceQueryBuilder
   * @return
   */
  private def getReferenceQueryBuilder(): ReferenceQueryBuilder = {
    new ReferenceQueryBuilder(onlyLocalReferences = resourceConf.referencePolicies.contains("local"))
  }

}


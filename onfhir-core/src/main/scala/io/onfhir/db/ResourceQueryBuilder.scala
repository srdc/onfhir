package io.onfhir.db

import io.onfhir.api._
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.{OnfhirConfig, SearchParameterConf}
import io.onfhir.exception.{BadRequestException, InvalidParameterException}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters._
import org.slf4j.{Logger, LoggerFactory}

/**
  * MongoDB query builder for FHIR search mechanisms
  */
object ResourceQueryBuilder {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Construct query for Normal Category Search Parameters
    * @param parameter Parsed search parameter
    * @param searchParameterConf Configuration for the search parameter
    * @return
    */
  def constructQueryForSimpleParameter(parameter:Parameter, searchParameterConf:SearchParameterConf):Bson = {
    //If parameter is on a extension, we should handle it differently
    /*if(searchParameterConf.onExtension) {
      constructQueryForExtensionParameter(parameter, searchParameterConf)
    } else {*/
      //This part handles search with simple query parameters
      val queries =
        parameter
          .valuePrefixList //Go over each value to OR them
          .map(pv => {
            //There exists either a prefix or modifier, not both
            val modifierOrPrefix = if (parameter.suffix == "") pv._1 else parameter.suffix
            //Handle common modifiers here
            modifierOrPrefix match {
              //Missing modifier is common
              case FHIR_PREFIXES_MODIFIERS.MISSING =>
                if(searchParameterConf.ptype == FHIR_PARAMETER_TYPES.COMPOSITE)
                  throw new InvalidParameterException(s"Missing modifier cannot be used with composite parameters!")
                val paths = searchParameterConf.extractElementPaths(withArrayIndicators = true)
                PrefixModifierHandler.missingHandler(paths.map(FHIRUtil.normalizeElementPath), parameter.valuePrefixList.head._2)

              //Not modifier is common
              case FHIR_PREFIXES_MODIFIERS.NOT =>
                not(constructQueryForSimple(pv._2, parameter.paramType, "", searchParameterConf))

              //Otherwise
              case _ =>
                constructQueryForSimple(pv._2, parameter.paramType, modifierOrPrefix, searchParameterConf)
            }
          })

      if (queries.length > 1) or(queries: _*) else queries.head
   // }
  }

  /**
    * Construct Query for simple search
    * @param value Query value
    * @param paramType Search type
    * @param modifierOrPrefix Modifier or prefix
    * @param searchParameterConf Search parameter configuration
    * @return
    */
  private def constructQueryForSimple(value:String, paramType:String, modifierOrPrefix:String, searchParameterConf:SearchParameterConf) = {
    //For each possible path, construct queries
    val queries =
      searchParameterConf
        .extractElementPathsTargetTypesAndRestrictions(withArrayIndicators = true)
        .map {
          case (path, targetType, Nil) =>
            SearchUtil
              .typeHandlerFunction(paramType)(value, modifierOrPrefix, path, targetType, searchParameterConf.targets)
          //If there is a restriction on the search we assume it is a direct field match e.g phone parameter on Patient
          //e.g. f:PlanDefinition/f:relatedArtifact[f:type/@value='depends-on']/f:resource -->  path = relatedArtifact[i].resource, restriction = @.type -->  (relatedArtifact[i], resource, type)
          //e.g. f:OrganizationAffiliation/f:telecom[system/@value='email']  --> path => telecom[i] , restriction = system --> (telecom[i], "", system)
          case (path, targetType, restrictions) =>
            val pathParts = path.split('.')
            val indexOfRestrictions  = FHIRUtil.findIndexOfRestrictionsOnPath(pathParts, restrictions)
            SearchUtil.queryWithRestrictions(pathParts, indexOfRestrictions, value, paramType, targetType, modifierOrPrefix, searchParameterConf.targets)
        }
    //OR the queries for multiple paths
    if(queries.size > 1) or(queries:_*) else queries.head
  }


  /**
    * Construct query for Extension Category Search Parameters
    * @param parameter Parsed search parameter
    * @param searchParameterConf Configuration for the search parameter
    * @return
    */
  private def constructQueryForExtensionParameter(parameter:Parameter, searchParameterConf:SearchParameterConf):Bson = {
    val queries = parameter
      .valuePrefixList //Go over each value to OR them
      .flatMap(pv => {
        //Extension path defined for search
        val paths = searchParameterConf.paths.asInstanceOf[Seq[Seq[(String, String)]]]
        paths.map(eachPath => {
          SearchUtil.extensionQuery(pv._2, eachPath, searchParameterConf.ptype, pv._1, searchParameterConf.targets)
        })
      })

    //OR the queries for multiple paths
    if(queries.length > 1) or(queries:_*) else queries.head
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
                  .filter(p => p._1.startsWith(commonPath))
                  .map(p => p._1.replace(commonPath +".", "") -> p._2)
            }
            //Construct query for this param
            val queriesForCombParam =
              subpathsAfterCommonPathAndTargetTypes.toSeq.map {
                case (path, spTargetType) =>
                  //Run query for each path
                  SearchUtil
                    .typeHandlerFunction(compParamConf.ptype)(queryPartValue, queryPartPrefix, path, spTargetType, compParamConf.targets) //Construct query for each path
              }
            if(queriesForCombParam.length > 1) or(queriesForCombParam:_*) else queriesForCombParam.head
          }
        //Queries on all combined components should hold
        val mainQuery = and(queriesForEachCombParam:_*)
        if(commonPath.endsWith("[i]"))
          elemMatch(FHIRUtil.normalizeElementPath(commonPath), mainQuery)
        else
          mainQuery
      }
    )

    //OR the queries for multiple values and multiple common paths
    if(queries.length > 1) or(queries:_*) else queries.head
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
    val reference =  compartmentParam.valuePrefixList.head._1 + "/" + compartmentParam.valuePrefixList.head._2
    val params = compartmentParam.chain.map(_._2)
    val queries = params.map(p => {
      val parameter = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.REFERENCE, p, Seq("" -> reference))
      constructQueryForSimpleParameter(parameter, validQueryParameters.apply(p))
    })
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
    * @param revIncludeReferences References of the resources that are the result of main query e.g. Goal/234234324
    * @param parameterConf Parameter configuration for the revInclude parameter
    * @return
    */
  def constructQueryForRevInclude(revIncludeReferences:Seq[String], parameterConf: SearchParameterConf):Bson = {
    val queries = parameterConf.paths.map {
      case normalPath: String =>
        val queries = revIncludeReferences.map(revIncludeRef =>
          SearchUtil.typeHandlerFunction(FHIR_PARAMETER_TYPES.REFERENCE)(revIncludeRef, "", normalPath, FHIR_DATA_TYPES.REFERENCE, Nil)
        )
        if(queries.length > 1) or(queries:_*) else queries.head
    }

    //Merge queries with or (for multiple paths parameters)
    if(queries.size > 1) or(queries:_*) else queries.head
  }

  /**
    * Construct query part on shard field for sharded clusters
    * @param rtype      Resource type
    * @param resource   Resource content
    * @return
    */
  def constructShardingQuery(rtype:String, resource: Resource):Option[Bson] = {
    if(!OnfhirConfig.mongoShardingEnabled)
      None
    else
      fhirConfig.shardKeys
        .get(rtype) //get shard key, if exist
        .flatMap(_.headOption) //Only take the first, as we support single shard key
        .filterNot(_ == FHIR_SEARCH_SPECIAL_PARAMETERS.ID) // if shard is on id, we don't need this query, id is already used
        .flatMap(shardParamName =>
          fhirConfig.getSupportedParameters(rtype).get(shardParamName)//Try to find the param configuration
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
                    Some(s"Collection for the resource type $rtype is sharded on path $elementPath! Therefore it is required, but the resource does not include the field! Please consult with the maintainer of the OnFhir repository."),
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
}


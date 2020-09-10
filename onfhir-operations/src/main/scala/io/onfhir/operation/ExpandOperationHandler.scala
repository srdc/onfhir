package io.onfhir.operation

import java.util.UUID

import akka.http.scaladsl.model.{DateTime, StatusCodes, Uri}

import io.onfhir.api.Resource
import io.onfhir.api.model._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIROperationHandlerService
import io.onfhir.api.util.FHIRUtil
import io.onfhir.db.ResourceManager
import io.onfhir.exception._
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JNothing, JObject, JValue}
import org.json4s.JsonDSL._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

/**
  * Created by mustafa on 10/26/2017.
  * Handles the $expand operation defined on ValueSet resource.
  */
class ExpandOperationHandler extends FHIROperationHandlerService {
  private val logger: Logger = LoggerFactory.getLogger("ExpandOperationHandler")

  final val RESOURCE_VALUESET: String = "ValueSet"
  final val VALUESET_COMPOSE: String = "compose"
  final val VALUESET_EXPANSION: String = "expansion"
  final val SEARCHPARAM_ID: String = "_id"
  final val SEARCHPARAM_URL: String = "url"
  final val EXPAND_PARAM_FILTER: String = "filter"

  /**
    * Execute the operation and prepare the output parameters for the operation
    *
    * @param operationName    Operation name as defined after '$' symbol e.g. meta-add
    * @param operationRequest Operation Request including the parameters
    * @param resourceType     The resource type that operation is called if exists
    * @param resourceId       The resource id that operation is called if exists
    * @return The response containing the Http status code and the output parameters
    */
  override def executeOperation(operationName: String, operationRequest: FHIROperationRequest, resourceType: Option[String], resourceId: Option[String]): Future[FHIROperationResponse] = {
    // Create ValueSet search parameters from the relevant operation parameters
    val searchParams: Map[String, List[String]] =
    if (resourceId.isDefined)
      Map(SEARCHPARAM_ID -> List(resourceId.get))
      //searchParams.put(SEARCHPARAM_ID, List(Parameter(FHIR_PARAMETER_TYPES.TOKEN, SEARCHPARAM_ID, resourceId.get)))
    else if (operationRequest.extractParamValue[String](SEARCHPARAM_ID).isDefined)
      //searchParams.put(SEARCHPARAM_ID, List(Parameter(FHIR_PARAMETER_TYPES.TOKEN, SEARCHPARAM_ID, operationRequest.extractParam[String](SEARCHPARAM_ID).get)))
      Map(SEARCHPARAM_ID -> List(operationRequest.extractParamValue[String](SEARCHPARAM_ID).get))
    else if (operationRequest.extractParamValue[String](SEARCHPARAM_URL).isDefined)
      //searchParams.put(SEARCHPARAM_URL, List(Parameter(FHIR_PARAMETER_TYPES.URI, SEARCHPARAM_URL, operationRequest.extractParam[String](SEARCHPARAM_URL).get)))
      Map(SEARCHPARAM_URL -> List(operationRequest.extractParamValue[String](SEARCHPARAM_URL).get))
    else {
      logger.debug("ValueSet cannot be identified, return 400 BadRequest...")
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.PROCESSING,
          None,
          Some("$expand operation at the type level (no ID specified) requires an identifier as part of the request"),
          Nil
        )
      ))
    }

    handleExpand(searchParams, operationRequest)
  }

  def handleExpand(queryParams: Map[String, List[String]], operationRequest: FHIROperationRequest): Future[FHIROperationResponse] = {

    val searchParams:List[Parameter] = FHIRSearchParameterValueParser.parseSearchParameters(RESOURCE_VALUESET, queryParams)

    ResourceManager
      .queryResources(RESOURCE_VALUESET, searchParams, count = 1, excludeExtraFields = true).map {
      case (0, _) =>
        logger.debug("resource not found, return 404 NotFound...")
        throw new NotFoundException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.INFORMATION,
            FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
            None,
            if (queryParams.contains(SEARCHPARAM_ID))
              Some(s"Resource with type '${RESOURCE_VALUESET}', id '${queryParams.apply(SEARCHPARAM_ID).head}' not found...")
            else
              Some(s"Resource with type '${RESOURCE_VALUESET}' not found...")
            ,
            Nil
          )
        ))
      //If there is a match
      // TODO: In case a ValueSet is queried with 'url', there can be multiple matching ValueSets (there shall not be!). But for the moment, we are just returning the first
      case (total, Seq(foundResource)) =>
        //Build expansion
        val resultingResource = buildExpansion(foundResource, operationRequest)
        val (rid, currentVersion, lastModified)  = FHIRUtil.extractBaseMetaFields(resultingResource)
        // 2.2) return the ValueSet
        logger.debug("resource found, returning...")
        val fhirResponse = new FHIROperationResponse(
          StatusCodes.OK, //HTTP Status code
          Some(Uri(FHIRUtil.resourceLocationWithVersion(RESOURCE_VALUESET, rid, currentVersion))),
          Some(lastModified), //HTTP Last-Modified header
          Some(currentVersion)) //HTTP Etag header

        fhirResponse.setResponse(resultingResource)
        fhirResponse
    }
/*
    // Build the query from common and resource specific parameters
    val query = ResourceQueryBuilder.buildCompleteQuery(RESOURCE_VALUESET, searchParams)

    // Search the resources
    val searchFuture = ResourceManager.searchDocument(RESOURCE_VALUESET, query, Config.fhirDefaultPageCount.toInt, 1, searchParams)

    // Count the resources for the query (without paging)
    val countFuture = ResourceManager.resourceCount(RESOURCE_VALUESET, query)

    // Run these queries simultaneously
    val aggregatedFutures = for {
      document <- searchFuture
      total <- countFuture
    } yield (total, document)

    aggregatedFutures map { futures =>
      val totalCount = futures._1
      val resources = futures._2._1 //actual matched documents

      // 1) ValueSet not found - return 404
      if (totalCount < 1) {
        logger.debug("resource not found, return 404 NotFound...")
        throw new NotFoundException(Seq(
          OutcomeIssue(
            ResultSeverityEnum.INFORMATION.getCode,
            FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
            None,
            if (searchParams.get(SEARCHPARAM_ID).isDefined)
              Some(s"Resource with type '${RESOURCE_VALUESET}', id '${searchParams.get(SEARCHPARAM_ID).get.head.value}' not found...")
            else
              Some(s"Resource with type '${RESOURCE_VALUESET}' not found...")
            ,
            Nil
          )
        ))
      }
      // 2) ValueSet located
      else {
        // TODO: In case a ValueSet is queried with 'url', there can be multiple matching ValueSets (there shall not be!). But for the moment, we are just returning the first
        val document = resources.head
        val currentVersion = FHIRUtil.extractVersionIdFromBson(document)
        val id = FHIRUtil.extractIdFromBson(document)
        val location = Uri(FHIRUtil.resourceLocationWithVersion(RESOURCE_VALUESET, id, currentVersion))
        val lastModified = DateTime(FHIRUtil.extractLastUpdateTimeFromBson(document))

        // 2.1) convert bson into regular map format
        val resource = FHIRUtil.clearExtraFields(document).fromBson

        val resultingResource = buildExpansion(resource, operationRequest)

        // 2.2) return the ValueSet
        logger.debug("resource found, returning...")
        val fhirResponse = new FHIROperationResponse(
          StatusCodes.OK, //HTTP Status code
          Some(location),
          Some(lastModified), //HTTP Last-Modified header
          Some(currentVersion)) //HTTP Etag header

        fhirResponse.setResponse(resultingResource)
        fhirResponse
      }
    }*/
  }

  def buildExpansion(valueSet:Resource, operationRequest: FHIROperationRequest):Resource = {
    val compose = (valueSet \ VALUESET_COMPOSE).extractOrElse(JObject())

    val filterKeys: Seq[String] = operationRequest.extractParamValue[String](EXPAND_PARAM_FILTER).getOrElse("").split(",")


    // 1) First, filter all the matching concepts
    val matchingList:Seq[JObject] = compose \ "include" match {
      case includes:JArray => includes.arr.flatMap(include => {
        include \ "concept" match {
          case concepts:JArray => concepts.arr.flatMap(concept => {
             if(containsAll(concept, filterKeys)){
               var matching = JObject()
               (include \ "system").extractOpt[String].foreach(s => {matching = matching ~ ("system" -> s)})
               (include \ "version").extractOpt[String].foreach(v => {matching = matching ~ ("version" -> v)})
               (concept \ "code").extractOpt[String].foreach(c => {matching = matching ~ ("code" -> c)})
               (concept \ "display").extractOpt[String].foreach(d => {matching = matching ~ ("display" -> d)})
               (concept \ "extension").extractOpt[JArray].foreach(arr => {matching = matching ~ ("extension" -> arr)})
               Some(matching)
             } else None
          })
          case _ => Nil
        }
      })
      case _ => Nil
    }

    /*val matchingList = ListBuffer[mutable.LinkedHashMap[String, Any]]()
    // 1) First, filter all the matching concepts
    if(compose.contains("include")) {
      compose("include").asInstanceOf[ListBuffer[Resource]] foreach { include =>
        if(include.contains("concept")) {
          include("concept").asInstanceOf[ListBuffer[Resource]] foreach { concept =>
            if (containsAll(concept, filterKeys)) {
              val matching = mutable.LinkedHashMap[String, Any]()
              if (include.contains("system")) matching("system") = include("system")
              if (include.contains("version")) matching("version") = include("version")
              if (concept.contains("code")) matching("code") = concept("code")
              if (concept.contains("extension")) matching("extension") = concept("extension")
              if (concept.contains("display")) matching("display") = concept("display")
              matchingList.append(matching)
            }
          }
        }
      }
    }*/

    var resultValueSet = valueSet
    // 2) If there is any matching, then create an expansion
    if(matchingList.nonEmpty) {
      // 2.1) First, remove if any expansion exists in the full ValueSet
      resultValueSet = resultValueSet.removeField(_._1 == VALUESET_EXPANSION).asInstanceOf[JObject]
      //valueSet.remove(VALUESET_EXPANSION)

      // 2.2) Then create the new expansion
      val expansion =
          ("identifier" -> UUID.randomUUID().toString) ~
          ("timestamp" -> (DateTime.now.toIsoDateTimeString + "Z")) ~
          ("total" -> matchingList.size) ~
          ("contains" -> matchingList)

      resultValueSet = resultValueSet ~ (VALUESET_EXPANSION -> expansion)

      /*val expansion = mutable.LinkedHashMap[String, Any]()
      expansion.put("identifier", UUID.randomUUID().toString)
      expansion.put("timestamp", DateTime.now.toIsoDateTimeString + "Z")
      expansion.put("total", matchingList.size)
      expansion.put("contains", matchingList)
      valueSet.put(VALUESET_EXPANSION, expansion)*/
    }

    // In any case, remove the compose element completely
    resultValueSet = resultValueSet.removeField(_._1 == VALUESET_COMPOSE).asInstanceOf[JObject]
    //valueSet.remove(VALUESET_COMPOSE)
    resultValueSet
  }

  def containsAll(input:JValue, keys:Seq[String]): Boolean = {

    keys foreach (key =>
      if(!((input \ "display").extractOpt[String].getOrElse("").toLowerCase.contains(key.toLowerCase) ||
        (input \ "code").extractOpt[String].getOrElse("").toLowerCase.contains(key.toLowerCase)))
        return false
    )
    true
  }

}
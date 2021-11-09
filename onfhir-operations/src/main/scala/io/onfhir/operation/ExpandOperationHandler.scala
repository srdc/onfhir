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
  final val EXPAND_PARAM_LANGUAGE: String = "displayLanguage"

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
  }

  def buildExpansion(valueSet:Resource, operationRequest: FHIROperationRequest):Resource = {
    val compose = (valueSet \ VALUESET_COMPOSE).extractOrElse(JObject())

    val filterKeys: Seq[String] = operationRequest.extractParamValue[String](EXPAND_PARAM_FILTER).getOrElse("").split(",")
    val language: Option[String] = operationRequest.extractParamValue[String](EXPAND_PARAM_LANGUAGE)

    // 1) First, filter all the matching concepts
    val matchingList:Seq[JObject] = compose \ "include" match {
      case includes:JArray => includes.arr.flatMap(include => {
        include \ "concept" match {
          case concepts:JArray => concepts.arr.flatMap(concept => {
            val filteredConcept = filterConcept(concept, filterKeys, language)
             if(filteredConcept.nonEmpty){
               var matching = JObject()
               (include \ "system").extractOpt[String].foreach(s => {matching = matching ~ ("system" -> s)})
               (include \ "version").extractOpt[String].foreach(v => {matching = matching ~ ("version" -> v)})
               (filteredConcept.get \ "code").extractOpt[String].foreach(c => {matching = matching ~ ("code" -> c)})
               (filteredConcept.get \ "display").extractOpt[String].foreach(d => {matching = matching ~ ("display" -> d)})
               (filteredConcept.get \ "extension").extractOpt[JArray].foreach(arr => {matching = matching ~ ("extension" -> arr)})
               Some(matching)
             } else None
          })
          case _ => Nil
        }
      })
      case _ => Nil
    }

    var resultValueSet = valueSet
    // 2) If there is any matching, then create an expansion
    if(matchingList.nonEmpty) {
      // 2.1) First, remove if any expansion exists in the full ValueSet
      resultValueSet = resultValueSet.removeField(_._1 == VALUESET_EXPANSION).asInstanceOf[JObject]

      // 2.2) Then create the new expansion
      val expansion =
          ("identifier" -> UUID.randomUUID().toString) ~
          ("timestamp" -> (DateTime.now.toIsoDateTimeString + "Z")) ~
          ("total" -> matchingList.size) ~
          ("contains" -> matchingList)

      resultValueSet = resultValueSet ~ (VALUESET_EXPANSION -> expansion)
    }

    // In any case, remove the compose element completely
    resultValueSet = resultValueSet.removeField(_._1 == VALUESET_COMPOSE).asInstanceOf[JObject]
    //valueSet.remove(VALUESET_COMPOSE)
    resultValueSet
  }

  def filterConcept(input:JValue, keys:Seq[String], language: Option[String]): Option[JObject] = {

    var output = JObject()
    (input \ "code").extractOpt[String].foreach(c => {output = output ~ ("code" -> c)})
    (input \ "display").extractOpt[String].foreach(d => {output = output ~ ("display" -> d)})
    (input \ "extension").extractOpt[JArray].foreach(arr => {output = output ~ ("extension" -> arr)})

    // If there is a language filter, update the display attribute accordingly
    if (language.nonEmpty) {
      val newDisplay: Option[String] = input \ "designation" match {
        case designations: JArray => designations.arr.filter(designation => {
          (designation \ "language").extractOpt[String].getOrElse("").equals(language.get)
        }).map(designation => (designation \ "value").extractOpt[String].get).headOption
        case _ => None
      }

      if (newDisplay.nonEmpty) {
        output = output.removeField(_._1 == "display").asInstanceOf[JObject]
        output = output ~ ("display" -> newDisplay.get)
      }
    }

    keys foreach (key =>
      if(!((output \ "display").extractOpt[String].getOrElse("").toLowerCase.contains(key.toLowerCase) ||
        (output \ "code").extractOpt[String].getOrElse("").toLowerCase.contains(key.toLowerCase)))
        return None
    )
    Some(output)
  }

}

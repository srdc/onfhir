package io.onfhir.api.service

import akka.http.scaladsl.model.{StatusCodes, Uri}
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.FhirConfigurationManager.fhirValidator
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.PreconditionFailedException

import scala.concurrent.Future

/**
  * Handler for FHIR Create interaction
  * @param transactionSession If this interaction will be a part of a FHIR transaction
  */
class FHIRCreateService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession) {

  /**
    * Validate if interaction is supported for the request
    * @param fhirRequest
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    //1.1) Validate if "create" is supported for Resource Type
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.CREATE, fhirRequest.resourceType.get, fhirRequest.ifNoneExist.isDefined)
    //1.2) Check if resource type in the content match with the given type in URL
    FHIRApiValidator.validateResourceType(fhirRequest.resource.get, fhirRequest.resourceType.get)
    //1.3) Validate the conformance of resource
    fhirValidator.validateResource(fhirRequest.resource.get, fhirRequest.resourceType.get)
      .map(_ =>
        //1.4) Extra business rules if exist
        FHIRApiValidator.validateExtraRules(fhirRequest)
      )
  }

  /**
    * Perform the interaction
    * @param fhirRequest
    * @param isTesting
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean): Future[FHIRResponse] = {
    logger.debug(s"Requesting 'Create' for ${fhirRequest.resourceType} ...")
    createResource(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.ifNoneExist, fhirRequest.prefer, fhirRequest.resourceId, isTesting)
  }


  /**
    * Create a resource from the given content
    * @param resource Resource content
    * @param _type Resource type
    * @param ifNoneExist If-None-Exist header for conditional create
    * @param prefer Prefer header
    * @param testCreate - Indicates if it is the real create operation
    * @return
    */
  private def createResource(resource:Resource, _type:String, ifNoneExist:Option[String] = None, prefer:Option[String] = None, generatedId:Option[String]=None, testCreate:Boolean = false) : Future[FHIRResponse] = {
    //2) User requested a conditional update, so check the search parameters in
    //   If-None-Exist header with the current version
    if(ifNoneExist.isDefined){
      val parameters = Uri.Query(ifNoneExist.get).toMultiMap
      //Parse the parameters
      val parsedParameters = FHIRSearchParameterValueParser.parseSearchParameters( _type, parameters, prefer)
      // Search the resources; only we need mandatory elements e.g. id, meta
      ResourceManager
        .queryResources(_type, parsedParameters, count = 2, elementsIncludedOrExcluded = Some(true -> Set.empty), excludeExtraFields = true).flatMap(matchedResources =>
        matchedResources._1 match {
          //No matches: The server processes the create as normal
          case 0 => performCreate(resource, _type, prefer, generatedId, testCreate)
          //One Match: The server ignore the post and returns 200 OK
          case 1 => Future.apply(constructIgnore(prefer, _type, matchedResources._2.head))
          //Multiple matches: The server returns a 412 Precondition Failed error indicating the client's criteria were not selective enough
          case _  => throw new PreconditionFailedException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR, //fatal
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Your query is not selective enough, more than 1 document matches."),
              Nil
            )
          ))
        }
      )
    } else {
      //Otherwise perform the create normally
      performCreate(resource, _type, prefer, generatedId, testCreate)
    }
  }

  /**
    * Construct ignoring response
    * @param prefer prefer header
    * @return
    */
  private def constructIgnore(prefer:Option[String], _type:String, resource:Resource):FHIRResponse = {
    if(prefer.isDefined && prefer.get.equalsIgnoreCase(FHIR_HTTP_OPTIONS.FHIR_RETURN_OPERATION_OUTCOME))
      FHIRResponse.errorResponse(StatusCodes.OK, Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.INFORMATION, FHIRResponse.OUTCOME_CODES.INFORMATIONAL, None, Some("Your query matches a resource, so ignoring create..."), Nil)))
    else {
      //Extract the base meta fields
      val (id, version, lastModified) = FHIRUtil.extractBaseMetaFields(resource)
      val location = Uri(FHIRUtil.resourceLocationWithVersion(_type, id, version))

      FHIRResponse(
        StatusCodes.OK,
        None,
        Some(location), //HTTP Location header
        Some(lastModified), //HTTP Last-Modified header
        Some(version) // HTTP ETag header)
      )
    }
  }

  /**
    * Perform the create operation
    * @param resource Parsed resource
    * @param _type ResourceType
    * @param prefer Prefer header
    * @param testCreate - Indicates if it is the real create operation
    * @return
    */
  private def performCreate(resource:Resource, _type:String, prefer:Option[String] = None, generatedId:Option[String] = None, testCreate:Boolean = false): Future[FHIRResponse] = {
    if(!testCreate)
      ResourceManager.createResource(_type, resource, generatedId)(transactionSession) flatMap { case (newId, newVersion, lastModified, createdResource) =>
        Future.apply(FHIRResponse (
          StatusCodes.Created, //Http Status code
          FHIRUtil.getResourceContentByPreference(createdResource, prefer), //HTTP body
          Some(Uri(FHIRUtil.resourceLocationWithVersion(_type, newId, newVersion))), //HTTP Location header
          Some(lastModified), //HTTP Last-Modified header
          Some(newVersion) // HTTP ETag header
        ))
      }
    else
      Future(FHIRResponse(StatusCodes.OK))
  }
}
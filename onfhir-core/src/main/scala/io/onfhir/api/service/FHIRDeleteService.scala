package io.onfhir.api.service

import akka.http.scaladsl.model.{StatusCode, StatusCodes, Uri}
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.PreconditionFailedException

import scala.concurrent.Future

class FHIRDeleteService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession) {

  /**
    * Validate if interaction is supported for the request
    *
    * @param fhirRequest  FHIR Request
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply {
      if (fhirRequest.resourceId.isDefined)
        validateDeleteInteraction(fhirRequest.resourceType.get, fhirRequest.resourceId.get)
      else {
        validateConditionalDeleteInteraction(fhirRequest.resourceType.get, fhirRequest.queryParams, fhirRequest.prefer)
      }
    }
  }

  /**
    * Perform the interaction
    *
    * @param fhirRequest  FHIR Request
    * @param authzContext Authorization context
    * @param isTesting    If true this is to test interaction
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean): Future[FHIRResponse] = {
    if(fhirRequest.resourceId.isDefined)
      deleteResource(fhirRequest.resourceType.get, fhirRequest.resourceId.get, isTesting)
    else
      conditionalDeleteResource(fhirRequest.resourceType.get, fhirRequest.queryParams, isTesting)
  }

  /**
    * Check if Conditional Delete interaction is valid
    * @param _type              Resource type
    * @param searchParameters   Search parameters that defines the condition
    */
  private def validateConditionalDeleteInteraction(_type:String, searchParameters:List[Parameter], prefer:Option[String]) = {
    //1.1) Validate if conditional "delete" is supported for Resource Type
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.DELETE, _type, conditional = true)
  }

  /**
    * Delete the resource if condition holds
    * @param _type            Resource type
    * @param searchParameters Search parameters that defines the condition
    * @return
    */
  private def conditionalDeleteResource(_type:String, searchParameters:List[Parameter], testDelete:Boolean) : Future[FHIRResponse] = {
    logger.debug(s"Requesting a conditional 'delete' on ${_type} ...")

    ResourceManager
      .queryResources(_type, searchParameters) flatMap {
      //1.a: If there is no match, return 200 OK with document not exist warning
      case (0, _) =>
        logger.debug("no document with given parameters, return 200 - OK...")
        Future( FHIRResponse.errorResponse(StatusCodes.OK, Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.WARNING,
            FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
            None,
            Some(s"There is no such resource matching with given parameters!!!"),
            Nil
          )
        )))

      //1.b delete document if it exists
      case (1, Seq(foundResource)) =>
        logger.debug("insert a new and empty document indicating that current document is deleted...")
        performDelete(foundResource, None, _type, StatusCodes.NoContent, testDelete)
            .map(newVersion => FHIRResponse(StatusCodes.NoContent, None, None, None, Some(newVersion)))

      //1.c. If more than one match
      case (_, foundResources) =>
        fhirConfig.resourceConfigurations.apply(_type).conditionalDelete match {
          case "single" =>
            logger.debug("Multiple matches exist and multiple conditional delete is not supported, returning 412 Precondition failed...")
            throw new PreconditionFailedException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
                None,
                Some(s"Multiple matches exist with given parameters and multiple conditional delete is not supported. "),
                Nil
              )
            ))
          case "multiple" =>
            logger.debug("multiple matching, all deleted...")
            Future.sequence(
              foundResources.map(r => {
                performDelete(r, None, _type,StatusCodes.OK, testDelete)
              })
            ).map(_ => FHIRResponse(StatusCodes.NoContent))
        }
    }
  }

  /**
    * Check if delete is supported and basically ok
    * @param _type  Resource type
    * @param _id    Resource id
    * @return
    */
  private def validateDeleteInteraction(_type:String,_id:String):Unit = {
    //1.1) Validate id
    FHIRApiValidator.validateId(_id)
    //1.2)  Validate if "delete" is supported for Resource Type
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.DELETE, _type)
  }

  /**
    * Delete the resource by id
    * @param _type Resource type
    * @param _id Identifier of resource
    * @return
    */
  private def deleteResource(_type:String, _id:String, testDelete:Boolean) : Future[FHIRResponse] = {
    logger.debug(s"requesting a 'delete' on ${_type} with id ${_id}...")
    //1) check if resource already exists
    ResourceManager.getResource(_type, _id).flatMap {
      case None =>
        logger.debug("no document with given identifier, return 204 - No Content...")
        Future(FHIRResponse(StatusCodes.NoContent))
      case Some(foundResource) =>
        if(FHIRUtil.isDeleted(foundResource)){
          logger.debug("document has already been marked as deleted, so simply return 204 - No Content...")
          val (_, currentVersion, lastModified) = FHIRUtil.extractBaseMetaFields(foundResource)
          Future(FHIRResponse(
            StatusCodes.NoContent,
            None,
            Some(Uri(FHIRUtil.resourceLocationWithVersion(_type, _id, currentVersion))), //HTTP Location header
            Some(lastModified), //HTTP Last-Modified header
            Some(currentVersion) // HTTP ETag header)
          ))
        } else {
          logger.debug("insert a new and empty document indicating that current document is deleted...")
          performDelete(foundResource, Some(_id), _type, StatusCodes.NoContent, testDelete) flatMap {newVersion =>
            Future(FHIRResponse(StatusCodes.NoContent, None, None, None, Some(newVersion)))
          }
        }
    }
  }

  /**
    * Perform the delete operation and return the new versionId of deleted resource
    * @param resource     FHIR resource
    * @param resourceId   Resoure id
    * @param rtype        Resource type
    * @param statusCode   Http Status to set for deletion result
    * @param testDelete   Indicates if it is the real delete operation
    * @return
    */
  def performDelete(resource:Resource, resourceId:Option[String], rtype:String, statusCode:StatusCode, testDelete:Boolean):Future[Long] = {
    val currentVersion = FHIRUtil.extractVersionFromResource(resource)         //extract current version id
                                   //new version is 1 incremented
    val id             = resourceId.getOrElse(FHIRUtil.extractIdFromResource(resource))

    if(!testDelete)
      ResourceManager.deleteResource(rtype, id, currentVersion -> resource, statusCode)(transactionSession)
        .map(_._1)
    else
      Future(0L)
  }

}

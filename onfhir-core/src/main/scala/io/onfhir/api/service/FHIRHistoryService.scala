package io.onfhir.api.service

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.NotFoundException

import scala.concurrent.Future

class FHIRHistoryService (transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession) {

  /**
    * Validate if interaction is supported for the request
    *
    * @param fhirRequest FHIR Request
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply(
      validateHistoryInteraction(fhirRequest.resourceType, fhirRequest.resourceId)
    )
  }

  /**
    * Perform the interaction
    *
    * @param fhirRequest FHIR Request
    * @param isTesting   If this is just a test
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean): Future[FHIRResponse] = {
    resourceHistory(fhirRequest.resourceType.get, fhirRequest.resourceId, fhirRequest.queryParams)
  }


  /**
    * Check if history interactions are supported and basically valid
    * @param _type  Resource type
    * @param _id    Resource id
    */
  private def validateHistoryInteraction(_type:Option[String], _id:Option[String]): Unit ={
    //1) Do the validations for the operation
    if(_type.isDefined)
      if(_id.isDefined) {
        //1.1) Validate if history operation is supported in instance level
        FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.HISTORY_INSTANCE, _type.get)
        FHIRApiValidator.validateId(_id.get)
      } else
      //1.1) Validate if history operation is supported in type level
        FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.HISTORY_TYPE, _type.get)
    else
    //1.1) Validate if history operation is supported in system level
      FHIRApiValidator.validateSystemLevelInteraction(FHIR_INTERACTIONS.HISTORY_SYSTEM)
  }

  /**
    * Handle instance-type-system level History interactions
    * @param _type Resource Type if exist
    * @param _id Resource id if exist
    * @param searchParameters Paramters for history interaction (e.g. count, etc)
    * @return
    */
  private def resourceHistory(_type:String, _id:Option[String], searchParameters:List[Parameter]):Future[FHIRResponse] = {
    logger.debug(s"Requesting 'history' for ${_type} with ${_id}")
    ResourceManager.getResourceHistory(_type, _id, searchParameters)(transactionSession).flatMap {
      case (0, emptyResources) =>
        //If id is given
        if(_id.isDefined)
          //Check if the resource really exist
          ResourceManager.getResource(_type, _id.get, None, includingOrExcludingFields = Some(true -> Set(FHIR_COMMON_FIELDS.ID))).map {
            //If not throw exception
            case None =>
              logger.debug(s"No such resource with type(${_type}) and id(${_id}), returning 404 NotFound")
              throw new NotFoundException(Seq(
                OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.INFORMATION,
                  FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
                  None,
                  Some(s"No such resource with type(${_type}) and id(${_id})"),
                  Nil
                )
              ))
            case Some(_) =>
              0L -> emptyResources
          }
        else
          Future.apply(0L -> emptyResources)

      case (total, foundResources) =>
        Future.apply(total -> foundResources)
    } .map {
      case (total, foundResources) =>

        //2.1) handle Bundle links
        val bundleLinks = FHIRUtil.generateBundleLinks(Some(_type), _id, total, searchParameters, isHistory = true)
        //2.2) handle bundle entries
        val bundleEntries = foundResources.map(r => FHIRUtil.createBundleEntry(r, FHIR_BUNDLE_TYPES.HISTORY, isSummarized = false))
        val bundle = FHIRUtil.createBundle(FHIR_BUNDLE_TYPES.HISTORY, bundleLinks, bundleEntries, total, None)

        logger.debug(s"returning history of $total resources...")
        FHIRResponse(StatusCodes.OK, Some(bundle))
    }
  }
}

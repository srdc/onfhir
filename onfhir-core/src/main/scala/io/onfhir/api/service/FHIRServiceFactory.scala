package io.onfhir.api.service

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}
import io.onfhir.api.parsers.BundleRequestParser
import io.onfhir.authz.AuthzContext
import io.onfhir.db.TransactionSession
import io.onfhir.exception.BadRequestException

import scala.concurrent.Future

/**
  * FHIR Interaction service factory
  */
object FHIRServiceFactory {

  /**
    * Service for Unknown/Unrecognized requests
    */
  val fhirUnknownService:FHIRInteractionService = new FHIRInteractionService() {
    def validateInteraction(fhirRequest:FHIRRequest):Future[Unit] = {
      Future.apply {
        if (fhirRequest.response.isEmpty)
          throw new BadRequestException(BundleRequestParser.invalidOperation(fhirRequest.interaction, fhirRequest.requestUri))
      }
    }

    def completeInteraction(fhirRequest:FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting:Boolean = false): Future[FHIRResponse] = {
      Future.apply(FHIRResponse.errorResponse(StatusCodes.NotFound, BundleRequestParser.invalidOperation(fhirRequest.interaction, fhirRequest.requestUri)))
    }
  }

  /**
    * Return a suitable FHIR Service according to interaction name
    * @param fhirRequest Parsed FHIRRequest Object
    * @return
    */
  def getFHIRService(fhirRequest:FHIRRequest, transactionSession: Option[TransactionSession] = None) : FHIRInteractionService = {
    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.DELETE => new FHIRDeleteService(transactionSession)
      case FHIR_INTERACTIONS.CREATE => new FHIRCreateService(transactionSession)
      case FHIR_INTERACTIONS.UPDATE => new FHIRUpdateService(transactionSession)
      case FHIR_INTERACTIONS.READ | FHIR_INTERACTIONS.VREAD => new FHIRReadService(transactionSession)
      case FHIR_INTERACTIONS.HISTORY_INSTANCE | FHIR_INTERACTIONS.HISTORY_TYPE | FHIR_INTERACTIONS.HISTORY_SYSTEM => new FHIRHistoryService(transactionSession)
      case FHIR_INTERACTIONS.SEARCH => new FHIRSearchService(transactionSession)
      case FHIR_INTERACTIONS.PATCH => new FHIRPatchService(transactionSession)
      //If it is an FHIR operation
      case op if op.startsWith("$") => new FHIROperationHandler(transactionSession)
      case _ => fhirUnknownService
    }
  }



}

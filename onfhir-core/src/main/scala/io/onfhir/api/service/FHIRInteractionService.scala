package io.onfhir.api.service

import io.onfhir.Onfhir
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}
import io.onfhir.authz.AuthzContext
import io.onfhir.db.TransactionSession
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by tuncay on 5/16/2017.
  */
abstract class FHIRInteractionService(val transactionSession:Option[TransactionSession] = None) {
  //Logger
  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass.getName)
  //Execution context
  implicit val executionContext:ExecutionContext = Onfhir.actorSystem.dispatcher

  /**
    * Validate if interaction is supported for the request. Throw exception if not valid.
    * @param fhirRequest Parsed FHIR Request object
    */
  protected def validateInteraction(fhirRequest:FHIRRequest):Future[Unit]

  /**
    * Complete the interaction
    * @param fhirRequest Parsed FHIR Request object
    * @param authzContext Authorization context if needed (only for transaction and batch)
    * @param isTesting If we are only testing the interaction or realy perform it
    */
  def completeInteraction(fhirRequest:FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting:Boolean = false): Future[FHIRResponse]

  /**
    * Validate and complete the interaction
    * @param fhirRequest Parsed FHIR Request object
    * @param authzContext Authorization context if needed (only for transaction and batch)
    * @param isTesting If we are only testing the interaction or realy perform it
    * @return
    */
  def executeInteraction(fhirRequest:FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting:Boolean = false):Future[FHIRResponse] = {
    validateInteraction(fhirRequest) flatMap { _ =>
      completeInteraction(fhirRequest, authzContext, isTesting).map(response => {
        fhirRequest.setResponse(response)
        response
      })
    }
  }
}

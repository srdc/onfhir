package io.onfhir.api.client

import io.onfhir.Onfhir
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}
import io.onfhir.api.service.FHIRServiceFactory

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import io.onfhir.config.OnfhirConfig
import io.onfhir.server.ErrorHandler

object OnFhirLocalClient extends BaseFhirClient {
  implicit val executionContext: ExecutionContextExecutor = Onfhir.actorSystem.dispatcher

  override def getBaseUrl(): String = OnfhirConfig.fhirRootUrl

  /**
   * Execute a FHIR Request internally and return FHIR Response
   * @param fhirRequest
   * @return
   */
  override def execute(fhirRequest: FHIRRequest): Future[FHIRResponse] = {

    val service = FHIRServiceFactory.getFHIRService(fhirRequest, None)

    service
      .executeInteraction(fhirRequest)
      .recover[FHIRResponse](ErrorHandler.fhirErrorHandlerToResponse)
  }

  /**
   *
   * @param bundle
   * @return
   */
  override def next[T<:FHIRPaginatedBundle](bundle: T):Future[T] = {
    bundle match {
      case s:FHIRSearchSetBundle =>
        val baseRequest = s.request
        baseRequest.page = Some(baseRequest.page.getOrElse(1L) + 1)
        baseRequest.executeAndReturnBundle().map(_.asInstanceOf[T])
      case h:FHIRHistoryBundle =>
        val baseRequest = h.request
        baseRequest.page = Some(baseRequest.page.getOrElse(1L) + 1)
        baseRequest.executeAndReturnBundle().map(_.asInstanceOf[T])
    }
  }

  /**
   * Special request to create/update resources in bulk
   * @return
   */
  def bulkUpsert(rtype:String): OnFhirBulkRequestBuilder = new OnFhirBulkRequestBuilder(this, rtype)
}

package io.onfhir.authz

import akka.http.scaladsl.model.HttpResponse
import io.onfhir.api.model.FHIRRequest

import scala.concurrent.Future

/**
  * Interface for custom audit/log handlers
  */
trait ICustomAuditHandler {

  /**
    *
    * @param fhirRequest FHIR request object
    * @param authContext Authentication context
    * @param authzContext Authorization context
    * @param httpResponse HTTP response
    */
  def createAndSendAudit(
                          fhirRequest: FHIRRequest,
                          authContext: AuthContext,
                          authzContext: Option[AuthzContext],
                          httpResponse: HttpResponse):Future[Boolean]
}

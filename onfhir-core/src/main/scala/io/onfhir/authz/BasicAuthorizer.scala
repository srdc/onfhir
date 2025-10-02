package io.onfhir.authz

import io.onfhir.api.model.FHIRRequest

/**
 * Authorization handler for basic authentication
 */
class BasicAuthorizer extends IAuthorizer {
  def furtherParamsInAuthzContext:List[String] = List.empty

  /**
   * All authenticated users have right to access anything
   * @param authzContext  Resolved authorization context
   * @param request       FHIR request details
   * @return
   */
  override def authorize(authzContext: AuthzContext, request: FHIRRequest): AuthzResult = AuthzResult.success()


  override def authorizeForPublic(request: FHIRRequest):AuthzResult =
    AuthzResult.failureInvalidRequest("")
}

package io.onfhir.authz

import io.onfhir.api.model.FHIRRequest

/**
  * Created by tuncay on 2/27/2017.
  * Interface for authorization modules for onFHIR for FHIR REST API
  */
trait IAuthorizer {
  /**
    * Further parameters required by the Authorizer within token or introspection response for policy enforcement
    * @return
    */
  def furtherParamsInAuthzContext:List[String]

  /**
   * Enforce policy for given FHIR request
   * @param authzContext  Resolved authorization context
   * @param fhirRequest   FHIR request details
   * @return
   */
  def authorize(authzContext: AuthzContext,
                fhirRequest:FHIRRequest
               ): AuthzResult

  /**
    * Policy enforcer for public resources (if there is no token)
    * @param fhirRequest   FHIR request details
    * @return
    */
  def authorizeForPublic(fhirRequest:FHIRRequest):AuthzResult

}

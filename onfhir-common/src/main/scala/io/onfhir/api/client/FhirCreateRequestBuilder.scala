package io.onfhir.api.client

import io.onfhir.api.{FHIR_INTERACTIONS, Resource}
import io.onfhir.api.model.FHIRRequest

/**
 * Request builder for FHIR create
 * @param onFhirClient
 * @param resource
 */
class FhirCreateRequestBuilder(onFhirClient: IOnFhirClient, rtype:String, resource: Resource)
  extends FhirSearchLikeRequestBuilder(
    onFhirClient,
    FHIRRequest(interaction = FHIR_INTERACTIONS.CREATE, requestUri = s"${onFhirClient.getBaseUrl()}/$rtype", resourceType = Some(rtype), resource = Some(resource))
  ) {
  type This = FhirCreateRequestBuilder

  /**
   * Conditional create params goes to If-None-Exist header
   */
  protected override def compile(): Unit = {
    if(conditionalParams.nonEmpty)
      request.ifNoneExist = Some(
        conditionalParams.map(p => s"${p._1}=${p._2.mkString(",")}").mkString("&")
      )
  }
}

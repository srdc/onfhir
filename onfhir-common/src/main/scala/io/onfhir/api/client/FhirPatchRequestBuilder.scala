package io.onfhir.api.client

import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.FHIRRequest

/**
 *  Request builder for FHIR Patch interaction
 * @param onFhirClient
 * @param rtype
 * @param rid
 */
class FhirPatchRequestBuilder(onFhirClient: IOnFhirClient, rtype:String, rid:Option[String])
  extends FhirSearchLikeRequestBuilder(onFhirClient,
    FHIRRequest(interaction = FHIR_INTERACTIONS.PATCH, requestUri = s"${onFhirClient.getBaseUrl()}/$rtype/$rid", resourceType = Some(rtype), resourceId = rid)
  ) {
  type This = FhirPatchRequestBuilder

  def fhirPathPatch():FhirPathPatchRequestBuilder = {
    compile()
    new FhirPathPatchRequestBuilder(this)
  }

  def jsonPatch():FhirJsonPatchRequestBuilder = {
    compile()
    new FhirJsonPatchRequestBuilder(this)
  }
}

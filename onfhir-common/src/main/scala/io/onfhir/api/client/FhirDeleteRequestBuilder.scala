package io.onfhir.api.client

import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.FHIRRequest

/**
 * Request builder for delete
 *
 * @param onFhirClient
 * @param rtype
 * @param rid
 */
class FhirDeleteRequestBuilder(onFhirClient: IOnFhirClient, rtype:String, rid:Option[String])
  extends FhirSearchLikeRequestBuilder(onFhirClient,
    FHIRRequest(interaction = FHIR_INTERACTIONS.DELETE, requestUri = s"${onFhirClient.getBaseUrl()}/$rtype${rid.map("/" + _).getOrElse("")}", resourceType = Some(rtype), resourceId = rid)
  ) {
  type This = FhirDeleteRequestBuilder

}

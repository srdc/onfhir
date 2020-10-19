package io.onfhir.api.client

import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.FHIRRequest

class FhirVReadRequestBuilder(onFhirClient:IOnFhirClient, rtype:String, rid:String, version:String) extends
  FhirRequestBuilder(
    onFhirClient,
    FHIRRequest(interaction = FHIR_INTERACTIONS.VREAD,
      requestUri = s"${onFhirClient.getBaseUrl()}/$rtype/$rid/_history/$version",
      resourceType = Some(rtype),
      resourceId = Some(rid),
      versionId = Some(version)
    )) {
  type This = FhirVReadRequestBuilder
}

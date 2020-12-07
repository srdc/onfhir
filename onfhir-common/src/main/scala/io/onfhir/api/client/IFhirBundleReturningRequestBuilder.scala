package io.onfhir.api.client

import io.onfhir.api.model.FHIRResponse

trait IFhirBundleReturningRequestBuilder {
  def constructBundle(fhirResponse:FHIRResponse):FHIRPaginatedBundle

  def nextPage():Unit
}

package io.onfhir.api.client

import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_INTERACTIONS, Resource}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.util.FHIRUtil

class OnFhirBulkRequestBuilder (onFhirClient:IOnFhirClient, rtype:String) extends
  FhirRequestBuilder(onFhirClient,
    FHIRRequest(interaction = FHIR_INTERACTIONS.BULK_UPSERT, requestUri = s"${onFhirClient.getBaseUrl()}", resourceType = Some(rtype))
  ){

  /**
   * Add entry to the bundle without a fullUrl
   *
   * @param rbFunction
   * @return
   */
  def upsert(resource:Resource): OnFhirBulkRequestBuilder = {
    FHIRUtil.extractValueOption[String](resource, FHIR_COMMON_FIELDS.ID) match {
      case Some(id) => request.childRequests = request.childRequests :+ FHIRRequest(requestUri = s"/$rtype/$id", interaction = FHIR_INTERACTIONS.UPDATE, resourceType = Some(rtype), resourceId = Some(id), resource = Some(resource))
      case None => request.childRequests = request.childRequests :+ FHIRRequest(requestUri = s"/$rtype", interaction = FHIR_INTERACTIONS.CREATE, resourceType = Some(rtype), resource = Some(resource))
    }
    this
  }
}

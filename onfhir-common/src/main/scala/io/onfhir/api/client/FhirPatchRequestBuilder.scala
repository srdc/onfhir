package io.onfhir.api.client

import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_CONTENT_TYPES, FHIR_INTERACTIONS}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.util.FHIRUtil
import org.json4s.{JArray, JObject, JValue}

/**
 *  Request builder for FHIR Patch interaction
 * @param onFhirClient
 * @param rtype
 * @param rid
 */
class FhirPatchRequestBuilder(onFhirClient: IOnFhirClient, rtype:String, rid:Option[String])
  extends FhirSearchLikeRequestBuilder(onFhirClient,
    FHIRRequest(interaction = FHIR_INTERACTIONS.PATCH, requestUri = s"${onFhirClient.getBaseUrl()}/$rtype/${rid.map("/" + _).getOrElse("")}", resourceType = Some(rtype), resourceId = rid)
  ) {
  type This = FhirPatchRequestBuilder

  /**
   * Directly provide the patch content
   * @param patch Patch content
   *
   *              - For JSON Patch, this can be array or object providing the patches or patch
   *              - For FHIR Path Patch, this should be the Parameters resource
   * @return
   */
  def patchContent(patch:JValue):FhirPatchRequestBuilder = {
    patch match {
      //If FHIR Path patch
      case obj:JObject if obj.obj.exists(f => f._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE && FHIRUtil.extractResourceType(obj) == "Parameters") =>
        request.contentType = Some(FHIR_CONTENT_TYPES.FHIR_JSON_CONTENT_TYPE)
        request.resource = Some(obj)
      //If JSON Patch with single patch
      case obj:JObject =>
        request.contentType = Some(FHIR_CONTENT_TYPES.FHIR_JSON_PATCH_CONTENT_TYPE)
        request.resource = Some(JObject("patches" -> JArray(List(obj))))
      //If JSON Patch with multiple patches
      case arr:JArray =>
        request.contentType = Some(FHIR_CONTENT_TYPES.FHIR_JSON_PATCH_CONTENT_TYPE)
        request.resource = Some(JObject("patches" -> arr))
    }
    this
  }

  def fhirPathPatch():FhirPathPatchRequestBuilder = {
    compile()
    new FhirPathPatchRequestBuilder(this)
  }

  def jsonPatch():FhirJsonPatchRequestBuilder = {
    compile()
    new FhirJsonPatchRequestBuilder(this)
  }
}

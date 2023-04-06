package io.onfhir.api.client

import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.FHIRRequest

class FhirMetadataRequestBuilder(onFhirClient: IOnFhirClient)
  extends FhirRequestBuilder(onFhirClient,
    FHIRRequest(
      interaction = FHIR_INTERACTIONS.CAPABILITIES,
      requestUri = s"${onFhirClient.getBaseUrl()}/metadata",
    )
  )
{
  private var modeParam:Option[String] = None

  def mode(md:String):FhirMetadataRequestBuilder  = {
    modeParam = Some(md)
    this
  }

  override protected def compile(): Unit = {
    super.compile()
    request.queryParams = modeParam.map(m => "mode" -> List(m)).toList.toMap
  }
}

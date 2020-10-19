package io.onfhir.api.client

import io.onfhir.api.model.{FHIRResponse}

case class FhirClientException(msg:String, serverResponse:Option[FHIRResponse] = None) extends Exception(msg) {

}

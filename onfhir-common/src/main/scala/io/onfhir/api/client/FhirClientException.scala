package io.onfhir.api.client

import io.onfhir.api.model.FHIRResponse
import io.onfhir.util.JsonFormatter

case class FhirClientException(msg:String, serverResponse:Option[FHIRResponse] = None) extends Exception(msg) {

  override def toString: String = {
    s"$msg"+
      s"${serverResponse
        .map(r =>
          s"\nStatus: ${r.httpStatus.intValue()}\n" +
            r.responseBody.map(JsonFormatter.convertToJson(_).toJson).getOrElse("")
        ).getOrElse("")}"
  }

}

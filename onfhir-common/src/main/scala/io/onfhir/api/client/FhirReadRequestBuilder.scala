package io.onfhir.api.client

import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.headers.{EntityTag, `If-Modified-Since`, `If-None-Match`}
import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.FHIRRequest

class FhirReadRequestBuilder(onFhirClient: IOnFhirClient, rtype:String, rid:String)
  extends FhirRequestBuilder(onFhirClient, FHIRRequest(interaction = FHIR_INTERACTIONS.READ, requestUri = s"${onFhirClient.getBaseUrl()}/$rtype/$rid", resourceType = Some(rtype), resourceId = Some(rid))) {
  type This = FhirReadRequestBuilder

  def ifModifiedSince(dateTime:DateTime):FhirReadRequestBuilder = {
    request.ifModifiedSince = Some(`If-Modified-Since`.apply(dateTime))
    this
  }

  def ifNoneMatch(version:Long):FhirReadRequestBuilder = {
    request.ifNoneMatch = Some(`If-None-Match`.apply(EntityTag.apply(""+version, weak = true)))
    this
  }

  def summary(s:String):FhirRequestBuilder = {
    request.queryParams = request.queryParams ++ Map("_summary" -> List(s))
    this
  }

  def elements(el:String*):FhirRequestBuilder = {
    request.queryParams = request.queryParams ++ Map("_elements" -> List(el.mkString(",")))
    this
  }

}

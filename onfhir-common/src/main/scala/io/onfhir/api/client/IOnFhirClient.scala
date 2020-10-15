package io.onfhir.api.client

import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRRequest,FHIRResponse}

import scala.concurrent.{ExecutionContext, Future}

trait IOnFhirClient {

  def getBaseUrl():String

  def execute(fhirRequest:FHIRRequest):Future[FHIRResponse]

  def next[T<:FHIRPaginatedBundle](bundle: T):Future[T]


  def create(resource:Resource):FhirCreateRequestBuilder

  def read(rtype:String, rid:String):FhirReadRequestBuilder

  def update(resource:Resource, forceVersionControl:Boolean = true):FhirUpdateRequestBuilder

  def delete(rtype:String, rid:String):FhirDeleteRequestBuilder

  def delete(rtype:String):FhirDeleteRequestBuilder

  def delete(resource: Resource):FhirDeleteRequestBuilder

  def vread(rtype:String, rid:String, vid:String):FhirVReadRequestBuilder

  def patch(rtype:String, rid:String):FhirPatchRequestBuilder

  def patch(rtype:String):FhirPatchRequestBuilder

  def patch(resource: Resource):FhirPatchRequestBuilder

  def history(rtype:String, rid:String):FhirHistoryRequestBuilder

  def history(rtype:String, rid:String, count:Int):FhirHistoryRequestBuilder

  def history(resource: Resource):FhirHistoryRequestBuilder

  def history(resource: Resource, count:Int):FhirHistoryRequestBuilder

  def history(rtype:String):FhirHistoryRequestBuilder

  def history(rtype:String, count:Int):FhirHistoryRequestBuilder

  def search(rtype:String):FhirSearchRequestBuilder

  def search(rtype:String, count:Int):FhirSearchRequestBuilder

  def operation(opName:String):FhirOperationRequestBuilder

  def batch():FhirBatchTransactionRequestBuilder

  def transaction():FhirBatchTransactionRequestBuilder
}

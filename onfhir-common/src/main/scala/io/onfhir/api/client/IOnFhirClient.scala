package io.onfhir.api.client

import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRRequest,FHIRResponse}

import scala.concurrent.{ExecutionContext, Future}

trait IOnFhirClient {
  /**
   * Get base url of FHIR repository that this client is configured for
   * @return
   */
  def getBaseUrl():String

  /**
   * Execute a FHIR request and retrieve the response
   * @param fhirRequest
   * @return
   */
  def execute(fhirRequest:FHIRRequest):Future[FHIRResponse]

  /**
   * For search and history interactions retrieve the next page for the given bundle
   * @param bundle
   * @tparam T
   * @return
   */
  def next[T<:FHIRPaginatedBundle](bundle: T):Future[T]

  /**
   * Return builder for FHIR create interaction
   * @param resource  Resource to create
   * @return
   */
  def create(resource:Resource):FhirCreateRequestBuilder

  /**
   * Return builder for FHIR read interaction
   * @param rtype Resource type to read
   * @param rid   Resource id to read
   * @return
   */
  def read(rtype:String, rid:String):FhirReadRequestBuilder

  /**
   * Return builder for FHIR update interaction (if resource does not have id, it can be used for conditional update)
   * @param resource              Resource content to update
   * @param forceVersionControl   If true and if resource has a version, it performs a version aware update with If-Match
   * @return
   */
  def update(resource:Resource, forceVersionControl:Boolean = true):FhirUpdateRequestBuilder

  /**
   * Return builder for FHIR delete interaction
   * @param rtype Resource type
   * @param rid   Resource id to delete
   * @return
   */
  def delete(rtype:String, rid:String):FhirDeleteRequestBuilder

  /**
   * Return builder for FHIR delete interaction for conditional delete
   * @param rtype
   * @return
   */
  def delete(rtype:String):FhirDeleteRequestBuilder

  /**
   * Return builder for FHIR delete interaction (resource should have an id)
   * @param resource
   * @return
   */
  def delete(resource: Resource):FhirDeleteRequestBuilder

  /**
   * Return builder for version read interaction
   * @param rtype Resource type
   * @param rid   Resource id
   * @param vid   Version id
   * @return
   */
  def vread(rtype:String, rid:String, vid:String):FhirVReadRequestBuilder

  /**
   * Return builder for FHIR patch interaction
   * @param rtype   Resource type
   * @param rid     Resource id
   * @return
   */
  def patch(rtype:String, rid:String):FhirPatchRequestBuilder

  /**
   * Return builder for FHIR patch interaction (conditional patch)
   * @param rtype
   * @return
   */
  def patch(rtype:String):FhirPatchRequestBuilder

  /**
   * Return builder for FHIR patch interaction (resource should have an id)
   * @param resource  Resource to patch
   * @return
   */
  def patch(resource: Resource):FhirPatchRequestBuilder

  /**
   * Return builder for history instance interaction
   * @param rtype   Resource type
   * @param rid     Resource id
   * @return
   */
  def history(rtype:String, rid:String):FhirHistoryRequestBuilder

  /**
   *  Return builder for history instance interaction with a given count for resources to return for each page
   * @param rtype
   * @param rid
   * @param count
   * @return
   */
  def history(rtype:String, rid:String, count:Int):FhirHistoryRequestBuilder

  /**
   * Return builder for history instance interaction (resource should have an id)
   * @param resource
   * @return
   */
  def history(resource: Resource):FhirHistoryRequestBuilder

  /**
   * Return builder for history instance interaction  (resource should have an id) with a given count for resources to return for each page
   * @param resource
   * @param count
   * @return
   */
  def history(resource: Resource, count:Int):FhirHistoryRequestBuilder

  /**
   * Return builder for history type interaction
   * @param rtype
   * @return
   */
  def history(rtype:String):FhirHistoryRequestBuilder

  /**
   * Return builder for history type interaction with a given count for resources to return for each page
   * @param rtype
   * @param count
   * @return
   */
  def history(rtype:String, count:Int):FhirHistoryRequestBuilder

  /**
   * Return builder for search-type interaction
   * @param rtype
   * @return
   */
  def search(rtype:String):FhirSearchRequestBuilder

  /**
   * Return builder for search-type interaction  with a given count for resources to return for each page
   * @param rtype
   * @param count
   * @return
   */
  def search(rtype:String, count:Int):FhirSearchRequestBuilder

  /**
   * Start search from a specific page
   * @param rtype
   * @param count
   * @param page
   * @return
   */
  def search(rtype:String, count:Int, page:Long):FhirSearchRequestBuilder

  /**
   * Return builder for FHIR operation
   * @param opName  Operation name
   * @return
   */
  def operation(opName:String):FhirOperationRequestBuilder

  /**
   * Return builder for FHIR batch interaction
   * @return
   */
  def batch():FhirBatchTransactionRequestBuilder

  /**
   * Return builder for FHIR transaction interaction
   * @return
   */
  def transaction():FhirBatchTransactionRequestBuilder
}

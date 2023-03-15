package io.onfhir.api.client

import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}
import io.onfhir.api.parsers.BundleRequestParser
import org.json4s.JsonAST.JObject

import scala.concurrent.{ExecutionContext, Future}

/**
 * Builder for FHIR transcation or batch requests
 * @param onFhirClient  OnFhir client instance
 * @param isBatch       If this is batch or transaction
 */
class FhirBatchTransactionRequestBuilder(onFhirClient:IOnFhirClient, isBatch:Boolean) extends
  FhirRequestBuilder(onFhirClient,
    FHIRRequest(interaction = if(isBatch) FHIR_INTERACTIONS.BATCH else FHIR_INTERACTIONS.TRANSACTION, requestUri = s"${onFhirClient.getBaseUrl()}")
  ){

  /**
   * Add Request entries from given FHIR Bundle to the request
   * @param bundle      FHIR Bundle including request entries
   * @return
   */
  def entriesFromBundle(bundle:JObject):FhirBatchTransactionRequestBuilder = {
    val parsedChildRequests =
      BundleRequestParser
        .parseBundleRequest(bundle, None, skipEntriesWithoutRequest = true)
    request.childRequests = request.childRequests ++ parsedChildRequests
    this
  }

  /**
   * Add entry to the bundle without a fullUrl
   * @param rbFunction
   * @return
   */
  def entry(rbFunction:IOnFhirClient => FhirRequestBuilder):FhirBatchTransactionRequestBuilder = {
    request.childRequests = request.childRequests :+ rbFunction(onFhirClient).compileRequest()
    this
  }

  /**
   * Add entry to the bundle with a given a fullUrl
   * @param fullUrlUuid
   * @param rbFunction
   * @return
   */
  def entry(fullUrlUuid:String, rbFunction:IOnFhirClient => FhirRequestBuilder):FhirBatchTransactionRequestBuilder = {
    if(!fullUrlUuid.startsWith("urn:uuid:"))
      throw FhirClientException(s"Given fullUrl $fullUrlUuid  is not in urn:uuid format!")

    request.childRequests = request.childRequests :+ rbFunction(onFhirClient).compileRequest().setId(Some(fullUrlUuid))
    this
  }

  /**
   * Execute the interaction and return the response as FHIR Bundle
   * @param executionContext
   * @return
   */
  def executeAndReturnBundle()(implicit executionContext: ExecutionContext):Future[FHIRTransactionBatchBundle] = {
    execute()
      .map(r => {
        if(r.httpStatus.isFailure() || r.responseBody.isEmpty)
          throw FhirClientException("Problem in FHIR batch/transaction!", Some(r))
        try {
          new FHIRTransactionBatchBundle(r.responseBody.get)
        } catch {
          case e:Throwable =>
            throw FhirClientException("Invalid transaction/batch result bundle!", Some(r))
        }
      })
  }
}

package io.onfhir.api.client

import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}

import scala.concurrent.{ExecutionContext, Future}

class FhirBatchTransactionRequestBuilder(onFhirClient:IOnFhirClient, isBatch:Boolean) extends
  FhirRequestBuilder(onFhirClient,
    FHIRRequest(interaction = if(isBatch) FHIR_INTERACTIONS.BATCH else FHIR_INTERACTIONS.TRANSACTION, requestUri = s"${onFhirClient.getBaseUrl()}")
  ){

  def entry(rbFunction:IOnFhirClient => FhirRequestBuilder):FhirBatchTransactionRequestBuilder = {
    request.childRequests = request.childRequests :+ rbFunction(onFhirClient).request
    this
  }

  def entry(fullUrlUuid:String, rbFunction:IOnFhirClient => FhirRequestBuilder):FhirBatchTransactionRequestBuilder = {
    if(!fullUrlUuid.startsWith("urn:uuid:"))
      throw new FhirClientException(s"Given fullUrl $fullUrlUuid  is not in urn:uuid format!")
    request.childRequests = request.childRequests :+ rbFunction(onFhirClient).request.setId(Some(fullUrlUuid))
    this
  }

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

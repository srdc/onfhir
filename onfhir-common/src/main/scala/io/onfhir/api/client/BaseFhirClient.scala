package io.onfhir.api.client

import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.BadRequestException

import scala.concurrent.ExecutionContext


abstract class BaseFhirClient extends IOnFhirClient {

  /**
   * Initiate FHIR Create request builder
   * @param resource
   * @return
   */
  def create(resource:Resource):FhirCreateRequestBuilder = {
    val rtype = FhirClientUtil.getResourceType(resource)
    new FhirCreateRequestBuilder(this , rtype, resource)
  }

  /**
   * Initiate FHIR Read request builder
   * @param rtype
   * @param rid
   * @return
   */
  def read(rtype:String, rid:String):FhirReadRequestBuilder = {
      new FhirReadRequestBuilder(this, rtype, rid)
  }

  /**
   * Initiate FHIR Update request builder
   * @param resource
   * @return
   */
  def update(resource:Resource, forceVersionControl:Boolean = true):FhirUpdateRequestBuilder = {
    val rtype = FhirClientUtil.getResourceType(resource)
    val rid = FHIRUtil.extractValueOption[String](resource, "id")
    new FhirUpdateRequestBuilder(this, rtype, rid, resource, forceVersionControl)
  }

  /**
   * Initiate FHIR delete request builder
   * @param rid
   * @return
   */
  def delete(rtype:String, rid:String):FhirDeleteRequestBuilder = {
      new FhirDeleteRequestBuilder(this, rtype, Some(rid))
  }
  def delete(rtype:String):FhirDeleteRequestBuilder = {
    new FhirDeleteRequestBuilder(this, rtype,None)
  }

  /**
   * Initiate FHIR delete request builder
   * @param resource
   * @return
   */
  def delete(resource: Resource):FhirDeleteRequestBuilder = {
      val rtype = FhirClientUtil.getResourceType(resource)
      val rid = FhirClientUtil.getResourceId(resource)
      delete(rtype, rid)
  }


  /**
   * Initiate FHIR vread request builder
   * @param rtype
   * @param rid
   * @return
   */
  def vread(rtype:String, rid:String, vid:String):FhirVReadRequestBuilder = {
    new FhirVReadRequestBuilder(this, rtype, rid, vid)
  }


  /**
   * Initiate FHIR patch request builder
   * @param rtype
   * @param rid
   * @return
   */
  def patch(rtype:String, rid:String):FhirPatchRequestBuilder = {
    new FhirPatchRequestBuilder(this, rtype, Some(rid))
  }

  /**
   * Initiate FHIR patch request builder for conditinal patching
   * @param rtype
   * @return
   */
  def patch(rtype:String):FhirPatchRequestBuilder = {
    new FhirPatchRequestBuilder(this, rtype, None)
  }

  /**
   * Initiate FHIR patch request builder for the given resource
   * @param resource
   * @return
   */
  def patch(resource: Resource):FhirPatchRequestBuilder = {
    val rtype = FhirClientUtil.getResourceType(resource)
    val rid = FhirClientUtil.getResourceId(resource)
    patch(rtype, rid)
  }

  /**
   * FHIR Instance level history
   * @param rtype
   * @param rid
   * @return
   */
  def history(rtype:String, rid:String):FhirHistoryRequestBuilder = {
    new FhirHistoryRequestBuilder(this, Some(rtype), Some(rid), None)
  }
  def history(rtype:String, rid:String, count:Int):FhirHistoryRequestBuilder = {
    new FhirHistoryRequestBuilder(this, Some(rtype), Some(rid), Some(count))
  }

  def history(resource: Resource):FhirHistoryRequestBuilder = {
    val rtype = FhirClientUtil.getResourceType(resource)
    val rid = FhirClientUtil.getResourceId(resource)
    history(rtype, rid)
  }
  def history(resource: Resource, count:Int):FhirHistoryRequestBuilder = {
    val rtype = FhirClientUtil.getResourceType(resource)
    val rid = FhirClientUtil.getResourceId(resource)
    history(rtype, rid, count)
  }

  /**
   * Type level history
   * @param rtype
   * @return
   */
  def history(rtype:String):FhirHistoryRequestBuilder = {
    new FhirHistoryRequestBuilder(this, Some(rtype), None, None)
  }
  def history(rtype:String, count:Int):FhirHistoryRequestBuilder = {
    new FhirHistoryRequestBuilder(this, Some(rtype), None, Some(count))
  }

  /**
   *
   * @param rtype
   * @return
   */
  def search(rtype:String):FhirSearchRequestBuilder = {
    new FhirSearchRequestBuilder(this, rtype, None)
  }
  def search(rtype:String, count:Int):FhirSearchRequestBuilder = {
    new FhirSearchRequestBuilder(this, rtype, Some(count))
  }
  def search(rtype:String, count:Int, page:Long):FhirSearchRequestBuilder = {
    new FhirSearchRequestBuilder(this, rtype, Some(count), Some(page))
  }

  /**
   * All FHIR operations
   * @param opName Name of the operation
   * @return
   */
  def operation(opName:String):FhirOperationRequestBuilder = {
    new FhirOperationRequestBuilder(this, opName)
  }

  def batch():FhirBatchTransactionRequestBuilder = {
    new FhirBatchTransactionRequestBuilder(this, isBatch = true)
  }

  def transaction():FhirBatchTransactionRequestBuilder = {
    new FhirBatchTransactionRequestBuilder(this, isBatch = false)
  }
}

object FhirClientUtil {
  def getResourceType(resource: Resource):String = {
    FHIRUtil.extractValueOption[String](resource, "resourceType") match {
      case Some(rt)=> rt
      case None =>
        throw new BadRequestException(Seq(OutcomeIssue(
          severity =  FHIRResponse.SEVERITY_CODES.ERROR,
          code = FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR request call, 'resourceType' element does not exist in given resource!"),
          Nil
        )))
    }
  }

  def getResourceId(resource: Resource):String = {
    FHIRUtil.extractValueOption[String](resource, "id") match {
      case Some(rid)=> rid
      case None =>
        throw new BadRequestException(Seq(OutcomeIssue(
          severity =  FHIRResponse.SEVERITY_CODES.ERROR,
          code = FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR request call, Resource id does not exist in given resource!"),
          Nil
        )))
    }
  }
}
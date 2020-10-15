package io.onfhir.api.client

import io.onfhir.api.{FHIR_HTTP_OPTIONS, Resource}
import io.onfhir.api.model.{FHIROperationResponse, FHIRRequest, FHIRResponse}

import scala.language.implicitConversions
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

object  FhirRequestBuilder {
  implicit def toExecution(fhirRequestBuilder: FhirRequestBuilder)(implicit ex:ExecutionContext): Future[FHIRResponse] = {
    fhirRequestBuilder.execute().map(_.asInstanceOf[FHIRResponse])
  }

  implicit def toExecutionReturnResource(fhirRequestBuilder: FhirRequestBuilder)(implicit ex:ExecutionContext): Future[Resource] = {
    fhirRequestBuilder
      .executeAndReturnResource()
  }

  implicit def toExecutionReturnResourceOpt(fhirRequestBuilder: FhirRequestBuilder)(implicit ex:ExecutionContext): Future[Option[Resource]] = {
    fhirRequestBuilder
      .executeAndReturnResourceOption()
  }

  implicit def toExecutionReturnBundle(fhirSearchRequestBuilder: FhirSearchRequestBuilder)(implicit ex:ExecutionContext):Future[FHIRSearchSetBundle] = {
    fhirSearchRequestBuilder
      .executeAndReturnBundle()
  }

  implicit def toExecutionReturnBundle(fhirHistoryRequestBuilder: FhirHistoryRequestBuilder)(implicit ex:ExecutionContext):Future[FHIRHistoryBundle] = {
    fhirHistoryRequestBuilder
      .executeAndReturnBundle()
  }

  implicit def toExecutionReturnBundle(fhirBatchTransactionRequestBuilder: FhirBatchTransactionRequestBuilder)(implicit ex:ExecutionContext):Future[FHIRTransactionBatchBundle] = {
    fhirBatchTransactionRequestBuilder
      .executeAndReturnBundle()
  }

  implicit def toExecutionFHIROperationResponse(fhirOperationRequestBuilder: FhirOperationRequestBuilder)(implicit ex:ExecutionContext):Future[FHIROperationResponse] = {
    fhirOperationRequestBuilder
      .executeAndReturnOperationOutcome()
  }
}
abstract class FhirRequestBuilder(val onFhirClient: IOnFhirClient, val request:FHIRRequest) {
  protected type This <: FhirRequestBuilder
  // Prefer header setting for return preferences
  def returnMinimal():This = {
    request.prefer = Some(FHIR_HTTP_OPTIONS.FHIR_RETURN_MINIMAL)
    this.asInstanceOf[This]
  }
  def returnOperationOutcome():This = {
    request.prefer = Some(FHIR_HTTP_OPTIONS.FHIR_RETURN_OPERATION_OUTCOME)
    this.asInstanceOf[This]
  }

  protected def compile():Unit = {}

  def execute():Future[FHIRResponse] = {
      compile()
      onFhirClient.execute(request)
  }


  def executeAndReturnResourceOption()(implicit ex:ExecutionContext):Future[Option[Resource]] = {
    execute()
      .map(_.responseBody)
  }

  def executeAndReturnResource()(implicit ex:ExecutionContext):Future[Resource] = {
    execute()
      .map {
        case r:FHIRResponse if r.isError || r.responseBody.isEmpty =>
          throw FhirClientException("Problem in executing FHIR request!", Some(r))
        case s:FHIRResponse =>
          s.responseBody.get
      }
  }
}


abstract class FhirSearchLikeRequestBuilder(onFhirClient: IOnFhirClient, request: FHIRRequest) extends FhirRequestBuilder(onFhirClient, request) {

  protected val conditionalParams:mutable.ListBuffer[(String, Seq[String])] = new ListBuffer[(String, Seq[String])]

  def where(param:String, value:String*):This = {
    conditionalParams.append(param-> value.toSeq)
    this.asInstanceOf[This]
  }

  override protected def compile():Unit = {
    if(conditionalParams.nonEmpty)
      request.queryParams = conditionalParams.groupBy(_._1).mapValues(v => v.map(_._2.mkString(",")).toList)
  }
}




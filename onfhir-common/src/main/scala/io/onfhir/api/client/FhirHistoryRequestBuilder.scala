package io.onfhir.api.client

import java.time.{Instant, ZonedDateTime}

import akka.http.scaladsl.model.DateTime
import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.FHIRRequest
import io.onfhir.util.DateTimeUtil

import scala.concurrent.{ExecutionContext, Future}

class FhirHistoryRequestBuilder(onFhirClient: IOnFhirClient, rtype:Option[String], rid:Option[String], count:Option[Int] = None, var page:Option[Int] = None)
  extends FhirRequestBuilder(onFhirClient,
    FHIRRequest(
      interaction =
        if(rid.isEmpty) FHIR_INTERACTIONS.HISTORY_TYPE else FHIR_INTERACTIONS.HISTORY_INSTANCE,
      requestUri = s"${onFhirClient.getBaseUrl()}",
      resourceType = rtype,
      resourceId = rid)
  ){
  type This = FhirHistoryRequestBuilder

  private var sinceParam:Option[(String, String)] = None
  private var atParam:Option[(String, String)] = None
  private var listParam:Option[(String, String)] = None

  def since(instant:Instant):FhirHistoryRequestBuilder = {
    sinceParam = Some("_since" -> DateTimeUtil.serializeInstant(instant))
    this
  }

  def at(time: ZonedDateTime):FhirHistoryRequestBuilder = {
    atParam = Some("_at", DateTimeUtil.serializeInstant(time.toInstant))
    this
  }

  def list(l:String*):FhirRequestBuilder = {
    listParam = Some("_list", l.toSeq.mkString(","))
    this
  }

  override protected def compile(): Unit = {
    super.compile()

    val allParams = sinceParam.toSeq ++ atParam.toSeq ++ listParam.toSeq ++ count.toSeq.map(c => "_count" -> (""+c)) ++ page.toSeq.map(p => "_page" -> (""+p))

    request.queryParams = allParams.map(p => p._1 -> List(p._2)).toMap
  }

  def executeAndReturnBundle()(implicit executionContext: ExecutionContext):Future[FHIRHistoryBundle] = {
    execute()
      .map(r => {
        if(r.httpStatus.isFailure() || r.responseBody.isEmpty)
          throw FhirClientException("Problem in FHIR search!", Some(r))
        try {
          new FHIRHistoryBundle(r.responseBody.get, this)
        } catch {
          case e:Throwable =>
            throw FhirClientException("Invalid history result bundle!", Some(r))
        }
      })
  }

}

package io.onfhir.client.parsers

import akka.http.scaladsl.model.headers.{ETag, Location, `Last-Modified`, `WWW-Authenticate`}
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import akka.stream.Materializer
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.{FHIR_CONTENT_TYPES, Resource}
import org.json4s.JsonAST.JObject
import  io.onfhir.util.JsonFormatter._

import scala.concurrent.{ExecutionContext, Future}

object FHIRResponseUnmarshaller {

  implicit val FhirResponseEntityUnmarshaller: Unmarshaller[HttpEntity, Option[Resource]] =
    Unmarshaller
      .stringUnmarshaller
      .forContentTypes(ContentTypeRange.apply(FHIR_CONTENT_TYPES.FHIR_JSON_CONTENT_TYPE))
      .mapWithInput {
        case (entity: HttpEntity, data: String) =>
          if (entity.isKnownEmpty() || data.isEmpty) {
            None
          } else {
            Some(data.parseJson)
          }
      }

  /**
   * Unmarshall an HTTPResponse to FHIR Response
   *
   * @param httpResponse
   * @param ex
   * @param mat
   * @return
   */
  def unmarshallResponse(httpResponse: HttpResponse)(implicit ex: ExecutionContext, mat: Materializer): Future[FHIRResponse] = {
    Unmarshal.apply(httpResponse.entity).to[Option[Resource]]
      .map(rbody => {
        if (httpResponse.status.isFailure())
          FHIRResponse(
            httpStatus = httpResponse.status,
            responseBody = None,
            location = getLocation(httpResponse),
            lastModified = getLastModified(httpResponse),
            newVersion = getNewVersion(httpResponse),
            outcomeIssues = rbody.map(parseOperationOutcome).getOrElse(Nil),
            authenticateHeader = getAuthenticateHeader(httpResponse),
            xCorrelationId = getRawHeader(httpResponse, "X-Correlation-Id"),
            xIntermediary = getRawHeader(httpResponse, "X-Intermediary")
          )
        else
          FHIRResponse(
            httpStatus = httpResponse.status,
            responseBody = rbody,
            location = getLocation(httpResponse),
            lastModified = getLastModified(httpResponse),
            newVersion = getNewVersion(httpResponse),
            authenticateHeader = getAuthenticateHeader(httpResponse),
            xCorrelationId = getRawHeader(httpResponse, "X-Correlation-Id"),
            xIntermediary = getRawHeader(httpResponse, "X-Intermediary")
          )
      })
  }

  private def getLocation(httpResponse: HttpResponse): Option[Uri] = {
    httpResponse.headers
      .find(_.isInstanceOf[Location])
      .map(_.asInstanceOf[Location])
      .map(_.uri)
  }

  private def getLastModified(httpResponse: HttpResponse): Option[DateTime] = {
    httpResponse.headers
      .find(_.isInstanceOf[`Last-Modified`])
      .map(_.asInstanceOf[`Last-Modified`])
      .map(_.date)
  }

  private def getNewVersion(httpResponse: HttpResponse): Option[String] = {
    httpResponse.headers
      .find(_.isInstanceOf[ETag])
      .map(_.asInstanceOf[ETag])
      .map(_.etag.tag)
  }

  private def parseOperationOutcome(operationOutcome: JObject): Seq[OutcomeIssue] = {
    FHIRUtil.extractValueOption[Seq[JObject]](operationOutcome, "issue")
      .getOrElse(Nil)
      .map(parseOutcomeIssue)
  }

  private def parseOutcomeIssue(outcomeIssue: JObject): OutcomeIssue = {
    OutcomeIssue(
      severity = FHIRUtil.extractValue[String](outcomeIssue, "severity"),
      code = FHIRUtil.extractValue[String](outcomeIssue, "code"),
      details = FHIRUtil.extractValueOptionByPath[Seq[String]](outcomeIssue, "details.coding.code").getOrElse(Nil).headOption,
      diagnostics = FHIRUtil.extractValueOption[String](outcomeIssue, "diagnostics"),
      expression =
        FHIRUtil.extractValueOption[Seq[String]](outcomeIssue, "expression")
          .orElse(FHIRUtil.extractValueOption[Seq[String]](outcomeIssue, "location"))
          .getOrElse(Nil)
    )
  }

  private def getAuthenticateHeader(httpResponse: HttpResponse): Option[`WWW-Authenticate`] = {
    httpResponse.headers
      .find(_.isInstanceOf[`WWW-Authenticate`])
      .map(_.asInstanceOf[`WWW-Authenticate`])
  }

  private def getRawHeader(httpResponse: HttpResponse, hname: String): Option[String] = {
    httpResponse.headers
      .find(_.name() == hname)
      .map(_.value())
  }


}

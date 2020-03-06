package io.onfhir.api.model

import akka.http.javadsl.model.headers.WWWAuthenticate
import akka.http.scaladsl.model._
import io.onfhir.api.Resource
import io.onfhir.api._
import io.onfhir.util.JsonFormatter._
import org.json4s.Extraction



/**
  * Object representing a HttpResponse for FHIR protocol
  * @param httpStatus Http status code
  * @param responseBody JSON parsed body of response
  * @param location Location header
  * @param lastModified Last modified header
  * @param newVersion New version number used for ETag header
  * @param outcomeIssues List of OutcomeIssues if response is an error response
  * @param authenticateHeader WWW Authenticate Header
  */
case class FHIRResponse(
                         httpStatus:StatusCode,
                         responseBody:Option[Resource] = None,
                         location:Option[Uri] = None,
                         lastModified:Option[DateTime] = None,
                         newVersion:Option[Long]=None,
                         outcomeIssues:Seq[OutcomeIssue] = Seq.empty,
                         authenticateHeader:Option[WWWAuthenticate] = None) {

  def isError:Boolean = httpStatus.isFailure()
}

/**
  *
  */
object FHIRResponse {
  //Operation Outcome fields and names
  final val OPERATION_OUTCOME = "OperationOutcome"
  final val ISSUE = "issue"
  final val SEVERITY = "severity"
  final val CODE = "code"
  final val DETAILS = "details"
  final val DIAGNOSTICS = "diagnostics"
  final val LOCATION = "location"

  object SEVERITY_CODES {
    val WARNING = "warning"
    val ERROR = "error"
    val FATAL = "fatal"
    val INFORMATION  = "information"
  }

  object OUTCOME_CODES {
    val INVALID = "invalid"
    val NOT_SUPPORTED = "not-supported"
    val INFORMATIONAL = "informational"
    val TRANSIENT = "transient"
    val PROCESSING= "processing"
    val SECURITY = "security"
  }

  /**
    * Helper method to construct FHIRResponse for error responses
    * @param code HTTP status code
    * @param issues Issues related with error
    * @param newVersion new version of resource
    * @return
    */
  def errorResponse(code:StatusCode, issues:Seq[OutcomeIssue], newVersion:Option[Long]=None):FHIRResponse = {
    FHIRResponse(httpStatus = code, newVersion= newVersion, outcomeIssues = issues)
  }

  /**
    * Construct authorization Error response
    * @param issues  issues related with error
    * @param authenticateHeader authentication header to return
    * @return
    */
  def authorizationErrorResponse(issues:Seq[OutcomeIssue], authenticateHeader:Option[WWWAuthenticate] = None):FHIRResponse = {
    FHIRResponse(httpStatus = StatusCodes.Unauthorized, outcomeIssues = issues, authenticateHeader = authenticateHeader)
  }

  /**
    * Create operation outcome from issues
    * @param issues Issues related with result
    * @return
    */
  def createOperationOutcome(issues:Seq[OutcomeIssue]):Resource = {
    import org.json4s.JsonDSL._

    (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> OPERATION_OUTCOME) ~
      (ISSUE -> issues.map(i => Extraction.decompose(i)))
  }

  /**
    * Create Operation Outcome with success
    * @return
    */
  def createOperationOutcomeWithSuccess():Resource = {
    createOperationOutcome(Seq(OutcomeIssue(SEVERITY_CODES.INFORMATION, OUTCOME_CODES.INFORMATIONAL, None, Some("Operation successful :)"), Nil)))
  }
}

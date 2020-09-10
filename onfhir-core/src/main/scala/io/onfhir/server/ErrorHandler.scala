package io.onfhir.server

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{Directives, ExceptionHandler}
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.JsonMappingException
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.exception._
import io.onfhir.api.model.FHIRMarshallers._

import scala.language.implicitConversions
import org.slf4j.{Logger, LoggerFactory}
object ErrorHandler {
  val logger: Logger = LoggerFactory.getLogger(this.getClass.getName)
  /**
    * Exception Handler object
    * @return
    */
  implicit def fhirErrorHandler(fhirRequest:FHIRRequest) =
    ExceptionHandler {
      case e:Exception if fhirErrorHandlerToResponse.isDefinedAt(e) => Directives.complete{
        val response = fhirErrorHandlerToResponse(e)
        fhirRequest.setResponse(response)
        response
      }
    }

  /**
    * Handling of exceptions by converting them to FHIRResponse
    * @return
    */
  def fhirErrorHandlerToResponse:PartialFunction[Exception, FHIRResponse] = {
      /** Unexpected exceptions while parsing - thrown by 3rd party libraries that we dont' control */
      case ex: JsonMappingException =>
        FHIRResponse.errorResponse(
          StatusCodes.BadRequest,
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some("Invalid JSON: " + ex.getMessage),
              Seq(ex.getPathReference)
            )
          )
        )
      case ex: JsonParseException =>
        FHIRResponse.errorResponse(
          StatusCodes.BadRequest,
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some("Invalid JSON: " + ex.getMessage),
              Seq(s"Row[${ex.getLocation.getLineNr}], Col[${ex.getLocation.getColumnNr}]")
            )
          )
        )
      case ex: NumberFormatException =>
        FHIRResponse.errorResponse(
          StatusCodes.BadRequest,
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some("Unaccepted numeric value: " + ex.getMessage),
              Nil
            )
          )
        )
      /** Intentional exceptions for FHIR - thrown intentionally based on cases */
      //Problem in search parameter
      case ex: InvalidParameterException =>
        FHIRResponse.errorResponse(
          StatusCodes.BadRequest,
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(ex.getMessage),
              Nil
            )
          )
        )
      //Unknown parameter
      case  ex:UnsupportedParameterException =>
        FHIRResponse.errorResponse(
          StatusCodes.NotImplemented,
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
              None,
              Some(ex.getMessage),
              Nil
            )
          )
        )
      /** For batch/transaction requests*/
      case ex:AuthorizationFailedException =>
        FHIRResponse.errorResponse(
          StatusCodes.Unauthorized,
          Seq(OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.SECURITY,
            None,
            Some(s"Error: ${ex.authzResult.errorCode.get}; ${ex.authzResult.errorDesc.get}"),
            Seq("Header: Authorization")
          )))
      case ex: ConflictException => FHIRResponse.errorResponse(StatusCodes.Conflict, Seq(ex.outcomeIssue))
      case ex: NotModifiedException => FHIRResponse(StatusCodes.NotModified)
      case ex: NotImplementedException => FHIRResponse.errorResponse(StatusCodes.NotImplemented, ex.outcomeIssues)
      case ex: UnprocessableEntityException => FHIRResponse.errorResponse(StatusCodes.UnprocessableEntity, ex.outcomeIssues)
      case ex: BadRequestException =>
        FHIRResponse.errorResponse(StatusCodes.BadRequest, ex.outcomeIssues)
      case ex: NotFoundException => FHIRResponse.errorResponse(StatusCodes.NotFound, ex.outcomeIssues)
      case ex: PreconditionFailedException => FHIRResponse.errorResponse(StatusCodes.PreconditionFailed, ex.outcomeIssues)
      case ex: MethodNotAllowedException => FHIRResponse.errorResponse(StatusCodes.MethodNotAllowed, ex.outcomeIssues)
      case ex: InternalServerException =>
        FHIRResponse.errorResponse(
          StatusCodes.InternalServerError,
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.TRANSIENT,
              None,
              Some(ex.getMessage),
              Nil
            )
          ) ++ ex.outcomeIssues
        )
      //Any exception
      case ex:Exception =>
        logger.error("Unexpected exception!", ex)
        FHIRResponse.errorResponse(
          StatusCodes.InternalServerError,
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.TRANSIENT,
              None,
              Some(ex.getMessage),
              Nil
            )
          )
        )
  }
}

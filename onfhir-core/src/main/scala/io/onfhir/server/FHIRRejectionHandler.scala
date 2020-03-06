package io.onfhir.server

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.{HttpChallenge, `WWW-Authenticate`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.JsonMappingException
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.exception.AuthorizationFailedRejection

import scala.language.implicitConversions
import scala.xml.{SAXException, SAXParseException}

/**
  * Created by tuncay on 2/22/2017.
  */
object FHIRRejectionHandler {
  /**
    * Rejection handling for onFhir
    * @param fhirRequest
    * @return
    */
  implicit def fhirRejectionHandler(fhirRequest: FHIRRequest): RejectionHandler = {
    RejectionHandler
      .newBuilder()
      .handle {
        case rj:Rejection if fhirRejectionToResponse.isDefinedAt(rj)=>
          val response = fhirRejectionToResponse(rj)
          fhirRequest.setResponse(response)
          complete(response)
      }
      .result()
      .withFallback(RejectionHandler.default)
  }

  /**
    * FHIR rejection handler
    */
  def fhirRejectionToResponse:PartialFunction[Rejection, FHIRResponse] = {
      // Unsupported content type for FHIR protocol
      case UnsupportedRequestContentTypeRejection(supported) =>
        FHIRResponse.errorResponse(
          StatusCodes.UnsupportedMediaType,
          Seq(OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some("Unsupported Content-Type!"),
            Seq("Header: Content-Type")
          ))
        )
      // Authorization Failed Rejection
      case AuthorizationFailedRejection(authzResponse) =>
          FHIRResponse.authorizationErrorResponse(
            Seq(OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.SECURITY,
              None,
              Some(s"Error: ${authzResponse.errorCode.get}; ${authzResponse.errorDesc.get}"),
              Seq("Header: Authorization")
            )),
            Some(`WWW-Authenticate`.apply(HttpChallenge.apply("Bearer", "fhir", Map("error" -> authzResponse.errorCode.get, "error_description" -> authzResponse.errorDesc.get))))
          )

      //Rejections due to exceptions in Request parsing
      case MalformedRequestContentRejection(message, cause)  =>
          cause match {
            /**
              * Our JSON parsing, exceptions
              */
            case jme:JsonMappingException =>
              FHIRResponse.errorResponse(
                StatusCodes.BadRequest,
                Seq(
                  OutcomeIssue(
                    FHIRResponse.SEVERITY_CODES.ERROR,
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some("Invalid JSON: "+jme.getMessage),
                    Seq(jme.getPathReference)
                  ))
              )
            case jpe: JsonParseException =>
              FHIRResponse.errorResponse(
                StatusCodes.BadRequest,
                Seq(
                  OutcomeIssue(
                    FHIRResponse.SEVERITY_CODES.ERROR,
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some("Invalid JSON: "+jpe.getMessage),
                    Seq(s"Row[${jpe.getLocation.getLineNr}], Col[${jpe.getLocation.getColumnNr}]")
                  ))
              )
            case ex: NumberFormatException =>
              FHIRResponse.errorResponse(
                StatusCodes.BadRequest,
                Seq(
                  OutcomeIssue(
                    FHIRResponse.SEVERITY_CODES.ERROR,
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some("Unaccepted numeric value: "+ex.getMessage),
                    Nil
                  ))
              )
            //HAPI XML Parsing
            case dfe:SAXParseException =>
              FHIRResponse.errorResponse(
                StatusCodes.BadRequest,
                Seq(
                  OutcomeIssue(
                    FHIRResponse.SEVERITY_CODES.ERROR,
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some("Invalid XML: "+dfe.getMessage),
                    Nil
                  ))
              )
            //Other
            case x:Throwable => {
              FHIRResponse.errorResponse(
                StatusCodes.BadRequest,
                Seq(
                  OutcomeIssue(
                    FHIRResponse.SEVERITY_CODES.ERROR,
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some("Invalid XML: "+x.getMessage),
                    Nil
                  )),
                None
              )
            }
          }
    }


}

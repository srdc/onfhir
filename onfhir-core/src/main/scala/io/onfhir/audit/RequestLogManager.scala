package io.onfhir.audit

import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.server.{Directive0, Directives}
import io.onfhir.Onfhir
import io.onfhir.api.model.FHIRRequest
import io.onfhir.audit.RequestLogManager.LogRequest
import io.onfhir.config.OnfhirConfig
import io.onfhir.util.JsonFormatter.formats
import org.json4s.jackson.Serialization
import org.json4s.{Extraction, JArray}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContextExecutor

/**
 * Actor that logs failed requests
 */
class RequestLogManager() extends Actor {
  //Actor system
  implicit val actorSystem: ActorSystem = Onfhir.actorSystem
  implicit val executionContext: ExecutionContextExecutor = context.dispatcher // actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")

  //Logger for actor
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Actor commands
   * @return
   */
  def receive: Receive = {
    //Only log errors
    case LogRequest(fhirRequest) if fhirRequest.response.exists(_.isError) =>
      logRequest(fhirRequest)
    //For batch requests, log child request errors
    case LogRequest(fhirRequest) if fhirRequest.childRequests.nonEmpty &&
      fhirRequest.childRequests.exists(_.response.exists(_.isError)) =>
      fhirRequest.childRequests.filter(_.response.exists(_.isError))
        .foreach(childRequest =>
          logRequest(childRequest)
        )
    //We don't log successful requests
    case LogRequest(_) =>
      //Nothing
  }

  /**
   * Log the request and response status and issues
   * @param fhirRequest
   */
  def logRequest(fhirRequest: FHIRRequest) = {
    logger.warn(
      "Problem in FHIR request: " + fhirRequest.getSummaryString() + "\n" +
      s"Response status: ${fhirRequest.response.get.httpStatus.intValue()}, Found issues: " + Serialization.write(JArray(fhirRequest.response.get.outcomeIssues.map(i => Extraction.decompose(i)).toList))
    )
  }

}

/**
 *
 */
object RequestLogManager {
  /**
   * Log the request-response
   * @param fhirRequest FHIR request including the response
   */
  case class LogRequest(fhirRequest: FHIRRequest)

  /**
   * Log request-response according to configurations
   * @param fhirRequest
   * @return
   */
  def logRequest(fhirRequest: FHIRRequest):Directive0 = {
    Directives.mapResponse { httpResponse =>
      //If configured log failed requests
      if(OnfhirConfig.logFailedRequests)
        Onfhir.actorSystem.actorSelection(s"/user/request-response-logger") ! LogRequest(fhirRequest)
      //Return HttpResponse as it is
      httpResponse
    }
  }

  /**
   * Props for Actor
   *
   * @return
   */
  def props() = Props(new RequestLogManager())

}

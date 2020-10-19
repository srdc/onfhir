package io.onfhir.api.endpoint

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Accept, `Content-Type`, `X-Forwarded-For`, `X-Forwarded-Host`}
import akka.http.scaladsl.server.Directives._
import io.onfhir.Onfhir
import io.onfhir.api.FHIR_HTTP_OPTIONS
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.util.FHIRUtil
import io.onfhir.server.ErrorHandler.fhirErrorHandler
import io.onfhir.server.FHIRRejectionHandler.fhirRejectionHandler
import io.onfhir.audit.AuditManager
import io.onfhir.authz.{AuthManager, AuthzConfigurationManager}
import io.onfhir.config.OnfhirConfig
import io.onfhir.server.CORSHandler

/**
  * Encapsulates all services and directives
  * Main Endpoint for FHIR server
  */
trait FHIREndpoint
  extends FHIRCreateEndpoint
  with FHIRDeleteEndpoint
  with FHIRHistoryEndpoint
  with FHIRReadEndpoint
  with FHIRSearchEndpoint
  with FHIRCompartmentSearchEndpoint
  with FHIRUpdateEndpoint
  with SecurityEndpoint
  with FHIRBatchTransactionEndpoint
  with FHIRPatchEndpoint
  with FHIROperationEndpoint
  with CORSHandler {

  /**
    * Transform the Accept header by resolving the response type from format parameter and Accept headers
    * @param resolvedMediaRange Media ranged resolved
    * @param req HttpRequest
    * @return
    */
  private def transformHeaders(resolvedMediaRange:MediaRange)(req:HttpRequest):HttpRequest = {
    val headers = req.headers.filter(_.name != "Accept") :+ Accept.apply(resolvedMediaRange)
    req.copy(req.method, req.uri, headers, req.entity, req.protocol)
  }

  /**
    * Comnination of all routes for a FHIR server applying the common directives
    */
  val routes =
    // logging requests and responses; enabled when necessary for debugging
    //logRequestResponse("REST API", Logging.InfoLevel) {
      corsHandler {
        parameters(FHIR_HTTP_OPTIONS.FORMAT.?) { format: Option[String] =>
          optionalHeaderValueByType[Accept](()) { acceptHeader:Option[Accept] =>
            optionalHeaderValueByType[`Content-Type`](()) { contentType:Option[`Content-Type`] =>
              optionalHeaderValueByType[`X-Forwarded-For`](()) { xForwardedFor =>
                optionalHeaderValueByType[`X-Forwarded-Host`](()) { xForwardedHost =>
                  optionalHeaderValueByName("X-Intermediary") { xIntermediary =>
                    optionalHeaderValueByName("X-Request-Id") { xRequestId =>
                      FHIRUtil.resolveResponseMediaRange(format, contentType, acceptHeader.map(_.mediaRanges).getOrElse(Seq.empty)) match {
                        //If we cannot find any Media Range to respond
                        case None => complete(HttpResponse.apply(StatusCodes.NotAcceptable))
                        case Some(resolvedMediaRange) =>
                          //Resolve the content type that we will respond based on format parameter and Accept headers, and transform the Accept header so spray's mechanism work correctly
                          mapRequest(transformHeaders(resolvedMediaRange)) {
                            extractUri { requestUri: Uri =>
                              //Initialize a FHIR request (child routes will fill the details)
                              val fhirRequest =
                                FHIRRequest(
                                  interaction = "unknown",
                                  requestUri = requestUri.toString(),
                                  xForwardedFor = xForwardedFor,
                                  xForwardedHost = xForwardedHost,
                                  xIntermediary = xIntermediary
                                ).setId(xRequestId)//Set the identifier of the request
                              //Resolve Token/Auth/Authz context
                              AuthManager.authenticate() { authContext =>
                                //Audit the interaction when result is available
                                AuditManager.audit(fhirRequest, authContext._1, authContext._2) {
                                  //Handle any exception with our own directive (See details in ErrorHandler)
                                  handleExceptions(fhirErrorHandler(fhirRequest)) {
                                    //Handle any rejection with our own directive (See details in FHIRRejectionHandler)
                                    handleRejections(fhirRejectionHandler(fhirRequest)) {
                                      //Merge all routes, ORDER IS IMPORTANT among same HTTP methods as they are merged as OR !!!!
                                      var routes =
                                        createRoute(fhirRequest, authContext) ~ updateRoute(fhirRequest, authContext) ~ deleteRoute(fhirRequest, authContext) ~ historyRoute(fhirRequest, authContext) ~ readRoute(fhirRequest, authContext) ~ searchRoute(fhirRequest, authContext) ~ compartmentSearchRoute(fhirRequest, authContext) ~ patchRoute(fhirRequest, authContext) ~ batchRoute(fhirRequest, authContext) ~ operationRoute(fhirRequest, authContext)
                                      //Add the external service routes
                                      Onfhir.apply().externalRoutes.foreach(er => routes = routes ~ er(fhirRequest, authContext))
                                      //Append the security route if our server is secure
                                      if (OnfhirConfig.authzConfig.isSecure())
                                        routes ~ securityRoute
                                      else
                                        routes
                                    }
                                  }
                                }
                              }
                            }
                          }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    //}
}

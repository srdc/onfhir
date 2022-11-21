package io.onfhir.api.endpoint

import akka.http.scaladsl.model.headers.{`If-Modified-Since`, `If-None-Match`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.onfhir.api.{FHIR_HTTP_OPTIONS, FHIR_SEARCH_RESULT_PARAMETERS}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.service.FHIRReadService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.FhirConfigurationManager.authzManager
import io.onfhir.config.OnfhirConfig

/**
  * FHIR Endpoint for Read interaction
  */
trait FHIRReadEndpoint {
  /**
    * Paths for HL7 FHIR read and vread services
    * @param fhirRequest FHIR Request object
    * @param authContext Authentication and Authorization context
    * @return FHIR response
    */
  def readRoute(fhirRequest: FHIRRequest, authContext:(AuthContext, Option[AuthzContext])): Route = {
    (get | head) {
      //GET [base]/metadata -> Return the conformance statement of the server configuration
      pathPrefix("metadata") {
        pathEndOrSingleSlash {
          complete {
            fhirRequest.initializeCapabilitiesRequest()
            new FHIRReadService().completeInteraction(fhirRequest)
          }
        }
      } ~
        //GET [base]/[type]/[id] {?_format=[mime-type]}
        pathPrefix(Segment / Segment) { (_type, _id) =>
          pathEndOrSingleSlash {
            parameters(FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY.?) { summary =>
              parameters(FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS.?) { elements =>
                optionalHeaderValueByType(`If-None-Match`) { ifNoneMatch =>
                  optionalHeaderValueByType(`If-Modified-Since`) { ifModifiedSince =>
                    //Create the FHIR request object
                    fhirRequest.initializeReadRequest(_type, _id, ifModifiedSince, ifNoneMatch, summary, elements)
                    //Enforce authorization
                    authzManager.authorize(authContext._2, fhirRequest) {
                      complete {
                        new FHIRReadService().executeInteraction(fhirRequest)
                      }
                    }

                  }
                }
              }
            }
          }
        } ~
        //GET [base]/[type]/[id]/_history/[vid] {?_format=[mime-type]}
        pathPrefix(Segment / Segment / FHIR_HTTP_OPTIONS.HISTORY / Segment) { (_type, _id, _vid) =>
          pathEndOrSingleSlash {
            //Create the FHIR request object
            fhirRequest.initializeVReadRequest(_type, _id, _vid)
            //Enforce authorization
            authzManager.authorize(authContext._2, fhirRequest) {
              complete {
                new FHIRReadService().executeInteraction(fhirRequest)
              }
            }
          }
        }
    }
  }
}
package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.onfhir.api.{FHIR_HTTP_OPTIONS, FHIR_INTERACTIONS}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIRHistoryService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.OnfhirConfig

/**
  * Endpoint for FHIR History interactions
  */
trait FHIRHistoryEndpoint {
  /**
    * Paths for HL7 FHIR history interactions
    * @param fhirRequest FHIR Request object
    * @param authContext Authentication and Authorization context
    * @return FHIR response
    */
  def historyRoute(fhirRequest: FHIRRequest, authContext:(AuthContext, Option[AuthzContext])):Route = {
    (get | head) {
      optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
        //GET [base]/[type]/[id]/_history {?[parameters]&_format=[mime-type]}
        pathPrefix(OnfhirConfig.baseUri / Segment / Segment / FHIR_HTTP_OPTIONS.HISTORY) { (_type, _id) =>
          pathEndOrSingleSlash {
            //Create the FHIR request object
            fhirRequest.initializeHistoryRequest(FHIR_INTERACTIONS.HISTORY_INSTANCE, Some(_type), Some(_id))
            //Parse search paremeters
            FHIRSearchParameterValueParser.parseSearchParametersFromUri(_type, prefer) { searchParameters =>
              //Put the parameters into the FHIR Request
              fhirRequest.queryParams = searchParameters
              //Enforce authorization, add the authorization filter params to search params
              AuthzManager.authorize(authContext._2, fhirRequest) {
                complete {
                  new FHIRHistoryService().executeInteraction(fhirRequest)
                }
              }
            }
          }
        } ~
          //GET [base]/[type]/_history {?[parameters]&_format=[mime-type]}
          pathPrefix(OnfhirConfig.baseUri / Segment / FHIR_HTTP_OPTIONS.HISTORY) { _type =>
            pathEndOrSingleSlash {
              //Create the FHIR request object
              fhirRequest.initializeHistoryRequest(FHIR_INTERACTIONS.HISTORY_TYPE, Some(_type), None)
              //Parse search paremeters
              FHIRSearchParameterValueParser.parseSearchParametersFromUri(_type, prefer) { searchParameters =>
                //Put the parameters into the FHIR Request
                fhirRequest.queryParams = searchParameters
                //Enforce authorization
                AuthzManager.authorize(authContext._2, fhirRequest) {
                  complete {
                    new FHIRHistoryService().executeInteraction(fhirRequest)
                  }
                }
              }

            }
          } ~
          //GET [base]/_history {?[parameters]&_format=[mime-type]}
          pathPrefix(OnfhirConfig.baseUri / FHIR_HTTP_OPTIONS.HISTORY) {
            pathEndOrSingleSlash {
              //Create the FHIR request object
              fhirRequest.initializeHistoryRequest(FHIR_INTERACTIONS.HISTORY_SYSTEM, None, None)
              //Parse search paremeters
              FHIRSearchParameterValueParser.parseSearchParametersFromUri("", prefer) { searchParameters =>
                fhirRequest.queryParams = searchParameters
                //Enforce authorization, add the authorization filter params to search params
                AuthzManager.authorize(authContext._2, fhirRequest) {
                  complete {
                    new FHIRHistoryService().executeInteraction(fhirRequest)
                  }
                }
              }
            }
          }
      }
    }
  }
}
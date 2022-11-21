package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directives, Route}
import io.onfhir.api.FHIR_HTTP_OPTIONS
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIRSearchService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.FhirConfigurationManager.authzManager
import io.onfhir.config.OnfhirConfig

/**
  * Endpoint for FHIR Search interaction
  */
trait FHIRSearchEndpoint {

  /**
    * Path for HL7 FHIR type level search service
    * GET [base]/[ResourceType]?parameter(s)
    * @param fhirRequest FHIR Request object
    * @param authContext Authentication and Authorization context
    * @return FHIR response
    */
  def searchRoute(fhirRequest: FHIRRequest, authContext:(AuthContext, Option[AuthzContext])):Route = {
    (get | head) {
        //GET [base]/[type]{?[parameters]{&_format=[mime-type]}}
        pathPrefix(Segment) { _type =>
          pathEndOrSingleSlash {
            optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { preferHeader =>
              //Initialize the FHIR request object
              fhirRequest.initializeSearchRequest(_type, preferHeader)
              //Parse search paremeters
              Directives.parameterMultiMap { searchParameters =>
                //Put the parameters and content into the FHIR Request
                fhirRequest.queryParams = searchParameters
                //Enforce authorization, add the authorization filter params to search params
                authzManager.authorize(authContext._2, fhirRequest) {
                  complete {
                      new FHIRSearchService().executeInteraction(fhirRequest)
                  }
                }
              }
            }
          }
        }
      } ~ post {
        //POST  [base]/[type]/_search{?[parameters]{&_format=[mime-type]}}
        pathPrefix(Segment / FHIR_HTTP_OPTIONS.SEARCH) { _type =>
          pathEndOrSingleSlash {
            optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { preferHeader =>
              //Create the FHIR request object
              fhirRequest.initializeSearchRequest(_type, preferHeader)
              Directives.parameterMultiMap { urlParameters =>
                Directives.formFieldMultiMap { entityParameters =>
                  //Put the parameters and content into the FHIR Request
                  fhirRequest.queryParams = urlParameters ++ entityParameters
                  //Enforce authorization, add the authorization filter params to search params
                  authzManager.authorize(authContext._2, fhirRequest) {
                    complete {
                      new FHIRSearchService().executeInteraction(fhirRequest)
                    }
                  }
                }
              }
            }
          }
        }
      } ~ (get | head) {
        //GET [base]{?_type=[resource-types][parameters]{&_format=[mime-type]}}
          pathEndOrSingleSlash {
            optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { preferHeader =>
              //Initialize the FHIR request object
              fhirRequest.initializeSearchRequest(preferHeader)
              //Parse search paremeters
              Directives.parameterMultiMap { searchParameters =>
                //Put the parameters and content into the FHIR Request
                fhirRequest.queryParams = searchParameters
                //Enforce authorization, add the authorization filter params to search params
                authzManager.authorize(authContext._2, fhirRequest) {
                  complete {
                    new FHIRSearchService().executeInteraction(fhirRequest)
                  }
                }
              }
            }
          }
      }
  }
}
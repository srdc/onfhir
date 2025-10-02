package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directives, Route}
import io.onfhir.api.{FHIR_HTTP_OPTIONS, RESOURCE_ID_REGEX, RESOURCE_TYPE_REGEX}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.service.{FHIRDeleteService, TargetResourceResolver}
import io.onfhir.authz.{AuthContext, AuthzContext}
import io.onfhir.config.FhirConfigurationManager.authzManager
import io.onfhir.config.FhirConfigurationManager.targetResourceResolver

/**
  * FHIR Delete Service endpoint
  */
trait FHIRDeleteEndpoint {
  /**
    * Routes for HL7 FHIR delete service
    * @param fhirRequest FHIR Request object
    * @param authContext Authentication and Authorization context
    * @return FHIR response
    */
  def deleteRoute(fhirRequest: FHIRRequest, authContext:(AuthContext, Option[AuthzContext])):Route  = {
    delete {
      //DELETE [base]/[type]/[id]
      pathPrefix(RESOURCE_TYPE_REGEX / RESOURCE_ID_REGEX) { (_type, _id) =>
        pathEndOrSingleSlash {
          //Create the FHIR request object
          fhirRequest.initializeDeleteRequest(_type, Some(_id))
          //Try to resolve target FHIR resource
          targetResourceResolver.resolveTargetResource(fhirRequest) {
            //Check authorization
            authzManager.authorize(authContext._2, fhirRequest) {
              //Complete the delete
              complete {
                new FHIRDeleteService().executeInteraction(fhirRequest)
              }
            }
          }
        }
      } ~
        // Conditional Delete [base]/[type]?{search-params}
        pathPrefix(RESOURCE_TYPE_REGEX) { _type =>
          pathEndOrSingleSlash {
            optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
              //Create the FHIR request object
              fhirRequest.initializeDeleteRequest(_type, None, prefer)
              //Extract parameters if exists
              Directives.parameterMultiMap { searchParameters =>
                //Put the parameters into the FHIR Request
                fhirRequest.queryParams = searchParameters
                //Authorize and filter
                authzManager.authorize(authContext._2, fhirRequest) {
                  //go on with the operation (further authorization may be needed)
                  complete {
                      new FHIRDeleteService().executeInteraction(fhirRequest)
                  }
                }
              }
            }
          }
        }
    }
  }
}
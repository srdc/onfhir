package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.onfhir.api.{Resource, FHIR_HTTP_OPTIONS}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.service.FHIRCreateService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.OnfhirConfig


trait FHIRCreateEndpoint {
  /**
    * Path for HL7 FHIR create service
    * POST [base]/[type] {?_format=[mime-type]}
    *
    * @param fhirRequest FHIR Request object
    * @param authContext Authentication and Authorization context
    * @return FHIR response
    */
  def createRoute(fhirRequest: FHIRRequest, authContext: (AuthContext, Option[AuthzContext])): Route = {
    post {
      pathPrefix(Segment) { _type =>
        pathEndOrSingleSlash {
          optionalHeaderValueByName(FHIR_HTTP_OPTIONS.IF_NONE_EXIST) { ifNoneExist => //for version-aware updates
            optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
              //Initialize the FHIR request object
              fhirRequest.initializeCreateRequest(_type, ifNoneExist, prefer)
              entity(as[Resource]) { resource =>
                //Put the content into the FHIR Request
                fhirRequest.resource = Some(resource)
                //Authorize the interaction
                AuthzManager.authorize(authContext._2, fhirRequest) {
                  //Complete the request
                  complete {
                    new FHIRCreateService().executeInteraction(fhirRequest)
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
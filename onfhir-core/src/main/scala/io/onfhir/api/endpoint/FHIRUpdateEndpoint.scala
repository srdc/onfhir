package io.onfhir.api.endpoint

import akka.http.scaladsl.model.headers.`If-Match`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directives, Route}
import io.onfhir.api.{FHIR_HTTP_OPTIONS, Resource}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIRUpdateService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.OnfhirConfig


trait FHIRUpdateEndpoint {

  /**
    * Path for HL7 FHIR update service
    * @param fhirRequest FHIR Request object
    * @param authContext Authentication and Authorization context
    * @return FHIR response
    */
  def updateRoute(fhirRequest: FHIRRequest, authContext:(AuthContext, Option[AuthzContext])):Route = {
    put{
      optionalHeaderValueByType[`If-Match`](()) { ifMatch => //for version-aware updates
        optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
          //PUT [base]/[type]/[id] {?_format=[mime-type]}  <-----
          pathPrefix(OnfhirConfig.baseUri / Segment / Segment) { (_type, _id) =>
            pathEndOrSingleSlash {
              //Initialize the FHIR request object
              fhirRequest.initializeUpdateRequest(_type, Some(_id), ifMatch, prefer)
              entity(as[Resource]) { resource =>
                //Put the content into the FHIR Request
                fhirRequest.resource = Some(resource)
                //Check authorization
                AuthzManager.authorize(authContext._2, fhirRequest) {
                  //Complete the request
                  complete {
                      new FHIRUpdateService().executeInteraction(fhirRequest)
                  }
                }
              }
            }
          } ~
            //PUT [base]/[type]/?[search parameters] <-----
            pathPrefix(OnfhirConfig.baseUri / Segment) { _type =>
              pathEndOrSingleSlash {
                //Initialize the FHIR request object
                fhirRequest.initializeUpdateRequest(_type, None, ifMatch, prefer)
                //Extract search parameters
                Directives.parameterMultiMap { searchParameters =>
                  fhirRequest.queryParams = searchParameters

                  entity(as[Resource]) { resource =>
                    //Put the parameters and content into the FHIR Request
                    fhirRequest.resource = Some(resource)

                    //Complete the operation, internal authorization !!!
                    AuthzManager.authorize(authContext._2, fhirRequest) {
                      complete {
                        new FHIRUpdateService().executeInteraction(fhirRequest)
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
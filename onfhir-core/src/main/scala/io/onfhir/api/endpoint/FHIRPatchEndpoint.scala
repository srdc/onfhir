package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.onfhir.api.{FHIR_HTTP_OPTIONS, Resource}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIRPatchService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.OnfhirConfig
/**
  * Created by tuncay on 4/28/2017.
  */
trait FHIRPatchEndpoint {

  /**
    * Path for HL7 FHIR patch interaction
    * @param fhirRequest
    * @param authContext
    * @return
    */
  def patchRoute(fhirRequest: FHIRRequest, authContext: (AuthContext, Option[AuthzContext])):Route = {
    patch {
      optionalHeaderValueByName(FHIR_HTTP_OPTIONS.IF_MATCH) { ifMatch => //for version-aware updates
        optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
          //Normal PATCH [base]/[type]/[id] {?_format=[mime-type]}
          pathPrefix(OnfhirConfig.baseUri / Segment / Segment) { (_type, _id) =>
            pathEndOrSingleSlash {
              //Create the FHIR request object
              fhirRequest.initializePatchRequest(_type, Some(_id), ifMatch, prefer)
              entity(as[Resource]) { resource =>
                fhirRequest.resource = Some(resource)
                //Check authorization
                AuthzManager.authorize(authContext._2, fhirRequest) {
                  complete {
                    new FHIRPatchService().executeInteraction(fhirRequest)
                  }
                }
              }
            }
          } ~
            //PATCH [base]/[type]/?[search parameters]
            pathPrefix(OnfhirConfig.baseUri / Segment) { _type =>
              pathEndOrSingleSlash {
                //Create the FHIR request object
                fhirRequest.initializePatchRequest(_type, None, ifMatch, prefer)
                FHIRSearchParameterValueParser.parseSearchParametersFromUri(_type, prefer) { searchParameters =>
                  entity(as[Resource]) { resource =>
                    //Put the parameters and content into the FHIR Request
                    fhirRequest.queryParams = searchParameters
                    fhirRequest.resource = Some(resource)
                    //Complete the operation, internal authorization !!!
                    AuthzManager.authorize(authContext._2, fhirRequest) {
                      complete {
                        new FHIRPatchService().executeInteraction(fhirRequest)
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

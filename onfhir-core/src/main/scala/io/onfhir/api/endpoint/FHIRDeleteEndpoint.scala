package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.onfhir.api.FHIR_HTTP_OPTIONS
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIRDeleteService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.OnfhirConfig

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
      pathPrefix(OnfhirConfig.baseUri / Segment / Segment) { (_type, _id) =>
        pathEndOrSingleSlash {
          //Create the FHIR request object
          fhirRequest.initializeDeleteRequest(_type, Some(_id))
          //Check authorization
          AuthzManager.authorize(authContext._2, fhirRequest) {
            //Complete the delete
            complete {
              new FHIRDeleteService().executeInteraction(fhirRequest)
            }
          }
        }
      } ~
        // Conditional Delete [base]/[type]?{search-params}
        pathPrefix(OnfhirConfig.baseUri / Segment) { _type =>
          pathEndOrSingleSlash {
            optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
              //Create the FHIR request object
              fhirRequest.initializeDeleteRequest(_type, None, prefer)
              FHIRSearchParameterValueParser.parseSearchParametersFromUri(_type, prefer) { searchParameters =>
                //Put the parameters into the FHIR Request
                fhirRequest.queryParams = searchParameters
                //Authorize and filter
                AuthzManager.authorize(authContext._2, fhirRequest) {
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
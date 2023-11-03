package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Directives._
import io.onfhir.api.{FHIR_HTTP_OPTIONS, RESOURCE_ID_REGEX, RESOURCE_TYPE_REGEX}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIRSearchService
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.FhirConfigurationManager.authzManager
import io.onfhir.config.OnfhirConfig

/**
  * Search a specified compartment with a specified resource type in that compartment:
  *    GET [base]/Patient/[id]/[ResourceType]?parameter(s)
  */
/**
  *  Endpoint for FHIR compartment search
  */
trait FHIRCompartmentSearchEndpoint {

  /**
    * Paths for FHIR compartment search interactions
    * @param fhirRequest FHIR Request object
    * @param authContext Authentication and Authorization context
    * @return FHIR response
    */
  def compartmentSearchRoute(fhirRequest: FHIRRequest, authContext:(AuthContext, Option[AuthzContext])) =
    (get | head) {
      //GET [base][/CompartmentType]/[CompartmentId]/[ResourceType]{?[parameters]{&_format=[mime-type]}} =>
      pathPrefix(RESOURCE_TYPE_REGEX / RESOURCE_ID_REGEX / RESOURCE_TYPE_REGEX) { (compartmentName, compartmentId, _type) =>
        pathEndOrSingleSlash {
          optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
            //Create the FHIR request object
            fhirRequest.initializeCompartmentSearchRequest(compartmentName, compartmentId, _type, prefer)
            //Extract search parameters
            Directives.parameterMultiMap { searchParameters =>
              //Set the Query params
              fhirRequest.queryParams = searchParameters
              //Check authorization
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
      //POST [base]/[CompartmentType]/[CompartmentId]/[ResourceType]/_search{?[parameters]{&_format=[mime-type]}} => power2dm.compartment Search(link problem with type)
      pathPrefix(OnfhirConfig.baseUri / RESOURCE_TYPE_REGEX / RESOURCE_ID_REGEX / RESOURCE_TYPE_REGEX / FHIR_HTTP_OPTIONS.SEARCH) { (compartmentName, compartmentId, _type) =>
        pathEndOrSingleSlash {
          optionalHeaderValueByName(FHIR_HTTP_OPTIONS.PREFER) { prefer =>
            //Create the FHIR request object
            fhirRequest.initializeCompartmentSearchRequest(compartmentName, compartmentId, _type, prefer)
            Directives.formFieldMultiMap { searchParameters =>
              //Set the Query params
              fhirRequest.queryParams = searchParameters
              //Check authorization
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
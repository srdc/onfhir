package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.service.FHIROperationHandler
import io.onfhir.authz.{AuthContext, AuthzContext, AuthzManager}
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.{OnfhirConfig, OperationConf}
/**
  * Created by tuncay on 10/2/2017.
  * Endpoint for FHIR operation interactions
  */
trait FHIROperationEndpoint {

  /**
    * Paths for FHIR operation interactions
    * @param fhirRequest
    * @param authContext
    * @return
    */
  def operationRoute(fhirRequest: FHIRRequest, authContext:(AuthContext, Option[AuthzContext])): Route = {
    //Construct all routes for supported operations
    val supportedOperationRoutes:Seq[Route] = fhirConfig.supportedOperations.flatMap(operationConf => {
      //Find out allowed operation method
      val operationMethod:Directive0 = if(operationConf.isHttpGetAllowed()) (post | get) else post
      //Construct all routes for the supported operation for each level
      val operationRoutes:Seq[Route] = operationConf.levels.toSeq.map( level => {
        val operationLevelRoute:Route = level match {
          /**
            * System level operations e.g. 127.0.0.1/fhir/$do-something
            */
          case "system" =>
            operationMethod {
              pathPrefix(OnfhirConfig.baseUri / ("$" + operationConf.name)) {
                finalizingRoute(fhirRequest, authContext, operationConf)
              }
            }
          /**
            * Type level operations e.g. 127.0.0.1/fhir/Observation/$do-something
            */
          case "type" =>
            //If the operation is defined for all Resources
            if (operationConf.resources.contains("Resource")) {
              operationMethod {
                pathPrefix(OnfhirConfig.baseUri / Segment / ("$" + operationConf.name)) { resourceType =>
                  finalizingRoute(fhirRequest, authContext, operationConf, Some(resourceType))
                }
              }
            } else {
              //Otherwise construct for only those Resources
              val typeRoutes: Seq[Route] = operationConf.resources.toSeq.map(rs => {
                operationMethod {
                  pathPrefix(OnfhirConfig.baseUri / rs / ("$" + operationConf.name)) {
                    finalizingRoute(fhirRequest, authContext, operationConf, Some(rs))
                  }
                }
              })
              //Merge all type routes for the operation to a single route
              typeRoutes.reduce((r1, r2) => r1 ~ r2)
            }

          /**
            * Instance level operations e.g. 127.0.0.1/fhir/Observation/545454/$do-something
             */
          case "instance" =>
            if (operationConf.resources.isEmpty || operationConf.resources.contains("Resource")) {
              operationMethod {
                pathPrefix(OnfhirConfig.baseUri / Segment / Segment / ("$" + operationConf.name)) { (resourceType, resourceId) =>
                  finalizingRoute(fhirRequest, authContext, operationConf, Some(resourceType), Some(resourceId))
                }
              }
            } else {
              //Otherwise construct for only those Resources
              val instanceRoutes: Seq[Route] = operationConf.resources.toSeq.map(rs => {
                operationMethod {
                  pathPrefix(OnfhirConfig.baseUri / rs / Segment / ("$" + operationConf.name)) { resourceId =>
                    finalizingRoute(fhirRequest, authContext, operationConf, Some(rs), Some(resourceId))
                  }
                }
              })
              //Merge all instance routes for the operation to a single route
              instanceRoutes.reduce((r1, r2) => r1 ~ r2)
            }
        }
        operationLevelRoute
      })
      operationRoutes
    })
    if(supportedOperationRoutes.nonEmpty)
      //Merge all routes to a single route
      supportedOperationRoutes.reduce((r1, r2) => r1 ~ r2)
    else //Otherwise just reject
      reject
  }

  /**
    * The main part of the Operation endpoint Route construction for a specific operation
    * @param fhirRequest request for the Operation
    * @param authContext Authorization context
    * @param operationConf Operation configuration for the operation that route will be constructed
    * @param resourceType Resource type if operation is on a specific resource type
    * @param resourceId Resource id if operation is on instances
    * @return
    */
  private def finalizingRoute(fhirRequest: FHIRRequest,
                              authContext:(AuthContext, Option[AuthzContext]),
                              operationConf:OperationConf,
                              resourceType:Option[String] = None,
                              resourceId:Option[String] = None):Route = {
    pathEndOrSingleSlash {
      parameterMultiMap { parameters =>
        //Initialize FHIR operation request
        fhirRequest.initializeOperationRequest("$"+operationConf.name, resourceType, resourceId)
        //Initialize parameters in the request
        fhirRequest.operationParameters = parameters
        entity(as[Resource]) { resource =>
          //Initialize operation body
          fhirRequest.resource = if(resource.obj.isEmpty) None else Some(resource)
          AuthzManager.authorize(authContext._2, fhirRequest) {
            complete {
              new FHIROperationHandler().validateAndCompleteOperation(fhirRequest, operationConf)
            }
          }
        }
      }
    }
  }


}

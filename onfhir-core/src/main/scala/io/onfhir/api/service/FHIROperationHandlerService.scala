package io.onfhir.api.service

import io.onfhir.Onfhir
import io.onfhir.api.model.{FHIROperationRequest, FHIROperationResponse}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by tuncay on 10/4/2017.
  * Interface for Operation implementations in onFhir
  */
trait FHIROperationHandlerService {
  implicit val executionContext:ExecutionContext = Onfhir.actorSystem.dispatcher
  /**
    * Execute the operation and prepare the output parameters for the operation
    * @param operationName Operation name as defined after '$' symbol e.g. meta-add
    * @param operationRequest Operation Request including the parameters
    * @param resourceType The resource type that operation is called if exists
    * @param resourceId The resource id that operation is called if exists
    * @return The response containing the Http status code and the output parameters
    */
  def executeOperation(operationName:String, operationRequest:FHIROperationRequest, resourceType:Option[String] = None, resourceId:Option[String] = None):Future[FHIROperationResponse]
}

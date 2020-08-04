package io.onfhir.api.model

/**
 * FHIR Operation request
 * @param operationParams   Input parameters for the operation
 * @param queryParams       Query parameters for the resource type (some operations get normal search parameters to function)
 */
class FHIROperationRequest(operationParams:Seq[(String, FHIROperationParam)], val queryParams:List[Parameter] = List.empty[Parameter]) extends FHIRMultiOperationParam(operationParams) {


}

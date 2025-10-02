package io.onfhir.api.model

import io.onfhir.api.Resource
import io.onfhir.authz.AuthzContext

/**
 * FHIR Operation request
 *
 * @param operationParams   Input parameters for the operation
 * @param queryParams       Query parameters for the resource type (some operations get normal search parameters to function)
 * @param targetResource    For instance level operations, resolved resource content for the target resource
 *                          e.g. Patient/121/$myop --> the Patient/121 resource
 * @param authzContext      Resolved authorization context
 */
class FHIROperationRequest(operationParams:Seq[(String, FHIROperationParam)],
                           val queryParams:List[Parameter] = List.empty[Parameter],
                           val targetResource:Option[Resource] = None,
                           val authzContext:Option[AuthzContext] = None
                          ) extends FHIRMultiOperationParam(operationParams) {


}

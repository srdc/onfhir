package io.onfhir.api.validation

import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRRequest

/**
 * Resource type specific extra validations for resource creation and update (business rules and logic) apart from Resource syntax
 */
trait IResourceSpecificValidator {
  /**
   * Validate extra business rules for the operation
   * @param fhirRequest
   */
  def validateRequest(fhirRequest:FHIRRequest):Unit

  /**
   * Validate rules about changes in the content
   * @param oldContent
   * @param newContent
   */
  def validateChanges(oldContent:Resource, newContent:Resource):Unit

}

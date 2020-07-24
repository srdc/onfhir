package io.onfhir.api.validation

import io.onfhir.api.Resource
import io.onfhir.api.model.FhirReference


trait IReferenceResolver {
  /**
   * Current resource validated
   */
  val resource: Resource

  /**
   *  If resource is a part of a bundle (for inter bundle references), the root part of fullUrl of resource validated and the whole Bundle
   */
  val bundle:Option[(String, Resource)]

  /**
   * Resolve a FHIR reference within onFhir
   * @param reference    FHIR  reference
   * @return
   */
  def resolveReference(reference: FhirReference):Option[Resource]

  /**
   * Check if a referenced resource exist
   * @param reference       FHIR  reference
   * @param profiles        Profiles that resource is expected to conform
   */
  def isReferencedResourceExist(reference: FhirReference, profiles:Set[String]):Boolean

}

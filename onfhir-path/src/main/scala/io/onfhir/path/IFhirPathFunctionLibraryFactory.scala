package io.onfhir.path

trait IFhirPathFunctionLibraryFactory extends Serializable {
  /**
   *
   * @param context
   * @param current
   * @return
   */
  def getLibrary(context:FhirPathEnvironment, current:Seq[FhirPathResult]):AbstractFhirPathFunctionLibrary
}

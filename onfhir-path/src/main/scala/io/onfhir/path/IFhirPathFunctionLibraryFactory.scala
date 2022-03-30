package io.onfhir.path

trait IFhirPathFunctionLibraryFactory {
  /**
   *
   * @param context
   * @param current
   * @return
   */
  def getLibrary(context:FhirPathEnvironment, current:Seq[FhirPathResult]):AbstractFhirPathFunctionLibrary
}

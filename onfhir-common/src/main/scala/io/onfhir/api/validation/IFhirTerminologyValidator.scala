package io.onfhir.api.validation

trait IFhirTerminologyValidator {

  /**
   * Check if a value set is supported for validation within OnFHir setup
   * @param vsUrl   URL of the value set
   * @param version Version of the value set (If not given, the latest version available is chosen)
   * @return
   */
  def isValueSetSupported(vsUrl:String, version:Option[String] = None):Boolean
  /**
   * List all codes in a value set
   * @param vsUrl   URL of the value set
   * @param version Version of the value set (If not given, the latest version available is chosen)
   * @return  All codes in value set from each code system within it
   */
  def getAllCodes(vsUrl:String, version:Option[String] = None):Map[String, Set[String]]

  /**
   * Check if a code is within a ValueSet
   * @param vsUrl       URL of the value set
   * @param version     Version of the value set (If not given, the latest version available is chosen)
   * @param codeSystem  Code system of the code if given
   * @param code        The code to be validated
   * @return
   */
  def validateCodeAgainstValueSet(vsUrl:String, version:Option[String], codeSystem:Option[String], code:String):Boolean
}

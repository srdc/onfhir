package io.onfhir.validation

import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ValueSetDef, ValueSetRestrictions}
import io.onfhir.config.FhirConfig

class FhirTerminologyValidator(fhirConfig:FhirConfig) {

  /**
   * Check if a given ValueSet is supported for validation in this onFhir setup
   * @param vsUrl   Url of ValueSet
   * @param version Business version of ValueSet
   * @return
   */
  def isValueSetSupported(vsUrl:String, version:Option[String] = None):Boolean = {
    getValueSet(vsUrl, version).isDefined
  }

  /**
   * Validate a given system and code pair against a value set
   * @param vsUrl
   * @param version
   * @param codeSystem
   * @param code
   * @return
   */
  def validateCodeAgainstValueSet(vsUrl:String, version:Option[String], codeSystem:Option[String], code:String):Boolean = {
    getValueSet(vsUrl, version) match {
      case None => true
      case Some(vs) =>
        validateCodeBinding(vs, codeSystem, code)
    }
  }

  /**
   * Validate system and code agains given value set definition
   * @param vs
   * @param system
   * @param code
   * @return
   */
  private def validateCodeBinding(vs: ValueSetRestrictions, system:Option[String], code:String):Boolean = {
    //should not be in excludes
    !vs.excludes
      .exists(exc =>
        getCodeList(system, exc).contains(code) || //if it is in excluded list
          exc.valueSets.map(FHIRUtil.parseCanonicalValue).flatMap(v => getValueSet(v._1, v._2)).exists(validateCodeBinding(_, system, code)))  && //or a member of excluded value sets
      //and should be in includes
      (getCodeList(system, vs.includes).contains(code) || //either direct code
        vs.includes.valueSets.map(FHIRUtil.parseCanonicalValue).flatMap(v => getValueSet(v._1, v._2)).exists(validateCodeBinding(_, system, code))) //or a member of a value set
  }

  /**
   * Get list of codes from ValueSet definition
   * @param system
   * @param vs
   * @return
   */
  private def getCodeList(system:Option[String], vs:ValueSetDef):Set[String] = {
    system match {
      case None => vs.codes.values.head
      case Some(s) => vs.codes.getOrElse(s, Set.empty)
    }
  }

  /**
   * Get the ValueSet
   * @param vsUrl
   * @param version
   * @return
   */
  private def getValueSet(vsUrl:String, version:Option[String] = None):Option[ValueSetRestrictions] = {
    fhirConfig.valueSetRestrictions
      .get(vsUrl)
      .flatMap(versionMap => version match {
        case Some(v) => versionMap.get(v)
        case None  => versionMap.headOption.map(_._2)
      })
  }
}

object FhirTerminologyValidator {

  def apply(fhirConfig: FhirConfig): FhirTerminologyValidator = new FhirTerminologyValidator(fhirConfig)
}

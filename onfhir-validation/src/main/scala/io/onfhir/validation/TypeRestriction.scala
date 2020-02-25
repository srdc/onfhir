package io.onfhir.validation

import io.onfhir.api
import io.onfhir.api.FHIR_ROOT_URL_FOR_DEFINITIONS
import io.onfhir.api.model.FHIRResponse
import io.onfhir.api.model.FHIRResponse.OUTCOME_CODES
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{AbstractFhirContentValidator, ConstraintFailure, FhirRestriction}
import io.onfhir.config.OnfhirConfig
import org.json4s.JsonAST.{JObject, JString, JValue}
import io.onfhir.util.JsonFormatter.formats
/**
 * Restriction on the type of an element
 *
 * @param dataTypesAndProfiles Allowed types
 */
case class TypeRestriction(dataTypesAndProfiles:Seq[(String, Seq[String])]) extends  FhirRestriction {
  /**
   * NOT USED (type is evaluated within the FhirContentValidator)
   * @param value Json value
   * @param fhirContentValidator
   * @return
   */
  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
   Nil
  }

  /**
   * Override the matches so we can match TypeRestrictions
   * @param value
   * @param fhirContentValidator
   * @return
   */
  override def matches(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Boolean = {
    value match {
      //If complex content, get all possible profiles
      case jobj:JObject =>
        val allProfiles = dataTypesAndProfiles.flatMap(dt =>
          dt._2 match {
            case Nil => Seq(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/${dt._1}")
            case oth => oth
          }
        )
        //It should match with one of the profiles
        allProfiles.exists(profile =>
          !fhirContentValidator.validateComplexContentAgainstProfile(fhirContentValidator.fhirConfig.findProfileChain(profile),jobj, None).exists(_.severity == FHIRResponse.SEVERITY_CODES.ERROR)
        )
      //Primitive
      case oth =>
        dataTypesAndProfiles.map(_._1).exists(dt => FhirContentValidator.validatePrimitive(oth, dt))
    }
  }
}

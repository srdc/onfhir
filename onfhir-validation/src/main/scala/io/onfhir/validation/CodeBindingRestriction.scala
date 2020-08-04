package io.onfhir.validation

import io.onfhir.api.FHIR_COMMON_FIELDS
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{AbstractFhirContentValidator, ConstraintFailure, FhirRestriction}
import org.json4s.JsonAST.{JArray, JObject, JString, JValue}

/**
 * Value Set binding if strength is given as 'required', 'extensible', 'preferred'
 * @param valueSetUrl   URL of ValueSet where codes are expected
 * @param version       Business Version of ValueSet where codes are expected
 * @param isRequired    If binding strength is required
 */
case class CodeBindingRestriction(valueSetUrl:String, version:Option[String], isRequired:Boolean = true) extends  FhirRestriction {

  /**
   * Evaluate if the fiven FHIR element has the code defined in the value set
   * @param value Json value
   * @return
   */
  override def evaluate(value: JValue, fhirContentValidator: AbstractFhirContentValidator): Seq[ConstraintFailure] = {
    val terminologyValidator =  FhirTerminologyValidator(fhirContentValidator.fhirConfig)
    if(terminologyValidator.isValueSetSupported(valueSetUrl, version)) {
      value match {
        //FHIR code
        case JString(c) =>
          if(!terminologyValidator.validateCodeAgainstValueSet(valueSetUrl, version, None, c))
            Seq(ConstraintFailure(s"Code binding failure, code '$c' is not defined in the ValueSet '$valueSetUrl' or is nor active and selectable!", !isRequired))
          else
            Nil

        //FHIR CodeableConcept
        case obj: JObject if (obj.obj.exists(_._1 == FHIR_COMMON_FIELDS.CODING)) =>
          val systemAndCodes =
            FHIRUtil.extractValueOption[Seq[JObject]](obj, FHIR_COMMON_FIELDS.CODING)
            .map(_.map(coding =>
                FHIRUtil.extractValueOption[String](coding, FHIR_COMMON_FIELDS.SYSTEM) ->
                  FHIRUtil.extractValueOption[String](coding, FHIR_COMMON_FIELDS.CODE))).getOrElse(Nil)
          //One of the codes should be bounded to given
          if(systemAndCodes.nonEmpty && !systemAndCodes.exists(sc => sc._1.isDefined && sc._2.isDefined && terminologyValidator.validateCodeAgainstValueSet(valueSetUrl, version, sc._1, sc._2.get)))
            Seq(ConstraintFailure(s"Code binding failure, none of the system-code pairing '${printSystemCodes(systemAndCodes)}' is defined in the ValueSet '$valueSetUrl' or is active and selectable!", !isRequired))
          else
            Nil

        //FHIR Quantity, Coding
        case obj: JObject =>
          //Extract the system and code if exists
          val (system, code)  = FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.SYSTEM) ->
            FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.CODE)
          //If there is binding, they should exist and defined in the value set
          if(system.isEmpty || code.isEmpty || ! terminologyValidator.validateCodeAgainstValueSet(valueSetUrl, version,system, code.get))
            Seq(ConstraintFailure(s"Code binding failure, system-code pairing '${printSystemCodes(Seq(system -> code))}' is not defined in the ValueSet '$valueSetUrl' or is not active and selectable!", !isRequired))
          else
            Nil
        case _ => Nil
      }
    } else {
      Seq(ConstraintFailure(s"Unknown or not processable ValueSet '$valueSetUrl' for validation, skipping code binding validation...", isWarning = true))
    }
  }

  private def printSystemCodes(systemAndCodes:Seq[(Option[String], Option[String])]) =
    systemAndCodes.map(sc => "(" +sc._1.getOrElse("' '") + "," + sc._2.getOrElse("' '") + ")" ).mkString(", ")
}

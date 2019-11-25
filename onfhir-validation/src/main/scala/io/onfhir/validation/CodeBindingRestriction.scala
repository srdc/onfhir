package io.onfhir.validation

import io.onfhir.api.FHIR_COMMON_FIELDS
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction}
import org.json4s.JsonAST.{JObject, JString, JValue}


/**
 * Value Set binding if required
 *
 * @param valueSetUrl URL of value set
 */
case class CodeBindingRestriction(valueSetUrl:String) extends  FhirRestriction {

  /**
   * Get system and code from the value
   *
   * @param value
   * @return
   */
  private def getSystemAndCodes(value: JValue): Seq[(Option[String], Option[String])] = {
    value match {
      //FHIR code
      case JString(s) => Seq(None -> Some(s))
      //FHIR CodeableConcept
      case obj: JObject if (obj.obj.exists(_._1 == FHIR_COMMON_FIELDS.CODING)) =>
        FHIRUtil.extractValue[Seq[JObject]](obj, FHIR_COMMON_FIELDS.CODING)
          .map(coding =>
            FHIRUtil.extractValueOption[String](coding, FHIR_COMMON_FIELDS.SYSTEM) ->
              FHIRUtil.extractValueOption[String](coding, FHIR_COMMON_FIELDS.CODE))
      case obj: JObject =>
        //FHIR Quantity, Coding
        Seq(FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.SYSTEM) ->
          FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.CODE))
      case _ => Nil
    }
  }

  override def evaluate(value: JValue): Seq[ConstraintFailure] = {
    val systemAndCodes = getSystemAndCodes(value)
    value match {
      //FHIR code
      case JString(_) =>
        val code = systemAndCodes.head._2.get
        //TODO Check if code is within the value set
        Nil
      //CodeableConcept, Coding, Quantity
      case JObject(_) =>
        if (systemAndCodes.exists(sc => sc._1.isEmpty || sc._2.isEmpty))
          Seq(ConstraintFailure(s"CodeableConcept element does not have 'code' or 'system' field although code binding is specified as required for valueset '$valueSetUrl'!"))
        else {
          systemAndCodes
            .map(sc => sc._1.get -> sc._2.get)
          //TODO Check if system and code is within the value set
          Nil
        }
      case _ =>
        Nil
    }
  }
}

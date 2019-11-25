package io.onfhir.validation

import io.onfhir.api.FHIR_ROOT_URL_FOR_DEFINITIONS
import io.onfhir.api.model.FHIRResponse.OUTCOME_CODES
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction}
import org.json4s.JsonAST.{JObject, JValue}
import io.onfhir.util.JsonFormatter.formats
/**
 * Restriction on the type of an element
 *
 * @param dataTypes Allowed types
 */
case class TypeRestriction(dataTypes:Set[String]) extends  FhirRestriction {
  override def evaluate(value:JValue):Seq[ConstraintFailure] = {
    value match {
      //If it is a complex type
      case JObject(obj) =>
        obj.find(_._1 == "reference") match {
          //Not a FHIR Reference
          case None =>
            //If it is valid against a data type
            if(dataTypes.exists(dt =>
              !FhirContentValidator.validateComplexContentAgainstProfile(FHIR_ROOT_URL_FOR_DEFINITIONS + "/" + dt, JObject(obj), None, Nil).exists(oi =>  oi.severity == OUTCOME_CODES.INVALID)
            ))
              Nil
            else
              Seq(ConstraintFailure(s"Type of the value does not match with any of expected FHIR types ${dataTypes}!"))
          //If it is a FHIR reference, check the type of the target refered resource
          case Some(rv) =>
            if(dataTypes.contains(FHIRUtil.parseReferenceValue(rv._2.extract[String])._2))
              Nil
            else
              Seq(ConstraintFailure(s"Type of the referenced value does not match with any of expected FHIR types ${dataTypes}!"))
        }
      //If it is a simple type, one of the data type should match
      case _ =>
        if(!dataTypes.filter(dt => dt.head.isLower).exists(dt => FhirContentValidator.validatePrimitive(value, dt)))
          Seq(ConstraintFailure(s"Type of the value does not match with any of expected FHIR types ${dataTypes}!"))
        else
          Nil
    }
  }
}

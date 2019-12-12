package io.onfhir.validation

import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction, AbstractFhirContentValidator}
import org.json4s.JsonAST.{JArray, JNothing, JNull, JValue}

/**
 * Indicate that element should be array or not
 */
case class ArrayRestriction(isArray:Boolean = true) extends  FhirRestriction {
  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    val isOk =
      value match {
        case JArray(_) | JNull | JNothing  => isArray //If array it should have less or equal elements
        case _ => !isArray
      }
    if(!isOk)
      Seq(ConstraintFailure(if(isArray) s"Element should be given as array not as an object or simple value!" else s"Element should not be given as array, it is an object or simple value!"))
    else
      Nil
  }
}

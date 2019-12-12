package io.onfhir.validation

import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction, AbstractFhirContentValidator}
import org.json4s.JsonAST.{JString, JValue}


/**
 * Max length restrictions for strings
 *
 * @param n Max length of string
 */
case class MaxLengthRestriction(n:Int) extends  FhirRestriction {
  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    val actual = value.asInstanceOf[JString].s.length
    val isOk = actual <= n
    if(!isOk)
      Seq(ConstraintFailure(s"Length of string ($actual) should not be more than expected 'maxLength=$n'!"))
    else
      Nil
  }
}


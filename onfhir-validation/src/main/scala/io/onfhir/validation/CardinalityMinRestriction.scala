package io.onfhir.validation

import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction, AbstractFhirContentValidator}
import org.json4s.JsonAST.{JArray, JNothing, JNull, JValue}

/**
 * Min cardinality restriction (If a min cardinality bigger than 0 is given)
 *
 * @param n
 */
case class CardinalityMinRestriction(n:Int) extends  FhirRestriction {
  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    val (isOk, actual) =
      value match {
        case JArray(arr) => (arr.length >= n) -> arr.length //If array it should have more or equal elements
        case JNull | JNothing => false -> 0
        case _ => (n == 1) -> 1 //If object or simple element, min restriction should equal 1
      }
    if(!isOk)
      Seq(ConstraintFailure(s"Min cardinality expected '$n' does not match with actual cardinality $actual!"))
    else
      Nil
  }
}

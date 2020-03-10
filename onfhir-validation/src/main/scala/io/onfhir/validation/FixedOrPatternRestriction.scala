package io.onfhir.validation

import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction, AbstractFhirContentValidator}
import org.json4s.JsonAST.{JArray, JNothing, JObject, JValue}
import org.json4s.jackson.JsonMethods.{compact, render}

/**
 * If there is a fixed value
 *
 * @param fixedValue
 */
case class FixedOrPatternRestriction(fixedValue:JValue, isFixed:Boolean) extends  FhirRestriction {
  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    if(isFixed) {
      //Take the difference
      val diff = value.diff(fixedValue)
      //If there is no difference, they are equal
      if(diff.added == JNothing && diff.changed == JNothing && diff.deleted == JNothing)
        Nil
      else
        Seq(ConstraintFailure(s"Given value (${compact(render(value))}) is not equal to fixed specified value; ${compact(render(fixedValue))}!"))
    } else {
      if(!hasPattern(fixedValue, value))
        Seq(ConstraintFailure(s"Given value (${compact(render(value))}) does not conform to pattern; ${compact(render(fixedValue))}!"))
      else
        Nil
    }
  }

  private def hasPattern(pattern:JValue, value:JValue): Boolean = {
    pattern.getClass == value.getClass &&
      (
        pattern match {
          case JObject(obj) => obj.forall(f => value.asInstanceOf[JObject].obj.exists(f2 => f2._1 == f._1 && hasPattern(f._2, f2._2)))
          case JArray(arr) =>
            val actualArr = value.asInstanceOf[JArray]
            arr.forall(i => actualArr.arr.exists(i2 => hasPattern(i, i2)))
          case _ => pattern == value
        }
        )
  }

}
package io.onfhir.validation

import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction, AbstractFhirContentValidator}
import io.onfhir.path.FhirPathEvaluator
import org.json4s.JsonAST.{JObject, JValue}
import io.onfhir.util.JsonFormatter.formats
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods._
/**
 * If there is a min or max value defined,
 *
 * @param comparedValue the literal FHIR path expression for the value
 * @param isMin if it is minimum
 */
case class MinMaxValueRestriction(comparedValue:JValue, isMin:Boolean) extends  FhirRestriction {

  /**
   * Convert a quantity object to literal
   * @param quantity
   * @return
   */
  private def convertQuantityToLiteral(quantity:JObject):String = {
    val literal = FHIRUtil.extractValue[Double](quantity, "value") + " " +
      FHIRUtil
        .extractValueOption[String](quantity, "code")
        .map(unit => "'"+unit+"'")
        .getOrElse("")

    literal.trim()
  }

  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    val comparator = if(isMin) "<=" else ">="

    val (compared, actual) = value match {
      case obj:JObject =>
        convertQuantityToLiteral(comparedValue.asInstanceOf[JObject])  -> convertQuantityToLiteral(obj)
      case simple => comparedValue.extract[String]   -> simple.extract[String]
    }
    val fhirExpression = compared + " " + comparator + " " + actual

    val isOk = FhirPathEvaluator().satisfies(fhirExpression, JObject())
    if(!isOk)
      Seq(ConstraintFailure(s"Given value ($actual) fails for '${if(isMin) "minValue" else "maxValue"}=$compared' restriction!"))
    else
      Nil
  }
}

package io.onfhir.validation

import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction}
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.path.grammar.FhirPathExprParser
import org.json4s.JsonAST.JValue


/**
 * FHIR Constraints on this element
 *
 * @param fhirConstraints
 */
case class ConstraintsRestriction(fhirConstraints: Seq[FhirConstraint]) extends FhirRestriction {
  override def evaluate(value: JValue): Seq[ConstraintFailure] = {
    fhirConstraints.flatMap(_.evaluate(value))
  }
}

/**
 * FHIR Constraints on this element
 *
 * @param key       Name of constraint
 * @param desc      Description of constraint
 * @param expr      Parsed FHIR path expression
 * @param isWarning If it is only warning
 */
case class FhirConstraint(key: String, desc: String, expr: FhirPathExprParser.ExpressionContext, isWarning: Boolean = false) {

  /**
   * Evaluate a FHIR Path constraint
   *
   * @param value
   * @return
   */
  def evaluate(value: JValue): Option[ConstraintFailure] = {
    if (!FhirPathEvaluator.satisfiesParsed(expr, value))
      Some(ConstraintFailure(s"Constraint '$key' is not satisfied for the given value! Constraint Description: $desc. FHIR Path expression: ${expr.getText}", isWarning))
    else
      None
  }
}

package io.onfhir.expression

import io.onfhir.path.{FhirPathBoolean, FhirPathComplex, FhirPathDateTime, FhirPathEvaluator, FhirPathNumber, FhirPathQuantity, FhirPathString, FhirPathTime}
import org.json4s.{JNothing, JValue}
import io.onfhir.util.JsonFormatter._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex

trait IFhirExpressionLanguageHandler {
  private val internalPlaceholderExpr = """\{\{([^"{}]+)}\}""".r
  /**
   * Support language mime type e.g. text/fhirpath, application/x-fhir-query, text/template
   */
  val languageSupported: String


  /**
   *
   * @param expression
   * @throws FhirExpressionException if expression has a problem in syntax
   */
  @throws[FhirExpressionException]
  def validateExpression(expression: FhirExpression):Unit

  /**
   * Evaluate the given expression
   *
   * @param expression    Expression content
   * @param contextParams Supplied context parameters for the evaluation
   * @param input         Input content for the evaluation
   * @return              FHIR content generated after evaluating the expression
   * @throws
   */
  @throws[FhirExpressionException]
  def evaluateExpression(expression: FhirExpression, contextParams: Map[String, JValue], input:JValue = JNothing)(implicit ex:ExecutionContext): Future[JValue]

  /**
   * Evaluate the given boolean expression
   * @param expression      Expression content
   * @param contextParams   Supplied context for the evaluation
   * @param input           Supplied input for the evaluation
   * @param ex              Execution context
   * @throws
   * @return
   */
  @throws[FhirExpressionException]
  def satisfies(expression: FhirExpression, contextParams: Map[String, JValue], input:JValue = JNothing)(implicit ex:ExecutionContext): Future[Boolean]


  /**
   * Utility function to handle internal placeholders
   * @param strValue
   * @param fhirPathEvaluator
   * @param input
   * @return
   */
  protected def handleInternalPlaceholderMatches(strValue:String, fhirPathEvaluator:FhirPathEvaluator, input:JValue):String = {
    def findMatch(m:Regex.Match):String = {
      val fhirPathExpression = m.group(1)
      val results = fhirPathEvaluator.evaluate(fhirPathExpression, input)
      if(results.isEmpty)
        throw FhirExpressionException(s"FHIR path expression '$fhirPathExpression' returns empty result although it is used within a FHIR search statement!")
      (
        results map {
          case FhirPathComplex(_) | FhirPathQuantity(_, _) =>
            throw FhirExpressionException(s"FHIR path expression '$fhirPathExpression' returns complex JSON object although it is used within a FHIR search statement!")
          case FhirPathString(s) => s
          case FhirPathNumber(n) => "" + n.toDouble.toString
          case FhirPathBoolean(b) => "" + b
          case dt:FhirPathDateTime => dt.toJson.toJson.dropRight(1).drop(1)
          case t:FhirPathTime => t.toJson.toJson.dropRight(1).drop(1)
        }
        ).mkString(",")  // If result is multiple
    }
    //Fill the placeholder values
    val filledValue = internalPlaceholderExpr.replaceAllIn(strValue, replacer = findMatch)
    filledValue
  }
}

package io.onfhir.expression

import org.json4s.{JNothing, JValue}
import scala.concurrent.{ExecutionContext, Future}

trait IFhirExpressionLanguageHandler {
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

}

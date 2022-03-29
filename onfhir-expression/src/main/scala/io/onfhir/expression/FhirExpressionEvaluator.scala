package io.onfhir.expression

import org.json4s.JsonAST.JValue

import scala.concurrent.{ExecutionContext, Future}

/**
 * Entrypoint for FHIR expression evaluation
 *
 * @param languageHandlers  Modules that handle different expression languages
 */
class FhirExpressionEvaluator(languageHandlers:Seq[IFhirExpressionLanguageHandler]) extends IFhirExpressionLanguageHandler {
  /**
   * Support language mime type e.g. text/fhirpath, application/x-fhir-query, text/template
   */
  override val languageSupported: String = "*"

  /**
   * Find the handler that supports the evaluation of given expression according to the expression language
   * @param expression  Parsed expression itself
   * @return
   */
  def findHandler(expression: FhirExpression):IFhirExpressionLanguageHandler = {
    languageHandlers
      .find(_.languageSupported == expression.language) match {
        case None => throw FhirExpressionException(s"Expression language '${expression.language}' not supported!")
        case Some(handler) => handler
      }
  }

  /**
   * Basic validation for the expression
   * @param expression  Parsed expression itself
   */
  override def validateExpression(expression:FhirExpression):Unit = {
    findHandler(expression)
      .validateExpression(expression)
  }

  /**
   * Evaluate the given expression
   * @param expression      Expression content
   * @param contextParams   Supplied context for the evaluation
   * @param input           Supplied input for the evaluation
   * @param ex              Execution context
   * @param ex
   * @return
   */
  def evaluateExpression(expression: FhirExpression, contextParams: Map[String, JValue], input:JValue)(implicit ex:ExecutionContext): Future[JValue] = {
    findHandler(expression)
      .evaluateExpression(expression, contextParams, input)
  }

  /**
   * Evaluate the given boolean expression
   * @param expression      Expression content
   * @param contextParams   Supplied context for the evaluation
   * @param input           Supplied input for the evaluation
   * @param ex              Execution context
   *  @return
   */
  def satisfies(expression: FhirExpression, contextParams: Map[String, JValue], input:JValue)(implicit ex:ExecutionContext): Future[Boolean] = {
    findHandler(expression)
      .satisfies(expression, contextParams, input)
  }
}

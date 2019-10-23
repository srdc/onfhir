package io.onfhir.path

import java.io.ByteArrayInputStream
import java.nio.charset.{Charset, StandardCharsets}
import java.time.{LocalTime, ZoneId}
import java.time.temporal.Temporal

import io.onfhir.api.Resource
import io.onfhir.path.grammar.{FhirPathExprLexer, FhirPathExprParser}
import org.antlr.runtime.{ANTLRInputStream, ANTLRStringStream, CharStream}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.slf4j.{Logger, LoggerFactory}

object FhirPathEvaluator {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Parse a FHIR path expression (by using antlr4)
    * @param input Expression
    * @return Parsed expression
    */
  def parse(input:String):FhirPathExprParser.ExpressionContext = {
    logger.debug(s"Parsing FHIR path expression '$input' ...")
    val stream = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
    val lexer = new FhirPathExprLexer(CharStreams.fromStream(stream, StandardCharsets.UTF_8))
    val parser = new FhirPathExprParser(new CommonTokenStream(lexer))
    parser.expression()
  }

  /**
    * Evaluate a FHIR path expression
    * @param expr       Parsed expression
    * @param resource   Input FHIR resource
    * @return
    */
  def evaluate(expr:FhirPathExprParser.ExpressionContext, resource:Resource):Seq[FhirPathResult] = {
    logger.debug(s"Evaluating FHIR path expression '${expr.getText}' ...")
    val environment = new FhirPathEnvironment(resource)
    val evaluator = new FhirPathExpressionEvaluator(environment, Seq(FhirPathComplex(resource)))
    evaluator.visit(expr)
  }

  /**
    * Parse and evaluate a FHIR path expression
    * @param expr
    * @param resource
    * @return
    */
  def evaluate(expr:String, resource: Resource):Seq[FhirPathResult] = {
    val parsedExpr = parse(expr)
    evaluate(parsedExpr, resource)
  }

  /**
    * Check if the given resource satisfies the FHIR path expression (should return boolean)
    * @param expr
    * @param resource
    * @return
    */
  def satisfies(expr:String, resource: Resource):Boolean = {
    val result = evaluate(expr, resource)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathBoolean])
      throw new Exception(s"Expression $expr does not evaluate to a boolean for the given resource!")
    result.head.asInstanceOf[FhirPathBoolean].b
  }

  def evaluateNumerical(expr:String, resource: Resource):BigDecimal = {
    val result = evaluate(expr, resource)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathNumber])
      throw new Exception(s"Expression $expr does not evaluate to a number for the given resource!")
    result.head.asInstanceOf[FhirPathNumber].v
  }

  def evaluateDateTime(expr:String, resource: Resource):Temporal = {
    val result = evaluate(expr, resource)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathDateTime])
      throw new Exception(s"Expression $expr does not evaluate to a datetime for the given resource!")
    result.head.asInstanceOf[FhirPathDateTime].dt
  }

  def evaluateTime(expr:String, resource: Resource):(LocalTime, Option[ZoneId]) = {
    val result = evaluate(expr, resource)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathTime])
      throw new Exception(s"Expression $expr does not evaluate to a time for the given resource!")
    val t = result.head.asInstanceOf[FhirPathTime]
    t.lt -> t.zone
  }

  def evaluateString(expr:String, resource: Resource):Seq[String] = {
    val result = evaluate(expr, resource)
    if(result.exists(!_.isInstanceOf[FhirPathString]))
      throw new Exception(s"Expression $expr does not evaluate to a string for the given resource!")
    result.map(_.asInstanceOf[FhirPathString].s)
  }

}

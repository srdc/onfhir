package io.onfhir.path

import java.io.ByteArrayInputStream
import java.nio.charset.{Charset, StandardCharsets}
import java.time.{LocalTime, ZoneId}
import java.time.temporal.Temporal

import io.onfhir.api.validation.IReferenceResolver
import io.onfhir.path.grammar.{FhirPathExprLexer, FhirPathExprParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.json4s.JsonAST.JValue
import org.slf4j.{Logger, LoggerFactory}

class FhirPathEvaluator (referenceResolver:Option[IReferenceResolver] = None) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private def normalizeInput(input:String):String = {
    //As function 'contains' and literal 'contains' clash in the grammar, we have
    if(input.contains("contains(")) input.replace("contains(", "_contains(") else input
  }

  /**
   * Parse a FHIR path expression (by using antlr4)
   * @param input Expression
   * @return Parsed expression
   */
  def parse(input:String):FhirPathExprParser.ExpressionContext = {
    logger.debug(s"Parsing FHIR path expression '$input' ...")
    val stream = new ByteArrayInputStream(normalizeInput(input).getBytes(StandardCharsets.UTF_8))
    val lexer = new FhirPathExprLexer(CharStreams.fromStream(stream, StandardCharsets.UTF_8))
    val parser = new FhirPathExprParser(new CommonTokenStream(lexer))
    parser.expression()
  }

  /**
   * Evaluate a FHIR path expression
   * @param expr Parsed expression
   * @param on   Input FHIR content
   * @return
   */
  private def evaluate(expr:FhirPathExprParser.ExpressionContext, on:JValue):Seq[FhirPathResult] = {
    logger.debug(s"Evaluating FHIR path expression '${expr.getText}' ...")
    val resource = FhirPathValueTransformer.transform(on)
    val environment = new FhirPathEnvironment(resource.head, referenceResolver)
    val evaluator = new FhirPathExpressionEvaluator(environment, resource)
    evaluator.visit(expr)
  }

  /**
   * Parse and evaluate a FHIR path expression
   * @param expr FHIR Path expression
   * @param on   Input FHIR content
   * @return
   */
  def evaluate(expr:String, on:JValue):Seq[FhirPathResult] = {
    val parsedExpr = parse(expr)
    evaluate(parsedExpr, on)
  }

  /**
   * Check if the given resource satisfies the FHIR path expression (should return boolean)
   * @param expr FHIR Path expression in string format
   * @param on  Input FHIR content
   * @return
   */
  def satisfies(expr:String, on:JValue):Boolean = {
    val result = evaluate(expr, on)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathBoolean])
      throw new FhirPathException(s"Expression $expr does not evaluate to a boolean for the given resource!")
    result.head.asInstanceOf[FhirPathBoolean].b
  }

  /**
   * Check if the given resource satisfies the FHIR path expression (should return boolean)
   * @param expr     Parsed FHIR Path expression
   * @param on       Input FHIR content
   * @return
   */
  def satisfiesParsed(expr:FhirPathExprParser.ExpressionContext, on:JValue):Boolean = {
    val result = evaluate(expr, on)
    result match {
      //Then rule is not relevant
      case Nil => true
      //Otherwise we are expecting a single boolean result
      case Seq(FhirPathBoolean(b)) => b
      case _ => throw new FhirPathException(s"Expression $expr does not evaluate to a single boolean for the given resource!")
    }
  }

  def evaluateNumerical(expr:String, on:JValue):BigDecimal = {
    val result = evaluate(expr, on)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException(s"Expression $expr does not evaluate to a number for the given resource!")
    result.head.asInstanceOf[FhirPathNumber].v
  }

  def evaluateDateTime(expr:String, on:JValue):Temporal = {
    val result = evaluate(expr, on)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathDateTime])
      throw new FhirPathException(s"Expression $expr does not evaluate to a datetime for the given resource!")
    result.head.asInstanceOf[FhirPathDateTime].dt
  }

  def evaluateTime(expr:String, on:JValue):(LocalTime, Option[ZoneId]) = {
    val result = evaluate(expr, on)
    if(result.length != 1 || !result.head.isInstanceOf[FhirPathTime])
      throw new FhirPathException(s"Expression $expr does not evaluate to a time for the given resource!")
    val t = result.head.asInstanceOf[FhirPathTime]
    t.lt -> t.zone
  }

  def evaluateString(expr:String, on:JValue):Seq[String] = {
    val result = evaluate(expr, on)
    if(result.exists(!_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Expression $expr does not evaluate to a string for the given resource!")
    result.map(_.asInstanceOf[FhirPathString].s)
  }
}

object FhirPathEvaluator {

  def apply(referenceResolver: IReferenceResolver): FhirPathEvaluator = new FhirPathEvaluator(Some(referenceResolver))

  def apply(): FhirPathEvaluator = new FhirPathEvaluator(None)

  def apply(referenceResolver: Option[IReferenceResolver]): FhirPathEvaluator = new FhirPathEvaluator(referenceResolver)
}

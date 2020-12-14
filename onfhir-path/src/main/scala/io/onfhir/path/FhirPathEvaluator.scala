package io.onfhir.path

import java.io.ByteArrayInputStream
import java.nio.charset.{Charset, StandardCharsets}
import java.time.{LocalTime, ZoneId}
import java.time.temporal.Temporal

import io.onfhir.api.validation.IReferenceResolver
import io.onfhir.path.grammar.{FhirPathExprLexer, FhirPathExprParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.json4s.JsonAST.{JArray, JValue}
import org.slf4j.{Logger, LoggerFactory}

/**
 * Fhir Path Engine implementation
 * @param referenceResolver     onFhir reference resolver (resolving literal and inter-bundle references)
 * @param environmentVariables  Supplied environment variables
 */
case class FhirPathEvaluator (referenceResolver:Option[IReferenceResolver] = None, environmentVariables:Map[String, JValue] = Map.empty) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private def normalizeInput(input:String):String = {
    //As function 'contains' and literal 'contains' clash in the grammar, we have
    if(input.contains("contains(")) input.replace("contains(", "_contains(") else input
  }

  def withEnviromentVariable(variable:String, value:JValue):FhirPathEvaluator = {
    this.copy(environmentVariables = environmentVariables + (variable -> value))
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
    val environment = new FhirPathEnvironment(resource, referenceResolver, environmentVariables.mapValues(FhirPathValueTransformer.transform))
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

  def evaluateOptionalNumerical(expr:String, on:JValue):Option[BigDecimal] = {
    val results = evaluateNumerical(expr, on)
    if(results.length > 1)
      throw new FhirPathException(s"Expression $expr does not evaluate to a single number for the given resource!")
    results.headOption
  }

  def evaluateNumerical(expr:String, on:JValue):Seq[BigDecimal] = {
    val result = evaluate(expr, on)
    if(result.exists(!_.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Expression $expr does not evaluate to numbers for the given resource!")
    result.map(_.asInstanceOf[FhirPathNumber].v)
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

  def evaluateOptionalString(expr:String, on:JValue):Option[String] = {
    val result = evaluate(expr, on)
    if(result.length > 1 || result.exists(!_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Expression $expr does not evaluate to a single string for the given resource!")
    result.headOption.map(_.asInstanceOf[FhirPathString].s)
  }

  def evaluateString(expr:String, on:JValue):Seq[String] = {
    val result = evaluate(expr, on)
    if(result.exists(!_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Expression $expr does not evaluate to a string for the given resource!")
    result.map(_.asInstanceOf[FhirPathString].s)
  }

  /**
   * Evaluate the expression and return it as JSON value
   * @param expr
   * @param on
   * @return
   */
  def evaluateAndReturnJson(expr:String, on:JValue):Option[JValue] = {
    val result = evaluate(expr, on)
    val results = result.map(_.toJson)
    results.length match {
      case 0 => None
      case 1 => Some(results.head)
      case _ => Some(JArray(results.toList))
    }
  }

  /**
   * Evaluate a FHIR Path expression that indicates single or multiple paths within a resource to find those paths e.g. Observation.code.coding.where(system ='...').code --> code.coding[2].code
   * @param expr  FHIR Path Expression
   * @param on    The resource that expression will be evaluated on
   * @return      List of paths consist of element names in order and their array index if exist
   */
  def evaluateToFindPaths(expr:String, on:JValue):Seq[Seq[(String, Option[Int])]] = {
    logger.debug(s"Evaluating FHIR path expression '${expr}' to find indicated paths  ...")
    val parsedExpr = parse(expr)
    val resource = FhirPathValueTransformer.transform(on)
    val environment = new FhirPathEnvironment(resource, referenceResolver,environmentVariables.mapValues(FhirPathValueTransformer.transform))
    val evaluator = new FhirPathPathFinder(environment, resource)
    evaluator.visit(parsedExpr)
    evaluator.getFoundPaths
  }

  private def normalizeExpression(expr:String):Option[String] = {
    var isReplaced = false
    var finalExpr = expr
    FhirPathEvaluator.keywords.foreach(k =>
      if(expr.contains(s".$k.")){
        isReplaced = true
        finalExpr = expr.replace(s".$k.", s".`$k`.") //Make it quosi identifier
      }
    )
    if(isReplaced)
      Some(finalExpr)
    else
      None
  }

  private def denormalizeExpression(expr:String):String = {
    var finalExpr = expr
    FhirPathEvaluator.keywords.foreach(k =>
      finalExpr =expr.replace(s".`$k`.", s".$k.")
    )
    finalExpr
  }

  /**
   * Extract path parts from a FHIR Path path expression (SearchParameter.expression) which indicates a path in FHIR content
   * @param expr
   * @return
   */
  def getPathItemsWithRestrictions(expr:String):Seq[(String, Seq[(String, String)])] = {
    val escapedExpr = expr.replaceAll("&#39;","'")
    val normalizedExpr = normalizeExpression(escapedExpr)
    val finalExpr = normalizedExpr.getOrElse(escapedExpr)

    val parsedExpr = parse(finalExpr)
    val pathExtractor = new FhirPathExtractor()
    val paths = pathExtractor.visit(parsedExpr)

    if(normalizedExpr.isDefined) {
      paths.map(p => denormalizeExpression(p._1)  -> p._2)
    } else
      paths
  }

}

object FhirPathEvaluator {
  //Keywords to normalize in expressions
  val keywords = Set("contains")

  def apply(referenceResolver: IReferenceResolver): FhirPathEvaluator = new FhirPathEvaluator(Some(referenceResolver))

  def apply(): FhirPathEvaluator = new FhirPathEvaluator(None)

  def apply(referenceResolver: Option[IReferenceResolver]): FhirPathEvaluator = new FhirPathEvaluator(referenceResolver)

  def apply(referenceResolver: IReferenceResolver, environmentVariables:Map[String, JValue]) = new FhirPathEvaluator(Some(referenceResolver), environmentVariables)

  def apply(environmentVariables:Map[String, JValue]): FhirPathEvaluator = new FhirPathEvaluator(None, environmentVariables)
}

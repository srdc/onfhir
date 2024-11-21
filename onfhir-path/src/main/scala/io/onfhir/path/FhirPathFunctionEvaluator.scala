package io.onfhir.path

import io.onfhir.api.model.FhirLiteralReference
import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.annotation.FhirPathFunction
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import org.apache.commons.text.StringEscapeUtils
import org.json4s.JsonAST._

import java.time._
import java.time.temporal.Temporal
import java.util.Base64
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
 * Evaluator for FHIR Path functions
 *
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathFunctionEvaluator(context: FhirPathEnvironment, current: Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {

  /**
   *
   * @param fprefix Function library prefix if external library (not an original FHIR Path function)
   * @param fname   Function name
   * @param params  Supplied parameters
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Calls the specified function with parameters.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`fprefix`**  \nFunction library prefix if external library (not an original FHIR Path function).\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`fname`**  \nFunction name.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`params`**  \nSupplied parameters.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"resourceType\": \"Patient\",\n  \"id\": \"12345\"\n}\n``` \n\uD83D\uDCA1 **E.g.** callFunction(None, \"resolveIdentifier\", Seq(FhirPathString(\"Patient\"), FhirPathString(\"12345\")))",
    insertText = "callFunction(<library-prefix>,<function-name>,<params>)", detail = "", label = "callFunction", kind = "Function", returnType = Seq(), inputType = Seq())
  def callFunction(fprefix: Option[String], fname: String, params: Seq[ExpressionContext]): Seq[FhirPathResult] = {
    fprefix match {
      //It is an original FHIR Path function or calling it without specificying a prefix
      case None =>
        val functionName = if (fname == "toString") "_toString" else fname
        if (getFunctionSignatures().contains(functionName -> params.length))
          callFhirPathFunction(functionName, params)
        else {
          context
            .functionLibraries
            .values
            .find(_.getLibrary(context, current).getFunctionSignatures().contains(fname -> params.length)) match {
            case None => throw new FhirPathException(s"Invalid FHIR Path function call, function $fname does not exist or take ${params.length} arguments !!!")
            case Some(fnlibFactory) =>
              fnlibFactory
                .getLibrary(context, current)
                .asInstanceOf[AbstractFhirPathFunctionLibrary]
                .callFhirPathFunction(fname, params)
          }
        }
      //Otherwise it is a external function
      case Some(prefix) =>
        context
          .functionLibraries
          .get(prefix) match {
          case None => throw new FhirPathException(s"Invalid function call, there is no function library registered with prefix $prefix !!!")
          case Some(fnlibFactory) =>
            fnlibFactory
              .getLibrary(context, current)
              .asInstanceOf[AbstractFhirPathFunctionLibrary]
              .callFhirPathFunction(fname, params)
        }
    }
  }

  /**
   * Resolve a reference
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Resolves a reference.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"resourceType\": \"Patient\",\n  \"id\": \"12345\"\n}\n``` \n\uD83D\uDCA1 **E.g.** resolve()",
    insertText = "resolve()", detail = "", label = "resolve", kind = "Method", returnType = Seq(), inputType = Seq())
  def resolve(): Seq[FhirPathResult] = {
    val fhirReferences = current.map {
      //TODO We cannot distinguish every case if it is string, so we may come up with a new reference type that may be both and resolve can handle that
      case FhirPathString(uri) if uri.startsWith("http") => FHIRUtil.parseCanonicalReference(uri)
      case FhirPathString(reference) =>
        val parsedReference = FHIRUtil.parseReferenceValue(reference)
        FhirLiteralReference(parsedReference._1, parsedReference._2, parsedReference._3, parsedReference._4)
      case FhirPathComplex(o) => FHIRUtil.parseReference(o)
      case _ => throw new FhirPathException("Invalid function call 'resolve', it should be called on a canonical value or FHIR reference!")
    }

    fhirReferences
      .flatMap(fr => context.referenceResolver.flatMap(rr => Await.result(rr.resolveReference(fr), 1 minutes)))
      .map(FhirPathComplex)
  }

  /**
   * Getting a specific extension
   *
   * @param urlExp
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a specific extension.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`urlExp`**  \nURL expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"url\": \"http://example.org/fhir/StructureDefinition/extension\",\n  \"valueString\": \"Example Value\"\n}\n``` \n\uD83D\uDCA1 **E.g.** extension(\"http://example.org/fhir/StructureDefinition/extension\")",
    insertText = "extension(<urlExp>)", detail = "", label = "extension", kind = "Method", returnType = Seq(), inputType = Seq())
  def extension(urlExp: ExpressionContext): Seq[FhirPathResult] = {
    val url = new FhirPathExpressionEvaluator(context, current).visit(urlExp)
    if (url.length != 1 || !url.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'extension', expression ${urlExp.getText} does not return a url!")

    val expr = FhirPathEvaluator.parse(s"extension.where(url = '${url.head.asInstanceOf[FhirPathString].s}')")

    var result = new FhirPathExpressionEvaluator(context, current).visit(expr)
    result
  }


  /**
   * Type functions, for these basic casting or type checking are done before calling the function on the left expression
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection that contains all items in the input collection that are of the given type or a subclass thereof.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`typ`**  \nThe type.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** ofType(\"Patient\")",
    insertText = "ofType(<expr>)", detail = "", label = "ofType", kind = "Method", returnType = Seq(), inputType = Seq())
  def ofType(typ: ExpressionContext): Seq[FhirPathResult] = current

  @FhirPathFunction(documentation = "\uD83D\uDCDC If the left operand is a collection with a single item and the second operand is an identifier, this operator returns the value of the left operand if it is of the type specified in the second operand, or a subclass thereof.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`typ`**  \nThe type.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"resourceType\": \"Patient\",\n  \"id\": \"12345\"\n}\n``` \n\uD83D\uDCA1 **E.g.** as(\"Patient\")",
    insertText = "as(<expr>)", detail = "", label = "as", kind = "Method", returnType = Seq(), inputType = Seq())
  def as(typ: ExpressionContext): Seq[FhirPathResult] = current

  @FhirPathFunction(documentation = "\uD83D\uDCDC If the left operand is a collection with a single item and the second operand is a type identifier, this operator returns true if the type of the left operand is the type specified in the second operand, or a subclass thereof.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`typ`**  \nThe type.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** is(\"Patient\")",
    insertText = "is(<expr>)", detail = "", label = "is", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def is(typ: ExpressionContext): Seq[FhirPathResult] = {
    current.length match {
      case 0 => Seq(FhirPathBoolean(false))
      case 1 => Seq(FhirPathBoolean(true))
      case _ => throw new FhirPathException("Invalid function call 'is', it should be called on single item!")
    }
  }


  /**
   * Existence functions http://hl7.org/fhirpath/#existence
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the input collection is empty ({ }) and false otherwise.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** empty()",
    insertText = "empty()", detail = "", label = "empty", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def empty(): Seq[FhirPathResult] = Seq(FhirPathBoolean(current.isEmpty))

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the input collection evaluates to false, and false if it evaluates to true.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** not()",
    insertText = "not()", detail = "", label = "not", kind = "Method", returnType = Seq("boolean"), inputType = Seq("boolean"))
  def not(): Seq[FhirPathResult] = current match {
    case Nil => Nil
    case Seq(FhirPathBoolean(b)) => Seq(FhirPathBoolean(!b))
    case _ => throw new FhirPathException("Function 'not' should run on FHIR path boolean!!!")
  }

  private def exists(expr: Option[ExpressionContext]): Seq[FhirPathResult] = {
    val result = expr match {
      case None => current.nonEmpty
      case Some(criteria) =>
        current
          .zipWithIndex
          .exists(c =>
            new FhirPathExpressionEvaluator(context.copy(_index = c._2), Seq(c._1))
              .visit(criteria) match {
              case Seq(FhirPathBoolean(true)) => true
              case _ => false
            })
    }
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns `true` if the collection has any elements satisfying the criteria, and `false` otherwise. For more information, refer to the [FHIRPath documentation](https://hl7.org/fhirpath/#existscriteria-expression-boolean).\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`criteria`**  \nThe condition or expression used to test elements within the collection. This can be any valid expression that evaluates to a boolean value (`true` or `false`).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** identifier.exists(use = 'official')",
    insertText = "exists(<expr>)", detail = "", label = "exists", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def exists(expr: ExpressionContext): Seq[FhirPathResult] = exists(Some(expr))

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns `true` if the collection has any elements, and `false` otherwise. For more information, refer to the [FHIRPath documentation](https://hl7.org/fhirpath/#existscriteria-expression-boolean).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** name.exists()",
    insertText = "exists()", detail = "", label = "exists", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def exists(): Seq[FhirPathResult] = exists(None)

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if for every element in the input collection, criteria evaluates to true.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`criteria`**  \nThe condition or expression used to test elements within the collection. This can be any valid expression that evaluates to a boolean value (`true` or `false`).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** all(identifier.use = 'official')",
    insertText = "all(<criteria>)", detail = "", label = "all", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def all(criteria: ExpressionContext): Seq[FhirPathResult] = {
    val result =
      current
        .zipWithIndex
        .forall(c =>
          new FhirPathExpressionEvaluator(context.copy(_index = c._2), Seq(c._1))
            .visit(criteria) match {
            case Seq(FhirPathBoolean(true)) => true
            case _ => false
          })
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Takes a collection of Boolean values and returns true if all the items are true.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** allTrue()",
    insertText = "allTrue()", detail = "", label = "allTrue", kind = "Method", returnType = Seq("boolean"), inputType = Seq("boolean"))
  def allTrue(): Seq[FhirPathResult] = {
    if (current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'allTrue' should run on collection of FHIR Path boolean values!!!")
    val result = current.forall(c => c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Takes a collection of Boolean values and returns true if any of the items is true.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** anyTrue()",
    insertText = "anyTrue()", detail = "", label = "anyTrue", kind = "Method", returnType = Seq("boolean"), inputType = Seq("boolean"))
  def anyTrue(): Seq[FhirPathResult] = {
    if (current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'anyTrue' should run on collection of FHIR Path boolean values!!!")
    val result = current.exists(c => c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Takes a collection of Boolean values and returns true if all the items are false.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** allFalse()",
    insertText = "allFalse()", detail = "", label = "allFalse", kind = "Method", returnType = Seq("boolean"), inputType = Seq("boolean"))
  def allFalse(): Seq[FhirPathResult] = {
    if (current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'allFalse' should run on collection of FHIR Path boolean values!!!")
    val result = current.forall(c => !c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Takes a collection of Boolean values and returns true if any of the items is false.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** anyFalse()",
    insertText = "anyFalse()", detail = "", label = "anyFalse", kind = "Method", returnType = Seq("boolean"), inputType = Seq("boolean"))
  def anyFalse(): Seq[FhirPathResult] = {
    if (current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'anyFalse' should run on collection of FHIR Path boolean values!!!")
    val result = current.exists(c => !c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if all items in the input collection are members of the collection passed as the other argument.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`other`**  \nThe other collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** subsetOf(Seq(FhirPathString(\"Patient/12345\")))",
    insertText = "subsetOf(<other>)", detail = "", label = "subsetOf", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def subsetOf(other: ExpressionContext): Seq[FhirPathResult] = {
    val otherCollection = new FhirPathExpressionEvaluator(context, current).visit(other)
    val result = current.forall(c => otherCollection.exists(o => c.isEqual(o).getOrElse(false)))
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if all items in the collection passed as the other argument are members of the input collection.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`other`**  \nThe other collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** supersetOf(Seq(FhirPathString(\"Patient/12345\")))",
    insertText = "supersetOf(<other>)", detail = "", label = "supersetOf", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def supersetOf(other: ExpressionContext): Seq[FhirPathResult] = {
    val otherCollection = new FhirPathExpressionEvaluator(context, current).visit(other)
    val result =
      if (current.isEmpty)
        false
      else
        otherCollection.forall(o => current.exists(c => c.isEqual(o).getOrElse(false)))
    Seq(FhirPathBoolean(result))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if all the items in the input collection are distinct.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** isDistinct()",
    insertText = "isDistinct()", detail = "", label = "isDistinct", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def isDistinct(): Seq[FhirPathResult] = {
    Seq(FhirPathBoolean(current.distinct.length == current.length))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection containing only the unique items in the input collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** distinct()",
    insertText = "distinct()", detail = "", label = "distinct", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def distinct(): Seq[FhirPathResult] = {
    current.distinct
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the integer count of the number of items in the input collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3, 5, etc.\n``` \n\uD83D\uDCA1 **E.g.** count()",
    insertText = "count()", detail = "", label = "count", kind = "Method", returnType = Seq("integer"), inputType = Seq())
  def count(): Seq[FhirPathResult] = {
    Seq(FhirPathNumber(current.length))
  }

  /**
   * Filtering and projection http://hl7.org/fhirpath/#filtering-and-projection
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection containing only those elements in the input collection for which the stated criteria expression evaluates to true.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`criteria`**  \nThe condition or expression used to test elements within the collection. This can be any valid expression that evaluates to a boolean value (`true` or `false`).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** where(identifier.use = 'official')",
    insertText = "where(<criteria>)", detail = "", label = "where", kind = "Method", returnType = Seq(), inputType = Seq())
  def where(criteria: ExpressionContext): Seq[FhirPathResult] = {
    current
      .zipWithIndex
      .filter(c =>
        new FhirPathExpressionEvaluator(context.copy(_index = c._2), Seq(c._1))
          .visit(criteria) match {
          case Seq(FhirPathBoolean(true)) => true
          case Seq(FhirPathBoolean(false)) => false
          case Nil => false
          case _ => throw new FhirPathException(s"Invalid criteria ${criteria.getText} function call 'where', it does not evaluate to boolean!!!")
        })
      .map(_._1)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Evaluates the projection expression for each item in the input collection.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`projection`**  \nThe projection expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n[\"Smith\", \"Kaplan\"]\n``` \n\uD83D\uDCA1 **E.g.** select(name.family)",
    insertText = "select(<projection>)", detail = "", label = "select", kind = "Method", returnType = Seq(), inputType = Seq())
  def select(projection: ExpressionContext): Seq[FhirPathResult] = {
    current
      .zipWithIndex
      .flatMap(c => {
        val r = new FhirPathExpressionEvaluator(context.copy(_index = c._2), Seq(c._1)).visit(projection)
        r
      })
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC A version of select that will repeat the projection and add it to the output collection.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`projection`**  \nThe projection expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n[\"Smith\", \"Smith\"]\n``` \n\uD83D\uDCA1 **E.g.** repeat(name.family)",
    insertText = "repeat(<projection>)", detail = "", label = "repeat", kind = "Method", returnType = Seq(), inputType = Seq())
  def repeat(projection: ExpressionContext): Seq[FhirPathResult] = {
    val firstResults = select(projection)
    if (firstResults.nonEmpty)
      firstResults ++ new FhirPathFunctionEvaluator(context, firstResults).repeat(projection)
    else
      Nil
  }

  /**
   * Subsetting http://hl7.org/fhirpath/#subsetting
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the single item in the input if there is just one item.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"resourceType\": \"Patient\",\n  \"id\": \"12345\"\n}\n``` \n\uD83D\uDCA1 **E.g.** single()",
    insertText = "single()", detail = "", label = "single", kind = "Method", returnType = Seq(), inputType = Seq())
  def single(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(s) => Seq(s)
      case _ => throw new FhirPathException(s"Function 'single' is called on a multi item collection $current!!!")
    }
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection containing only the first item in the input collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** first()",
    insertText = "first()", detail = "", label = "first", kind = "Method", returnType = Seq(), inputType = Seq())
  def first(): Seq[FhirPathResult] = current.headOption.toSeq

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection containing only the last item in the input collection. Returns an empty collection if the input collection has no items.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** last()",
    insertText = "last()", detail = "", label = "last", kind = "Method", returnType = Seq(), inputType = Seq())
  def last(): Seq[FhirPathResult] = current.lastOption.toSeq

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection containing all but the first item in the input collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** tail()",
    insertText = "tail()", detail = "", label = "tail", kind = "Method", returnType = Seq(), inputType = Seq())
  def tail(): Seq[FhirPathResult] = if (current.isEmpty) Nil else current.tail

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection containing all but the first num items in the input collection.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`numExpr`**  \nThe number of items to be skipped.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** skip(2)",
    insertText = "skip(<numExpr>)", detail = "", label = "skip", kind = "Method", returnType = Seq(), inputType = Seq())
  def skip(numExpr: ExpressionContext): Seq[FhirPathResult] = {
    val numValue = new FhirPathExpressionEvaluator(context, current).visit(numExpr)
    if (numValue.length != 1 || !numValue.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException(s"Invalid function call 'skip', num expression ${numExpr.getText} does not return a single number!")
    val inum = numValue.head.asInstanceOf[FhirPathNumber]
    if (!inum.isInteger())
      throw new FhirPathException(s"Invalid function call 'skip', num expression ${numExpr.getText} does not return a integer")

    val i = inum.v.toInt
    if (i < 0)
      current
    else if (i > current.length)
      Nil
    else
      current.drop(i)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection containing the first num items in the input collection, or less if there are less than num items.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`numExpr`**  \nThe number of items to be included in the collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** take(1)",
    insertText = "take(<numExpr>)", detail = "", label = "take", kind = "Method", returnType = Seq(), inputType = Seq())
  def take(numExpr: ExpressionContext): Seq[FhirPathResult] = {
    val numValue = new FhirPathExpressionEvaluator(context, current).visit(numExpr)
    if (numValue.length != 1 || !numValue.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException(s"Invalid function call 'take', num expression ${numExpr.getText} does not return a single number!")
    val inum = numValue.head.asInstanceOf[FhirPathNumber]
    if (!inum.isInteger())
      throw new FhirPathException(s"Invalid function call 'take', num expression ${numExpr.getText} does not return a integer")
    val i = inum.v.toInt
    if (i <= 0) Nil
    else
      current.take(i)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the set of elements that are in both collections.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`otherCollExpr`**  \nThe other collection expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** intersect(Seq(FhirPathString(\"Patient/12345\")))",
    insertText = "intersect(<otherCollExpr>)", detail = "", label = "intersect", kind = "Method", returnType = Seq(), inputType = Seq())
  def intersect(otherCollExpr: ExpressionContext): Seq[FhirPathResult] = {
    val otherSet = new FhirPathExpressionEvaluator(context, current).visit(otherCollExpr)
    current.filter(c => otherSet.exists(o => c.isEqual(o).getOrElse(false))).distinct
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the set of elements that are not in the other collection.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`otherCollExpr`**  \nThe other collection expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"resourceType\": \"Patient\",\n    \"id\": \"12345\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** exclude(Seq(FhirPathString(\"Patient/54321\")))",
    insertText = "exclude(<otherCollExpr>)", detail = "", label = "exclude", kind = "Method", returnType = Seq(), inputType = Seq())
  def exclude(otherCollExpr: ExpressionContext): Seq[FhirPathResult] = {
    val otherSet = new FhirPathExpressionEvaluator(context, current).visit(otherCollExpr)
    current.filterNot(c => otherSet.exists(o => c.isEqual(o).getOrElse(false))).distinct
  }

  /**
   * Returns the absolute value of the input. When taking the absolute value of a quantity, the unit is unchanged.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the absolute value of the input. When taking the absolute value of a quantity, the unit is unchanged. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3, 3.5, etc.\n``` \n\uD83D\uDCA1 **E.g.** 3.abs()",
    insertText = "abs()", detail = "", label = "abs", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def abs(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(n.abs))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(q.v.abs), unit))
      case _ => throw new FhirPathException("Invalid function call 'abs' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the first integer greater than or equal to the input.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the first integer greater than or equal to the input. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3, 5, etc.\n``` \n\uD83D\uDCA1 **E.g.** 2.5.ceiling()",
    insertText = "ceiling()", detail = "", label = "ceiling", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def ceiling(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.ceil(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.ceil(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'ceiling' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns e raised to the power of the input.
   * If the input collection contains an Integer, it will be implicitly converted to a Decimal and the result will be a Decimal.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns e raised to the power of the input. If the input collection contains an Integer, it will be implicitly converted to a Decimal and the result will be a Decimal. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n7.38905609893065\n``` \n\uD83D\uDCA1 **E.g.** 2.exp()",
    insertText = "exp()", detail = "", label = "exp", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def exp(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.exp(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.exp(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'exp' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the first integer less than or equal to the input.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the first integer less than or equal to the input. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3, 5, etc.\n``` \n\uD83D\uDCA1 **E.g.** 3.5.floor()",
    insertText = "floor()", detail = "", label = "floor", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def floor(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.floor(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.floor(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'floor' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the natural logarithm of the input (i.e. the logarithm base e).
   * When used with an Integer, it will be implicitly converted to a Decimal.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the natural logarithm of the input (i.e. the logarithm base e). When used with an Integer, it will be implicitly converted to a Decimal. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n2.302585092994046\n``` \n\uD83D\uDCA1 **E.g.** 10.ln()",
    insertText = "ln()", detail = "", label = "ln", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def ln(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.log(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.log(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'ln' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the logarithm base base of the input number.
   * When used with Integers, the arguments will be implicitly converted to Decimal.
   * If base is empty, the result is empty.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @param baseExp
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the logarithm base \"base\" of the input number. When used with Integers, the arguments will be implicitly converted to Decimal. If base is empty, the result is empty. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`baseExp`**  \nThe base expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n0.301029995663981\n``` \n\uD83D\uDCA1 **E.g.** 2.log(10)",
    insertText = "log(<baseExp>)", detail = "", label = "log", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def log(baseExp: ExpressionContext): Seq[FhirPathResult] = {
    val baseResult = new FhirPathExpressionEvaluator(context, current).visit(baseExp)
    if (baseResult.length != 1 || !baseResult.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException("Invalid function call 'log', given base expression should return single numeric value!")
    val base = baseResult.head.asInstanceOf[FhirPathNumber].v
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.log(n.toDouble) / Math.log(base.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.log(q.v.toDouble) / Math.log(base.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'ln' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Raises a number to the exponent power. If this function is used with Integers, the result is an Integer. If the function is used with Decimals, the result is a Decimal. If the function is used with a mixture of Integer and Decimal, the Integer is implicitly converted to a Decimal and the result is a Decimal.
   * If the power cannot be represented (such as the -1 raised to the 0.5), the result is empty.
   * If the input is empty, or exponent is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @param exponentExpr
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Raises a number to the exponent power. If this function is used with Integers, the result is an Integer. If the function is used with Decimals, the result is a Decimal. If the function is used with a mixture of Integer and Decimal, the Integer is implicitly converted to a Decimal and the result is a Decimal. If the power cannot be represented (such as the -1 raised to the 0.5), the result is empty. If the input is empty, or exponent is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`exponentExpr`**  \nThe exponent expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n8, 1.414213562373095, etc.\n``` \n\uD83D\uDCA1 **E.g.** 2.power(3), 2.power(0.5), etc.",
    insertText = "power(<exponentExpr>)", detail = "", label = "power", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def power(exponentExpr: ExpressionContext): Seq[FhirPathResult] = {
    val exponentResult = new FhirPathExpressionEvaluator(context, current).visit(exponentExpr)
    if (exponentResult.length != 1 || !exponentResult.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException("Invalid function call 'power', given exponent expression should return single numeric value!")
    val exponent = exponentResult.head.asInstanceOf[FhirPathNumber].v
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.pow(n.toDouble, exponent.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.pow(q.v.toDouble, exponent.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'power' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Rounds the decimal to the nearest whole number using a traditional round (i.e. 0.5 or higher will round to 1).
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Rounds the decimal to the nearest whole number using a traditional round (i.e. 0.5 or higher will round to 1). If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3, 5, etc.\n``` \n\uD83D\uDCA1 **E.g.** 2.9.round()",
    insertText = "round()", detail = "", label = "round", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def round(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.round(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.round(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'power' on multi item collection or non-numeric value!")
    }
  }

  /**
   * The precision argument determines the decimal place at which the rounding will occur.
   * The number of digits of precision must be >= 0 or the evaluation will end and signal an error to the calling environment.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @param precisionExpr
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Rounds the decimal in a way that the precision argument determines the decimal place at which the rounding will occur. The number of digits of precision must be >= 0 or the evaluation will end and signal an error to the calling environment. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`precisionExpr`**  \nThe precision expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3.33\n``` \n\uD83D\uDCA1 **E.g.** 3.3333.round(2)",
    insertText = "round(<precisionExpr>)", detail = "", label = "round", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def round(precisionExpr: ExpressionContext): Seq[FhirPathResult] = {
    val precisionResult = new FhirPathExpressionEvaluator(context, current).visit(precisionExpr)
    precisionResult match {
      case Seq(n: FhirPathNumber) if n.isInteger() && n.v > 0 =>
        val precision = n.v.toInt
        current match {
          case Nil => Nil
          case Seq(FhirPathNumber(n)) =>
            Seq(FhirPathNumber(n.setScale(precision, RoundingMode.HALF_UP)))
          case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(q.v.setScale(precision, RoundingMode.HALF_UP)), unit))
          case _ => throw new FhirPathException("Invalid function call 'round' on multi item collection or non-numeric value!")
        }
      case _ => throw new FhirPathException("Invalid function call 'round', given precision expression should return single positive integer value!")
    }
  }

  /**
   * Returns the square root of the input number as a Decimal.
   * If the square root cannot be represented (such as the square root of -1), the result is empty.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the square root of the input number as a Decimal. If the square root cannot be represented (such as the square root of -1), the result is empty. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n1.414213562373095\n``` \n\uD83D\uDCA1 **E.g.** 2.sqrt()",
    insertText = "sqrt()", detail = "", label = "sqrt", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def sqrt(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) =>
        val result = Math.sqrt(n.toDouble)
        if (result.isNaN)
          Nil
        else
          Seq(FhirPathNumber(result))
      case Seq(FhirPathQuantity(q, unit)) =>
        val result = Math.sqrt(q.v.toDouble)
        if (result.isNaN)
          Nil
        else
          Seq(FhirPathQuantity(FhirPathNumber(result), unit))
      case _ => throw new FhirPathException("Invalid function call 'sqrt' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the integer portion of the input.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the integer portion of the input. If the input collection is empty, the result is empty. If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3, 5, etc.\n``` \n\uD83D\uDCA1 **E.g.** 3.9.truncate()",
    insertText = "truncate()", detail = "", label = "truncate", kind = "Method", returnType = Seq("number", "quantity"), inputType = Seq("number", "quantity"))
  def truncate(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(n.toInt))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(q.v.toInt), unit))
      case _ => throw new FhirPathException("Invalid function call 'truncate' on multi item collection or non-numeric value!")
    }
  }


  /**
   * Combining http://hl7.org/fhirpath/#combining
   *
   * @param other
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Merges the input and other collections into a single collection without eliminating duplicate values.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`other`**  \nThe other collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n[\"1\", \"2\", \"3\", \"4\", \"5\"]\n``` \n\uD83D\uDCA1 **E.g.** combine(['1', '2', '3'])",
    insertText = "combine(<other>)", detail = "", label = "combine", kind = "Method", returnType = Seq(), inputType = Seq())
  def combine(other: ExpressionContext): Seq[FhirPathResult] = {
    val otherCollection = new FhirPathExpressionEvaluator(context, current).visit(other)
    current ++ otherCollection
  }

  /**
   * Conversion functions http://hl7.org/fhirpath/#conversion
   */
  private def iif(criterium: ExpressionContext, trueResult: ExpressionContext, otherwiseResult: Option[ExpressionContext]): Seq[FhirPathResult] = {
    val criteriaResult = new FhirPathExpressionEvaluator(context, current).visit(criterium)
    val conditionResult = criteriaResult match {
      case Nil => false
      case Seq(FhirPathBoolean(false)) => false
      case Seq(FhirPathBoolean(true)) => true
    }
    if (conditionResult)
      new FhirPathExpressionEvaluator(context, current).visit(trueResult)
    else
      otherwiseResult.map(ore => new FhirPathExpressionEvaluator(context, current).visit(ore)).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Evaluates a given criterion and returns one of two specified results based on whether the criterion is true or false. It functions similarly to an inline if-else statement.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`criterium`**  \nThe condition or expression to be evaluated. This can be any expression that resolves to a boolean value (`true` or `false`).\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`trueResult`**  \nThe value to be returned if the `criterium` evaluates to true. This can be any valid value or expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`otherwiseResult`**  \nThe value to be returned if the `criterium` evaluates to false. This can be any valid value or expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \nThe method returns the value of `trueResult` if `criterium` is true, otherwise it returns the value of `otherwiseResult`.\n\n\uD83D\uDCA1 **E.g.** iif(measuredLabValue >= 50, 'High', 'Normal')",
    insertText = "iif(<criterium>, <trueResult>, <otherwiseResult>)", detail = "", label = "iif", kind = "Function", returnType = Seq(), inputType = Seq())
  def iif(criterium: ExpressionContext, trueResult: ExpressionContext, otherwiseResult: ExpressionContext): Seq[FhirPathResult] = iif(criterium, trueResult, Some(otherwiseResult))

  @FhirPathFunction(documentation = "\uD83D\uDCDC Evaluates a given criterion and returns a specified result if the criterion is true. If the criterion is false, it returns an empty value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`criterium`**  \nThe condition or expression to be evaluated. This can be any expression that resolves to a boolean value (`true` or `false`).\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`trueResult`**  \nThe value to be returned if the `criterium` evaluates to true. This can be any valid value or expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \nThe method returns the value of `trueResult` if `criterium` is true. If `criterium` is false, it returns an empty value.\n\n\uD83D\uDCA1 **E.g.** iif(measuredLabValue >= 50, 'High')",
    insertText = "iif(<criterium>,<trueResult>)", detail = "", label = "iif", kind = "Function", returnType = Seq(), inputType = Seq())
  def iif(criterium: ExpressionContext, trueResult: ExpressionContext): Seq[FhirPathResult] = iif(criterium, trueResult, None)


  /**
   * Integer conversion function
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to an integer.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3, 5, etc.\n``` \n\uD83D\uDCA1 **E.g.** '3'.toInteger()",
    insertText = "toInteger()", detail = "", label = "toInteger", kind = "Method", returnType = Seq("integer"), inputType = Seq("integer", "string", "boolean"))
  def toInteger(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n: FhirPathNumber) if n.isInteger() => Seq(n)
      case Seq(n: FhirPathNumber) if !n.isInteger() => throw new FhirPathException(s"Invalid function call 'toInteger' on value $n !!!")
      case Seq(FhirPathString(s)) => Try(s.toInt).toOption match {
        case Some(i) => Seq(FhirPathNumber(i))
        case None => throw new FhirPathException(s"Invalid function call 'toInteger' on value $s of string type cannot be converted to integer!!")
      }
      case Seq(FhirPathBoolean(b)) => if (b) Seq(FhirPathNumber(1)) else Seq(FhirPathNumber(0))
      case Seq(oth) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toInteger' on multiple values!!!")
    }
  }

  /**
   * Decimal conversion function
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to a decimal.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n3.5, 5.8, etc.\n``` \n\uD83D\uDCA1 **E.g.** '3.5'.toDecimal()",
    insertText = "toDecimal()", detail = "", label = "toDecimal", kind = "Method", returnType = Seq("decimal"), inputType = Seq("dateTime", "number", "string", "boolean"))
  def toDecimal(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      //This may be due to parsing strings
      case Seq(FhirPathDateTime(y: Year)) => Seq(FhirPathNumber(y.getValue))
      case Seq(n: FhirPathNumber) => Seq(n)
      case Seq(FhirPathString(s)) => Try(s.toDouble).toOption match {
        case Some(d) => Seq(FhirPathNumber(d))
        case None => throw new FhirPathException(s"Invalid function call 'toDecimal' on value $s of string type cannot be converted to decimal!!")
      }
      case Seq(FhirPathBoolean(b)) => if (b) Seq(FhirPathNumber(1.0)) else Seq(FhirPathNumber(0.0))
      case Seq(oth) => throw new FhirPathException(s"Invalid function call 'toDecimal' on value $oth !!!")
      case _ => throw new FhirPathException(s"Invalid function call 'toDecimal' on multiple values!!!")
    }
  }

  /**
   * Check if the current item can be converted to decimal
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Checks if the current item can be converted to decimal. If so, returns true. Otherwise, returns false.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** '3.5'.convertsToDecimal()",
    insertText = "convertsToDecimal()", detail = "", label = "convertsToDecimal", kind = "Method", returnType = Seq("boolean"), inputType = Seq("dateTime", "number", "boolean", "string"))
  def convertsToDecimal(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathDateTime(_: Year)) => Seq(FhirPathBoolean(true))
      case Seq(FhirPathNumber(_)) => Seq(FhirPathBoolean(true))
      case Seq(FhirPathBoolean(_)) => Seq(FhirPathBoolean(true))
      case Seq(FhirPathString(s)) =>
        Try(s.toDouble).toOption match {
          case Some(d) => Seq(FhirPathBoolean(true))
          case None => Seq(FhirPathBoolean(false))
        }
      case Seq(oth) => Seq(FhirPathBoolean(false))
      case _ => throw new FhirPathException(s"Invalid function call 'toDecimal' on multiple values!!!")
    }
  }

  /**
   * Boolean conversion function
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to a boolean.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** 'true'.toBoolean()",
    insertText = "toBoolean()", detail = "", label = "toBoolean", kind = "Method", returnType = Seq("boolean"), inputType = Seq("number", "string", "boolean"))
  def toBoolean(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n: FhirPathNumber) if n.v == 1.0 => Seq(FhirPathBoolean(true))
      case Seq(n: FhirPathNumber) if n.v == 0 => Seq(FhirPathBoolean(false))
      case Seq(FhirPathString(s)) if
        s.equalsIgnoreCase("T") ||
          s.equalsIgnoreCase("true") ||
          s.equalsIgnoreCase("Y") ||
          s.equalsIgnoreCase("yes") ||
          s.equalsIgnoreCase("1") || s.equalsIgnoreCase("1.0") => Seq(FhirPathBoolean(true))

      case Seq(FhirPathString(s)) if
        s.equalsIgnoreCase("F") ||
          s.equalsIgnoreCase("false") ||
          s.equalsIgnoreCase("N") ||
          s.equalsIgnoreCase("no") ||
          s.equalsIgnoreCase("0") || s.equalsIgnoreCase("0.0") => Seq(FhirPathBoolean(false))

      case Seq(b: FhirPathBoolean) => Seq(b)
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toBoolean' on multiple values!!!")
    }
  }

  /**
   * Date conversion function
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to a Date format. This method follows the FHIRPath standard and only accepts a single format: YYYY-MM-DD. For more information, refer to the [FHIRPath documentation](https://hl7.org/fhirpath/#todate-date).  \n⚠\uFE0F For custom patterns and conversion purposes, please use the `utl` library methods (e.g., `utl:toFhirDate`).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \nThe method returns a Date string in the format YYYY-MM-DD.\n```json\n\"2023-05-23\"\n```\n\uD83D\uDCA1 **E.g.** '2012-01-01'.toDate()",
    insertText = "toDate()", detail = "", label = "toDate", kind = "Method", returnType = Seq("dateTime"), inputType = Seq("dateTime", "string"))
  def toDate(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathDateTime(dt: LocalDateTime)) => Seq(FhirPathDateTime(dt.toLocalDate))
      case Seq(FhirPathDateTime(dt: ZonedDateTime)) => Seq(FhirPathDateTime(dt.toLocalDate))
      case Seq(FhirPathDateTime(_)) => current
      case Seq(FhirPathString(s)) => Try(FhirPathLiteralEvaluator.parseFhirDateBest(s)).toOption match {
        case None => Nil
        case Some(t) => Seq(FhirPathDateTime(t))
      }
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toDate' on multiple values!!!")
    }
  }

  /**
   * DateTime conversion function
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to a DateTime format. This method follows the FHIRPath standard and only accepts a single format: YYYY-MM-DDThh:mm:ss.fff(+|-)hh:mm. For more information, refer to the [FHIRPath documentation](https://hl7.org/fhirpath/#todatetime-datetime).  \n⚠\uFE0F For custom patterns and conversion purposes, please use the `utl` library methods (e.g., `utl:toFhirDateTime`).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \nThe method returns a DateTime string in the format YYYY-MM-DDThh:mm:ss.fff(+|-)hh:mm.\n```json\n\"2023-05-23T13:45:30.000+02:00\"\n```\n\uD83D\uDCA1 **E.g.** '2012-01-01T10:00'.toDateTime()",
    insertText = "toDateTime()", detail = "", label = "toDateTime", kind = "Method", returnType = Seq("dateTime"), inputType = Seq("dateTime", "string"))
  def toDateTime(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathDateTime(dt: Year)) => Seq(FhirPathDateTime(dt.atMonth(1).atDay(1).atTime(LocalTime.of(0, 0))))
      case Seq(FhirPathDateTime(dt: YearMonth)) => Seq(FhirPathDateTime(dt.atDay(1).atTime(LocalTime.of(0, 0))))
      case Seq(FhirPathDateTime(dt: LocalDate)) => Seq(FhirPathDateTime(dt.atTime(LocalTime.of(0, 0))))
      case Seq(FhirPathDateTime(_)) => current
      case Seq(FhirPathString(s)) => Try(FhirPathLiteralEvaluator.parseFhirDateTimeBest(s)).toOption match {
        case None => Nil
        case Some(t) => Seq(FhirPathDateTime(t))
      }
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toDateTime' on multiple values!!!")
    }
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to a quantity.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"value\": 3,\n  \"unit\": \"1\"\n}\n``` \n\uD83D\uDCA1 **E.g.** '3'.toQuantity()",
    insertText = "toQuantity()", detail = "", label = "toQuantity", kind = "Method", returnType = Seq("quantity"), inputType = Seq("number", "quantity", "string", "boolean"))
  def toQuantity(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n: FhirPathNumber) => Seq(FhirPathQuantity(n, "1"))
      case Seq(q: FhirPathQuantity) => Seq(q)
      case Seq(c: FhirPathComplex) => c.toQuantity().map(Seq(_)).getOrElse(Nil)
      case Seq(FhirPathString(s)) => FhirPathLiteralEvaluator.parseFhirQuantity(s).toSeq
      case Seq(FhirPathBoolean(true)) => Seq(FhirPathQuantity(FhirPathNumber(1.0), "1"))
      case Seq(FhirPathBoolean(false)) => Seq(FhirPathQuantity(FhirPathNumber(0.0), "1"))
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toQuantity' on multiple values!!!")
    }
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to a quantity with the given unit.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`unitExpr`**  \nThe unit expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"value\": 3,\n  \"unit\": \"mg\"\n}\n``` \n\uD83D\uDCA1 **E.g.** '3'.toQuantity('mg')",
    insertText = "toQuantity(<unitExpr>)", detail = "", label = "toQuantity", kind = "Method", returnType = Seq("quantity"), inputType = Seq("number", "quantity", "string", "boolean"))
  def toQuantity(unitExpr: ExpressionContext): Seq[FhirPathResult] = {
    val unitValue = new FhirPathExpressionEvaluator(context, current).visit(unitExpr)
    if (!unitValue.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'toQuantity', given expression should return a string value!!!")
    val unit = unitValue.headOption.map(_.asInstanceOf[FhirPathString].s).getOrElse("1")
    current match {
      case Nil => Nil
      case Seq(n: FhirPathNumber) => Seq(FhirPathQuantity(n, unit))
      case Seq(q: FhirPathQuantity) if q.unit == unit => Seq(q)
      case Seq(c: FhirPathComplex) => c.toQuantity().map(q => Seq(FhirPathQuantity(q.q, unit))).getOrElse(Nil)
      case Seq(FhirPathString(s)) => FhirPathLiteralEvaluator.parseFhirQuantity(s).toSeq
      case Seq(FhirPathBoolean(true)) => Seq(FhirPathQuantity(FhirPathNumber(1.0), unit))
      case Seq(FhirPathBoolean(false)) => Seq(FhirPathQuantity(FhirPathNumber(0.0), unit))
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toQuantity' on multiple values!!!")
    }
  }

  /**
   * String conversion function
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the input to a string.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n\"3\"\n``` \n\uD83D\uDCA1 **E.g.** 3.toString()",
    insertText = "toString()", detail = "", label = "toString", kind = "Method", returnType = Seq("string"), inputType = Seq("number", "string", "dateTime", "time", "quantity", "boolean"))
  def _toString(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n: FhirPathNumber) if n.isInteger() => Seq(FhirPathString(n.v.toLong.toString()))
      case Seq(n: FhirPathNumber) => Seq(FhirPathString(n.v.toString()))
      case Seq(s: FhirPathString) => Seq(s)
      case Seq(dt: FhirPathDateTime) => Seq(FhirPathString(FhirPathLiteralEvaluator.format(dt)))
      case Seq(t: FhirPathTime) => Seq(FhirPathString(FhirPathLiteralEvaluator.format(t)))
      case Seq(q: FhirPathQuantity) => Seq(FhirPathString(q.q.v.toString + " " + q.unit))
      case Seq(FhirPathBoolean(b)) => if (b) Seq(FhirPathString("'true'")) else Seq(FhirPathString("'false'"))
      case Seq(oth) => Nil
      case _ => throw new FhirPathException(s"Invalid function call '_toString' on multiple values !!!")
    }
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts a quantity to a FHIR Path Object type (JSON).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"value\": 3,\n  \"unit\": \"mg\"\n}\n``` \n\uD83D\uDCA1 **E.g.** {'value': 3, 'unit': 'mg'}.toComplex()",
    insertText = "toComplex()", detail = "", label = "toComplex", kind = "Method", returnType = Seq(), inputType = Seq("quantity"))
  def toComplex(): Seq[FhirPathResult] = {
    current match {
      case Seq(q: FhirPathQuantity) => Seq(FhirPathComplex(q.toJson.asInstanceOf[JObject]))
      case _ => Nil
    }
  }


  /**
   * String manipulation functions http://hl7.org/fhirpath/#string-manipulation
   *
   * @return
   */
  private def checkSingleString() =
    if (current.length > 1 || current.headOption.exists(!_.isInstanceOf[FhirPathString]))
      throw new FhirPathException("Invalid function call on multi item collection or non-string value!")

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the 0-based index of the first position the given substring is found in the input string, or -1 if it is not found.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`substringExpr`**  \nThe substring expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n1, 3, etc.\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.indexOf('bc')",
    insertText = "indexOf(<substringExpr>)", detail = "", label = "indexOf", kind = "Method", returnType = Seq("integer"), inputType = Seq("string"))
  def indexOf(substringExpr: ExpressionContext): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(substringExpr) match {
        case Seq(FhirPathString(ss)) => if (ss == "") Seq(FhirPathNumber((0))) else Seq(FhirPathNumber(c.asInstanceOf[FhirPathString].s.indexOf(ss)))
        case _ => throw new FhirPathException(s"Invalid function call 'indexOf', the substring expression ${substringExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  private def substring(startExpr: ExpressionContext, lengthExpr: Option[ExpressionContext]): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val start = new FhirPathExpressionEvaluator(context, current).visit(startExpr) match {
        case Seq(n: FhirPathNumber) if n.isInteger() => n.v.toInt
        case _ => throw new FhirPathException(s"Invalid function call 'substring', the start expression ${startExpr.getText} does not return integer!")
      }
      val length = lengthExpr.map(lexpr => new FhirPathExpressionEvaluator(context, current).visit(lexpr) match {
        case Seq(n: FhirPathNumber) if n.isInteger() => n.v.toInt
        case _ => throw new FhirPathException(s"Invalid function call 'substring', the length expression ${startExpr.getText} does not return integer!")
      })
      val str = c.asInstanceOf[FhirPathString].s
      if (start > str.length || start < 0)
        Nil
      else {
        if (length.isEmpty)
          Try(Seq(FhirPathString(str.substring(start)))).toOption.getOrElse(Nil)
        else {
          val endIndex = if (start + length.get >= str.length) str.length else start + length.get
          Try(Seq(FhirPathString(str.substring(start, endIndex)))).toOption.getOrElse(Nil)
        }
      }
    }).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the part of the string starting at position start (zero-based).\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`startExpr`**  \nThe start position expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n\"bcdefg\"\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.substring(1)",
    insertText = "substring(<startExpr>)", detail = "", label = "substring", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def substring(startExpr: ExpressionContext): Seq[FhirPathResult] = substring(startExpr, None)

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the part of the string starting at position start (zero-based) and being of the given length.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`startExpr`**  \nThe start position expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`lengthExpr`**  \nThe length expression for the substring.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n\"bcd\"\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.substring(1, 3)",
    insertText = "substring(<startExpr>,<lengthExpr>)", detail = "", label = "substring", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def substring(startExpr: ExpressionContext, lengthExpr: ExpressionContext): Seq[FhirPathResult] = substring(startExpr, Some(lengthExpr))

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the input string starts with the given prefix.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`prefixExpr`**  \nThe prefix expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.startsWith('abc')",
    insertText = "startsWith(<prefixExpr>)", detail = "", label = "startsWith", kind = "Method", returnType = Seq("boolean"), inputType = Seq("string"))
  def startsWith(prefixExpr: ExpressionContext): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val prefix = new FhirPathExpressionEvaluator(context, current).visit(prefixExpr)
      prefix match {
        case Seq(FhirPathString(ss)) =>
          Seq(FhirPathBoolean(ss == "" || c.asInstanceOf[FhirPathString].s.startsWith(ss)))
        case oth =>
          throw new FhirPathException(s"Invalid function call 'startsWith', the prefixExpr expression ${prefixExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the input string ends with the given suffix.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`suffixExpr`**  \nThe suffix expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.endsWith('efg')",
    insertText = "endsWith(<suffixExpr>)", detail = "", label = "endsWith", kind = "Method", returnType = Seq("boolean"), inputType = Seq("string"))
  def endsWith(suffixExpr: ExpressionContext): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(suffixExpr) match {
        case Seq(FhirPathString(ss)) => Seq(FhirPathBoolean(ss == "" || c.asInstanceOf[FhirPathString].s.endsWith(ss)))
        case _ => throw new FhirPathException(s"Invalid function call 'endsWith', the suffixExpr expression ${suffixExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the given substring is a substring of the input string.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`substringExpr`**  \nThe substring expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.contains('cde')",
    insertText = "contains(<substringExpr>)", detail = "", label = "contains", kind = "Method", returnType = Seq("boolean"), inputType = Seq("string"))
  def _contains(substringExpr: ExpressionContext): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(substringExpr) match {
        case Seq(FhirPathString(ss)) => Seq(FhirPathBoolean(ss == "" || c.asInstanceOf[FhirPathString].s.contains(ss)))
        case _ => throw new FhirPathException(s"Invalid function call 'contains', the substring expression ${substringExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the input string with all instances of the given pattern replaced with the given substitution.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`patternExpr`**  \nThe pattern expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`substitutionExpr`**  \nThe substitution expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n\"ab123fg\"\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.replace('cde', '123')",
    insertText = "replace(<patternExpr>,<substitutionExpr>)", detail = "", label = "replace", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def replace(patternExpr: ExpressionContext, substitutionExpr: ExpressionContext): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val pattern = new FhirPathExpressionEvaluator(context, current).visit(patternExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the pattern expression ${patternExpr.getText} does not return string!")
      }

      val substitution = new FhirPathExpressionEvaluator(context, current).visit(substitutionExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the substitiution expression ${substitutionExpr.getText} does not return string!")
      }

      Seq(FhirPathString(c.asInstanceOf[FhirPathString].s.replace(pattern, substitution)))
    }).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the input value matches the given regular expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`regexExpr`**  \nThe regular expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.matches('a.c.e.g')",
    insertText = "matches(<regexExpr>)", detail = "", label = "matches", kind = "Method", returnType = Seq("boolean"), inputType = Seq("string", "dateTime", "number"))
  def matches(regexExpr: ExpressionContext): Seq[FhirPathResult] = {
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(regexExpr) match {
        case Seq(FhirPathString(ss)) =>
          val unexcapedScript = StringEscapeUtils.unescapeEcmaScript(ss)
          // convert the evaluated FhirPath result to string if possible
          val value: String = c match {
            case FhirPathString(s) => s
            case FhirPathDateTime(d) => d.toString
            case FhirPathNumber(n) => n.toString
            case _ => throw new FhirPathException(s"Invalid function call 'matches', the value ${c.toJson} is not a string or can not be cast into a string!")
          }
          val isMatch = value.matches(unexcapedScript)
          Seq(FhirPathBoolean(isMatch))
        case _ =>
          throw new FhirPathException(s"Invalid function call 'matches', the regular expression ${regexExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Matches the input using the given regular expression and replaces each match with the given substitution string.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`regexExpr`**  \nThe regular expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`substitutionExpr`**  \nThe substitution expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n\"ab123fg\"\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.replaceMatches('c.e', '123')",
    insertText = "replaceMatches(<regexExpr>,<substitutionExpr>)", detail = "", label = "replaceMatches", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def replaceMatches(regexExpr: ExpressionContext, substitutionExpr: ExpressionContext): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val regex = new FhirPathExpressionEvaluator(context, current).visit(regexExpr) match {
        case Seq(FhirPathString(ss)) => StringEscapeUtils.unescapeEcmaScript(ss)
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the regex expression ${regexExpr.getText} does not return string!")
      }

      val substitution = new FhirPathExpressionEvaluator(context, current).visit(substitutionExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the substitiution expression ${substitutionExpr.getText} does not return string!")
      }

      Seq(FhirPathString(c.asInstanceOf[FhirPathString].s.replaceAll(regex, substitution)))
    }).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the length of the input string.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n5, 7, etc.\n``` \n\uD83D\uDCA1 **E.g.** 'abcdefg'.length()",
    insertText = "length()", detail = "", label = "length", kind = "Method", returnType = Seq("integer"), inputType = Seq("string"))
  def length(): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => Seq(FhirPathNumber(c.asInstanceOf[FhirPathString].s.length))).getOrElse(Nil)
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Encodes the input string to a Base64-encoded binary.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\nZmluYWw=\n``` \n\uD83D\uDCA1 **E.g.** 'final'.encode()",
    insertText = "encode()", detail = "", label = "encode", kind = "Method", returnType = Seq("base64Binary"), inputType = Seq("string"))
  def encode(): Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => Seq(FhirPathString(Base64.getEncoder.encodeToString(c.asInstanceOf[FhirPathString].s.getBytes("UTF-8"))))).getOrElse(Nil)
  }

  /**
   * Tree navigation function http://hl7.org/fhirpath/#tree-navigation
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection with all immediate child nodes of all items in the input collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"linkId\": \"1\",\n    \"text\": \"Do you have allergies?\",\n    \"type\": \"boolean\"\n  },\n  {\n    \"linkId\": \"2\",\n    \"text\": \"General health questions\",\n    \"type\": \"group\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** Questionnaire.children().select(item)",
    insertText = "children()", detail = "", label = "children", kind = "Method", returnType = Seq(), inputType = Seq())
  def children(): Seq[FhirPathResult] = {
    current
      .filter(_.isInstanceOf[FhirPathComplex])
      .map(_.asInstanceOf[FhirPathComplex])
      .flatMap(pc => pc.json.obj.map(_._2).flatMap(i => FhirPathValueTransformer.transform(i, context.isContentFhir)))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a collection with all descendant nodes of all items in the input collection.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n[\n  {\n    \"linkId\": \"1\",\n    \"text\": \"Do you have allergies?\",\n    \"type\": \"boolean\"\n  },\n  {\n    \"linkId\": \"2\",\n    \"text\": \"General health questions\",\n    \"type\": \"group\"\n  },\n  {\n    \"linkId\": \"2.1\",\n    \"text\": \"Do you have any chronic diseases?\",\n    \"type\": \"boolean\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** Questionnaire.descendants().select(item)",
    insertText = "descendants()", detail = "", label = "descendants", kind = "Method", returnType = Seq(), inputType = Seq())
  def descendants(): Seq[FhirPathResult] = {
    val results = children()
    if (results.nonEmpty)
      results ++ new FhirPathFunctionEvaluator(context, results).descendants()
    else
      results
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the input collection contains values.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** someCollection.hasValue()",
    insertText = "hasValue()", detail = "", label = "hasValue", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def hasValue(): Seq[FhirPathResult] = {
    Seq(FhirPathBoolean(current.nonEmpty))
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns true if the engine executing the FHIRPath statement can compare the singleton Quantity with the singleton other Quantity and determine their relationship to each other.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** someQuantity.comparable(otherQuantity)",
    insertText = "comparable(<thatQuantity>)", detail = "", label = "comparable", kind = "Method", returnType = Seq("boolean"), inputType = Seq("Quantity"))
  def comparable(thatQuantity: ExpressionContext): Seq[FhirPathResult] = {
    if (current.length > 1) throw new FhirPathException(s"Invalid function call 'comparable' on multi item collection!")
    val that = new FhirPathExpressionEvaluator(context, context._this).visit(thatQuantity) match {
      case Seq(q: FhirPathQuantity) => q
      case oth =>
        throw new FhirPathException(s"Invalid function call 'comparable', the thatQuantity expression ${thatQuantity.getText} does not return Quantity! It returns '${oth.getClass}'")
    }
    current match {
      case Nil => Nil
      case Seq(FhirPathQuantity(_, unit)) => Seq(FhirPathBoolean(unit == that.unit))
      case oth =>
        throw new FhirPathException(s"Invalid function call 'comparable'. It cannot be executed on type '${oth.getClass}'")
    }
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the low boundary of the input that this function is called upon. The input can be a decimal number, time or dateTime.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n2.38550, 2015-01-01T00:00:00, etc.\n``` \n\uD83D\uDCA1 **E.g.** 2.386.lowBoundary(), 2015.utl:toFhirDateTime('yyyy').lowBoundary(), etc.",
    insertText = "lowBoundary()", detail = "", label = "lowBoundary", kind = "Method", returnType = Seq("number", "time", "date", "dateTime"), inputType = Seq("number", "time", "date", "dateTime"))
  def lowBoundary(): Seq[FhirPathResult] = lowBoundary(None)

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the low boundary of the input that this function is called upon by optionally accepting a precision for boundary calculation. The input can be a decimal number, time or dateTime.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`precisionExpr`**  \nThe precision expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n2.3855000, 2018-01, etc.\n``` \n\uD83D\uDCA1 **E.g.** 2.386.lowBoundary(7), '2018'.utl:toFhirDateTime('yyyy').lowBoundary(6), etc.",
    insertText = "lowBoundary(<precision>)", detail = "", label = "lowBoundary", kind = "Method", returnType = Seq("number", "time", "date", "dateTime"), inputType = Seq("number", "time", "date", "dateTime"))
  def lowBoundary(precisionExpr: ExpressionContext): Seq[FhirPathResult] = lowBoundary(Some(precisionExpr))

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the high boundary of the input that this function is called upon. The input can be a decimal number, time or dateTime.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n2.38650, 2015-12-31T23:59:59, etc.\n``` \n\uD83D\uDCA1 **E.g.** 2.386.highBoundary(), 2015.utl:toFhirDateTime('yyyy').highBoundary(), etc.",
    insertText = "highBoundary()", detail = "", label = "highBoundary", kind = "Method", returnType = Seq("number", "time", "date", "dateTime"), inputType = Seq("number", "time", "date", "dateTime"))
  def highBoundary(): Seq[FhirPathResult] = highBoundary(None)

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the high boundary of the input that this function is called upon by optionally accepting a precision for boundary calculation. The input can be a decimal number, time or dateTime.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`precisionExpr`**  \nThe precision expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```\n2.3865000, 2018-12, etc.\n``` \n\uD83D\uDCA1 **E.g.** 2.386.highBoundary(7), '2018'.utl:toFhirDateTime('yyyy').highBoundary(6), etc.",
    insertText = "highBoundary(<precision>)", detail = "", label = "highBoundary", kind = "Method", returnType = Seq("number", "time", "date", "dateTime"), inputType = Seq("number", "time", "date", "dateTime"))
  def highBoundary(precisionExpr: ExpressionContext): Seq[FhirPathResult] = highBoundary(Some(precisionExpr))

  /**
   * Calculate the low boundary on the applied input (current) using the optional precision expression.
   *
   * @param precisionExpr The precision to be used during boundary calculation.
   * @return
   */
  private def lowBoundary(precisionExpr: Option[ExpressionContext]): Seq[FhirPathResult] = {
    checkSingleElement("lowBoundary")
    val precision: Option[Int] = precisionExpr.map(pExpr => new FhirPathExpressionEvaluator(context, current).visit(pExpr) match {
      case Seq(FhirPathNumber(nn)) if nn.isWhole => nn.intValue
      case _ =>
        throw new FhirPathException(s"Invalid function call 'lowBoundary', the precisionExpr expression ${pExpr.getText} does not return integer!")
    })
    current.head match {
      case num: FhirPathNumber =>
        Seq(FhirPathNumber(adjustDecimalBoundary(num.v, precision, _ - _)))
      case quantity: FhirPathQuantity =>
        Seq(FhirPathQuantity(FhirPathNumber(adjustDecimalBoundary(quantity.q.v, precision, _ - _)), quantity.unit))
      case dateTime: FhirPathDateTime =>
        Seq(FhirPathDateTime(adjustDateTimeBoundary(dateTime.dt, precision, 1, 1, 0, 0, 0, 0)))
      case time: FhirPathTime =>
        Seq(FhirPathTime(adjustTimeBoundary(time.lt, time.zone, precision, 0, 0, 0), time.zone))
      case oth =>
        throw new FhirPathException(s"Invalid function call 'lowBoundary'. It cannot be executed on type '${oth.getClass}'")
    }
  }

  /**
   * Calculate the high boundary on the applied input (current) using the optional precision expression.
   *
   * @param precisionExpr The precision to be used during boundary calculation.
   * @return
   */
  private def highBoundary(precisionExpr: Option[ExpressionContext]): Seq[FhirPathResult] = {
    checkSingleElement("highBoundary")
    val precision: Option[Int] = precisionExpr.map(pExpr => new FhirPathExpressionEvaluator(context, current).visit(pExpr) match {
      case Seq(FhirPathNumber(nn)) if nn.isWhole => nn.intValue
      case oth =>
        throw new FhirPathException(s"Invalid function call 'highBoundary', the precisionExpr expression ${pExpr.getText} does not return integer!")
    })
    current.head match {
      case num: FhirPathNumber =>
        Seq(FhirPathNumber(adjustDecimalBoundary(num.v, precision, _ + _)))
      case quantity: FhirPathQuantity =>
        Seq(FhirPathQuantity(FhirPathNumber(adjustDecimalBoundary(quantity.q.v, precision, _ + _)), quantity.unit))
      case dateTime: FhirPathDateTime =>
        Seq(FhirPathDateTime(adjustDateTimeBoundary(dateTime.dt, precision, 12, 31, 23, 59, 59, 999)))
      case time: FhirPathTime =>
        Seq(FhirPathTime(adjustTimeBoundary(time.lt, time.zone, precision, 59, 59, 999), time.zone))
      case oth =>
        throw new FhirPathException(s"Invalid function call 'lowBoundary'. It cannot be executed on type '${oth.getClass}'")
    }
  }

  /**
   * Check the current element is single or not.
   *
   * @param fName The function name from which checkSingleElement is called (for logging purposes)
   */
  private def checkSingleElement(fName: String): Unit = {
    if (current.isEmpty)
      throw new FhirPathException(s"Invalid function call '$fName' on empty element/list!")
    if (current.length > 1)
      throw new FhirPathException(s"Invalid function call '$fName' on multi item collection!")
  }

  /**
   * Calculate the (low or high) decimal boundary on the given input.
   *
   * @param input     The input number
   * @param precision The precision of the output. E.g., precision 4 outputs an adjusted decimal having 4 digits after the point.
   * @param op        Either _ - _ (to calculate low boundary) or _ + _ (to calculate high boundary)
   * @return
   */
  private def adjustDecimalBoundary(input: BigDecimal, precision: Option[Int], op: (BigDecimal, BigDecimal) => BigDecimal): BigDecimal = {
    val decimalParts = input.toString().split('.')
    val fractionalDigits = if (decimalParts.length > 1) decimalParts(1).length else 0
    // FIXME: The documentation says that # https://hl7.org/fhir/fhirpath.html
    //  "The function is not very useful on integer, since it is not a continuously distributed value domain,
    //  and the lowBoundary of an integer is always the same value, but it is defined on integer for language consistency."
    //  Since our FHIRPath parser does not differentiate between integers (all numbers are put into FhirPathNumber which holds a BigDecimal inside),
    //  we cannot handle this integer processing now. We cannot return the same value for integers.
    val adjustment = BigDecimal(5) * BigDecimal(Math.pow(10, -(fractionalDigits + 1)))
    val adjustedInput = op(input, adjustment)
    precision match {
      case Some(p) =>
        val precisionAsString = "0." + "0" * p
        val additional = BigDecimal(precisionAsString)
        adjustedInput + additional
      case None => adjustedInput
    }
  }

  /**
   * Calculate the (low or high) date or datetime boundary on the given input.
   *
   * @param input        The input temporal. It can be a Year, YearMonth, LocalDate, LocalDateTime or ZonedDateTime
   * @param precision    The precision of the boundary calculation.
   *                     4 -> precision at the year level (yyyy),
   *                     6 -> precision at the month level (yyyy-MM),
   *                     8 -> precision at the day level (yyyy-MM-dd),
   *                     14 -> precision at the second level (yyyy-MM-ddTHH-mm-ss),
   *                     >=17 (or no precision) -> precision at the millisecond level yyyy-MM-ddTHH-mm-ss.SSS
   * @param months       Month to be used at the boundary. 1 -> lowBoundary, 12 -> highBoundary
   * @param days         Day to be used at the boundary. 1 -> lowBoundary, 31 -> highBoundary
   * @param hours        Hour to be used at the boundary. 0 -> lowBoundary, 23 -> highBoundary
   * @param minutes      Minute to be used at the boundary. 0 -> lowBoundary, 59 -> highBoundary
   * @param seconds      Second to be used at the boundary. 0 -> lowBoundary, 59 -> highBoundary
   * @param milliseconds Millisecond to be used at the boundary. 0 -> lowBoundary, 999 -> highBoundary
   * @return
   */
  private def adjustDateTimeBoundary(input: Temporal, precision: Option[Int], months: Int, days: Int, hours: Int, minutes: Int, seconds: Int, milliseconds: Int): Temporal = {
    input match {
      case dt: Year =>
        val dayInMonth = if (days == 1) days else YearMonth.of(dt.getValue, months).lengthOfMonth()
        precision match {
          case Some(4) => dt
          case Some(6) => YearMonth.of(dt.getValue, months)
          case Some(8) => LocalDate.of(dt.getValue, months, dayInMonth)
          case Some(14) => LocalDateTime.of(dt.getValue, months, dayInMonth, hours, minutes, seconds)
          case p if p.isEmpty || p.get >= 17 => LocalDateTime.of(dt.getValue, months, dayInMonth, hours, minutes, seconds)
          case _ => throw new IllegalArgumentException("Unsupported DateTime precision for a Temporal (Year) type.")
        }
      case dt: YearMonth =>
        val dayInMonth = if (days == 1) days else dt.lengthOfMonth()
        precision match {
          case Some(4) => Year.of(dt.getYear)
          case Some(6) => dt
          case Some(8) => LocalDate.of(dt.getYear, dt.getMonthValue, dayInMonth)
          case Some(14) => LocalDateTime.of(dt.getYear, dt.getMonthValue, dayInMonth, hours, minutes, seconds)
          case p if p.isEmpty || p.get >= 17 => LocalDateTime.of(dt.getYear, dt.getMonthValue, dayInMonth, hours, minutes, seconds, milliseconds * 1000000)
          case _ => throw new IllegalArgumentException("Unsupported DateTime precision for a Temporal (YearMonth) type.")
        }
      case dt: LocalDate =>
        precision match {
          case Some(4) => Year.of(dt.getYear)
          case Some(6) => YearMonth.of(dt.getYear, dt.getMonthValue)
          case Some(8) => dt
          case Some(14) => LocalDateTime.of(dt.getYear, dt.getMonthValue, dt.getDayOfMonth, hours, minutes, seconds)
          case p if p.isEmpty || p.get >= 17 => LocalDateTime.of(dt.getYear, dt.getMonthValue, dt.getDayOfMonth, hours, minutes, seconds, milliseconds * 1000000)
          case _ => throw new IllegalArgumentException("Unsupported DateTime precision for a Temporal (LocalDate) type.")
        }
      case dt: LocalDateTime =>
        precision match {
          case Some(4) => Year.of(dt.getYear)
          case Some(6) => YearMonth.of(dt.getYear, dt.getMonthValue)
          case Some(8) => LocalDate.of(dt.getYear, dt.getMonthValue, dt.getDayOfMonth)
          case Some(14) => dt
          case p if p.isEmpty || p.get >= 17 => LocalDateTime.of(dt.getYear, dt.getMonthValue, dt.getDayOfMonth, dt.getHour, dt.getMinute, dt.getSecond, milliseconds * 1000000)
          case _ => throw new IllegalArgumentException("Unsupported DateTime precision for a Temporal (LocalDateTime) type.")
        }
      case dt: ZonedDateTime =>
        precision match {
          case Some(4) => Year.of(dt.getYear)
          case Some(6) => YearMonth.of(dt.getYear, dt.getMonthValue)
          case Some(8) => LocalDate.of(dt.getYear, dt.getMonthValue, dt.getDayOfMonth)
          case Some(14) => LocalDateTime.of(dt.getYear, dt.getMonthValue, dt.getDayOfMonth, dt.getHour, dt.getMinute, dt.getSecond)
          case p if p.isEmpty || p.get >= 17 => ZonedDateTime.of(dt.getYear, dt.getMonthValue, dt.getDayOfMonth, dt.getHour, dt.getMinute, dt.getSecond, milliseconds * 1000000, dt.getZone)
          case _ => throw new IllegalArgumentException("Unsupported DateTime precision for a Temporal (LocalDateTime) type.")
        }
      case _ => throw new IllegalArgumentException("Unsupported Temporal type")
    }
  }

  /**
   * Calculate the (low or high) time boundary on the given input.
   *
   * @param input     The input LocalTime
   * @param zone      Optional ZoneId for the time.
   * @param precision The precision of the boundary calculation.
   *                  2 -> precision at the hour level (HH),
   *                  4 -> precision at the minute level (HH-mm),
   *                  6 -> precision at the second level (HH-mm-ss),
   *                  9 or no precision -> precision at the millisecond level -> HH-mm-ss.SSS
   * @param minutes
   * @param seconds
   * @param milliseconds
   * @return
   */
  private def adjustTimeBoundary(input: LocalTime, zone: Option[ZoneId], precision: Option[Int], minutes: Int, seconds: Int, milliseconds: Int): LocalTime = {
    precision match {
      case Some(2) => // Hour precision
        LocalTime.of(input.getHour, minutes, seconds, milliseconds * 1000000)
      case Some(4) => // Minute precision
        LocalTime.of(input.getHour, input.getMinute, seconds, milliseconds * 1000000)
      case Some(6) => // Second precision
        LocalTime.of(input.getHour, input.getMinute, input.getSecond, milliseconds * 1000000)
      case Some(9) | None => // Fraction (millisecond) precision or default
        LocalTime.of(input.getHour, input.getMinute, input.getSecond, input.getNano / 1000000 * 1000000 + milliseconds * 1000000)
      case _ => throw new IllegalArgumentException(s"Unsupported Time precision for boundary operation: $precision")
    }
  }

  /**
   * FHIR Path aggregate function
   *
   * @param aggExpr       Aggregation expression
   * @param initValueExpr Given initial value for aggregation
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Performs general-purpose aggregation by evaluating the aggregator expression for each element of the input collection.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`aggExpr`**  \nThe aggregation expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`initValueExpr`**  \nGiven initial value for aggregation.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"total\": 15\n}\n``` \n\uD83D\uDCA1 **E.g.** value.aggregate($this + $total, 0)",
    insertText = "aggregate(<aggExpr>,<initValueExpr>)", detail = "", label = "aggregate", kind = "Method", returnType = Seq(), inputType = Seq())
  def aggregate(aggExpr: ExpressionContext, initValueExpr: ExpressionContext): Seq[FhirPathResult] = {
    val initValue = new FhirPathExpressionEvaluator(context, current).visit(initValueExpr)
    if (initValue.length > 1)
      throw new FhirPathException(s"Invalid function call 'aggregate', the initValue expression should return a single value!")

    handleAggregate(context.copy(_total = initValue.headOption), aggExpr)
  }

  /**
   * Helper function to evaluate aggregate
   *
   * @param initialCntx Initial context for total
   * @param aggExpr     Aggregation expression
   * @return
   */
  private def handleAggregate(initialCntx: FhirPathEnvironment, aggExpr: ExpressionContext): Seq[FhirPathResult] = {
    val finalContext =
      current
        .zipWithIndex
        .foldLeft(initialCntx) {
          case (cntx, cur) =>
            val totalResult = new FhirPathExpressionEvaluator(cntx.copy(_index = cur._2), Seq(cur._1)).visit(aggExpr)
            cntx.copy(_total = totalResult.headOption)
        }

    finalContext._total.toSeq
  }

  /**
   * FHIR Path aggregate function
   *
   * @param aggExpr Aggregation expression
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Performs general-purpose aggregation by evaluating the aggregator expression for each element of the input collection.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`aggExpr`**  \nThe aggregation expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"total\": 15\n}\n``` \n\uD83D\uDCA1 **E.g.** value.aggregate($this + $total)",
    insertText = "aggregate(<aggExpr>)", detail = "", label = "aggregate", kind = "Method", returnType = Seq(), inputType = Seq())
  def aggregate(aggExpr: ExpressionContext): Seq[FhirPathResult] = {
    handleAggregate(context, aggExpr)
  }

  /**
   * Utility functions http://hl7.org/fhirpath/#utility-functions
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the current date.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"date\": \"2024-07-19\"\n}\n``` \n\uD83D\uDCA1 **E.g.** today()",
    insertText = "today()", detail = "", label = "today", kind = "Function", returnType = Seq("dateTime"), inputType = Seq())
  def today(): Seq[FhirPathResult] = Seq(FhirPathDateTime(LocalDate.now()))

  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the current date and time, including timezone offset.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"dateTime\": \"2024-07-19T14:23:45.678+02:00\"\n}\n``` \n\uD83D\uDCA1 **E.g.** now()",
    insertText = "now()", detail = "", label = "now", kind = "Function", returnType = Seq("dateTime"), inputType = Seq())
  def now(): Seq[FhirPathResult] = Seq(FhirPathDateTime(ZonedDateTime.now()))

  @FhirPathFunction(documentation = "\uD83D\uDCDC Adds a String representation of the input collection to the diagnostic log, using the name argument as the name in the log.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`nameExpr`**  \nThe name expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"trace\": \"Logging collection with name: unmatched\",\n  \"collection\": [1, 2, 3]\n}\n``` \n\uD83D\uDCA1 **E.g.** contained.where(criteria).trace('unmatched').empty()",
    insertText = "trace(<nameExpr>)", detail = "", label = "trace", kind = "Method", returnType = Seq(), inputType = Seq())
  def trace(nameExpr: ExpressionContext): Seq[FhirPathResult] = {
    //TODO log the current value with the given name
    current
  }

  @FhirPathFunction(documentation = "\uD83D\uDCDC Adds a String representation of the input collection to the diagnostic log, using the name argument as the name in the log and the other argument as additional information.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`nameExpr`**  \nThe name expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`othExpr`**  \nThe other expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>\n```json\n{\n  \"trace\": \"Logging collection with name: unmatched, additional info: id\",\n  \"collection\": [1, 2, 3]\n}\n``` \n\uD83D\uDCA1 **E.g.** contained.where(criteria).trace('unmatched', id).empty()",
    insertText = "trace(<nameExpr>,<othExpr>)", detail = "", label = "trace", kind = "Method", returnType = Seq(), inputType = Seq())
  def trace(nameExpr: ExpressionContext, othExpr: ExpressionContext): Seq[FhirPathResult] = {
    current
  }
}

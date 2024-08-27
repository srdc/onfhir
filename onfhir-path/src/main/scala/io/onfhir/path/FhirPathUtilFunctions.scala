package io.onfhir.path

import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.annotation.FhirPathFunction
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import org.json4s.{JArray, JField, JObject, JString, JValue}

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoUnit, Temporal}
import java.util.UUID
import scala.util.Try


/**
 * Default FHIR Path function library for onFhir for time utilities and processing
 *
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathUtilFunctions(context: FhirPathEnvironment, current: Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {

  /**
   * Generate an UUID
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Generates an UUID.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n\"3f12a3e4-74c2-48c1-9836-53c1c1016554\"\n``` \n\uD83D\uDCA1 **E.g.** utl:uuid()",
    insertText = "utl:uuid()", detail = "utl", label = "utl:uuid", kind = "Function", returnType = Seq("string"), inputType = Seq())
  def uuid(): Seq[FhirPathResult] = {
    Seq(FhirPathString(UUID.randomUUID().toString))
  }

  /**
   * Check whether the given string or character starts with or is a letter
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Checks whether the given string or character starts with or is a letter.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** 'abc'.utl:isLetter()",
    insertText = "utl:isLetter()", detail = "utl", label = "utl:isLetter", kind = "Method", returnType = Seq("boolean"), inputType = Seq("string"))
  def isLetter(): Seq[FhirPathResult] = {
    current match {
      case Seq(FhirPathString(l)) => Seq(FhirPathBoolean(l.head.isLetter))
      case _ => Nil
    }
  }

  /**
   * Trim the strings in the current set
   *
   * - If the string is all white spaces then it is eliminated (return Nil)
   *
   * @example ' ali  '.trim() --> 'ali'
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Trims the strings in the current set so that they do not contain whitespaces. If the string is all whitespaces then returns Nil.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n\"ali\"\n``` \n\uD83D\uDCA1 **E.g.** ' ali '.utl:trim()",
    insertText = "utl:trim()", detail = "utl", label = "utl:trim", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def trim(): Seq[FhirPathResult] = {
    current.flatMap {
      case FhirPathString(s) =>
        s.trim() match {
          case "" => None
          case oth => Some(FhirPathString(oth))
        }

      case oth => Some(oth)
    }
  }

  /**
   * Create FHIR Reference object(s) with given resource type and id(s)
   * If ridExp returns Nil, the function also returns Nil
   *
   * @param resourceTypeExp Expression to provide FHIR Resource type 'Patient'
   * @param ridExp          Resource identifier(s)
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Creates FHIR Reference object(s) with given resource type and id(s). If ridExp returns Nil, the function also returns Nil.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`resourceTypeExp`**  \nExpression to provide FHIR Resource type 'Patient'.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`ridExp`**  \nResource identifier(s).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n[\n  {\n    \"reference\": \"Observation/123\"\n  },\n  {\n    \"reference\": \"Observation/456\"\n  }\n]\n```\n\uD83D\uDCA1 **E.g.** utl:createFhirReference('Observation', id)",
    insertText = "utl:createFhirReference(<resourceTypeExp>, <ridExp>)", detail = "utl", label = "utl:createFhirReference", kind = "Function", returnType = Seq(), inputType = Seq())
  def createFhirReference(resourceTypeExp: ExpressionContext, ridExp: ExpressionContext): Seq[FhirPathResult] = {
    val rids = new FhirPathExpressionEvaluator(context, current).visit(ridExp)
    if (!rids.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirReference', ridExp (2nd parameter) should return FHIR Path string values!")

    val rtype = new FhirPathExpressionEvaluator(context, current).visit(resourceTypeExp)
    if (rtype.length != 1 || !rtype.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'createFhirReference', resourceTypeExp (1st parameter) should return single FHIR Path string value!")

    rids
      .map(_.asInstanceOf[FhirPathString])
      .map(rid =>
        FhirPathComplex(JObject(List(JField("reference", JString(s"${rtype.head.asInstanceOf[FhirPathString].s}/${rid.s}")))))
      )
  }

  /**
   * Create a FHIR Coding content with given system code and optional display
   * If system or code parameters return nil, the function returns Nil
   * All parameters should return 0 or 1 string value
   *
   * @param systemExp   Expression to give the system value
   * @param codeExpr    Expression to give the code value
   * @param displayExpr Expression to give the display value (optional)
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Creates a FHIR Coding content with given system code and optional display. If system or code parameters return Nil, the function returns Nil. All parameters should return 0 or 1 string value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`systemExp`**  \nExpression to give the system value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`codeExpr`**  \nExpression to give the code value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`displayExpr`**  \nExpression to give the display value (optional).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n[\n  {\n    \"system\": \"http://snomed.info/sct\",\n    \"code\": \"246103008\",\n    \"display\": \"Certainty\"\n  }\n]\n```\n\uD83D\uDCA1 **E.g.** utl:createFhirCoding('http://snomed.info/sct', '246103008', 'Certainty')",
    insertText = "utl:createFhirCoding(<system>, <code>, <display>)", detail = "utl", label = "utl:createFhirCoding", kind = "Function", returnType = Seq(), inputType = Seq())
  def createFhirCoding(systemExp: ExpressionContext, codeExpr: ExpressionContext, displayExpr: ExpressionContext): Seq[FhirPathResult] = {
    val system = new FhirPathExpressionEvaluator(context, current).visit(systemExp)
    if (system.length > 1 || !system.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirCoding', expression for system should return 0 or 1 string value!")

    val code = new FhirPathExpressionEvaluator(context, current).visit(codeExpr)
    if (code.length > 1 || !code.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirCoding', expression for code should return 0 or 1 string value!")

    val display = new FhirPathExpressionEvaluator(context, current).visit(displayExpr)
    if (display.length > 1 || !code.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirCoding', expression for display should return 0 or 1 string value!")

    if (system.isEmpty || code.isEmpty)
      Nil
    else {
      var codingElems =
        List(
          JField("system", system.head.toJson),
          JField("code", code.head.toJson)
        )
      display.headOption.foreach(d => codingElems = codingElems :+ JField("display", d.toJson))
      Seq(FhirPathComplex(JObject(codingElems)))
    }
  }

  /**
   * Create a FHIR CodeableConcept content with given system code and optional display
   * If system or code parameters return nil, the function returns Nil
   * All parameters should return 0 or 1 string value
   *
   * @param systemExp   Expression to give the system value
   * @param codeExpr    Expression to give the code value
   * @param displayExpr Expression to give the display value (optional)
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Create a FHIR CodeableConcept content with given system code and optional display. If system or code parameters return Nil, the function returns Nil. All parameters should return 0 or 1 string value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`systemExp`**  \nExpression to give the system value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`codeExpr`**  \nExpression to give the code value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`displayExpr`**  \nExpression to give the display value (optional).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n[\n  {\n    \"system\": \"http://snomed.info/sct\",\n    \"code\": \"246103008\",\n    \"display\": \"Certainty\"\n  }\n]\n```\n\uD83D\uDCA1 **E.g.** utl:createFhirCodeableConcept('http://snomed.info/sct', '246103008', 'Certainty')",
    insertText = "utl:createFhirCodeableConcept(<system>, <code>, <display>)", detail = "utl", label = "utl:createFhirCodeableConcept", kind = "Function", returnType = Seq(), inputType = Seq())
  def createFhirCodeableConcept(systemExp: ExpressionContext, codeExpr: ExpressionContext, displayExpr: ExpressionContext): Seq[FhirPathResult] = {
    val system = new FhirPathExpressionEvaluator(context, current).visit(systemExp)
    if (system.length > 1 || !system.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirCodeableConcept', expression for system should return 0 or 1 string value!")

    val code = new FhirPathExpressionEvaluator(context, current).visit(codeExpr)
    if (code.length > 1 || !code.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirCodeableConcept', expression for code should return 0 or 1 string value!")

    val display = new FhirPathExpressionEvaluator(context, current).visit(displayExpr)
    if (display.length > 1 || !code.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirCodeableConcept', expression for display should return 0 or 1 string value!")

    if (system.isEmpty || code.isEmpty)
      Nil
    else {
      var codingElems =
        List(
          JField("system", system.head.toJson),
          JField("code", code.head.toJson)
        )

      display.headOption.foreach(d => codingElems = codingElems :+ JField("display", d.toJson))

      val codeableConcept = JObject(
        JField("coding", JArray(List(
          JObject(
            codingElems
          )
        )))
      )
      Seq(FhirPathComplex(codeableConcept))
    }
  }

  /**
   * Create FHIR Quantity json object with given value and unit (system or code not set)
   * If value expression returns Nil, the function also returns Nil
   *
   * @param valueExpr Expression for quantity value
   * @param unitExpr  Expression for quantity unit
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Creates FHIR Quantity json object with given value and unit (system or code not set). If value expression returns Nil, the function also returns Nil.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`valueExpr`**  \nExpression for quantity value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`unitExpr`**  \nExpression for quantity unit.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n[\n  {\n    \"value\": 1,\n    \"unit\": \"mg\"\n  }\n]\n```\n\uD83D\uDCA1 **E.g.** utl:createFhirQuantity(1, 'mg')",
    insertText = "utl:createFhirQuantity(<value>, <unit>)", detail = "utl", label = "utl:createFhirQuantity", kind = "Function", returnType = Seq(), inputType = Seq())
  def createFhirQuantity(valueExpr: ExpressionContext, unitExpr: ExpressionContext): Seq[FhirPathResult] = {
    val value = handleFhirQuantityValue(valueExpr)
    val unit = new FhirPathExpressionEvaluator(context, current).visit(unitExpr)
    if (unit.length > 1 || !unit.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter unit expression should return 0 or 1 string value!")

    if (value.isEmpty)
      Nil
    else
      Seq(FhirPathComplex(constructFhirQuantity(value.get._1, unit.map(_.toJson).headOption, None, None, value.get._2)))
  }

  /**
   * Create FHIR Quantity json object with given value, system and unit
   * If any parameter expression returns Nil, the function will also return Nil
   *
   * @param valueExpr  Expression to return the quantity value
   * @param systemExpr Expression to return the system for the unit
   * @param codeExpr   Expression to return the the unit
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Creates FHIR Quantity json object with given value, system and unit. If any parameter expression returns Nil, the function will also return Nil.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`valueExpr`**  \nExpression to return the quantity value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`systemExpr`**  \nExpression to return the system for the unit.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`codeExpr`**  \nExpression to return the unit.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n[\n  {\n    \"value\": 15.2,\n    \"unit\": \"mg\",\n    \"system\": \"%ucum\"\n  }\n]\n```\n\uD83D\uDCA1 **E.g.** utl:createFhirQuantity(15.2, %ucum, 'mg')",
    insertText = "utl:createFhirQuantity(<valueExpr>, <systemExpr>, <codeExpr>)", detail = "utl", label = "utl:createFhirQuantity", kind = "Function", returnType = Seq(), inputType = Seq())
  def createFhirQuantity(valueExpr: ExpressionContext, systemExpr: ExpressionContext, codeExpr: ExpressionContext): Seq[FhirPathResult] = {
    val value = handleFhirQuantityValue(valueExpr)

    val system = new FhirPathExpressionEvaluator(context, current).visit(systemExpr)
    if (system.length > 1 || !system.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter system expression should return 0 or 1 string value!")

    val unit = new FhirPathExpressionEvaluator(context, current).visit(codeExpr)
    if (unit.length > 1 || !unit.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter unit expression should return 0 or 1 string value!")

    if (value.isEmpty)
      Nil
    else
      Seq(FhirPathComplex(constructFhirQuantity(value.get._1, unit.map(_.toJson).headOption, system.headOption.map(_.toJson), unit.headOption.map(_.toJson), value.get._2)))
  }

  /**
   * Create FHIR Quantity json object with given value unit and optional system and code
   * If value or unit parameter expression returns Nil, the function will also return Nil
   *
   * @param valueExpr  Expression to return the quantity value
   * @param unitExpr   Expression to return the unit
   * @param systemExpr Expression to return the system for the unit
   * @param codeExpr   Expression to return the unit code
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Creates FHIR Quantity json object with given value, unit and optional system and code. If value or unit parameter expression returns Nil, the function will also return Nil.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`valueExpr`**  \nExpression to return the quantity value.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`unitExpr`**  \nExpression to return the unit.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`systemExpr`**  \nExpression to return the system for the unit.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`codeExpr`**  \nExpression to return the unit code.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n[\n  {\n    \"value\": 15.2,\n    \"unit\": \"mg\",\n    \"system\": \"%ucum\",\n    \"code\": \"mg\"\n  }\n]\n```\n\uD83D\uDCA1 **E.g.** utl:createFhirQuantity(15.2, 'mg', %ucum, 'mg')",
    insertText = "utl:createFhirQuantity(<valueExpr>, <unitExpr>, <systemExpr>, <codeExpr>)", detail = "utl", label = "utl:createFhirQuantity", kind = "Function", returnType = Seq(), inputType = Seq())
  def createFhirQuantity(valueExpr: ExpressionContext,
                         unitExpr: ExpressionContext,
                         systemExpr: ExpressionContext,
                         codeExpr: ExpressionContext
                        ): Seq[FhirPathResult] = {
    val value = handleFhirQuantityValue(valueExpr)

    val unit = new FhirPathExpressionEvaluator(context, current).visit(unitExpr)
    if (unit.length > 1 || !unit.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter unit expression should return 0 or 1 string value!")

    val code = new FhirPathExpressionEvaluator(context, current).visit(codeExpr)
    if (code.length > 1 || !code.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter code expression should return 0 or 1 string value!")

    val system = new FhirPathExpressionEvaluator(context, current).visit(systemExpr)
    if (system.length > 1 || !system.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter system expression should return 0 or 1 string value!")

    if (value.isEmpty)
      Nil
    else
      Seq(FhirPathComplex(constructFhirQuantity(value.get._1, unit.map(_.toJson).headOption, system.headOption.map(_.toJson), code.headOption.map(_.toJson), value.get._2)))
  }

  /**
   * Construct a FHIR Quantity object from given values
   *
   * @param value      Json number
   * @param unit       Json string for unit
   * @param system     Optional system JString
   * @param code       Optional code JString
   * @param comparator Optional comparator
   * @return
   */
  private def constructFhirQuantity(value: JValue, unit: Option[JValue], system: Option[JValue], code: Option[JValue], comparator: Option[String]): JObject = {
    var fields = List("value" -> value) ++ unit.map(u => "unit" -> u).toSeq
    if (code.isDefined && system.isDefined)
      fields = fields ++ Seq("system" -> system.get, "code" -> code.get)
    comparator.foreach(c => fields = fields :+ ("comparator" -> JString(c)))
    JObject(fields)
  }

  /**
   * Check if the given value is a FHIR Quantity expression e.g. 5.2, >7.1, etc
   * Returns true if given value is numeric
   * Returns true if given value evaluates to string but can be converted to numeric value
   * Returns true if given value is string starting with comparators and then remaining string can be converted to numeric
   * Returns false otherwise
   * If current value return multiple values, throws exception
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Checks if the given value is a FHIR Quantity expression e.g. 5.2, >7.1, etc. Returns true if given value is numeric, or given value evaluates to string but can be converted to numeric, or given value is string starting with comparators and the remaining string can be converted to numeric; otherwise returns false. If current value returns multiple values, throws exception.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\ntrue or false\n``` \n\uD83D\uDCA1 **E.g.** 5.utl:isFhirQuantityExpression()",
    insertText = "utl:isFhirQuantityExpression()", detail = "utl", label = "utl:isFhirQuantityExpression", kind = "Method", returnType = Seq("boolean"), inputType = Seq("dateTime", "number", "string"))
  def isFhirQuantityExpression(): Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathDateTime(_: Year)) => Seq(FhirPathBoolean(true))
      case Seq(FhirPathNumber(_)) => Seq(FhirPathBoolean(true))
      case Seq(FhirPathString(s)) if s.startsWith(">") || s.startsWith("<") =>
        Try(s.drop(1).toDouble).toOption match {
          case Some(_) => Seq(FhirPathBoolean(true))
          case None => Seq(FhirPathBoolean(false))
        }
      case Seq(FhirPathString(s)) if s.startsWith(">=") || s.startsWith("<=") =>
        Try(s.drop(2).toDouble).toOption match {
          case Some(_) => Seq(FhirPathBoolean(true))
          case None => Seq(FhirPathBoolean(false))
        }
      case Seq(FhirPathString(s)) =>
        Try(s.toDouble).toOption match {
          case Some(_) => Seq(FhirPathBoolean(true))
          case None => Seq(FhirPathBoolean(false))
        }
      case Seq(oth) => Seq(FhirPathBoolean(false))
      case _ => throw new FhirPathException(s"Invalid function call 'toDecimal' on multiple values!!!")
    }
  }

  /**
   * Parse a FHIR quantity expression e.g. >50.5, 42
   *
   * If the given value is empty or not an quantity expression, return Nil
   *
   * @param valueExpr FHIR Quantity expression with optional comparator
   * @return Seq of results where first element is the numeric value and second if exists is the comparator
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Parses a FHIR quantity expression e.g. >50.5, 42, etc. Returns a sequence of results where the first element is the numeric value and the second if exists is the comparator. If the given value is empty or not a quantity expression, returns Nil.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`valueExpr`**  \nFHIR Quantity expression with optional comparator.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n{\n  \"numericValue\": 50.5,\n  \"comparator\": \">\",\n}\n```\n\uD83D\uDCA1 **E.g.** utl:parseFhirQuantityExpression('>50.5')",
    insertText = "utl:parseFhirQuantityExpression(<valueExpr>)", detail = "utl", label = "utl:parseFhirQuantityExpression", kind = "Function", returnType = Seq(), inputType = Seq())
  def parseFhirQuantityExpression(valueExpr: ExpressionContext): Seq[FhirPathResult] = {
    handleFhirQuantityValue(valueExpr) match {
      case None => Nil
      case Some(q -> None) => FhirPathValueTransformer.transform(q)
      case Some(q -> Some(c)) => FhirPathValueTransformer.transform(q) :+ FhirPathString(c)
    }
  }

  /**
   * Handle parsing the FHIR quantity value
   *
   * @param valueExpr
   * @return
   */
  private def handleFhirQuantityValue(valueExpr: ExpressionContext): Option[(JValue, Option[String])] = {
    val value = new FhirPathExpressionEvaluator(context, current).visit(valueExpr)
    value match {
      case Nil => None
      case Seq(n: FhirPathNumber) => Some(n.toJson -> None)
      case Seq(FhirPathString(s)) if s.startsWith(">") || s.startsWith("<") =>
        Try(s.drop(1).toDouble)
          .toOption match {
          case None => throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter value expression should return 0 or 1 numeric value or nuneric value with comparator prefix (e.g. >, <) !")
          case Some(d) => Some(FhirPathNumber(d).toJson -> Some(s.take(1)))
        }
      case Seq(FhirPathString(s)) if s.startsWith(">=") || s.startsWith("<=") =>
        Try(s.drop(2).toDouble)
          .toOption match {
          case None => throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter value expression should return 0 or 1 numeric value or nuneric value with comparator prefix (e.g. >, <) !")
          case Some(d) => Some(FhirPathNumber(d).toJson -> Some(s.take(2)))
        }
      case Seq(FhirPathString(s)) =>
        Try(s.toDouble)
          .toOption match {
          case None => throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter value expression should return 0 or 1 numeric value or nuneric value with comparator prefix (e.g. >, <) !")
          case Some(d) => Some(FhirPathNumber(d).toJson -> None)
        }
      case _ =>
        throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter value expression should return 0 or 1 numeric value or nuneric value with comparator prefix (e.g. >, <) !")
    }
  }


  /**
   * Retrieve the duration between given FHIR dateTimes as FHIR Duration with a suitable duration unit (either minute, day, or month)
   *
   * @param fromDate Given date expression
   * @param toDate   Other date expression
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Retrieves the duration between given FHIR dateTimes as FHIR Duration with a suitable duration unit (either minute, day, or month).\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`fromDate`**  \nGiven date expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`toDate`**  \nOther date expression.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n{\n  \"durationValue\": 30,\n  \"unit\": \"d\"\n}\n```\n\uD83D\uDCA1 **E.g.** utl:parseFhirQuantityExpression('2020-04-20T10:30:00Z', '2020-05-20T10:30:00Z')",
    insertText = "utl:getDurationAsQuantityObject(<startTime>, <endTime>)", detail = "utl", label = "utl:getDurationAsQuantityObject", kind = "Function", returnType = Seq(), inputType = Seq())
  def getDurationAsQuantityObject(fromDate: ExpressionContext, toDate: ExpressionContext): Seq[FhirPathResult] = {
    val fdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(fromDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Seq(FhirPathString(s)) =>
        FhirPathValueTransformer.transform(JString(s), isContentFhir = true)
          .headOption flatMap {
          case FhirPathDateTime(dt) => Some(dt)
          case _ => None
        }
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getDurationAsQuantityObject', second expression ${fromDate.getText} does not evaluate to a single FHIR date time!")
    }

    val tdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(toDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Seq(FhirPathString(s)) =>
        FhirPathValueTransformer.transform(JString(s), isContentFhir = true)
          .headOption flatMap {
          case FhirPathDateTime(dt) => Some(dt)
          case _ => None
        }
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getDurationAsQuantityObject', second expression ${toDate.getText} does not evaluate to a single FHIR date time!")
    }
    if (fdate.isEmpty || tdate.isEmpty)
      Nil
    else {
      val result =
        (fdate.get, tdate.get) match {
          case (y1: Year, y2: Year) => y1.until(y2, ChronoUnit.YEARS) * 1.0 -> "a"
          case (y1: Year, m2: YearMonth) => y1.atMonth(1).until(m2, ChronoUnit.MONTHS) * 1.0 -> "mo"
          case (y1: Year, d2: LocalDate) => y1.atMonth(1).atDay(1).until(d2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (y1: Year, dt2: LocalDateTime) => y1.atMonth(1).atDay(1).until(dt2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (y1: Year, zdt2: ZonedDateTime) => y1.atMonth(1).atDay(1).atTime(0, 0).atZone(ZoneId.systemDefault()).until(zdt2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (m1: YearMonth, y2: Year) => m1.until(y2.atMonth(1), ChronoUnit.MONTHS) * 1.0 -> "mo"
          case (m1: YearMonth, m2: YearMonth) => m1.until(m2, ChronoUnit.MONTHS) * 1.0 -> "mo"
          case (m1: YearMonth, d2: LocalDate) => m1.atDay(1).until(d2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (m1: YearMonth, dt2: LocalDateTime) => m1.atDay(1).until(dt2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (m1: YearMonth, zdt2: ZonedDateTime) => m1.atDay(1).atTime(0, 0).atZone(ZoneId.systemDefault()).until(zdt2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (d1: LocalDate, y2: Year) => d1.until(y2.atMonth(1).atDay(1), ChronoUnit.DAYS) * 1.0 -> "d"
          case (d1: LocalDate, m2: YearMonth) => d1.until(m2.atDay(1), ChronoUnit.DAYS) * 1.0 -> "d"
          case (d1: LocalDate, d2: LocalDate) => d1.until(d2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (d1: LocalDate, dt2: LocalDateTime) => d1.atTime(0, 0).until(dt2, ChronoUnit.DAYS) * 1.0 -> "d"
          case (d1: LocalDate, zdt2: ZonedDateTime) => handleDuration(d1.atTime(0, 0).atZone(ZoneId.systemDefault()).until(zdt2, ChronoUnit.MINUTES))
          case (dt1: LocalDateTime, y2: Year) => dt1.until(y2.atMonth(1).atDay(1).atTime(0, 0), ChronoUnit.DAYS) * 1.0 -> "d"
          case (dt1: LocalDateTime, m2: YearMonth) => dt1.until(m2.atDay(1).atTime(0, 0), ChronoUnit.DAYS) * 1.0 -> "d"
          case (dt1: LocalDateTime, d2: LocalDate) => dt1.until(d2.atTime(0, 0).atZone(ZoneId.systemDefault()), ChronoUnit.DAYS) * 1.0 -> "d"
          case (dt1: LocalDateTime, dt2: LocalDateTime) => handleDuration(dt1.until(dt2, ChronoUnit.MINUTES))
          case (dt1: LocalDateTime, zdt2: ZonedDateTime) => handleDuration(dt1.atZone(ZoneId.systemDefault()).until(zdt2, ChronoUnit.MINUTES))
          case (zdt1: ZonedDateTime, y2: Year) => zdt1.until(y2.atMonth(1).atDay(1).atTime(0, 0).atZone(ZoneId.systemDefault()), ChronoUnit.DAYS) * 1.0 -> "d"
          case (zdt1: ZonedDateTime, m2: YearMonth) => zdt1.until(m2.atDay(1).atTime(0, 0).atZone(ZoneId.systemDefault()), ChronoUnit.DAYS) * 1.0 -> "d"
          case (zdt1: ZonedDateTime, d2: LocalDate) => zdt1.until(d2.atTime(0, 0).atZone(ZoneId.systemDefault()), ChronoUnit.DAYS) * 1.0 -> "d"
          case (zdt1: ZonedDateTime, dt2: LocalDateTime) => handleDuration(zdt1.until(dt2.atZone(ZoneId.systemDefault()), ChronoUnit.MINUTES))
          case (zdt1: ZonedDateTime, zdt2: ZonedDateTime) => handleDuration(zdt1.until(zdt2, ChronoUnit.MINUTES))
          case (oth1, oth2) => throw new FhirPathException(s"Invalid datetime comparison between $oth1 and $oth2 ...")
        }

      Seq(FhirPathComplex(FhirPathQuantity(FhirPathNumber(result._1), result._2).toJson.asInstanceOf[JObject]))
    }
  }

  /**
   *
   * @param durationInMin
   * @return
   */
  private def handleDuration(durationInMin: Long): (Double, String) = {
    if (durationInMin < 60 * 24)
      durationInMin * 1.0 -> "min"
    else if (durationInMin < 60 * 24 * 3)
      (durationInMin / 60.0) -> "h"
    else if (durationInMin < 60 * 24 * 60)
      (durationInMin / (60.0 * 24.0)) -> "d"
    else
      (durationInMin / (60.0 * 24.0 * 30)) -> "mo"
  }

  /**
   * Split the current string value by given split character or string
   * e.g. Observation.valueSampleData.data.split(' ') --> Split by empty space
   *
   * @param splitCharExpr Expression to return split character(s)
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Splits the current string value by given split character or string.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`splitCharExpr`**  \nExpression to return split string(s).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n{\n  \"first\",\n  \"second\",\n  \"third\"\n}\n```\n\uD83D\uDCA1 **E.g.** Observation.valueSampleData.data.utl:split(' ') --> Split by empty space",
    insertText = "utl:split(<splitCharExpr>)", detail = "utl", label = "utl:split", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def split(splitCharExpr: ExpressionContext): Seq[FhirPathResult] = {

    val splitChar =
      new FhirPathExpressionEvaluator(context, current).visit(splitCharExpr) match {
        case Seq(FhirPathString(s)) if s.length == 1 => s.head
        case _ =>
          throw new FhirPathException(s"Invalid function call 'split', given expression should return a character to use for the split!")
      }

    current match {
      case Nil => Nil
      case Seq(FhirPathString(s)) =>
        s
          .trim()
          .split(splitChar)
          .map(FhirPathString)
          .toIndexedSeq
      case _ =>
        throw new FhirPathException(s"Invalid function call 'split' on non string value or sequence of values!")
    }
  }

  /**
   * Take the prefix of a string until one of the given stop character
   * - If stopCharExpr returns empty list, return the whole string
   * - If given stopCharExpr does not return single character string, throws an exception
   * - If current is not a string, throws an exception
   *
   * @param stopCharExpr List of stop characters
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Takes the prefix of a string until one of the given stop characters is encountered. If stopCharExpr returns empty list, returns the whole string. If given stopCharExpr does not return single character strings or current is not a string, throws an exception.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`stopCharExpr`**  \nList of stop characters.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n\"VERY LOW\"\n``` \n\uD83D\uDCA1 **E.g.** 'VERY LOW.'.utl:takeUntil('.' | '*')",
    insertText = "utl:takeUntil(<stopCharExpr>)", detail = "utl", label = "utl:takeUntil", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def takeUntil(stopCharExpr: ExpressionContext): Seq[FhirPathResult] = {
    val stopChars: Set[Char] =
      (new FhirPathExpressionEvaluator(context, current)
        .visit(stopCharExpr) map {
        case FhirPathString(stopChar) if stopChar.length == 1 => stopChar.head
        case _ => throw new FhirPathException(s"Invalid function call 'takeUntil', 'stopCharExpr' returns non-character value!")
      }).toSet

    current map {
      case FhirPathString(s) => FhirPathString(s.takeWhile(p => !stopChars.contains(p)))
      case _ => throw new FhirPathException(s"Invalid function call 'takeUntil', called on a non-string value!")
    }
  }

  /**
   * Create a sequence of indices between from-to integers
   * e.g. indices(1, 10) -> Seq(1,2,....10)
   *
   * @param fromExpr Starting index
   * @param toExpr   End index (inclusive)
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Creates a list of integers between and including given two integers.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`fromExpr`**  \nStart index (inclusive).\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`toExpr`**  \nEnd index (inclusive).\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n``` \n\uD83D\uDCA1 **E.g.** utl:indices(1, 10)",
    insertText = "utl:indices(<startnumber>, <endnumber>)", detail = "utl", label = "utl:indices", kind = "Function", returnType = Seq("integer"), inputType = Seq())
  def indices(fromExpr: ExpressionContext, toExpr: ExpressionContext): Seq[FhirPathResult] = {
    val from = new FhirPathExpressionEvaluator(context, current).visit(fromExpr)
    if (from.length != 1 || !from.forall(_.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Invalid function call 'indices', given expressions should return a integer value!")

    val to = new FhirPathExpressionEvaluator(context, current).visit(toExpr)
    if (to.length != 1 || !to.forall(_.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Invalid function call 'indices', given expressions should return a integer value!")

    (from.head.asInstanceOf[FhirPathNumber].v.toInt to to.head.asInstanceOf[FhirPathNumber].v.toInt).map(i => FhirPathNumber(i))
  }

  /**
   * Return the indices (starting from 1) in the current sequence of results where given expression returns true
   * If cond expression returns non boolean value, it returns false
   *
   * @param condExpr Boolean expression to check for each item
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the indices (starting from 1) in the current sequence of results where the given expression returns true. If condition expression returns non boolean value, it returns false.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`condExpr`**  \nBoolean expression to check for each item.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n[1, 3, 5]\n``` \n\uD83D\uDCA1 **E.g.** Observation.code.coding.utl:indicesWhere($this.system='http://snomed.info/sct')",
    insertText = "utl:indicesWhere(<condExpr>)", detail = "utl", label = "utl:indicesWhere", kind = "Method", returnType = Seq("integer"), inputType = Seq())
  def indicesWhere(condExpr: ExpressionContext): Seq[FhirPathResult] = {
    current
      .zipWithIndex
      .map(r => {
        val result = new FhirPathExpressionEvaluator(context, Seq(r._1)).visit(condExpr)
        (r._2 + 1) -> result.find(_.isInstanceOf[FhirPathBoolean]).exists(_.asInstanceOf[FhirPathBoolean].b)
      })
      .filter(_._2)
      .map(i => FhirPathNumber(i._1))
  }

  /**
   * Combine current string results with the given separator and return a string
   * If current is Nil, return Nil
   *
   * @param separatorExp Expression to return a separator string
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Combines the current string results with the given separator and returns a string. If current is Nil, returns Nil.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`separatorExp`**  \nExpression to return a separator string.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n\"first | second | third\"\n``` \n\uD83D\uDCA1 **E.g.** code.utl:mkString(' | ')",
    insertText = "utl:mkString(<separatorExp>)", detail = "utl", label = "utl:mkString", kind = "Method", returnType = Seq("string"), inputType = Seq("string"))
  def mkString(separatorExp: ExpressionContext): Seq[FhirPathResult] = {
    if (!current.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'mkString' on non string value(s)!")
    val separator = new FhirPathExpressionEvaluator(context, current).visit(separatorExp)
    if (separator.length != 1 || !separator.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'mkString', given expression for seperator should return string value!")

    if (current == Nil)
      Nil
    else
      Seq(FhirPathString(
        current
          .map(_.asInstanceOf[FhirPathString].s)
          .mkString(separator.head.asInstanceOf[FhirPathString].s)
      ))
  }

  /**
   * Evaluate the given FHIR Path expression in string format on current content and context
   * e.g. evaluateExpression('Observation.code[' & i &']') --> Return the ith code
   *
   * @param fhirPathExpression FHIR Path expression in string format
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Evaluates the given FHIR Path expression in string format on current content and context.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`fhirPathExpression`**  \nFHIR Path expression in string format.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n{\n  \"type\": \"Integer\",\n  \"value\": 2\n}\n```\n\uD83D\uDCA1 **E.g.** utl:evaluateExpression('Observation.code[' & i &']') --> Return the ith code",
    insertText = "utl:evaluateExpression(<expression>)", detail = "utl", label = "utl:evaluateExpression", kind = "Function", returnType = Seq(), inputType = Seq())
  def evaluateExpression(fhirPathExpression: ExpressionContext): Seq[FhirPathResult] = {
    val fhirPath = new FhirPathExpressionEvaluator(context, current).visit(fhirPathExpression)
    if (fhirPath.length != 1 || !fhirPath.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'evaluateExpression', given expression should return a string value!")
    val expr = fhirPath.head.asInstanceOf[FhirPathString].s
    try {
      val parsedExpr = FhirPathEvaluator.parse(expr)
      val result = new FhirPathExpressionEvaluator(context, current).visit(parsedExpr)
      result
    } catch {
      case e: Throwable =>
        throw new FhirPathException(s"Invalid function call 'evaluateExpression', given FHIR Path expression is not valid!")
    }
  }

  /**
   * Throw a FHIR Path exception with the given msg
   *
   * @param msgExpr Message for the exception
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Throws a FHIR Path exception with the given message.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`msgExpr`**  \nMessage for the exception.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n{\n  \"exception\": {\n    \"message\": \"An error occurred\"\n  }\n}\n```\n\uD83D\uDCA1 **E.g.** utl:throwException('An error occurred')",
    insertText = "utl:throwException(<msgExpr>)", detail = "utl", label = "utl:throwException", kind = "Function", returnType = Seq(), inputType = Seq())
  def throwException(msgExpr: ExpressionContext): Seq[FhirPathResult] = {
    val excMsg = new FhirPathExpressionEvaluator(context, current).visit(msgExpr)
    if (excMsg.length != 1 || !excMsg.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'throwException', given expression should return a string value!")

    throw new FhirPathException(excMsg.head.asInstanceOf[FhirPathString].s)
  }

  /**
   * Get a period between the FHIR date time given in fromDate and the FHIR date time given in toDate
   *
   * @param fromDate Given start date expression
   * @param toDate   Given end date expression
   * @param period   Period requested to calculate; either 'years','months','weeks','days','hours' or 'minutes'
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns a period between the FHIR date time given in fromDate and the FHIR date time given in toDate.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`fromDate`**  \nGiven start date expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`toDate`**  \nGiven end date expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`period`**  \nPeriod requested to calculate; either 'years','months','weeks','days','hours' or 'minutes'.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n{\n  \"period\": {\n    \"value\": 3,\n    \"unit\": \"years\",\n    \"system\": \"http://unitsofmeasure.org\",\n    \"code\": \"a\"\n  }\n}\n```\n\uD83D\uDCA1 **E.g.** effectivePeriod.utl:getPeriod(start, @2020-09-07T10:00:00Z, 'years')",
    insertText = "utl:getPeriod(<fromDate>, <toDate>, <period>)", detail = "utl", label = "utl:getPeriod", kind = "Method", returnType = Seq("number"), inputType = Seq("dateTime"))
  def getPeriod(fromDate: ExpressionContext, toDate: ExpressionContext, period: ExpressionContext): Seq[FhirPathResult] = {
    var fdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(fromDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', first expression ${fromDate.getText} does not evaluate to a single FHIR date time!")
    }

    var tdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(toDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', second expression ${toDate.getText} does not evaluate to a single FHIR date time!")
    }
    //If any one is empty return nil
    if (fdate.isEmpty || tdate.isEmpty)
      Nil
    else {
      val chronoPeriod =
        new FhirPathExpressionEvaluator(context, current).visit(period) match {
          case Seq(FhirPathString(p)) =>
            p match {
              case "year" | "years" | "a" => ChronoUnit.YEARS
              case "month" | "months" | "mo" => ChronoUnit.MONTHS
              case "week" | "weeks" | "wk" => ChronoUnit.WEEKS
              case "day" | "days" | "d" => ChronoUnit.DAYS
              case "hour" | "hours" | "h" => ChronoUnit.HOURS
              case "minute" | "minutes" | "min" => ChronoUnit.MINUTES
              case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', the period expression ${period.getText} does not evaluate to a valid (valid values: 'years', 'months', 'weeks', 'days') time-valued quantity !")
            }
          case _ =>
            throw new FhirPathException(s"Invalid function call 'getPeriod', the period expression ${period.getText} does not evaluate to a valid (valid values:  'years', 'months', 'weeks', 'days') time-valued quantity !")
        }

      try {
        // Transform the input dates to LocalDateTime as the Java library does not allow to use
        // shorter chrono periods for longer temporal periods while calculating the difference between them e.g. hour-based difference for years.
        fdate = Some(transformToDateTimeIfApplicable(fdate.get))
        tdate = Some(transformToDateTimeIfApplicable(tdate.get))
        Seq(FhirPathNumber(fdate.get.until(tdate.get, chronoPeriod)))
      } catch {
        case e: Throwable => throw FhirPathException.apply("Invalid function call 'getPeriod', both date time instances should be either with zone or not!", e)
      }
    }
  }

  /**
   * Transforms the given temporal value into date if the value's type is Year, YearMonth or LocalDate. Otherwise, the value is returned as is.
   * First possible value is chosen for the missing attributes while constructing the date time value.
   *
   * @param value Value to be transformed.
   * @return
   */
  private def transformToDateTimeIfApplicable(value: Temporal): Temporal = {
    value match {
      case v: Year => v.atMonth(1).atDay(1).atTime(0, 0)
      case v: YearMonth => v.atDay(1).atTime(0, 0)
      case v: LocalDate => v.atTime(0, 0)
      case _ => value
    }
  }

  /**
   * Paster a custom date string
   *
   * @param sourcePattern Java time date pattern e.g. dd.MM.yyyy
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts a given date string, based on the specified Java time date pattern, to a custom date string that is compliant with FHIR standards.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`sourcePattern`**  \nThe Java time date pattern that defines the format of the input date string. This pattern must follow the Java SimpleDateFormat conventions. The pattern should be designed to match the format of the date strings that will be input to this function. For example:\n- yyyy-MM-dd for dates like 2021-12-25\n- dd/MM/yyyy for dates like 25/12/2024\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n\"25/12/2024\"\n```\n\n\uD83D\uDCA1 **E.g.** '25.12.2024'.utl:toFhirDate('dd.MM.yyyy')",
    insertText = "utl:toFhirDate(<sourcePattern>)", detail = "utl", label = "utl:toFhirDate", kind = "Method", returnType = Seq("dateTime"), inputType = Seq("string"))
  def toFhirDate(sourcePattern: ExpressionContext): Seq[FhirPathResult] = {
    val pattern = new FhirPathExpressionEvaluator(context, current).visit(sourcePattern)
    if (pattern.isEmpty || !pattern.forall(_.isInstanceOf[FhirPathString])) {
      throw FhirPathException("Invalid function call to 'toFhirDate'. The sourcePattern expression (the function parameter) should evaluate to a single string value.")
    }
    current.map {
      case FhirPathString(st) =>
        val formatsToTry =
          pattern.map(p => DateTimeFormatter.ofPattern(p.asInstanceOf[FhirPathString].s))

        formatsToTry
          .flatMap(formatToTry => Try(formatToTry.parseBest(st, LocalDate.from(_), YearMonth.from(_), Year.from(_)).asInstanceOf[Temporal]).toOption)
          .headOption match {
          case None => throw FhirPathException(s"Invalid function call 'toFhirDate'. Given date is not recognized according to the formats ${formatsToTry.mkString(", ")} !")
          case Some(temporal) => FhirPathDateTime(temporal)
        }
      case dt: FhirPathDateTime => dt
      case _ => throw FhirPathException(s"Invalid function call 'toFhirDate'. It should be used on string values !")
    }
  }

  /**
   * Convert the current string value to FHIR Time format (ISO TIME). The current
   * string representation of the time is expected to be in the following format:
   * "HH:mm:ss"
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the current string value of time to the FHIR Time format. The current string representation of the time is expected to be in the following formats:  \n- HH:mm:ss\n- HH:mm:ss.SSS\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n\"22:10:45.123\"\n```\n\n\uD83D\uDCA1 **E.g.** '22:10:45'.utl:toFhirTime()",
    insertText = "utl:toFhirTime()", detail = "utl", label = "utl:toFhirTime", kind = "Method", returnType = Seq("time"), inputType = Seq("string"))
  def toFhirTime(): Seq[FhirPathResult] = {
    val patternsToTry = Seq("HH:mm:ss.SSS", "HH:mm:ss")
    _toFhirTime(patternsToTry)
  }

  /**
   * Convert the current string value to FHIR Time format (ISO TIME). The current string
   * representation of the time is expected to be in the format given with sourcePattern.
   *
   * @param sourcePattern The format pattern of the time string to be converted.
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts a given time string, based on the specified format pattern, to a FHIR Time format, compliant with ISO 8601 standards.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`sourcePattern`**  \nThe format pattern of the time string to be converted. Multiple formats at the same time are also supported using `|` character. The pattern should be designed to match the format of the time strings that will be input to this function. For example:\n- 'HH:mm:ss' for times such as 22:10:45\n- 'HH:mm' for times such as 22:10\n- 'HH:mm:ss.SSS' for times with milliseconds, such as 22:10:45.123\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n\"22:10:45.123\"\n```\n\n\uD83D\uDCA1 **E.g.** '22:10:45'.utl:toFhirTime('HH:mm:ss' | 'HH:mm')",
    insertText = "utl:toFhirTime(<sourcePattern>)", detail = "utl", label = "utl:toFhirTime", kind = "Method", returnType = Seq("time"), inputType = Seq("string"))
  def toFhirTime(sourcePattern: ExpressionContext): Seq[FhirPathResult] = {
    val pattern = new FhirPathExpressionEvaluator(context, current).visit(sourcePattern)
    if (pattern.isEmpty || !pattern.forall(_.isInstanceOf[FhirPathString])) {
      throw FhirPathException("Invalid function call to 'toFhirTime'. The sourcePattern expression (the function parameter) should evaluate to a single string value.")
    }
    _toFhirTime(pattern.map(_.asInstanceOf[FhirPathString].s))
  }

  /**
   * Handler for FHIR time conversions
   *
   * @param patternsToTry Time formats to try
   * @param zoneId        Zone identifier
   * @return
   */
  private def _toFhirTime(patternsToTry: Seq[String], zoneId: ZoneId = ZoneId.systemDefault()): Seq[FhirPathResult] = {
    current.map {
      case time: FhirPathTime => time
      case FhirPathString(st) =>
        val formatsToTry = patternsToTry.map(DateTimeFormatter.ofPattern(_).withZone(zoneId))
        formatsToTry
          .flatMap(formatToTry => Try(LocalTime.parse(st, formatToTry)).toOption)
          .headOption match {
          case None =>
            throw FhirPathException(s"Invalid function call 'toFhirTime'. Time format of the expression is not recognized for the given value '$st'. Valid formats are: ${patternsToTry.mkString(", ")} !")
          case Some(temporal) =>
            FhirPathTime(temporal, Some(zoneId))
        }
      case _ =>
        throw FhirPathException(s"Invalid function call 'toFhirTime'. Time format of the expression is not recognized. Valid formats are: ${patternsToTry.mkString(",")} !")
    }
  }

  /**
   * Convert the current string value to string representation of Fhir DateTime format (ISO DATE TIME). The current
   * string representation of the date-time is expected to be in one of these formats:
   * "yyyy-MM-dd HH:mm:ss", "yyyy-MM-ddHH:mm:ss"
   *
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the current string value to the string representation of FHIR DateTime format, which complies with ISO 8601 standards. The current string representation of the date-time is expected to be in one of these formats:\n- \"yyyy-MM-dd HH:mm:ss\"\n- \"yyyy-MM-ddHH:mm:ss\"\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n\"2012-01-13 22:10:45\"\n```\n\n\uD83D\uDCA1 **E.g.** '2012-01-13 22:10:45'.utl:toFhirDateTime()",
    insertText = "utl:toFhirDateTime()", detail = "utl", label = "utl:toFhirDateTime", kind = "Method", returnType = Seq("dateTime"), inputType = Seq("string"))
  def toFhirDateTime(): Seq[FhirPathResult] = {
    val patternsToTry = Seq("yyyy-MM-dd HH:mm:ss", "yyyy-MM-ddHH:mm:ss")
    _toFhirDateTime(patternsToTry)
  }

  /**
   * Convert the current string value to string representation of Fhir DateTime format (ISO DATE TIME). The current string
   * representation of the date-time is expected to be in the format given with sourcePattern.
   *
   * @param sourcePattern The format pattern of the date-time string to be converted.
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the current string value to the string representation of FHIR DateTime format, which complies with ISO 8601 standards. \n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`sourcePattern`**  \nThe format pattern of the date-time string to be converted. Multiple formats at the same time are also supported using `|` character. The pattern should be designed to match the format of the time strings that will be input to this function. For example:\n- 'yyyy-MM-dd HH:mm:ss.SSS' for times such as 2024-01-13 22:10:45.167\n- 'yyyyMMdd.HH:mm:ss' for times such as 20240113.22:10:45\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n\"2012-01-13 22:10:45\"\n```\n\n\uD83D\uDCA1 **E.g.** '20240113.22:10:45'.utl:toFhirDateTime('yyyy-MM-dd HH:mm:ss' | 'yyyyMMdd.HH:mm:ss')",
    insertText = "utl:toFhirDateTime(<sourcePattern>)", detail = "utl", label = "utl:toFhirDateTime", kind = "Method", returnType = Seq("dateTime"), inputType = Seq("string"))
  def toFhirDateTime(sourcePattern: ExpressionContext): Seq[FhirPathResult] = {
    val pattern = new FhirPathExpressionEvaluator(context, current).visit(sourcePattern)
    if (pattern.isEmpty || !pattern.forall(_.isInstanceOf[FhirPathString])) {
      throw FhirPathException("Invalid function call to 'toFhirDateTime'. The sourcePattern expression (the function parameter) should evaluate to a single string value.")
    }
    _toFhirDateTime(pattern.map(_.asInstanceOf[FhirPathString].s))
  }

  /**
   * Convert the current string value to string representation of Fhir DateTime format (ISO DATE TIME) with the given time zone. The current string
   * representation of the date-time is expected to be in the format given with sourcePattern.
   *
   * @param sourcePattern The format pattern of the date-time string to be converted.
   * @param zoneId        Java ZoneId representation e.g. Europe/Berlin
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Converts the current string value to the string representation of FHIR DateTime format (ISO DATE TIME) with the given time zone.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`sourcePattern`**  \nThe format pattern of the date-time string to be converted. Multiple formats at the same time are also supported using `|` character. The pattern should be designed to match the format of the time strings that will be input to this function. For example:\n- 'yyyy-MM-dd HH:mm:ss.SSS' for times such as 2024-01-13 22:10:45.167\n- 'yyyyMMdd.HH:mm:ss' for times such as 20240113.22:10:45\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`zoneId`**  \nThe Java ZoneId representation of the time zone to be applied to the date-time string. For example:\n- Europe/Berlin\n- America/New_York\n- Asia/Tokyo\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n\"2024-01-13 22:10:45\"\n```\n\n\uD83D\uDCA1 **E.g.** '20240113.22:10:45'.utl:toFhirDateTime('yyyy-MM-dd HH:mm:ss', 'Europe/Berlin')",
    insertText = "utl:toFhirDateTime(<sourcePattern>, <zoneId>)", detail = "utl", label = "utl:toFhirDateTime", kind = "Method", returnType = Seq("dateTime"), inputType = Seq("string"))
  def toFhirDateTime(sourcePattern: ExpressionContext, zoneId: ExpressionContext): Seq[FhirPathResult] = {
    val pattern = new FhirPathExpressionEvaluator(context, current).visit(sourcePattern)
    if (pattern.isEmpty || !pattern.forall(_.isInstanceOf[FhirPathString])) {
      throw FhirPathException("Invalid function call to 'toFhirDateTime'. The sourcePattern expression (the function parameter) should evaluate to a single string value.")
    }
    val zoneIdStr = new FhirPathExpressionEvaluator(context, current).visit(zoneId)

    if (zoneIdStr.length != 1 || !zoneIdStr.head.isInstanceOf[FhirPathString]) {
      throw FhirPathException("Invalid function call to 'toFhirDateTime'. The zoneId expression (the function parameter) should evaluate to a single string value.")
    }
    Try(ZoneId.of(zoneIdStr.head.asInstanceOf[FhirPathString].s)).toOption match {
      case None => throw FhirPathException("Invalid function call to 'toFhirDateTime'. The zoneId expression (the function parameter) should evaluate to a valid Java ZoneId string!")
      case Some(zid) =>
        _toFhirDateTime(pattern.map(_.asInstanceOf[FhirPathString].s), zid)
    }
  }

  /**
   * Handler FHIR date time conversions
   *
   * @param patternsToTry Datetime formats to try
   * @param zoneId        Zone identifier
   * @return
   */
  private def _toFhirDateTime(patternsToTry: Seq[String], zoneId: ZoneId = ZoneId.systemDefault()): Seq[FhirPathResult] = {
    current.map {
      case dt: FhirPathDateTime => dt
      case FhirPathString(st) =>
        val formatsToTry = patternsToTry.map(DateTimeFormatter.ofPattern(_).withZone(zoneId))
        formatsToTry
          .flatMap(formatToTry => Try(formatToTry.parseBest(st, ZonedDateTime.from(_), LocalDate.from(_), YearMonth.from(_), Year.from(_), LocalDate.from(_)).asInstanceOf[Temporal]).toOption)
          .headOption match {
          case None =>
            throw FhirPathException(s"Invalid function call 'toFhirDateTime'. Datetime format of the expression is not recognized for the given value '$st'. Valid formats are: ${patternsToTry.mkString(", ")} !")
          case Some(temporal) =>
            FhirPathDateTime(temporal)
        }
      case _ =>
        throw FhirPathException(s"Invalid function call 'toFhirDateTime'. Datetime format of the expression is not recognized. Valid formats are: ${patternsToTry.mkString(",")} !")
    }
  }

  /**
   * For FHIR coding elements, filter the ones that match the given coding list as FHIR code query
   *
   * @param codingList Expression that evaluates to Seq[FhirPathString] in FHIR token query format [system]|[code]
   * @return
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC For FHIR coding elements, filters the ones that match the given coding list as FHIR code query.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`codingList`**  \nExpression that evaluates to Seq[FhirPathString] in FHIR token query format [system]|[code]\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\ntrue or false\n```\n\n\uD83D\uDCA1 **E.g.** 'http://loinc.org|12345-6'.utl:isCodingIn(Observation.code.coding)",
    insertText = "utl:isCodingIn(<codingListExpr>)", detail = "utl", label = "utl:isCodingIn", kind = "Method", returnType = Seq("boolean"), inputType = Seq())
  def isCodingIn(codingList: ExpressionContext): Seq[FhirPathResult] = {
    val systemAndCodes: Seq[(Option[String], Option[String])] = new FhirPathExpressionEvaluator(context, current).visit(codingList) match {
      case s: Seq[_] if s.forall(i => i.isInstanceOf[FhirPathString]) =>
        s
          .map(_.asInstanceOf[FhirPathString])
          .map(i =>
            try {
              FHIRUtil.parseTokenValue(i.s)
            } catch {
              case t: Throwable => throw new FhirPathException(s"Invalid function call 'filterCodings', first expression ${codingList.getText} does not evaluate to sequence of FHIR Path Strings in FHIR token query format!", Some(t))
            }
          )
      case _ => throw new FhirPathException(s"Invalid function call 'filterCodings', first expression ${codingList.getText} does not evaluate to sequence of FHIR Path Strings!")
    }

    val codingMatch =
      current
        .filter(_.isInstanceOf[FhirPathComplex])
        .map(_.asInstanceOf[FhirPathComplex])
        .exists(coding =>
          systemAndCodes
            .exists {
              // |code query
              case (Some(""), Some(c)) =>
                FHIRUtil.extractValueOption[String](coding.json, "system").isEmpty &&
                  FHIRUtil.extractValueOption[String](coding.json, "code").contains(c)
              // system|code
              case (Some(s), Some(c)) =>
                FHIRUtil.extractValueOption[String](coding.json, "system").contains(s) &&
                  FHIRUtil.extractValueOption[String](coding.json, "code").contains(c)
              // code
              case (None, Some(c)) =>
                FHIRUtil.extractValueOption[String](coding.json, "code").contains(c)
              // system|
              case (Some(s), None) =>
                FHIRUtil.extractValueOption[String](coding.json, "system").contains(s)
              case _ =>
                throw new FhirPathException("Invalid given system codes pairings!")
            }
        )

    Seq(FhirPathBoolean(codingMatch))
  }

}

object FhirPathUtilFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix: String = "utl"

  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathUtilFunctions(context, current)
}

package io.onfhir.path

import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import org.json4s.{JArray, JField, JObject, JString, JValue}

import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoUnit, Temporal}
import java.time._
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
  def uuid(): Seq[FhirPathResult] = {
    Seq(FhirPathString(UUID.randomUUID().toString))
  }

  /**
   * Trim the strings in the current set
   * @return
   */
  def trim():Seq[FhirPathResult] = {
    current.map {
      case FhirPathString(s) => FhirPathString(s.trim)
      case oth => oth
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
  def createFhirCoding(systemExp: ExpressionContext, codeExpr: ExpressionContext, displayExpr: ExpressionContext):Seq[FhirPathResult] = {
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
   * If value or unit expression returns Nil, the function also returns Nil
   *
   * @param valueExpr Expression for quantity value
   * @param unitExpr  Expression for quantity unit
   * @return
   */
  def createFhirQuantity(valueExpr: ExpressionContext, unitExpr: ExpressionContext): Seq[FhirPathResult] = {
    val value = handleFhirQuantityValue(valueExpr)
    val unit = new FhirPathExpressionEvaluator(context, current).visit(unitExpr)
    if (unit.length > 1 || !unit.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter unit expression should return 0 or 1 string value!")

    if (value.isEmpty || unit.isEmpty)
      Nil
    else
      Seq(FhirPathComplex(constructFhirQuantity(value.get._1, unit.head.toJson, None,None, value.get._2) ))
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
  def createFhirQuantity(valueExpr: ExpressionContext, systemExpr: ExpressionContext, codeExpr: ExpressionContext): Seq[FhirPathResult] = {
    val value = handleFhirQuantityValue(valueExpr)

    val system = new FhirPathExpressionEvaluator(context, current).visit(systemExpr)
    if (system.length > 1 || !system.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter system expression should return 0 or 1 string value!")

    val unit = new FhirPathExpressionEvaluator(context, current).visit(codeExpr)
    if (unit.length > 1 || !unit.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirQuantity', parameter unit expression should return 0 or 1 string value!")

    if (value.isEmpty || system.isEmpty || unit.isEmpty)
      Nil
    else
      Seq(FhirPathComplex(constructFhirQuantity(value.get._1, unit.head.toJson, system.headOption.map(_.toJson), unit.headOption.map(_.toJson), value.get._2) ))
  }

  /**
   * Create FHIR Quantity json object with given value unit and optional system and code
   * If value or unit parameter expression returns Nil, the function will also return Nil
   *
   * @param valueExpr Expression to return the quantity value
   * @param unitExpr    Expression to return the unit
   * @param systemExpr  Expression to return the system for the unit
   * @param codeExpr    Expression to return the unit code
   * @return
   */
  def createFhirQuantity(valueExpr: ExpressionContext,
                         unitExpr: ExpressionContext,
                         systemExpr: ExpressionContext,
                         codeExpr:ExpressionContext
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

    if (value.isEmpty || unit.isEmpty)
      Nil
    else
      Seq(FhirPathComplex(constructFhirQuantity(value.get._1, unit.head.toJson, system.headOption.map(_.toJson), code.headOption.map(_.toJson), value.get._2) ))
  }

  /**
   * Construct a FHIR Quantity object from given values
   * @param value       Json number
   * @param unit        Json string for unit
   * @param system      Optional system JString
   * @param code        Optional code JString
   * @param comparator  Optional comparator
   * @return
   */
  private def constructFhirQuantity(value:JValue, unit:JValue, system:Option[JValue], code:Option[JValue], comparator:Option[String]):JObject = {
    var fields = List("value" -> value, "unit" -> unit)
    if(code.isDefined && system.isDefined)
      fields = fields ++ Seq("system" -> system.get,"code" -> code.get)
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
   * @param valueExpr Expression to return the value
   * @return
   */
  def isFhirQuantityExpression():Seq[FhirPathResult] = {
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
   * Handle parsing the FHIR quantity value
   * @param valueExpr
   * @return
   */
  private def handleFhirQuantityValue(valueExpr:ExpressionContext):Option[(JValue, Option[String])] = {
    val value = new FhirPathExpressionEvaluator(context, current).visit(valueExpr)
    value match {
      case Nil => None
      case Seq(n:FhirPathNumber) => Some(n.toJson -> None)
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
  def getDurationAsQuantityObject(fromDate: ExpressionContext, toDate: ExpressionContext): Seq[FhirPathResult] = {
    val fdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(fromDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getDurationAsQuantityObject', second expression ${fromDate.getText} does not evaluate to a single FHIR date time!")
    }

    val tdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(toDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
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
    else if (durationInMin < 60 * 24 * 6)
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
  def split(splitCharExpr: ExpressionContext): Seq[FhirPathResult] = {
    if (!current.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'split' on non string value!")

    val splitChar = new FhirPathExpressionEvaluator(context, current).visit(splitCharExpr)
    if (splitChar.length != 1 || !splitChar.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'split', given expression should return a string value!")

    val splitter = splitChar.head.asInstanceOf[FhirPathString].s
    current
      .map(_.asInstanceOf[FhirPathString])
      .map(_.s.trim.split(splitter))
      .flatMap(_.map(s => FhirPathString(s)))
  }

  /**
   * Create a sequence of indices between from-to integers
   * e.g. indices(1, 10) -> Seq(1,2,....10)
   *
   * @param fromExpr Starting index
   * @param toExpr   End index (inclusive)
   * @return
   */
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
  def throwException(msgExpr: ExpressionContext): Seq[FhirPathResult] = {
    val excMsg = new FhirPathExpressionEvaluator(context, current).visit(msgExpr)
    if (excMsg.length != 1 || !excMsg.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'throwException', given expression should return a string value!")

    throw new FhirPathException(excMsg.head.asInstanceOf[FhirPathString].s)
  }

  /**
   * Get a period between the FHIR date time given in current and  FHIR date time given in first expression
   *
   * @param toDate Given date expression
   * @param period Period requested to calculate; either 'years','months','weeks','days'
   * @return
   */
  def getPeriod(fromDate: ExpressionContext, toDate: ExpressionContext, period: ExpressionContext): Seq[FhirPathResult] = {
    val fdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(fromDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', second expression ${fromDate.getText} does not evaluate to a single FHIR date time!")
    }

    val tdate: Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(toDate) match {
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
        Seq(FhirPathNumber(fdate.get.until(tdate.get, chronoPeriod)))
      } catch {
        case e: Throwable => throw FhirPathException.apply("Invalid function call 'getPeriod', both date time instances should be either with zone or not!", e)
      }
    }
  }

  /**
   * Paster a custom date string
   * @param sourcePattern Java time date pattern e.g. dd.MM.yyyy
   * @return
   */
  def toFhirDate(sourcePattern: ExpressionContext):Seq[FhirPathResult] = {
    val pattern = new FhirPathExpressionEvaluator(context, current).visit(sourcePattern)
    if (pattern.length != 1 || !pattern.head.isInstanceOf[FhirPathString]) {
      throw FhirPathException("Invalid function call to 'toFhirDate'. The sourcePattern expression (the function parameter) should evaluate to a single string value.")
    }
    current.map {
      case FhirPathString(st) =>
        val formatToTry:DateTimeFormatter = DateTimeFormatter.ofPattern(pattern.head.asInstanceOf[FhirPathString].s)
        Try(formatToTry.parseBest(st, LocalDate.from(_), YearMonth.from(_), Year.from(_)).asInstanceOf[Temporal])
          .toOption match {
            case None => throw FhirPathException(s"Invalid function call 'toFhirDate'. Given date is not recognized according to the format ${formatToTry.toString} !")
            case Some(temporal) => FhirPathDateTime(temporal)
          }
      case dt:FhirPathDateTime => dt
      case _ =>   throw FhirPathException(s"Invalid function call 'toFhirDate'. It should be used on string values !")
    }
  }

  /**
   * Convert the current string value to string representation of Fhir DateTime format (ISO DATE TIME). The current
   * string representation of the date-time is expected to be in one of these formats:
   * "yyyy-MM-dd HH:mm:ss", "yyyy-MM-ddHH:mm:ss"
   *
   * @return
   */
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
  def toFhirDateTime(sourcePattern: ExpressionContext): Seq[FhirPathResult] = {
    val pattern = new FhirPathExpressionEvaluator(context, current).visit(sourcePattern)
    if (pattern.length != 1 || !pattern.head.isInstanceOf[FhirPathString]) {
      throw FhirPathException("Invalid function call to 'toFhirDateTime'. The sourcePattern expression (the function parameter) should evaluate to a single string value.")
    }
    _toFhirDateTime(Seq(pattern.head.asInstanceOf[FhirPathString].s))
  }

  /**
   *  Convert the current string value to string representation of Fhir DateTime format (ISO DATE TIME) with the given time zone. The current string
   * representation of the date-time is expected to be in the format given with sourcePattern.
   *
   * @param sourcePattern The format pattern of the date-time string to be converted.
   * @param zoneId        Java ZoneId representation e.g. Europe/Berlin
   * @return
   */
  def toFhirDateTime(sourcePattern:ExpressionContext, zoneId:ExpressionContext):Seq[FhirPathResult] = {
    val pattern = new FhirPathExpressionEvaluator(context, current).visit(sourcePattern)
    if (pattern.length != 1 || !pattern.head.isInstanceOf[FhirPathString]) {
      throw FhirPathException("Invalid function call to 'toFhirDateTime'. The sourcePattern expression (the function parameter) should evaluate to a single string value.")
    }
    val zoneIdStr =  new FhirPathExpressionEvaluator(context, current).visit(zoneId)

    if (pattern.length != 1 || !pattern.head.isInstanceOf[FhirPathString]) {
      throw FhirPathException("Invalid function call to 'toFhirDateTime'. The zoneId expression (the function parameter) should evaluate to a single string value.")
    }
    Try(ZoneId.of(zoneIdStr.head.asInstanceOf[FhirPathString].s)).toOption match {
      case None => throw FhirPathException("Invalid function call to 'toFhirDateTime'. The zoneId expression (the function parameter) should evaluate to a valid Java ZoneId string!")
      case Some(zid) => _toFhirDateTime(Seq(pattern.head.asInstanceOf[FhirPathString].s), zid)
    }
  }

  /**
   * Handler for Date time to FHIR date time conversions
   * @param patternsToTry
   * @param zoneId
   * @return
   */
  private def _toFhirDateTime(patternsToTry: Seq[String], zoneId:ZoneId = ZoneId.systemDefault()): Seq[FhirPathResult] = {
    current.map {
      case dt: FhirPathDateTime => dt
      case FhirPathString(st) =>
        val formatsToTry = patternsToTry.map(DateTimeFormatter.ofPattern(_).withZone(zoneId))
        val dateTime = formatsToTry.collectFirst {
          case format if Try(ZonedDateTime.parse(st, format)).isSuccess => ZonedDateTime.parse(st, format)
        }
        if (dateTime.isDefined) {
          FhirPathDateTime(dateTime.get)
        } else {
          throw FhirPathException(s"Invalid function call 'toFhirDateTime'. Datetime format of the expression is not recognized. Valid formats are: ${patternsToTry.mkString(",")} !")
        }
    }
  }

  /**
   * For FHIR coding elements, filter the ones that match the given coding list as FHIR code query
   *
   * @param codingList Expression that evaluates to Seq[FhirPathString] in FHIR token query format [system]|[code]
   * @return
   */
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

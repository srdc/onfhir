package io.onfhir.path

import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import org.json4s.{JArray, JField, JObject, JString}

import java.time.Period
import java.time.temporal.{ChronoUnit, Temporal}


/**
 * Default FHIR Path function library for onFhir for time utilities and processing
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathUtilFunctions(context:FhirPathEnvironment, current:Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {

  /**
   * Create FHIR Reference object(s) with given resource type and id(s)
   * @param resourceTypeExp  Expression to provide FHIR Resource type 'Patient'
   * @param ridExp           Resource identifier(s)
   * @return
   */
  def createFhirReference(resourceTypeExp:ExpressionContext, ridExp:ExpressionContext):Seq[FhirPathResult] = {
    val rids = new FhirPathExpressionEvaluator(context, current).visit(ridExp)
    if(!rids.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'createFhirReference', ridExp (2nd parameter) should return FHIR Path string values!")

    val rtype = new FhirPathExpressionEvaluator(context, current).visit(resourceTypeExp)
    if(rtype.length != 1 || !rtype.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'createFhirReference', resourceTypeExp (1st parameter) should return single FHIR Path string value!")

    rids
      .map(_.asInstanceOf[FhirPathString])
      .map(rid =>
        FhirPathComplex(JObject(List(JField("reference", JString(s"${rtype.head.asInstanceOf[FhirPathString].s}/${rid.s}")))))
      )
  }

  /**
   * Create a FHIR CodeableConcept content with given system code and optional display
   * @param systemExp     Expression to give the system value
   * @param codeExpr      Expression to give the code value
   * @param displayExpr   Expression to give the display value (optional)
   * @return
   */
  def createFhirCodeableConcept(systemExp:ExpressionContext, codeExpr:ExpressionContext, displayExpr:ExpressionContext):Seq[FhirPathResult] = {
    val system = new FhirPathExpressionEvaluator(context, current).visit(systemExp)
    if(system.length != 1 || !system.forall(_.isInstanceOf[FhirPathString]))
      return Nil

    val code = new FhirPathExpressionEvaluator(context, current).visit(codeExpr)
    if(code.length != 1 || !code.forall(_.isInstanceOf[FhirPathString]))
      return Nil

    val display = new FhirPathExpressionEvaluator(context, current).visit(displayExpr)
    if(display.length > 1 || !code.forall(_.isInstanceOf[FhirPathString]))
      return Nil

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

  /**
   * Create FHIR Quantity json object with given value and unit
   * @param valueExpr
   * @param unitExpr
   * @return
   */
  def createFhirQuantity(valueExpr:ExpressionContext, unitExpr:ExpressionContext):Seq[FhirPathResult] = {
    val value = new FhirPathExpressionEvaluator(context, current).visit(valueExpr)
    if(value.length != 1 || !value.forall(_.isInstanceOf[FhirPathNumber]))
      return Nil

    val unit = new FhirPathExpressionEvaluator(context, current).visit(unitExpr)
    if(unit.length != 1 || !unit.forall(_.isInstanceOf[FhirPathString]))
      return Nil

    Seq(FhirPathComplex(JObject(List(JField("value", value.head.toJson), "unit" -> unit.head.toJson))))
  }

  /**
   * Create FHIR Quantity json object with given value, system and unit
   * @param valueExpr
   * @param systemExpr
   * @param unitExpr
   * @return
   */
  def createFhirQuantity(valueExpr:ExpressionContext, systemExpr:ExpressionContext, unitExpr:ExpressionContext):Seq[FhirPathResult] = {
    val value = new FhirPathExpressionEvaluator(context, current).visit(valueExpr)
    if(value.length != 1 || !value.forall(_.isInstanceOf[FhirPathNumber]))
      return Nil

    val system = new FhirPathExpressionEvaluator(context, current).visit(systemExpr)
    if(system.length != 1 || !system.forall(_.isInstanceOf[FhirPathString]))
      return Nil

    val unit = new FhirPathExpressionEvaluator(context, current).visit(unitExpr)
    if(unit.length != 1 || !unit.forall(_.isInstanceOf[FhirPathString]))
      return Nil

    Seq(FhirPathComplex(JObject(List("value" -> value.head.toJson, "system" -> system.head.toJson, "unit" -> unit.head.toJson, "code" -> unit.head.toJson))))
  }

  /**
   * Retrieve the duration between given FHIR dateTimes as FHIR Duration with a suitable duration unit (either minute, day, or month)
   * @param fromDate  Given date expression
   * @param toDate    Other date expression
   * @return
   */
  def getDurationAsQuantityObject(fromDate:ExpressionContext, toDate:ExpressionContext):Seq[FhirPathResult] = {
    val fdate:Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(fromDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', second expression ${fromDate.getText} does not evaluate to a single FHIR date time!")
    }

    val tdate:Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(toDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', second expression ${toDate.getText} does not evaluate to a single FHIR date time!")
    }
    if(fdate.isEmpty || tdate.isEmpty)
      Nil
    else {
      val durationInMin = fdate.get.until(tdate.get, ChronoUnit.MINUTES)
      if(durationInMin < 60 * 24)
        Seq(FhirPathComplex(FhirPathQuantity(FhirPathNumber(durationInMin), "min").toJson.asInstanceOf[JObject]))
      else if(durationInMin < 60 * 24 * 6)
        Seq(FhirPathComplex(FhirPathQuantity(FhirPathNumber(durationInMin / (60.0 * 24.0)), "d").toJson.asInstanceOf[JObject]))
      else
        Seq(FhirPathComplex(FhirPathQuantity(FhirPathNumber(durationInMin / (60.0 * 24.0 * 30)), "mo").toJson.asInstanceOf[JObject]))
    }
  }

  /**
   * Split the current string value by given split character or string
   * @param splitCharExpr
   * @return
   */
  def split(splitCharExpr:ExpressionContext):Seq[FhirPathResult] = {
    if(!current.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'split' on non string value!")

    val splitChar = new FhirPathExpressionEvaluator(context, current).visit(splitCharExpr)
    if(splitChar.length!=1 || !splitChar.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'split', given expression should return a string value!")

    val splitter = splitChar.head.asInstanceOf[FhirPathString].s
    current
      .map(_.asInstanceOf[FhirPathString])
      .map(_.s.split(splitter))
      .flatMap(_.map(s => FhirPathString(s)))
  }

  /**
   * Create a sequence of indices between from-to integers
   * e.g. indices(1, 10) -> Seq(1,2,....10)
   * @param fromExpr  Starting index
   * @param toExpr    End index (inclusive)
   * @return
   */
  def indices(fromExpr:ExpressionContext, toExpr:ExpressionContext):Seq[FhirPathResult] = {
    val from = new FhirPathExpressionEvaluator(context, current).visit(fromExpr)
    if(from.length != 1 || !from.forall(_.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Invalid function call 'indices', given expressions should return a integer value!")

    val to = new FhirPathExpressionEvaluator(context, current).visit(toExpr)
    if(to.length != 1 || !to.forall(_.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Invalid function call 'indices', given expressions should return a integer value!")

    (from.head.asInstanceOf[FhirPathNumber].v.toInt to to.head.asInstanceOf[FhirPathNumber].v.toInt).map(i => FhirPathNumber(i))
  }

  /**
   * Return the indices (starting from 1) in the current sequence of results where given expression returns true
   * @param condExpr  Boolean expression to check for each item
   * @return
   */
  def indicesWhere(condExpr:ExpressionContext):Seq[FhirPathResult] = {
    current
      .zipWithIndex
      .map(r => {
        val result = new FhirPathExpressionEvaluator(context, Seq(r._1)).visit(condExpr)
        (r._2+1) -> result.find(_.isInstanceOf[FhirPathBoolean]).exists(_.asInstanceOf[FhirPathBoolean].b)
      })
      .filter(_._2)
      .map(i => FhirPathNumber(i._1))
  }

  /**
   * Throw a FHIR Path exception with the given msg
   * @param msgExpr Message for the exception
   * @return
   */
  def throwException(msgExpr:ExpressionContext):Seq[FhirPathResult] = {
    val excMsg = new FhirPathExpressionEvaluator(context, current).visit(msgExpr)
    if(excMsg.length!=1 || !excMsg.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'throwException', given expression should return a string value!")

    throw new FhirPathException(excMsg.head.asInstanceOf[FhirPathString].s)
  }

  /**
   * Get a period between the FHIR date time given in current and  FHIR date time given in first expression
   * @param toDate Given date expression
   * @param period Period requested to calculate; either 'years','months','weeks','days'
   * @return
   */
  def getPeriod(fromDate:ExpressionContext, toDate:ExpressionContext, period:ExpressionContext):Seq[FhirPathResult] = {
    val fdate:Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(fromDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', second expression ${fromDate.getText} does not evaluate to a single FHIR date time!")
    }

    val tdate:Option[Temporal] = new FhirPathExpressionEvaluator(context, current).visit(toDate) match {
      case Seq(FhirPathDateTime(dt)) => Some(dt)
      case Nil => None
      case _ => throw new FhirPathException(s"Invalid function call 'getPeriod', second expression ${toDate.getText} does not evaluate to a single FHIR date time!")
    }

    if(fdate.isEmpty || tdate.isEmpty)
      Seq(FhirPathNumber(0))
    else {
      val chronoPeriod =
        new FhirPathExpressionEvaluator(context, current).visit(period) match {
          case Seq(FhirPathString(p)) =>
            p match {
              case "year" | "years" => ChronoUnit.YEARS
              case "month" | "months" => ChronoUnit.MONTHS
              case "week" | "weeks" => ChronoUnit.WEEKS
              case "day" | "days" => ChronoUnit.DAYS
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
}

object FhirPathUtilFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix:String = "utl"
  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathUtilFunctions(context,current)
}
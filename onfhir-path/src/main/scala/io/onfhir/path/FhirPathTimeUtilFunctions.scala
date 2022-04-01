package io.onfhir.path

import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import org.json4s.JObject

import java.time.Period
import java.time.temporal.{ChronoUnit, Temporal}


/**
 * Default FHIR Path function library for onFhir for time utilities and processing
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathTimeUtilFunctions(context:FhirPathEnvironment, current:Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {

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

object FhirPathTimeUtilFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix:String = "tutl"
  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathTimeUtilFunctions(context,current)
}
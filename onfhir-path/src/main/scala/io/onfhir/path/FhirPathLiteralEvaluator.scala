package io.onfhir.path

import java.time.{LocalDate, LocalDateTime, LocalTime, Year, YearMonth, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.{Temporal, TemporalAccessor}

import io.onfhir.path.grammar.{FhirPathExprBaseVisitor, FhirPathExprParser}

import scala.util.Try

/**
  * Evaluator for FHIR Path literals
  */
object FhirPathLiteralEvaluator extends FhirPathExprBaseVisitor[Seq[FhirPathResult]] {
  // FHIR Path date time formatter
  val fhirPathDateTimeFormatter =  DateTimeFormatter.ofPattern("yyyy[-MM[-dd['T'HH[:mm[:ss[.SSS][XXX]]]]]]")

  //Date time units
  private val dtUnits= Map (
    "year" -> "'a'", "years" -> "'a'",
    "month" -> "'mo'", "months" -> "'mo'",
    "week" -> "'wk'", "weeks" ->  "'wk'",
    "day" -> "'d'", "days" -> "'d'",
    "hour" -> "'h'", "hours" -> "'h'",
    "minute" -> "'min'", "minutes" -> "'min'",
    "second" -> "'s'", "seconds" -> "'s'",
    "millisecond" -> "'ms'", "milliseconds" -> "'ms'"
  )

  /**
    *
    * @param ctx the parse tree
    *     */
  override def visitNullLiteral(ctx: FhirPathExprParser.NullLiteralContext): Seq[FhirPathResult] = {
    Nil
  }

  /**
    *
    * @param ctx the parse tree
    *     */
  override def visitBooleanLiteral(ctx: FhirPathExprParser.BooleanLiteralContext): Seq[FhirPathResult]  = {
    Seq(ctx.getText() match {
      case "true" => FhirPathBoolean(true)
      case "false" => FhirPathBoolean(false)
    })
  }

  /**
    *
    * @param ctx the parse tree
    *     */
  override def visitStringLiteral(ctx: FhirPathExprParser.StringLiteralContext): Seq[FhirPathResult]  = {
    Seq(FhirPathString(ctx.STRING().getText.drop(1).dropRight(1)))
  }

  /**
    *
    * @param ctx the parse tree
    *     */
  override def visitNumberLiteral(ctx: FhirPathExprParser.NumberLiteralContext): Seq[FhirPathResult]  = {
    Seq(parseNumberLiteral(ctx.NUMBER().getText))
  }

  /**
    *
    * @param ctx the parse tree
    *     */
  override def visitDateTimeLiteral(ctx: FhirPathExprParser.DateTimeLiteralContext): Seq[FhirPathResult]  = {
    Seq(FhirPathDateTime(parseFhirDateTimeBest(ctx.DATETIME().getText.drop(1))))
  }

  /**
    *
    * @param ctx the parse tree
    *     */
  override def visitTimeLiteral(ctx: FhirPathExprParser.TimeLiteralContext): Seq[FhirPathResult]  = {
    val (lt, zdt) = parseFhirTime(ctx.TIME().getText.drop(1))
    Seq(FhirPathTime(lt, zdt))
  }

  /**
    *
    * @param ctx the parse tree
    *     */
  override def visitQuantityLiteral(ctx: FhirPathExprParser.QuantityLiteralContext): Seq[FhirPathResult]  = {
    val n = parseNumberLiteral(ctx.quantity().NUMBER().getText)
    val unitCtx = ctx.quantity().unit()
    val unit = if(unitCtx.dateTimePrecision() != null)
      unitCtx.dateTimePrecision().getText
    else if(unitCtx.pluralDateTimePrecision() != null)
      unitCtx.pluralDateTimePrecision().getText
    else
      unitCtx.STRING().getText
    Seq(FhirPathQuantity(n, dtUnits.getOrElse(unit, unit)))
  }

  /**
    * Parse a number literal
    * @param n number in string format
    * @return
    */
  private def parseNumberLiteral(n:String):FhirPathNumber = {
    FhirPathNumber(n.toDouble)
  }


  /**
    * Parse FHIR Date time
    * @param dt FHIR Path date time string
    * @return
    */
  def parseFhirDateTimeBest(dt:String):Temporal = {
    fhirPathDateTimeFormatter.parseBest(dt, ZonedDateTime.from(_), LocalDateTime.from(_), LocalDate.from(_), YearMonth.from(_), Year.from(_)).asInstanceOf[Temporal]
  }


  def format(dt:FhirPathDateTime):String = fhirPathDateTimeFormatter.format(dt.dt)
  def format(t:FhirPathTime):String = {
    val temporal = t.zone match {
      case Some(z) => LocalDate.of(2000, 1, 1).atTime(t.lt).atZone(z)
      case None => LocalDate.of(2000, 1, 1).atTime(t.lt)
    }

    format(FhirPathDateTime(temporal)).substring(10)
  }

  /**
    *
    * @param t
    * @return
    */
  def parseFhirTime(t:String):(LocalTime, Option[ZoneOffset]) = {
    parseFhirDateTimeBest("2019-01-01"+t) match {
      case ldt:LocalDateTime => ldt.toLocalTime -> None
      case zdt:ZonedDateTime => zdt.toLocalTime -> Some(zdt.getOffset)
    }
  }

  def parseIdentifier(identifier:String):String = {
    if(identifier.head == '`')
      identifier.drop(1).dropRight(1)
    else
      identifier
  }
}

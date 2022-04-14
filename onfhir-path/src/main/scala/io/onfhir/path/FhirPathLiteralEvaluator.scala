package io.onfhir.path

import java.time.{LocalDate, LocalDateTime, LocalTime, Year, YearMonth, ZoneOffset, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.Temporal
import io.onfhir.path.grammar.{FhirPathExprBaseVisitor, FhirPathExprParser}

import java.time.temporal.ChronoField.{DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, MONTH_OF_YEAR, NANO_OF_SECOND, SECOND_OF_MINUTE, YEAR}

/**
  * Evaluator for FHIR Path literals
  */
object FhirPathLiteralEvaluator extends FhirPathExprBaseVisitor[Seq[FhirPathResult]] {
  val fhirPathTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendValue(HOUR_OF_DAY, 2)
      .optionalStart()
      .appendLiteral(':')
      .appendValue(MINUTE_OF_HOUR, 2)
      .optionalStart
      .appendLiteral(':')
      .appendValue(SECOND_OF_MINUTE, 2)
      .optionalStart
      .appendFraction(NANO_OF_SECOND, 0, 3, true)
      .parseStrict()
      .toFormatter


  // FHIR Path date time formatter
  val fhirPathDateTimeFormatter =
    new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
    .optionalStart()
    .appendLiteral('-')
    .appendValue(MONTH_OF_YEAR, 2)
    .optionalStart()
    .appendLiteral('-')
    .appendValue(DAY_OF_MONTH, 2)
    .optionalStart()
    .appendLiteral('T')
    .appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(MINUTE_OF_HOUR, 2)
    .optionalStart
    .appendLiteral(':')
    .appendValue(SECOND_OF_MINUTE, 2)
    .optionalStart
    .appendFraction(NANO_OF_SECOND, 0, 3, true)
    .optionalStart()
    .appendOffset("+HH:MM", "Z")
    .parseStrict()
    .toFormatter

    //DateTimeFormatter.ofPattern("yyyy[-MM[-dd['T'HH[:mm[:ss[.SSS][XXX]]]]]]")

  val fhirQuantityRegExp =   """(?<value>(\+|-)?\d+(\.\d+)?)\s*('(?<unit>[^']+)'|(?<time>[a-zA-Z]+))?""".r

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
    val (lt, zdt) = parseFhirTime(ctx.TIME().getText.drop(2)) //drop the '@T' part
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
   *
   * @param q
   * @return
   */
  def parseFhirQuantity(q:String):Option[FhirPathQuantity] = {
    fhirQuantityRegExp
      .findFirstMatchIn(q)
      .flatMap(m => {
        val value = m.group(1)
        var unit = Option(m.group(5))
        if(unit.isEmpty) unit = dtUnits.get(m.group(6)).map(_.drop(1).dropRight(1))
        unit.map(u =>FhirPathQuantity(FhirPathNumber(value.toDouble), u))
      })
  }

  /**
    * Parse FHIR Date time
    * @param dt FHIR Path date time string
    * @return
    */
  def parseFhirDateTimeBest(dt:String):Temporal = {
    fhirPathDateTimeFormatter.parseBest(dt, ZonedDateTime.from(_), LocalDateTime.from(_), LocalDate.from(_), YearMonth.from(_), Year.from(_)).asInstanceOf[Temporal]
  }

  def parseFhirDateBest(dt:String):Temporal = {
    fhirPathDateTimeFormatter.parseBest(dt, LocalDate.from(_), YearMonth.from(_), Year.from(_)).asInstanceOf[Temporal]
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
    * @param t  Time in FHIR Path time format without the initial '@T'
   *            e.g. 10, 10:05, 10:00:02, 10:25:37.254
   * @return
   */
  def parseFhirTime(t:String):(LocalTime, Option[ZoneOffset]) = {
    LocalTime.parse(t, fhirPathTimeFormatter) -> None
    /*
    parseFhirDateTimeBest("2019-01-01"+t) match {
      case ldt:LocalDateTime => ldt.toLocalTime -> None
      case zdt:ZonedDateTime => zdt.toLocalTime -> Some(zdt.getOffset)
    }*/
  }

  def parseIdentifier(identifier:String):String = {
    if(identifier.head == '`')
      identifier.drop(1).dropRight(1)
    else
      identifier
  }
}

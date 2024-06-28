package io.onfhir.util

import java.time.{Instant, LocalDate, LocalDateTime, Year, YearMonth, ZoneId, ZoneOffset, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.util.Date
import akka.http.scaladsl.model.DateTime
import io.onfhir.api.MONTH_DAY_MAP
import org.apache.commons.lang3.time.FastDateFormat
import org.slf4j.LoggerFactory

import java.time.temporal.ChronoField.{DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, MONTH_OF_YEAR, NANO_OF_SECOND, SECOND_OF_MINUTE, YEAR}
import java.time.temporal.Temporal
import scala.util.Try

object DateTimeUtil {
  private val httpDateFormat = FastDateFormat.getInstance("EEE, dd MMM yyyy HH:mm:ss zzz")
  //FHIR instant
  private val fhirDateTimeWMiliFormat = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
  //FHIR dateTime or instant with second precision
  private val fhirDateTimeWSecFormat = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ssXXX")

  //Regex for dateTime search parameter value
  private val dateTimeRegex = """^([1-2]{1}[0|1|8|9][0-9]{2})(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):([0-5][0-9])(:[0-5][0-9])?(\.[0-9]+)?(Z|(\+|-|\s)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?$""".r

  val fhirDateTimeFormatter =
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

  val logger = LoggerFactory.getLogger("DateTimeUtil")
  /**
    * Populates the upper limit for the date range in dateTime
    * format. Some months are have 31 days and some months are
    * 30 days long(except feb).
    *
    * @param string date string(yyyy-mm)
    * @return date time string for the upper limit
    */
  def dayMonthConversionDateTime(string:String): String = {
    val yearMonth = string.split("-")
    val year = yearMonth.head.toInt
    val month = yearMonth.last.toInt
    // If year is divisible by 4 then that year is called leap year(Feb has 29 days)
    if(year%4==0 && month==2) {
      "-29T23:59:59.999Z"
    } else {
      // Else fetch the days from the constants
      MONTH_DAY_MAP(month) + "T23:59:59.999Z"
    }
  }

  /**
    * Populates the upper limit for the date range in date format.
    * Some months are have 31 days and some months are 30 days
    * long(except feb).
    *
    * @param string date string(yyyy-mm)
    * @return date string for the upper limit
    */
  def dayMonthConversionDate(string:String): String = {
    val yearMonth = string.split("-")
    val year = yearMonth.head.toInt
    val month = yearMonth.last.toInt
    // If year is divisible by 4 then that year is called leap year(Feb has 29 days)
    if(year%4==0 && month==2) {
      "-29"
    } else {
      // Else fetch the days from the constants
      MONTH_DAY_MAP(month)
    }
  }

  /**
    * Populates boundary values for input dates that are representing
    * a range implicitly. For example; search value 2016-10-10 covers
    * the interval (2016-10-10T00:00:00, 2016-10-10T23:59:59)
    *
    * @param value search value
    * @return tuple of date time boundaries
    */
  def populateImplicitDateTimeRanges(value:String) : (String, String) = {
    value.count(_ == '-') match {
      case 0 =>
        // YYYY value types
        (value + "-01-01T00:00:00.000Z", value + "-12-31T23:59:59.999Z")
      case 1 =>
        // YYYY-MM value types
        (value + "-01T00:00:00.000Z", value + DateTimeUtil.dayMonthConversionDateTime(value))
      case 2 =>
        // If there is already a defined time field(complement if required)
        if(value.contains("T")) {
          //instant precision
          if(value.contains(".")){
            (value, value)
          } else {
            val timePrecision = value.count(_ == ':')
            //With a time zone
            if (value.contains('+')) {
              // ..HH:MM+.. => ..HH:MM:00+../..HH:MM:59+..
              val split = value.split('+')
              if(timePrecision == 2) // Hour and minute
                (split.head + ":00.000+" + split.last, split.head + ":59.999+" + split.last)
              else  //Hour minute second
                (split.head + ".000+" + split.last, split.head + ".999+" + split.last)
            } else if (value.contains('Z')) {
              val split = value.split('Z')
              if(timePrecision == 1)
                // ..HH:MMZ => ..HH:MM:00Z/..HH:MM:59Z
                (split.head + ":00.000Z", split.head + ":59.999Z")
              else
                (split.head + ".000Z", split.head + ".999Z")
            } else if (!value.contains('Z')) {
              if(timePrecision == 1)
                // ..HH:MM => ..HH:MM:00Z/..HH:MM:59Z
                (value + ":00.000Z", value + ":59.999Z")
              else
              // ..HH:MM:SS => ..HH:MM:SSZ/..HH:MM:SSZ
                (value + ".000Z", value + ".999Z")
            } else {
              //Not possible
              (value, value)
            }
          }
        } else {
          // Fully date provided without time field
          (value + "T00:00:00.000Z", value + "T23:59:59.999Z")
        }
      case 3 =>
        if(value.contains("."))
          (value, value)
        else
          // Means time zone sperated with the '-'
          if(value.count(_ == ':') == 2) {
            // ..HH:MM-.. => ..HH:MM:00-../..HH:MM:59-..
            val split = value.splitAt(value.lastIndexOf('-'))
            (split._1 + ":00.000" + split._2, split._1 + ":59.999" + split._2)
          } else {
            val split = value.splitAt(value.lastIndexOf('-'))
            (split._1 + ".000" + split._2, split._1 + ".999" + split._2)
          }
    }
  }


  /**
    * Populates boundary values for input dates that are representing
    * a range implicitly. For example; search value 2016-10 covers
    * the interval(2016-10-01, 2016-10-31). Also trims time part
    * for the compatible comparison.
    *
    * @param value search value
    * @return tuple of date boundaries
    */
  def populateImplicitDateRanges(value:String) : (String, String) = {
    value.count(_ == '-') match {
      case 0 =>
        // YYYY value types
        (value + "-01-01", value + "-12-31")
      case 1 =>
        // YYYY-MM value types
        (value + "-01", value + DateTimeUtil.dayMonthConversionDate(value))
      case _ =>
        if(value.contains("T")) {
          // If the value has a defined time field then trim it to the date format
          (value.substring(0, value.indexOf('T')), value.substring(0, value.indexOf('T')))
        } else {
          (value, value)
        }
    }
  }

  /**
    * Parses Http date format (e.g. Wed, 09 Apr 2008 23:55:38 GMT) to
    * spray.DateTime object
    * @param value DateTime string
    * @return
    */
  def parseHttpDateToDateTime(value:String):DateTime = {
    DateTime(httpDateFormat.parse(value).getTime)
  }

  /**
    * Parse FHIR instant (xs:dateTime)
    * @param value FHIR instant string
    * @return
    */
  def parseInstant(value: String):Option[DateTime] = {
    Try(instantToDateTime(parseFhirDateTimeOrInstant(value))).toOption
    //DateTime.fromIsoDateTimeString(value)
  }

  /**
    * Serialize instant
    * @param instant Instant to serialize
    * @return
    */
  def serializeInstant(instant:Instant):String = {
    fhirDateTimeWMiliFormat.format(Date.from(instant))
    //DateTimeFormatter.ISO_INSTANT.format(instant)
  }


  def parseFhirDateTimeOrInstant(value:String):Instant = {
    if(value.contains('.'))
      fhirDateTimeWMiliFormat.parse(value).toInstant
    else {
      fhirDateTimeWSecFormat.parse(value).toInstant
    }
  }

  def instantToDateTime(i:Instant):DateTime = {
    DateTime.apply(i.toEpochMilli)
    /*
    var instStr = serializeInstant(i)
    logger.debug("INSTANT: "+ instStr)
    //Akka DateTime has a bug that it cannot parse when nanosecond is 0 and ISO instant is serialized as ...T10:05:58Z
    if(i.getNano == 0)
      instStr = instStr.dropRight(1) + ".000Z"
    val dt = DateTime.fromIsoDateTimeString(instStr)
    dt.get*/
  }

  def dateTimeToInstant(dateTime: DateTime):Instant = {
    Instant.ofEpochMilli(dateTime.clicks)
  }

  def serializeDateTime(dateTime: DateTime):String = {
    serializeInstant(dateTimeToInstant(dateTime))
  }

  /**
   * Parse FHIR Date time
   *
   * @param dt FHIR Path date time string
   * @return
   */
  def parseFhirDateTimeBest(dt: String): Temporal = {
    fhirDateTimeFormatter.parseBest(dt, ZonedDateTime.from(_), LocalDateTime.from(_), LocalDate.from(_), YearMonth.from(_), Year.from(_)).asInstanceOf[Temporal]
  }

  def parseFhirDateTimeBestExceptYear(dt: String): Temporal = {
    fhirDateTimeFormatter.parseBest(dt, ZonedDateTime.from(_), LocalDateTime.from(_), LocalDate.from(_), YearMonth.from(_)).asInstanceOf[Temporal]
  }

  def parseFhirDateBest(dt: String): Temporal = {
    fhirDateTimeFormatter.parseBest(dt, LocalDate.from(_), YearMonth.from(_), Year.from(_)).asInstanceOf[Temporal]
  }

  /**
   * Find implicit range for date value in query
   * @param value
   * @return
   */
  def findImplicitRangeForDate(value: String): Option[Seq[LocalDate]] = {
    Try(parseFhirDateBest(value)).toOption.map {
      case ld: LocalDate =>
        Seq(ld)
      case ym: YearMonth =>
        Seq(
          ym.atDay(1),
          ym.atEndOfMonth()
        )
      case y: Year =>
        Seq(
          y.atDay(1),
          y.plusYears(1).atDay(1).minusDays(1)
        )
    }
  }

  /**
   * Find implicit range of given datetime value in query
   * @param value Given datetime string
   * @return
   */
  def findImplicitInstantRangeForDateTime(value: String): Option[Seq[Instant]] = {
    value match {
      //Only year given e.g. 2012
      case dateTimeRegex(year, null, null, null, null, null, null, null, null, null, null, null, null, null) =>
        val y = Year.parse(year)
        Some(Seq(
          y.atDay(1).atStartOfDay(ZoneId.systemDefault()).toInstant,
          y.plusYears(1).atDay(1).atStartOfDay(ZoneId.systemDefault()).minusNanos(1).toInstant,
        ))
      //Only Year-month given  e.g. 2012-05
      case dateTimeRegex(year, _, month, null, null, null, null, null, null, null, null, null, null, null) =>
        val ym = YearMonth.of(year.toInt, month.toInt)
        Some(Seq(
          ym.atDay(1).atStartOfDay(ZoneId.systemDefault()).toInstant,
          ym.plusMonths(1).atDay(1).atStartOfDay(ZoneId.systemDefault()).minusNanos(1).toInstant,
        ))
      //Only date given e.g. 2015-05-03
      case dateTimeRegex(year, _, month, _, date, null, null, null, null, null, null, null, null, null) =>
        val dt = LocalDate.of(year.toInt, month.toInt, date.toInt)
        Some(Seq(
          dt.atStartOfDay(ZoneId.systemDefault()).toInstant,
          dt.plusDays(1).atStartOfDay(ZoneId.systemDefault()).minusNanos(1).toInstant,
        ))
      //Date, hour, minute given with or without time zone e.g. 2015-05-03T10:00Z
      case dateTimeRegex(year, _, month, _, date, _, hour, minute, null, null, tz, _, _, _) =>
        val ldt = LocalDateTime.of(year.toInt, month.toInt, date.toInt, hour.toInt, minute.toInt, 0)
        val timeZone = Option(tz).map(ZoneOffset.of).getOrElse(ZoneId.systemDefault())
        Some(Seq(
          ldt.atZone(timeZone).toInstant,
          ldt.plusSeconds(59).atZone(timeZone).toInstant,
        ))
      //  Date, hour, minute, second given e.g. 2015-05-03T10:00:05Z
      case dateTimeRegex(year, _, month, _, date, _, hour, minute, second, null, tz, _, _, _) =>
        val ldt = LocalDateTime.of(year.toInt, month.toInt, date.toInt, hour.toInt, minute.toInt, second.drop(1).toInt)
        val timeZone = Option(tz).map(ZoneOffset.of).getOrElse(ZoneId.systemDefault())
        Some(Seq(
          ldt.atZone(timeZone).toInstant,
          ldt.plusSeconds(1).minusNanos(1).atZone(timeZone).toInstant,
        ))
      //Millisecond precision given e.g. 2015-05-03T10:00:05.056Z
      case dateTimeRegex(year, _, month, _, date, _, hour, minute, second, ms, tz, _, _, _) =>
        Some(Seq(ZonedDateTime.parse(value).toInstant))
      //Any other format is invalid
      case _ => None
    }
  }
}

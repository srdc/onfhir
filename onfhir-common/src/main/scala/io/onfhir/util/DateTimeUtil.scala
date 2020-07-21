package io.onfhir.util

import java.time.{Instant, ZonedDateTime}
import java.time.format.DateTimeFormatter

import akka.http.scaladsl.model.DateTime
import io.onfhir.api.MONTH_DAY_MAP
import org.apache.commons.lang3.time.FastDateFormat

object DateTimeUtil {
  private val httpDateFormat = FastDateFormat.getInstance("EEE, dd MMM yyyy HH:mm:ss zzz")

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
    DateTime.fromIsoDateTimeString(value)
  }

  /**
    * Serialize instant
    * @param instant Instant to serialize
    * @return
    */
  def serializeInstant(instant:Instant):String = DateTimeFormatter.ISO_INSTANT.format(instant)


  def parseFhirDateTimeOrInstant(value:String):Instant = {
    Instant.parse(value)
  }

  def instantToDateTime(i:Instant):DateTime = {
    var instStr = serializeInstant(i)
    //Akka DateTime has a bug that it cannot parse when nanosecond is 0 and ISO instant is serialized as ...T10:05:58Z
    if(i.getNano == 0)
      instStr = instStr.dropRight(1) + ".000Z"
    val dt = DateTime.fromIsoDateTimeString(instStr)
    dt.get
  }
}

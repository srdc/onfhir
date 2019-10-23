package io.onfhir.util

import java.time.Instant
import java.time.format.DateTimeFormatter

import akka.http.scaladsl.model.DateTime

import io.onfhir.api.{MONTH_DAY_MAP}

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
      "-29T23:59:59Z"
    } else {
      // Else fetch the days from the constants
      MONTH_DAY_MAP(month) + "T23:59:59Z"
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
        (value + "-01-01T00:00:00Z", value + "-12-31T23:59:59Z")
      case 1 =>
        // YYYY-MM value types
        (value + "-01T00:00:00Z", value + DateTimeUtil.dayMonthConversionDateTime(value))
      case 2 =>
        // If there is already a defined time field(complement if required)
        if(value.contains("T")) {
          val timePrecision = value.count(_ == ':')
          if(value.contains('+') && timePrecision == 2) {
            // ..HH:MM+.. => ..HH:MM:00+../..HH:MM:59+..
            val split = value.split('+')
            (split.head + ":00+" + split.last, split.head + ":59+" + split.last)
          } else if(value.contains('Z') && timePrecision == 1) {
            // ..HH:MMZ => ..HH:MM:00Z/..HH:MM:59Z
            val split = value.split('Z')
            (split.head + ":00Z", split.head + ":59Z")
          } else if(!value.contains('Z') && timePrecision == 1) {
            // ..HH:MM => ..HH:MM:00Z/..HH:MM:59Z
            (value + ":00Z", value + ":59Z")
          } else if(!value.contains('Z') && timePrecision == 2) {
            // ..HH:MM:SS => ..HH:MM:SSZ/..HH:MM:SSZ
            (value + "Z", value + "Z")
          } else {
            (value, value)
          }
        } else {
          // Fully date provided without time field
          (value + "T00:00:00Z", value + "T23:59:59Z")
        }
      case 3 =>
        // Means time zone sperated with the '-'
        if(value.count(_ == ':') == 1) {
          // ..HH:MM-.. => ..HH:MM:00-../..HH:MM:59-..
          val split = value.splitAt(value.lastIndexOf('-'))
          (split._1 + ":00" + split._2, split._1 + ":59" + split._2)
        } else {
          (value, value)
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

}

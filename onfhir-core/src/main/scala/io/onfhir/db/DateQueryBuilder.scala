package io.onfhir.db

import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_DATA_TYPES, FHIR_EXTRA_FIELDS, FHIR_PREFIXES_MODIFIERS}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.InternalServerException
import io.onfhir.util.DateTimeUtil
import org.mongodb.scala.bson.{BsonDateTime, BsonValue}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

/**
 * Utility class to construct MongoDB queries for FHIR date type search parameters
 */
object DateQueryBuilder extends IFhirQueryBuilder {

  /**
   * Construct query for date queries
   * @param prefixAndValues
   * @param path
   * @param targetType
   * @return
   */
  def getQuery(prefixAndValues: Seq[(String, String)], path: String, targetType: String): Bson = {
    //Split the parts
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
    orQueries(
      prefixAndValues.map {
        case (prefix, value) =>
          // '+' in time zone field is replaced with ' ' by the server
          val dateValue = value.replace(" ", "+")
          // Construct main query on date object
          val mainQuery =
            targetType match {
              case FHIR_DATA_TYPES.DATE |
                   FHIR_DATA_TYPES.DATETIME |
                   FHIR_DATA_TYPES.INSTANT =>
                getQueryForTimePoint(queryPath.getOrElse(""), dateValue, prefix)
              case FHIR_DATA_TYPES.PERIOD =>
                getQueryForPeriod(queryPath.getOrElse(""), dateValue, prefix, isTiming = false)
              case FHIR_DATA_TYPES.TIMING =>
                //TODO Handle event case better by special query on array (should match for all elements, not or)
                Filters.or(
                  getQueryForTiming(queryPath.getOrElse(""), dateValue, prefix),
                  getQueryForPeriod(queryPath.getOrElse(""), dateValue, prefix, isTiming = true)
                )
              case other =>
                throw new InternalServerException(s"Unknown target element type $other !!!")
            }

          //If an array exist, use elemMatch otherwise return the query
          getFinalQuery(elemMatchPath, mainQuery)
      }
    )
  }

  /**
   * Handles prefix for date values(implicit range) for date parameters.
   * For further information about using prefixes with range values
   * please refer to prefix table's third column in page;
   * https://www.hl7.org/fhir/search.html#prefix
   *
   * @param path   absolute path of the parameter
   * @param value  value of the parameter
   * @param prefix prefix of the parameter
   * @return BsonDocument for the query
   */
  def getQueryForTimePoint(path: String, value: String, prefix: String): Bson = {
    // Populate Implicit ranges(e.g. 2010-10-10 represents the range 2010-10-10T00:00Z/2010-10-10T23:59ZZ)
    val implicitRanges = DateTimeUtil.populateImplicitDateTimeRanges(value)
    // Generate the implicit range paths(i.e. the paths created by the server)
    val rangePaths = (FHIRUtil.mergeElementPath(path, FHIR_EXTRA_FIELDS.TIME_RANGE_START), FHIRUtil.mergeElementPath(path, FHIR_EXTRA_FIELDS.TIME_RANGE_END))

    //If it is a datetime or instant base query
    if (value.contains("T")) {
      //We handle this specially as onfhir store this in millisecond precision
      if (path == "meta.lastUpdated")
        getQueryForDateTime(FHIRUtil.mergeElementPath(path, FHIR_EXTRA_FIELDS.TIME_TIMESTAMP), prefix, implicitRanges)
      else
      // Build dateTime query on date time and period query on implicit ranges and combine them.
        Filters.or(
          getQueryForDateTime(FHIRUtil.mergeElementPath(path, FHIR_EXTRA_FIELDS.TIME_TIMESTAMP), prefix, implicitRanges),
          getQueryForPeriodRange(rangePaths, prefix, implicitRanges)
        )
    } else {
      if (path == "meta.lastUpdated")
        getQueryForDate(FHIRUtil.mergeElementPath(path, FHIR_EXTRA_FIELDS.TIME_DATE), prefix, implicitRanges)
      else
      //If it is year, year-month, or date query
      //Query over the sub date field
        Filters.or(getQueryForDate(FHIRUtil.mergeElementPath(path, FHIR_EXTRA_FIELDS.TIME_DATE), prefix, implicitRanges), getQueryForPeriodRange(rangePaths, prefix, implicitRanges))
    }
  }

  /**
   * Handle prefixes for period parameters. For further information
   * about the prefixes with range values please refer to prefix table's
   * third column in page; https://www.hl7.org/fhir/search.html#prefix
   *
   * @param path     absolute path of the parameter
   * @param value    value of the parameter
   * @param prefix   prefix of the parameter
   * @param isTiming determines if the field is timing
   * @return BsonDocument for the query
   */
  def getQueryForPeriod(path: String, value: String, prefix: String, isTiming: Boolean): Bson = {
    // Generate period fields
    val periodPath = if (isTiming) FHIRUtil.mergeElementPath(path, s"${FHIR_COMMON_FIELDS.REPEAT}.${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}") else path
    val periodRanges = (
      FHIRUtil.mergeElementPath(periodPath, s"${FHIR_COMMON_FIELDS.START}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
      FHIRUtil.mergeElementPath(periodPath, s"${FHIR_COMMON_FIELDS.END}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}")
    )
    // Populate implicit date ranges(e.g. 2010 represents the range 2010-01-01/2010-12-31)
    //val implicitDate = DateTimeUtil.populateImplicitDateRanges(value)
    // Populate implicit date time ranges(i.e. same process with the time ranges)
    val implicitDateTime = DateTimeUtil.populateImplicitDateTimeRanges(value)
    // Generate queries for both date and date time ranges
    getQueryForPeriodRange(periodRanges, prefix, implicitDateTime)
    //or(periodQueryBuilder(periodRanges, prefix, implicitDate), periodQueryBuilder(periodRanges, prefix, implicitDateTime))
  }

  /**
   * Special processing for Timing.event; all elements should satisfy the query
   *
   * @param path   absolute path of the parameter
   * @param value  value of the parameter
   * @param prefix prefix of the parameter
   * @return
   */
  def getQueryForTiming(path: String, value: String, prefix: String): Bson = {
    // Populate Implicit ranges(e.g. 2010-10-10 represents the range 2010-10-10T00:00Z/2010-10-10T23:59ZZ)
    val implicitRanges = DateTimeUtil.populateImplicitDateTimeRanges(value)
    // Convert implicit range to dateTime objects(inputted values have already been converted to dataTime format)
    var (floor, ceil) = (OnFhirBsonTransformer.dateToISODate(implicitRanges._1), OnFhirBsonTransformer.dateToISODate(implicitRanges._2))

    val subpath = if (value.contains("T")) FHIR_EXTRA_FIELDS.TIME_TIMESTAMP else FHIR_EXTRA_FIELDS.TIME_DATE
    val oppositeQuery = prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.or(Filters.lt(subpath, floor), Filters.gt(subpath, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.or(Filters.lt(subpath, floor), Filters.and(Filters.gte(subpath, floor), Filters.lt(subpath, ceil)), Filters.equal(path, floor))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.or(Filters.gt(subpath, ceil), Filters.and(Filters.gte(subpath, floor), Filters.lt(subpath, ceil)), Filters.equal(subpath, floor))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.lt(subpath, floor)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.gt(subpath, ceil)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(Filters.lt(subpath, floor), Filters.gt(subpath, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => Filters.or(Filters.lt(subpath, ceil), Filters.equal(subpath, ceil))
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => Filters.or(Filters.gt(subpath, floor), Filters.equal(subpath, floor))
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        if (ceil == floor)
          Filters.or(Filters.lt(subpath, floor), Filters.gt(subpath, ceil))
        else {
          val delta: Long = ((ceil.asInstanceOf[BsonDateTime].getValue - floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
          ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
          floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
          Filters.or(Filters.lt(subpath, floor), Filters.gt(subpath, ceil))
        }
    }

    val (fieldStart, fieldEnd) = (FHIR_EXTRA_FIELDS.TIME_RANGE_START, FHIR_EXTRA_FIELDS.TIME_RANGE_END)
    val oppositeQuery2 = prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.or(Filters.lt(fieldStart, floor), Filters.gt(fieldEnd, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.lte(fieldEnd, ceil)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.gte(fieldStart, floor)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.or(Filters.lte(fieldEnd, ceil), Filters.lt(fieldStart, floor), Filters.gt(fieldEnd, ceil))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.or(Filters.gte(fieldStart, floor), Filters.lt(fieldStart, floor), Filters.gt(fieldEnd, ceil))
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(Filters.lt(fieldStart, floor), Filters.gt(fieldEnd, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => Filters.lte(fieldStart, ceil)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => Filters.gte(fieldEnd, floor)
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        if (ceil == floor)
          Filters.or(Filters.lt(fieldStart, floor), Filters.gt(fieldEnd, ceil))
        else {
          val delta: Long = ((ceil.asInstanceOf[BsonDateTime].getValue - floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
          ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
          floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
          Filters.or(Filters.lt(fieldStart, floor), Filters.gt(fieldEnd, ceil))
        }
    }

    Filters.and(
      Filters.exists(FHIRUtil.mergeElementPath(path, "event")),
      if (prefix == FHIR_PREFIXES_MODIFIERS.NOT_EQUAL)
        Filters.elemMatch(FHIRUtil.mergeElementPath(path, "event"), Filters.or(oppositeQuery, oppositeQuery2))
      else
        Filters.nor(Filters.elemMatch(FHIRUtil.mergeElementPath(path, "event"), Filters.or(oppositeQuery, oppositeQuery2)))
    )
  }

  /**
   * Query builders for dateTime type searches e.g. ge2012-10-15T10:00:00Z
   *
   * @param path       path to the target value
   * @param prefix     prefix of the date
   * @param valueRange value of lower and upper boundaries
   * @return BsonDocument for the target query
   */
  private def getQueryForDateTime(path: String, prefix: String, valueRange: (String, String)): Bson = {
    // Convert implicit range to dateTime objects(inputted values have already been converted to dataTime format)
    var (floor, ceil) = (OnFhirBsonTransformer.dateToISODate(valueRange._1), OnFhirBsonTransformer.dateToISODate(valueRange._2))
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.or(Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil)), Filters.equal(path, floor))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.gt(path, ceil)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.lt(path, floor)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.or(getQueryForDateTime(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), getQueryForDateTime(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.or(getQueryForDateTime(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), getQueryForDateTime(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(Filters.lt(path, floor), Filters.gt(path, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => Filters.gt(path, ceil)
      //or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => Filters.lt(path, floor)
      //or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        if (ceil == floor)
          Filters.or(Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil)), Filters.equal(path, floor))
        else {
          val delta: Long = ((ceil.asInstanceOf[BsonDateTime].getValue - floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
          ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
          floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
          Filters.or(Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil)), Filters.equal(path, floor))
        }
    }
  }

  /**
   * Query builders for period type searches
   *
   * @param path       path to the lower and upper boundaries
   * @param prefix     prefix for comparison
   * @param valueRange value of lower and upper boundaries
   * @return BsonDocument for the target query
   */
  private def getQueryForPeriodRange(path: (String, String), prefix: String, valueRange: (String, String)): Bson = {
    val isoDate: (BsonValue, BsonValue) = (OnFhirBsonTransformer.dateToISODate(valueRange._1), OnFhirBsonTransformer.dateToISODate(valueRange._2))

    // Initiliaze start and end fields of the ranges
    val (fieldStart, fieldEnd) = path
    // Implicit date range
    var (floor, ceil) = isoDate
    // BsonDocuments that represent the nonexistence of boundary values
    val fieldEndNotExist = Filters.and(Filters.exists(fieldStart, exists = true), Filters.exists(fieldEnd, exists = false))
    val fieldStartNotExist = Filters.and(Filters.exists(fieldStart, exists = false), Filters.exists(fieldEnd, exists = true))

    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.and(Filters.gte(fieldStart, floor), Filters.lte(fieldEnd, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.or(Filters.gt(fieldEnd, ceil), fieldEndNotExist)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.or(Filters.lt(fieldStart, floor), fieldStartNotExist)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.or(getQueryForPeriodRange(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), getQueryForPeriodRange(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.or(getQueryForPeriodRange(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), getQueryForPeriodRange(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(getQueryForPeriodRange(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange), getQueryForPeriodRange(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => Filters.gt(fieldStart, ceil)
      //or(periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => Filters.lt(fieldEnd, floor)
      //or(periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        if (ceil == floor)
          Filters.and(Filters.gte(fieldStart, floor), Filters.lte(fieldEnd, ceil))
        else {
          val delta: Long = ((ceil.asInstanceOf[BsonDateTime].getValue - floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
          ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
          floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
          Filters.and(Filters.gte(fieldStart, floor), Filters.lte(fieldEnd, ceil))
        }
    }
  }


  /**
   * Query builders for date type searches e.g. ge2012-10-15  or eq2012-05
   *
   * @param path       path to the target value
   * @param prefix     prefix of the date
   * @param valueRange value of lower and upper boundaries
   * @return BsonDocument for the target query
   */
  private def getQueryForDate(path: String, prefix: String, valueRange: (String, String)): Bson = {
    // Convert implicit range to dateTime objects(inputted values have already been converted to dataTime format)
    var (floor, ceil) = (OnFhirBsonTransformer.dateToISODate(valueRange._1), OnFhirBsonTransformer.dateToISODate(valueRange._2))
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.or(Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil)), Filters.equal(path, floor))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.gt(path, ceil)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.lt(path, floor)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.gte(path, floor)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.lte(path, ceil)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(Filters.lt(path, floor), Filters.gt(path, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => Filters.gt(path, ceil)
      //or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => Filters.lt(path, floor)
      //or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        if (ceil == floor)
          Filters.or(Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil)), Filters.equal(path, floor))
        else {
          val delta: Long = ((ceil.asInstanceOf[BsonDateTime].getValue - floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
          ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
          floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
          Filters.or(Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil)), Filters.equal(path, floor))
        }
    }
  }

}

package io.onfhir.db

import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_DATA_TYPES, FHIR_PREFIXES_MODIFIERS}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.InternalServerException
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

/**
 *
 */
object NumberQueryBuilder extends IFhirQueryBuilder {

  /**
   * Search based on numerical values and range
   *
   * @param prefixAndValues  Supplied prefix and values
   * @param path       Path to the target element to be queried
   * @param targetType FHIR Type of the target element
   * @return respective BsonDocument for target query
   */
  def getQuery(prefixAndValues: Seq[(String, String)],  path:String, targetType:String):Bson = {
    orQueries(
      prefixAndValues.map {
        case (prefix, value) =>
          targetType match {
            case FHIR_DATA_TYPES.RANGE =>
              //Split the parts
              val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
              //Main query on Range object
              val mainQuery = getQueryForRange(queryPath.getOrElse(""), value, prefix)
              //If an array exist, use elemMatch otherwise return the query
              getFinalQuery(elemMatchPath, mainQuery)
            //Ad these are simple searches we don't need to handle arrays with elemMatch
            case FHIR_DATA_TYPES.INTEGER => getQueryForInteger(FHIRUtil.normalizeElementPath(path), value, prefix)
            case FHIR_DATA_TYPES.DECIMAL => getQueryForDecimal(FHIRUtil.normalizeElementPath(path), value, prefix)
            case other => throw new InternalServerException(s"Unknown target element type $other !!!")
          }
      }
    )
  }

  /**
   * Handles prefixes for integer values
   *
   * @param path   absolute path of the parameter
   * @param value  value of the parameter
   * @param prefix prefix of the parameter
   * @return BsonDocument for the query
   */
  def getQueryForInteger(path: String, value: String, prefix: String): Bson = {
    //If there is non-zero digits after a decimal point, there cannot be any matches
    if (value.toDouble != value.toDouble.toLong * 1.0)
      Filters.equal(path, 0.05)
    else {
      //If the value is given in exponential form, we use precision
      if (value.contains("e") || value.contains("E")) {
        val precision = FHIRUtil.calculatePrecisionDelta(value)
        // Generated function values for comparison
        val floor = value.toDouble - precision
        val ceil = value.toDouble + precision

        prefix match {
          case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil))
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.gt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.lt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.gte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.lte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(Filters.lt(path, floor), Filters.gte(path, ceil))
          case FHIR_PREFIXES_MODIFIERS.APPROXIMATE => Filters.and(Filters.gte(path, value.toDouble * 0.9), Filters.lte(path, value.toDouble * 1.1))
          case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER | FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => throw new IllegalArgumentException("Prefixes sa and eb can not be used with integer values.")
        }
      } else { //Otherwise we need extact integer match
        // Prefix matching and query filter generation
        prefix match {
          case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.equal(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.gt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.lt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.gte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.lte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.not(Filters.equal(path, value.toLong))
          case FHIR_PREFIXES_MODIFIERS.APPROXIMATE => Filters.and(Filters.gte(path, value.toDouble * 0.9), Filters.lte(path, value.toDouble * 1.1))
          case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER | FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => throw new IllegalArgumentException("Prefixes sa and eb can not be used with integer values.")
        }
      }
    }
  }

  /**
   * Handles prefixes for decimal values
   *
   * @param path   absolute path of the parameter
   * @param prefix prefix of the parameter
   * @return BsonDocument for the query
   */
  def getQueryForDecimal(path: String, value: String, prefix: String): Bson = {
    // Calculation of precision to generate implicit ranges
    val precision = FHIRUtil.calculatePrecisionDelta(value)
    //if(!value.contains('.')) 0.5 else pow(0.1, value.length - (value.indexOf(".") + 1)) * 0.5
    // Generated function values for comparison
    val floor = value.toDouble - precision
    val ceil = value.toDouble + precision
    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.and(Filters.gte(path, floor), Filters.lt(path, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.gt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.lt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.gte(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.lte(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(Filters.lt(path, floor), Filters.gte(path, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => Filters.gt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => Filters.lt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        val approximateLow = getQueryForDecimal(path, (value.toDouble * 0.9).toString, FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL)
        val approximateHigh = getQueryForDecimal(path, (value.toDouble * 1.1).toString, FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL)
        Filters.and(approximateLow, approximateHigh)
    }
  }

  /**
   * Handles prefixes for range type
   *
   * @param path   absolute path of the parameter
   * @param value  value of the parameter
   * @param prefix prefix of the parameter
   * @return BsonDocument for the query
   */
  def getQueryForRange(path: String, value: String, prefix: String, isSampleData: Boolean = false): Bson = {
    // Calculation of precision to generate implicit ranges
    val precision = FHIRUtil.calculatePrecisionDelta(value)
    // Paths to the range structure's high and low values
    val pathLow = if (isSampleData) FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.LOWER_LIMIT) else FHIRUtil.mergeElementPath(path, s"${FHIR_COMMON_FIELDS.LOW}.${FHIR_COMMON_FIELDS.VALUE}")
    val pathHigh = if (isSampleData) FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.UPPER_LIMIT) else FHIRUtil.mergeElementPath(path, s"${FHIR_COMMON_FIELDS.HIGH}.${FHIR_COMMON_FIELDS.VALUE}")
    // Implicit input value ranges
    val floor = value.toDouble - precision
    val ceil = value.toDouble + precision
    // BsonDocuments to represent nonexistence of high and low values
    val fieldHighNotExist = Filters.and(Filters.exists(pathLow, exists = true), Filters.exists(pathHigh, exists = false))
    val fieldLowNotExist = Filters.and(Filters.exists(pathLow, exists = false), Filters.exists(pathHigh, exists = true))
    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => Filters.and(Filters.gte(pathLow, floor), Filters.lt(pathHigh, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => Filters.or(Filters.gt(pathHigh, value.toDouble), fieldHighNotExist)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => Filters.or(Filters.lt(pathLow, value.toDouble), fieldLowNotExist)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => Filters.or(Filters.gte(pathHigh, value.toDouble), fieldHighNotExist)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => Filters.or(Filters.lte(pathLow, value.toDouble), fieldLowNotExist)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => Filters.or(Filters.lt(pathLow, floor), Filters.gte(pathHigh, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => Filters.gt(pathLow, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => Filters.lt(pathHigh, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        val approximateLow = getQueryForDecimal(FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.LOW), (value.toDouble * 0.9).toString, FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL)
        val approximateHigh = getQueryForDecimal(FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.HIGH), (value.toDouble * 1.1).toString, FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL)
        Filters.and(approximateLow, approximateHigh)
    }
  }
}

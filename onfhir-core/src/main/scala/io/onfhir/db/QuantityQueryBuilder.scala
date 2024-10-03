package io.onfhir.db

import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_DATA_TYPES}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.InternalServerException
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

object QuantityQueryBuilder extends IFhirQueryBuilder {

  /**
   * A quantity parameter searches on the Quantity data type. The syntax for the
   * value follows the form:
   *
   * [parameter]=[prefix][number]|[system]|[code] matches a quantity with the given unit
   *
   * @param prefixAndValues  Supplied prefix and values
   * @param path             Path for the element
   * @param targetType       Type of target element
   * @return
   */
  def getQuery(prefixAndValues: Seq[(String, String)], path: String, targetType: String): Bson = {
    orQueries(
    prefixAndValues
      .map {
        case (prefix, parameterValue) =>
          //Parse the given value
          val (value, system, code) = FHIRUtil.parseQuantityValue(parameterValue)

          //Find out the elemMatch and query parts of the path
          val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)

          //Try to construct main query
          val mainQuery = targetType match {
            case FHIR_DATA_TYPES.QUANTITY |
                 FHIR_DATA_TYPES.SIMPLE_QUANTITY |
                 FHIR_DATA_TYPES.MONEY_QUANTITY |
                 FHIR_DATA_TYPES.AGE |
                 FHIR_DATA_TYPES.DISTANCE |
                 FHIR_DATA_TYPES.COUNT |
                 FHIR_DATA_TYPES.DURATION =>
              //Query on the quentity
              val valueQuery = NumberQueryBuilder.getQueryForDecimal(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.VALUE), value, prefix)
              //Also merge it with query on system and code
              val sysCodeQuery = getQueryForUnitSystemCode(system, code, queryPath, FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT)
              sysCodeQuery
                .map(sq => Filters.and(valueQuery, sq))
                .getOrElse(valueQuery)

            case FHIR_DATA_TYPES.MONEY =>
              //Query on the quatity
              val valueQuery = NumberQueryBuilder.getQueryForDecimal(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.VALUE), value, prefix)
              //Also merge it with query on currency code
              val sysCodeQuery = code.map(c => Filters.eq(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.CURRENCY), c))
              sysCodeQuery.map(sq => Filters.and(valueQuery, sq)).getOrElse(valueQuery)

            //Handle range
            case FHIR_DATA_TYPES.RANGE =>
              //Query on range values
              val valueQuery = NumberQueryBuilder.getQueryForRange(queryPath.getOrElse(""), value, prefix)
              val lowPath = FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.LOW)
              val highPath = FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.HIGH)

              val lq =
                getQueryForUnitSystemCode(system, code, Some(lowPath), FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT)
                  .map(sq => Filters.or(Filters.exists(lowPath, exists = false), sq))
              val hq =
                getQueryForUnitSystemCode(system, code, Some(highPath), FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT)
                  .map(sq => Filters.or(Filters.exists(highPath, exists = false), sq))
              //Merge all (both lq and hq should be SOME or NONE
              lq.map(Filters.and(_, hq.get, valueQuery)).getOrElse(valueQuery)

            case FHIR_DATA_TYPES.SAMPLED_DATA =>
              //For SampleData, we should check for lowerLimit and upperLimit like a range query
              val valueQuery = NumberQueryBuilder.getQueryForRange(queryPath.getOrElse(""), value, prefix, isSampleData = true)
              val sysCodeQuery =
                getQueryForUnitSystemCode(system, code, queryPath,
                systemPath = s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.SYSTEM}",
                codePath = s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.CODE}",
                unitPath = s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.UNIT}")
              sysCodeQuery
                .map(sq => Filters.and(valueQuery, sq))
                .getOrElse(valueQuery)
          }
          //If an array exist, use elemMatch otherwise return the query
          getFinalQuery(elemMatchPath, mainQuery)
      }
    )
  }

  /**
   * Merge the query ont the Quantity value with system and code restrictions
   *
   * @param system     Expected system
   * @param code       Expected code/unit
   * @param queryPath  Main path to the FHIR quantity element
   * @param systemPath system field path within the element
   * @param codePath   code field path within the element
   * @param unitPath   unit field path within the element
   * @return
   */
  private def getQueryForUnitSystemCode(system: Option[String], code: Option[String], queryPath: Option[String], systemPath: String, codePath: String, unitPath: String): Option[Bson] = {
    (system, code) match {
      //Only query on value
      case (None, None) =>
        None
      //Query on value + unit
      case (None, Some(c)) =>
        Some(
          Filters.or(
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, codePath), c),
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, unitPath), c)
          )
        )
      //query on value + system + code
      case (Some(s), Some(c)) =>
        Some(
          Filters.and(
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, codePath), c),
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, systemPath), s)
          )
        )
      case _ => throw new InternalServerException("Invalid state!")
    }
  }

}

package io.onfhir.db

import io.onfhir.api.{FHIR_DATA_TYPES, FHIR_PREFIXES_MODIFIERS, STRING_TYPE_SEARCH_SUBPATHS}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.InvalidParameterException
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.{elemMatch, equal, or, regex}

object StringQueryBuilder extends IFhirQueryBuilder {

  /**
   * String parameter serves as the input for a case- and accent-insensitive search against sequences of
   * characters, :exact modifier is used for case and accent sensitive search, :contains modifier is
   * used for case and accent insensitive partial matching search.
   *
   * @param value      Query value of the string parameter
   * @param modifier   Prefix to be handled
   * @param path       Path to the target element to be queried
   * @param targetType FHIR Type of the target element
   * @return
   */
  def getQuery(values:Seq[String], modifier:String,  path:String, targetType:String):Bson = {
    targetType match {
      case FHIR_DATA_TYPES.STRING =>
        orQueries(values.map(value => getQueryOnTargetStringElement(FHIRUtil.normalizeElementPath(path), value, modifier)))

      // Only we support these, Normally, FHIR states that string search can be done on any complex type  by searching text fields of that complex type TODO
      case FHIR_DATA_TYPES.HUMAN_NAME | FHIR_DATA_TYPES.ADDRESS =>
        orQueries(values.map(value => getQueryOnComplexTarget(path, value, modifier, targetType)))

      case other =>
        throw new InvalidParameterException(s"String type search is not supported on target type $other!")
    }
  }

  /**
   *
   * @param path
   * @param value
   * @param modifier
   * @param targetType
   * @return
   */
  private def getQueryOnComplexTarget(path: String, value: String, modifier: String, targetType:String):Bson = {
    //Split the parts
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
    val queries =
      STRING_TYPE_SEARCH_SUBPATHS(targetType)
        .map(subpath => FHIRUtil.mergeElementPath(queryPath, subpath))
        .map(p => getQueryOnTargetStringElement(p, value, modifier))
    val mainQuery = or(queries: _*)
    //If an array exist, use elemMatch otherwise return the query
    elemMatchPath match {
      case None => mainQuery
      case Some(emp) => elemMatch(emp, mainQuery)
    }
  }

  /**
   * Construct query for string search on string type element
   * @param path      Normalized path to the element
   * @param value
   * @param modifier
   * @return
   */
  def getQueryOnTargetStringElement(path: String, value: String, modifier: String): Bson = {
    //TODO Ignorance of accents or other diacritical marks, punctuation and non-significant whitespace is not supported yet
    // Escape characters for to have valid regular expression
    val regularExpressionValue = FHIRUtil.escapeCharacters(value)
    // Generator for regular expression queries(Only regex fields empty)
    val caseInsensitiveStringRegex = regex(path, _: String, "i")
    modifier match {
      case FHIR_PREFIXES_MODIFIERS.EXACT =>
        // Exact match provided with $eq mongo operator
        Filters.eq(path, value)
      case FHIR_PREFIXES_MODIFIERS.CONTAINS =>
        // Partial match
        caseInsensitiveStringRegex(".*" + regularExpressionValue + ".*")
      //No modifier
      case "" =>
        // Case insensitive, partial matches at the beginning (prefix search)
        caseInsensitiveStringRegex("^" + regularExpressionValue)
      case other =>
        throw new InvalidParameterException(s"Modifier $other is not supported for FHIR string queries!")
    }
  }
}

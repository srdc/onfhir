package io.onfhir.api.parsers

import akka.http.scaladsl.model.Uri
import io.onfhir.api.model.Parameter
import io.onfhir.config.FhirServerConfig

/**
 * A utility class to parse/validate a FHIR query statement against the fhir configuration
 * @param fhirServerConfig The FHIR server configuration
 */
class FhirQueryParser(fhirServerConfig: FhirServerConfig) {
  private val searchParamParser = new FHIRSearchParameterValueParser(fhirServerConfig)

  /**
   * Parse the given x-fhir-query statement without any FHIR Path referencing
   * e.g. Patient?gender=male
   *
   * @param query FHIR Query statement
   * @return
   */
  def parseQuery(query: String): (String, List[Parameter]) = {
    val uri = Uri.apply(query)
    val queryParams = uri.query().toMultiMap
    val rtype =
      uri.path.length match {
        case 1 if uri.path.head != "" => uri.path.toString()
        case _ =>
          throw new IllegalArgumentException("Invalid FHIR query, FHIR resource type is missing")
      }

    rtype -> searchParamParser.parseSearchParameters(rtype, queryParams)
  }

  /**
   * Parse the given FHIR query for the specified FHIR Resource type
   *
   * @param rtype FHIR resource type
   * @param query FHIR Query statement
   *              e.g. ?code=...&value=...
   * @return
   */
  private def parseQuery(rtype: String, query: String): List[Parameter] = {
    val queryParams =
      Uri
        .apply(query)
        .query()
        .toMultiMap

    searchParamParser.parseSearchParameters(rtype, queryParams)
  }
}

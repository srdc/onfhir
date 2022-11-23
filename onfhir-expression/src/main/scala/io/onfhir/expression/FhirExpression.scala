package io.onfhir.expression

import org.json4s.JValue

/**
 * Represents FHIR Expression data type
 *
 * @param name       Name of the expression
 * @param language   Language of expression e.g. text/fhirpath , application/x-fhir-query
 * @param expression Expression itself
 * @param reference  Uri to the expression content (template languages or FHIR Mapping expression may use this; URL of structure map)
 * @param value      JSON value as content (fhir-template, or direct value assignment)
 */
case class FhirExpression(name: String,
                          language: String,
                          expression: Option[String],
                          reference: Option[String],
                          value: Option[JValue])


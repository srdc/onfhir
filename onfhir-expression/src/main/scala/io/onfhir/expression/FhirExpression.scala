package io.onfhir.expression

import org.json4s.JValue

/**
 * Represents FHIR Expression data type
 *
 * @param name       Name of the expression
 * @param language   Language of expression e.g. text/fhirpath , application/x-fhir-query
 * @param description Description of the expression
 * @param expression Expression itself given in the format of expression language
 * @param reference  Uri to the expression content (template languages or FHIR Mapping expression may use this; URL of structure map)
 * @param value      If expression is complex content, JSON serialization as content (fhir-template, or direct value assignment)
 */
case class FhirExpression(name: String,
                          language: String,
                          description:Option[String] = None,
                          expression: Option[String] = None,
                          reference: Option[String] = None,
                          value: Option[JValue] = None)


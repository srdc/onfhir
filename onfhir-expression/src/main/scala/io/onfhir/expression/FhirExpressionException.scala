package io.onfhir.expression

/**
 * Exception while evaluate Fhir expression
 * @param msg Message
 * @param t   Inner exception
 */
case class FhirExpressionException(msg:String, t:Option[Throwable] = None) extends Exception(msg)

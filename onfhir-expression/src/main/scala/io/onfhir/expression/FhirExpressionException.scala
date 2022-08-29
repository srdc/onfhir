package io.onfhir.expression

/**
 * Exception while evaluate Fhir expression
 * @param msg         Message
 * @param expression  Problematic expression part
 * @param t   Inner exception
 */
case class FhirExpressionException(
                                    msg:String,
                                    expression:Option[String] = None,
                                    t:Option[Throwable] = None
                                  ) extends Exception(msg)

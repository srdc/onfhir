package io.onfhir.api.model

/**
  * Outcome Issue object to represent OutcomeIssue in FHIR Operation Outcome
  * @param severity     Severity of issue
  * @param code         Code of the issue
  * @param details      Details of the issue
  * @param diagnostics  Diagnostics
  * @param expression     Location of the issue
  */
case class OutcomeIssue(severity:String, code:String, details:Option[String], diagnostics:Option[String], expression:Seq[String]) {
  def isError:Boolean = severity == FHIRResponse.SEVERITY_CODES.ERROR ||  severity == FHIRResponse.SEVERITY_CODES.FATAL
}

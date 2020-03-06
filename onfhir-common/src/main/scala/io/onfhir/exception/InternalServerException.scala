package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue

class InternalServerException(reason:String, issues:Seq[OutcomeIssue] = Nil) extends Exception(reason:String) {
  val outcomeIssues = issues
}

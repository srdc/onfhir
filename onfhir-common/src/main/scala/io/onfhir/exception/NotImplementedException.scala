package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue

/**
  * Created by abdulkadir on 11.2.2016.
  */
class NotImplementedException(issues:Seq[OutcomeIssue]) extends  Exception() {
  val outcomeIssues = issues
}

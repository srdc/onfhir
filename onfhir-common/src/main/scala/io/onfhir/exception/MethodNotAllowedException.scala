package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue

/**
  * Created by tuncay on 12/23/2016.
  */
class MethodNotAllowedException (issues:Seq[OutcomeIssue]) extends Exception {
  val outcomeIssues = issues
}

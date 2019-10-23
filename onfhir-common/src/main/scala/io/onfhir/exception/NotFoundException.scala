package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue

/**
  * Created by tuncay on 12/15/2016.
  *
  */
class NotFoundException (issues:Seq[OutcomeIssue]) extends Exception {
  val outcomeIssues = issues
}

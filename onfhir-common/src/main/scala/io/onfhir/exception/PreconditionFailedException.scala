package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue

/**
  * Created by tuncay on 12/16/2016.
  */
class PreconditionFailedException (issues:Seq[OutcomeIssue]) extends Exception {
  val outcomeIssues = issues
}


package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue

/**
  * Created by tuncay on 12/15/2016.
  * If FHIR request is problematic in general
  * related with HTTP 400 Bad Request
  */
class BadRequestException(issues:Seq[OutcomeIssue]) extends Exception {
  val outcomeIssues = issues
}

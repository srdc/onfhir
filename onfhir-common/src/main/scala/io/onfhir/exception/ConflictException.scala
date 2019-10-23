package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue


class ConflictException(issue:OutcomeIssue) extends Exception() {
  val outcomeIssue = issue
}

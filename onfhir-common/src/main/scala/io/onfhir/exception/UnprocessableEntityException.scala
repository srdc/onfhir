package io.onfhir.exception

import io.onfhir.api.model.OutcomeIssue

/**
  * For exceptions related with the content
  * and HTTP 422 Unprocessable Entity (the proposed resource violated applicable FHIR profiles or server business rules)
 *
  * @param issues
  */
class UnprocessableEntityException(issues:Seq[OutcomeIssue]) extends Exception {
  val outcomeIssues = issues
}

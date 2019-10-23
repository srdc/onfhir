package io.onfhir.api.validation

import io.onfhir.api.Resource
import io.onfhir.api.model.OutcomeIssue

import scala.concurrent.Future

/**
  *
  */
trait IFhirResourceValidator {

  /**
    * Validates resource based on business rules
    * Return OutcomeIssue if successful or throw exception
    * @param content Parsed JSON content of the resource
    * @param silent If true, does not throw exception but return issues
    */
  def validateResource(resource: Resource, silent:Boolean = false): Future[Seq[OutcomeIssue]]

}

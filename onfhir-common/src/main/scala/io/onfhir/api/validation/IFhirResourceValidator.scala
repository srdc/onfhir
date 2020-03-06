package io.onfhir.api.validation

import io.onfhir.api.Resource
import io.onfhir.api.model.OutcomeIssue

import scala.concurrent.Future

/**
  *
  */
trait IFhirResourceValidator {

  /**
   * Validates resource based on business rules (get the base profile from CapabilityStatement)
   * Return OutcomeIssue if successful or throw exception
   * @param resource   Parsed JSON content of the resource
   * @param rtype      Resource type expected
   * @param silent     If true, does not throw exception but return issues
   */
  def validateResource(resource: Resource, rtype:String, silent:Boolean=false): Future[Seq[OutcomeIssue]]

  /**
    * Validates resource based on business rules
    * Return OutcomeIssue if successful or throw exception
    * @param resource   Parsed JSON content of the resource
    * @param rtype      Resource type expected
    * @param profile    Profile that resource is expected to conform (if not exist just validated for base resource type)
    * @param silent     If true, does not throw exception but return issues
    */
  def validateResourceAgainstProfile(resource: Resource, rtype:String, profile:Option[String], silent:Boolean = false): Future[Seq[OutcomeIssue]]

}

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
   * @param parentPath Parent path for the resource (if resource is within a Bundle or contained)
   * @param bundle     Bundle content if this resource is given within a bundle and the resource full url in the bundle
   * @param silent     If true, does not throw exception but return issues
   */
  def validateResource(resource: Resource, rtype:String, parentPath:Option[String] = None,  bundle:Option[(Option[String],Resource)] = None, silent:Boolean=false): Future[Seq[OutcomeIssue]]

  /**
    * Validates resource based on business rules
    * Return OutcomeIssue if successful or throw exception
    * @param resource   Parsed JSON content of the resource
    * @param rtype      Resource type expected
    * @param profile    Profile that resource is expected to conform (if not exist just validated for base resource type)
    * @param parentPath Parent path for the resource (if resource is within a Bundle or contained)
    * @param bundle     Bundle content if this resource is given within a bundle and the resource full url in the bundle
    * @param silent     If true, does not throw exception but return issues
    */
  def validateResourceAgainstProfile(resource: Resource, rtype:String, profile:Option[String], parentPath:Option[String] = None,  bundle:Option[(Option[String],Resource)] = None, silent:Boolean = false): Future[Seq[OutcomeIssue]]

}

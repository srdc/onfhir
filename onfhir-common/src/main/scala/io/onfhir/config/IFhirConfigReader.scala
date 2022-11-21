package io.onfhir.config

import io.onfhir.api.Resource

/**
 * Interface for implementations that reads/retrieves base FHIR foundation resources from different ways
 * e.g. reading from file system, access through FHIR API, from onfhir database
 */
trait IFhirConfigReader {

  /**
   * Read and parse FHIR Bundle zip file (JSON version) published by FHIR for specific versions and
   * return specific FHIR foundation resources e.g. StructureDefinitions defined in the standard
   * e.g. https://www.hl7.org/fhir/definitions.json.zip
   *
   * @param fileName           Name of the file in the FHIR standard definitions zip e.g. profiles-resources.json --> Base FHIR resource definitions (StructureDefinitions)
   * @param resourceTypeFilter Types of the resources to extract from the Bundle
   * @return
   */
  def readStandardBundleFile(fileName:String, resourceTypeFilter:Set[String]) :Seq[Resource]
  /**
   * Read FHIR infrastructure resources
   * @param rtype Resource type of the foundation resource e.g. StructureDefinition, ValueSet, etc
   * @return
   */
  def getInfrastructureResources(rtype:String):Seq[Resource]

  /**
   * Read the FHIR Capability Statement
   * @return
   */
  def readCapabilityStatement():Resource
}

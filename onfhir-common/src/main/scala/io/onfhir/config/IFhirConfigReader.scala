package io.onfhir.config

import io.onfhir.api.{FHIR_COMMON_FIELDS, Resource}
import io.onfhir.api.util.FHIRUtil

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
   * Read All FHIR infrastructure resources
   * @param rtype Resource type of the foundation resource e.g. StructureDefinition, ValueSet, etc
   * @return
   */
  def getInfrastructureResources(rtype:String):Seq[Resource]

  /**
   * Read Infrastructure resources with given canonical urls
   * @param rtype           Resource type e.g. StructureDefinition, ValueSet, etc
   * @param canonicalUrls   URLs to filter
   *                        e.g. http://fhir.acme.com/Questionnaire/example
   *                        e.g. http://fhir.acme.com/Questionnaire/example|1.0
   * @return
   */
  def getInfrastructureResources(rtype:String, canonicalUrls:Set[String]):Seq[Resource] = {
    val parsedCanonicals = canonicalUrls.map(curl => FHIRUtil.parseCanonicalValue(curl))
    getInfrastructureResources(rtype)
      .filter(r =>
        parsedCanonicals.exists {
          case (url, version) =>
            FHIRUtil.extractValueOption[String](r, FHIR_COMMON_FIELDS.URL).contains(url) &&
              version.forall(v => FHIRUtil.extractValueOption[String](r, FHIR_COMMON_FIELDS.VERSION).contains(v))
        }
      )
  }

  /**
   * Read the FHIR Capability Statement
   * @return
   */
  def readCapabilityStatement():Resource
}

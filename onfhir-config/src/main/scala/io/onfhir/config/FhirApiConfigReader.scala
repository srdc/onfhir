package io.onfhir.config

import io.onfhir.api.Resource
import io.onfhir.api.client.IOnFhirClient
import io.onfhir.api.util.FHIRUtil

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import scala.language.postfixOps

/**
 * Fhir configuration reader from a target FHIR API
 * @param fhirVersion                The version of the FHIR to be used (R4 or R5). For now, this affects which default base definitions will be loaded (R4 or R5) unless fhirStandardZipFilePath is given.
 * @param fhirStandardZipFilePath    Path to the FHIR standard zip file (definitions.json.zip)
 * @param fhirClient                 FHIR client to access the FHIR API
 * @param ec                         Execution context
 */
class FhirApiConfigReader(fhirVersion: String,
                          fhirStandardZipFilePath: Option[String] = None,
                          fhirClient:IOnFhirClient)(implicit ec:ExecutionContext) extends BaseConfigReader(fhirVersion, fhirStandardZipFilePath) {
  /**
   * Read FHIR infrastructure resources from FHIR API
   *
   * @param rtype Resource type of the foundation resource e.g. StructureDefinition, ValueSet, etc
   * @return
   */
  def getInfrastructureResources(rtype: String): Seq[Resource] = {
    val bundle = Await.result(fhirClient.search(rtype).executeAndMergeBundle(), 5 seconds)
    bundle.searchResults
  }

  /**
   * Read specific infrastructure resources
   * @param rtype           Resource type e.g. StructureDefinition, ValueSet, etc
   * @param canonicalUrls   URLs to filter
   *                        e.g. http://fhir.acme.com/Questionnaire/example
   *                        e.g. http://fhir.acme.com/Questionnaire/example|1.0
   *  @return
   */
  override def getInfrastructureResources(rtype:String, canonicalUrls:Set[String]):Seq[Resource] = {
    val parsedCanonicals = canonicalUrls.map(curl => FHIRUtil.parseCanonicalValue(curl)).toMap
    val bundle = Await.result(fhirClient.search(rtype).where("url", parsedCanonicals.keySet.toSeq:_*).executeAndMergeBundle(), 5 seconds)
    if(parsedCanonicals.exists(_._2.isDefined))
      bundle.searchResults.filter(r =>
        parsedCanonicals(FHIRUtil.extractValue(r, "url"))
          .forall(v => FHIRUtil.extractValueOption(r, "version").contains(v))
      )
    else
      bundle.searchResults
  }

  /**
   * Read the FHIR Capability Statement from FHIR API e.g. GET <base-url>/metatada
   *
   * @return
   */
  def readCapabilityStatement(): Resource = {
    val capabilityStatement = Await.result(fhirClient.capabilities().executeAndReturnResource(), 5 seconds)
    capabilityStatement
  }
}

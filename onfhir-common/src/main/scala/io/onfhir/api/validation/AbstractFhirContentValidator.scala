package io.onfhir.api.validation

import io.onfhir.api.FHIR_ROOT_URL_FOR_DEFINITIONS
import io.onfhir.api.model.{FhirReference, OutcomeIssue}
import io.onfhir.config.BaseFhirConfig
import org.json4s.JsonAST.JObject

import scala.collection.mutable
import scala.concurrent.Future

/**
 * Abstract FHIR content validator
 * @param fhirConfig        Base FHIR configurations (profiles, value sets, etc.,)
 * @param profileUrl        Url of the lead profile for the validation
 * @param referenceResolver A resolver if references will be validated
 */
abstract class AbstractFhirContentValidator(
                                             val fhirConfig:BaseFhirConfig,
                                             val profileUrl:String,
                                             val referenceResolver:Option[IReferenceResolver] = None) {
  //Chain of profiles for this profile, where parents are on the right in hierarchy order e.g. MyObservation2 -> MyObservation -> Observation -> DomainResource -> Resource
  val rootProfileChain: Seq[ProfileRestrictions] = fhirConfig.findProfileChain(profileUrl)

  //FHIR reference and expected target profiles to check for existence
  val referencesToCheck = new mutable.ListBuffer[(FhirReference, Set[String])]()

  /**
   * Find resource type for a profile chain
   * @param profileChain  Profile chain for the validated resource
   * @return
   */
  def findResourceType(profileChain:Seq[ProfileRestrictions]):Option[String] = {
    profileChain.findLast(!_.isAbstract).map(_.url.split('/').last)
  }

  /**
   * Get Resource type of the profile
   * @return
   */
  def getResourceOrDataType: String =
    rootProfileChain.findLast(!_.isAbstract).get.url.replace(FHIR_ROOT_URL_FOR_DEFINITIONS + "/StructureDefinition/", "")
  /**
   * Validate a FHIR JSON content
   * @param value   JSON FHIR content (a resource or contents of complex element)
   * @return
   */
  def validateComplexContent(value: JObject): Future[Seq[OutcomeIssue]]

  /**
   * Validate FHIR JSON content according to given profile chain with extra element restrictions
   * @param profileChain                  Chain of profiles
   * @param value                         FHIR JSON content
   * @param parentPath                    Parent path for the element
   * @param resourceElementRestrictions   Extra element constraints coming from outside
   * @param forceRecognitionOfElementsEvenForAbstractChain  Normally if all profiles in validation chain are abstract,
   *                                                        we ignore unknown elements as they most probably validated
   *                                                        in other chains, by setting this param to true, we can force
   *                                                        returning issues for unrecognized elements
   * @return
   */
  def validateComplexContentAgainstProfile(profileChain: Seq[ProfileRestrictions], value: JObject, parentPath: Option[String], resourceElementRestrictions: Seq[Seq[(String, ElementRestrictions)]] = Nil, forceRecognitionOfElementsEvenForAbstractChain:Boolean = false): Future[Seq[OutcomeIssue]]
}

package io.onfhir.api.validation

import io.onfhir.api.FHIR_ROOT_URL_FOR_DEFINITIONS
import io.onfhir.api.model.{FhirReference, OutcomeIssue}
import io.onfhir.config.FhirConfig
import org.json4s.JsonAST.{JObject, JValue}

import scala.collection.mutable

abstract class AbstractFhirContentValidator(val fhirConfig:FhirConfig, val profileUrl:String, val referenceResolver:Option[IReferenceResolver]) {
  //Chain of profiles for this profile, where parents are on the right in hierarchy order e.g. MyObservation2 -> MyObservation -> Observation -> DomainResource -> Resource
  val rootProfileChain = fhirConfig.findProfileChain(profileUrl)

  //FHIR reference and expected target profiles to check for existence
  val referencesToCheck = new mutable.ListBuffer[(FhirReference, Set[String])]()


  /**
   * Find resource type for a profile chain
   * @param profileChain
   * @return
   */
  def findResourceType(profileChain:Seq[ProfileRestrictions]):Option[String] = {
    profileChain.reverse.find(!_.isAbstract).map(_.url.split('/').last)
  }

  /**
   * Get Resource type of the profile
   * @return
   */
  def getResourceOrDataType() =
    rootProfileChain.reverse.find(!_.isAbstract).get.url.replace(FHIR_ROOT_URL_FOR_DEFINITIONS + "/StructureDefinition/", "")
  /**
   * Validate a FHIR JSON content
   * @param value   JSON FHIR content (a resource or contents of complex element)
   * @return
   */
  def validateComplexContent(value: JObject): Seq[OutcomeIssue]

  /**
   * Validate FHIR JSON content according to given profile chain with extra element restrictions
   * @param profileChain                  Chain of profiles
   * @param value                         FHIR JSON content
   * @param parentPath                    Parent path for the element
   * @param resourceElementRestrictions   Extra element constraints coming from outside
   * @return
   */
  def validateComplexContentAgainstProfile(profileChain: Seq[ProfileRestrictions], value: JObject, parentPath: Option[String], resourceElementRestrictions: Seq[Seq[(String, ElementRestrictions)]] = Nil): Seq[OutcomeIssue]

  /**
   * Find profile with the given URL
   * @param profileUrl
   * @return
   */
  def findProfile(profileUrl: String):Option[ProfileRestrictions] = {
    fhirConfig.profileRestrictions.get(profileUrl)
  }
  /**
   * Find a chain of parent profiles until the base FHIR spec given the profile
   *
   * @param profileUrl Profile definition
   * @return Profiles in order of evaluation (inner profile - base profile)
   */
  def findProfileChain(profileUrl: String): Seq[ProfileRestrictions] = {
    findProfile(profileUrl) match {
      case None => Nil
      case Some(profile) => findChain(fhirConfig.profileRestrictions)(profile)
    }
  }

  /**
   * Supplementary method for profile chain finding
   *
   * @param restrictions
   * @param profile
   * @return
   */
  private def findChain(restrictions: Map[String, ProfileRestrictions])(profile: ProfileRestrictions): Seq[ProfileRestrictions] = {
    profile
      .baseUrl
      .map(burl =>
        restrictions
          .get(burl)
          .fold[Seq[ProfileRestrictions]](Seq(profile))(parent => profile +: findChain(restrictions)(parent))
      )
      .getOrElse(Seq(profile))
  }

}

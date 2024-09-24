package io.onfhir.config

import io.onfhir.api.FHIR_ROOT_URL_FOR_DEFINITIONS
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ProfileRestrictions, ValueSetRestrictions}

/** *
 * FHIR related configurations for this instance of application
 * @param version Main FHIR version e.g. R4, R5
 *                Note: This is used to parse the FHIR foundation resources accordingly
 */
class BaseFhirConfig(version:String) {
  /**
   * Numeric FHIR version supported e.g. 4.0.1
   */
  var fhirVersion: String = _

  /** FHIR Profile definitions including the base profiles (For validation) Profile Url -> Map(version -> Definition) * */
  var profileRestrictions: Map[String, Map[String, ProfileRestrictions]] = _
  /** Supported FHIR value set urls with this server (For validation) ValueSet Url -> Map(Version ->Definitions) */
  var valueSetRestrictions: Map[String, Map[String, ValueSetRestrictions]] = _

  /** Base FHIR Resource Types defined in the standard */
  var FHIR_RESOURCE_TYPES: Set[String] = _
  /** Base FHIR Complex data types defined in the standard */
  var FHIR_COMPLEX_TYPES: Set[String] = _
  /** Base FHIR primitive data types defined in the standard */
  var FHIR_PRIMITIVE_TYPES: Set[String] = _


  /**
   * Return the base FHIR standard profile restrictions for the given FHIR resource type
   * @param rtype FHIR resource type
   * @return
   */
  def getBaseProfile(rtype: String): ProfileRestrictions = {
    findProfile(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype") match {
      case Some(p) => p
      case None =>
        null
    }
  }

  /**
   * Return the base profile chain (hierarchy of profiles) for the given FHIR resource type
   * @param rtype FHIR resource type
   * @return
   */
  def getBaseProfileChain(rtype: String): Seq[ProfileRestrictions] = {
    findProfileChain(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype")
  }

  /**
   * Find profile restrictions with the given URL
   * @param profileUrl Profile URL (StructureDefinition.url)
   * @return
   */
  def findProfile(profileUrl: String, version:Option[String] = None): Option[ProfileRestrictions] = {
    FHIRUtil.getMentionedProfile(profileUrl -> version, profileRestrictions)
  }

  /**
   * Find a chain of parent profiles until the base FHIR specification profile from given url and optional version
   *
   * @param profileUrl Profile URL (StructureDefinition.url)
   * @param version    Version of definition (StructureDefinition.version)
   * @return Profiles in order of evaluation (inner profile,..., base profile)
   */
  def findProfileChain(profileUrl: String, version:Option[String] = None): Seq[ProfileRestrictions] = {
    findProfile(profileUrl, version) match {
      case None => Nil
      case Some(profile) => findChain(profile)
    }
  }

  /**
   * Find a chain of parent profiles until the base FHIR specification profile from given Canonical reference to profile
   * @param profileCanonicalRef Canonical reference
   *                            e.g. http://onfhir.io/StructureDefinition/MyProfile
   *                            e.g. http://onfhir.io/StructureDefinition/MyProfile|2.0
   * @return
   */
  def findProfileChainByCanonical(profileCanonicalRef:String):Seq[ProfileRestrictions] = {
    val (profileUrl, version) = FHIRUtil.parseCanonicalValue(profileCanonicalRef)
    findProfileChain(profileUrl, version)
  }

  /**
   * Find target resource/data type of a profile
   * @param profileUrl Profile URL (StructureDefinition.url)
   * @return
   */
  def findResourceType(profileUrl: String): Option[String] = {
    findProfileChain(profileUrl).findLast(!_.isAbstract).map(_.url.split('/').last)
  }

  /**
   * Supplementary method for profile chain finding
   *
   * @param profile  Profile itself
   * @return
   */
  private def findChain(profile: ProfileRestrictions): Seq[ProfileRestrictions] = {
    profile
      .baseUrl
      .map(burl =>
        findProfile(burl._1, burl._2)
          .map(parent => profile +: findChain(parent))
          .getOrElse(Nil)
      )
      .getOrElse(Seq(profile))
  }

  /**
   * Whether this is for older version DSTU2
   *
   * @return
   */
  def isDstu2: Boolean =
    version match {
      case "DSTU2" | "1.0.2" => true
      case _ => false
    }

}

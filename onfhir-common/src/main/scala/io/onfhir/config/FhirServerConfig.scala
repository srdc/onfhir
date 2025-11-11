package io.onfhir.config

import scala.collection.immutable.HashMap
import akka.http.scaladsl.model._
import io.onfhir.api.{FHIR_ROOT_URL_FOR_DEFINITIONS, FHIR_VERSIONING_OPTIONS}

/**
  * Central OnFhir configuration defining FHIR server capabilities
  * @param version FHIR version
  */
class FhirServerConfig(version:String) extends BaseFhirConfig(version) {

  /***
    *  Dynamic configurations for this instance of FHIR repository
    */
  /** List of supported resource types and profiles for each resource; resource-type -> Map(profile-url -> Set(versions)) */
  var supportedProfiles:Map[String, Map[String, Set[String]]] = HashMap()
  /** Rest configuration for each Resource*/
  var resourceConfigurations:Map[String, ResourceConf] = HashMap()

  /** Supported Query Parameters for each resource-profile pair ; resource-type -> Map(parameter-name -> parameter-configuration)
    * e.g. Observation -> Map(subject -> ..., code -> ...)
    **/
  var resourceQueryParameters:Map[String, Map[String, SearchParameterConf]] = HashMap()
  /** Represents the common query parameters of all resources ; parameter-name -> parameter-configuration
    * e.g. _lastUpdated -> ...
    **/
  var commonQueryParameters:Map[String, SearchParameterConf] = HashMap()

  /** Supported compartments and relations with other resources; parameters to link them;  compartment -> Map(resource-type-> Set(parameter-name))
    * e.g. Patient -> Map (Observation -> Set(subject, ....))
    **/
  var compartmentRelations:Map[String, Map[String, Set[String]]] = HashMap()
  /** Supported system level interactions e.g. history-system, search-system, transaction, batch*/
  var supportedInteractions:Set[String] = _
  /** FHIR perations supported; parsing Capability Statement and OperationDefinitions **/
  var supportedOperations:Seq[OperationConf] = _

  /** Shard Keys for FHIR resource types; resourceType -> Seq[fhir-search-parameter-name-indicating-the-shard-key] */
  var shardKeys:Map[String, Set[String]] = Map.empty[String, Set[String]]

  /***
    * Configurations specific to FHIR version
    */
  /** List of FHIR Result parameters this FHIR version support */
  var FHIR_RESULT_PARAMETERS:Seq[String] = _
  /** List of FHIR Special parameters this FHIR version support */
  var FHIR_SPECIAL_PARAMETERS:Seq[String] = _

  /** MediaType configurations for this FHIR version */
  // List of Supported FHIR JSON Media Types
  var FHIR_JSON_MEDIA_TYPES:Seq[MediaType] = _
  // List of Supported FHIR XML Media Types
  var FHIR_XML_MEDIA_TYPES:Seq[MediaType] = _
  // Json patch media type supported
  var FHIR_PATCH_MEDIA_TYPES:Seq[MediaType] = _
  //Map from _format param value to actual MediaType
  var FHIR_FORMAT_MIME_TYPE_MAP:Map[String, MediaType] = _
  //Default media type used when no match
  var FHIR_DEFAULT_MEDIA_TYPE:MediaType = _
  //Allowed media types for FHIR Binary resources if supported
  var FHIR_ALLOWED_BINARY_TYPES:Seq[MediaType.Binary] = _

  //Code system to indicate a search result is summarized
  var FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM = "http://terminology.hl7.org/CodeSystem/v3-ObservationValue" //"http://hl7.org/fhir/v3/ObservationValue"

  //List of all media types supported for requests
  def FHIR_SUPPORTED_REQUEST_MEDIA_TYPES =
    FHIR_JSON_MEDIA_TYPES ++ FHIR_XML_MEDIA_TYPES ++ FHIR_PATCH_MEDIA_TYPES.toSeq ++ Seq(MediaTypes.`text/plain`)
  // List of Media Types for Returned Results (that our FHIR repository can return)
  def FHIR_SUPPORTED_RESULT_MEDIA_TYPES =
    FHIR_JSON_MEDIA_TYPES ++ FHIR_XML_MEDIA_TYPES ++ Seq(MediaTypes.`text/html`)
  //Test plain is supported for our tests to work
  def FHIR_SUPPORTED_CONTENT_TYPE_RANGES =
    FHIR_SUPPORTED_REQUEST_MEDIA_TYPES
      .map(mediaType => ContentTypeRange.apply(MediaRange.apply(mediaType), HttpCharsetRange.apply(HttpCharsets.`UTF-8`)))
  //Calculate supported result content types
  def FHIR_SUPPORTED_RESULT_CONTENT_TYPES = FHIR_SUPPORTED_RESULT_MEDIA_TYPES.flatMap {
    case MediaTypes.`application/json` => Some(ContentTypes.`application/json`)
    case MediaTypes.`text/html` => Some(ContentTypes.`text/html(UTF-8)`)
    case oth => Some(ContentType.apply(oth, () => HttpCharsets.`UTF-8`))
  }

  /**
   * Check if the profile is supported
   *
   * @param profileUrl Profile URL (StructureDefinition.url)
   * @return
   */
  def isProfileSupported(profileUrl:String, version:Option[String] = None):Boolean = {
    supportedProfiles.flatMap(_._2).exists(profiles =>
      profiles._1 == profileUrl && version.forall(v => profiles._2.contains(v))
    )
  }

  /**
    * Try to find supported search parameter
    * @param rtype Resource type
    * @param pname Parameter name
    * @return onFhir Search parameter configuration
    */
  def findSupportedSearchParameter(rtype:String, pname:String):Option[SearchParameterConf] = {
    if(pname.startsWith("_"))
      commonQueryParameters.get(pname)
    else
      resourceQueryParameters.get(rtype).flatMap(_.get(pname))
  }

  /**
    * Get all supported search parameter configurations
    * @param rtype Resource type
    * @return Map of onFhir Search parameter configurations; parameter name --> configuration
    */
  def getSupportedParameters(rtype:String):Map[String, SearchParameterConf] = {
    commonQueryParameters ++ resourceQueryParameters.getOrElse(rtype, Map.empty)
  }

  /**
    * Get Compartment related search parameters for the given resource type
    * @param compartment Given compartment
    * @param rtype Given resource type
    * @return
    */
  def getCompartmentRelatedParameters(compartment:String, rtype:String):Seq[String] = {
    compartmentRelations.get(compartment).map(_.getOrElse(rtype, Set.empty)).reduce((s1,s2) => s1.union(s2)).toSeq
  }

  /**
    * Get compartment related element paths for the given resource type
    * @param compartment Given compartment
    * @param rtype Given resource type
    * @return
    */
  def getCompartmentRelatedPaths(compartment:String, rtype:String):Seq[String] = {
    getCompartmentRelatedParameters(compartment, rtype)
      .flatMap(p => resourceQueryParameters.getOrElse(rtype, Map.empty).get(p))
      .flatMap(p => p.extractElementPaths())
  }

  /**
   * Whether versioning is supported for the given resource type
   * @param rtype FHIR Resoure type
   * @return
   */
  def isResourceTypeVersioned(rtype:String):Boolean = {
    resourceConfigurations.get(rtype).forall(_.versioning != FHIR_VERSIONING_OPTIONS.NO_VERSION)
  }

  /**
   * Get summary elements defined for
   * @param rtype
   * @return
   */
  def getSummaryElements(rtype:String):Set[String] = {
    val cProfile = resourceConfigurations(rtype).profile.getOrElse(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype")
    findProfile(cProfile).map(_.summaryElements).getOrElse(Set.empty)
  }
}



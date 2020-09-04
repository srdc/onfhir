package io.onfhir.config

import scala.collection.immutable.HashMap
import akka.http.scaladsl.model._
import io.onfhir.api.model.InternalEntity
import io.onfhir.api.{FHIR_ROOT_URL_FOR_DEFINITIONS, FHIR_VERSIONING_OPTIONS}
import io.onfhir.api.validation.{ProfileRestrictions, ValueSetRestrictions}

/**
  * Central OnFhir configuration defining FHIR server capabilities
  * @param version FHIR version
  */
class FhirConfig(version:String) {
  /***
    *  Dynamic configurations for this instance of FHIR repository
    */
  /** List of supported resource types and profiles for each resource; resource-type -> Set(profile-url) */
  var supportedProfiles:Map[String, Set[String]] = HashMap()
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
  /** FHIR Profile definitions including the base profiles (For validation) Profile Url -> Definitions **/
  var profileRestrictions: Map[String, ProfileRestrictions] = _
  /** Supported FHIR value set urls with this server (For validation) ValueSet Url -> Map(Version ->Definitions) */
  var valueSetRestrictions:Map[String, Map[String, ValueSetRestrictions]] = _
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

  /** Base FHIR Resource Types defined in the standard */
  var FHIR_RESOURCE_TYPES:Set[String] = _
  /** Base FHIR Complex data types defined in the standard */
  var FHIR_COMPLEX_TYPES:Set[String] = _
  /** Base FHIR primitive data types defined in the standard */
  var FHIR_PRIMITIVE_TYPES:Set[String] = _
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
  var FHIR_JSON_PATCH_MEDIA_TYPE:Option[MediaType] = _
  //Map from _format param value to actual MediaType
  var FHIR_FORMAT_MIME_TYPE_MAP:Map[String, MediaType] = _
  //Default media type used when no match
  var FHIR_DEFAULT_MEDIA_TYPE:MediaType = _
  //Code system to indicate a search result is summarized
  var FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM = "http://terminology.hl7.org/CodeSystem/v3-ObservationValue" //"http://hl7.org/fhir/v3/ObservationValue"

  //List of all media types supported for requests
  def FHIR_SUPPORTED_REQUEST_MEDIA_TYPES =
    FHIR_JSON_MEDIA_TYPES ++ FHIR_XML_MEDIA_TYPES ++ FHIR_JSON_PATCH_MEDIA_TYPE.toSeq ++ Seq(MediaTypes.`text/plain`)
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
    * @param profile
    * @return
    */
  def isProfileSupported(profile:String):Boolean = {
    supportedProfiles.flatMap(_._2).exists(_.equals(profile))
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

  def isDstu2() = version match {
    case "DSTU2" | "1.0.2" => true
    case _ => false
  }


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

    profileRestrictions(cProfile).summaryElements
  }

  def getBaseProfile(rtype:String):ProfileRestrictions = {
    findProfile(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype").get
  }

  def getBaseProfileChain(rtype:String):Seq[ProfileRestrictions] = {
    findProfileChain(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype")
  }

  /**
   * Find profile with the given URL
   * @param profileUrl
   * @return
   */
  def findProfile(profileUrl: String):Option[ProfileRestrictions] = {
    profileRestrictions.get(profileUrl)
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
      case Some(profile) => findChain(profileRestrictions)(profile)
    }
  }

  /**
   * Find target resource/data type of a profile
   * @param profileUrl
   * @return
   */
  def findResourceType(profileUrl: String):Option[String] = {
    findProfileChain(profileUrl).reverse.find(!_.isAbstract).map(_.url.split('/').last)
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

/**
  * onFHIR configuration for a Search Parameter
  * @param pname          Name of the parameter
  * @param ptype          FHIR search parameter type (number, date, string, token, etc)
  * @param paths          Extracted element paths for the parameters  e.g. subject, compose.include.concept.code
  * @param targets        Target types or composite parameter names;
  *                         - For reference parameter types, the possible target Resource types
  *                         - For composite parameter types, the names of the parameters to combine in the same order of query usage
  * @param modifiers      Supported modifiers of the parameter
  * @param targetTypes    Seq of target type for each path (indices are inline with paths)
  * @param restrictions   Further restriction on the search for each path; (indices are inline with paths)
  *                          First element provides the sub path to the property with 0 or more prefixes '@.' indicating the position of restriction from right. 0 prefix means the restriction is on the final path element.
  *                          Second element provides the expected value for the property
  *                          e.g. For "f:OrganizationAffiliation/f:telecom[system/@value=&#39;email&#39;]" --> Seq(system -> email)
  *                          e.g. /Goal/extension[@url='http://hl7.org/fhir/StructureDefinition/goal-target']/extension[@url='measure']/valueCodeableConcept --> Seq(@.@.url -> http://hl7.org/fhir/StructureDefinition/goal-target, @.url -> measure)
 *
  * @param multipleOr     If or on parameter is supported
  * @param multipleAnd    If and on parameter is supported
  * @param comparators    Supported comparators for parameter
  */
case class SearchParameterConf(pname:String,
                               ptype:String,
                               paths:Seq[String],
                               targets:Seq[String],
                               modifiers:Set[String],
                               targetTypes:Seq[String] = Nil,
                               //onExtension:Boolean = false,
                               restrictions:Seq[Seq[(String, String)]] = Nil,
                               multipleOr:Boolean = true,
                               multipleAnd:Boolean = true,
                               comparators:Set[String] = Set.empty[String]
                              ) extends InternalEntity {

  /**
    * Extract the possible element paths for the search parameter
    * @param withArrayIndicators If true, it returns the path with array indicators ([i]); e.g. component[i].code
    * @return
    */
  def extractElementPaths(withArrayIndicators:Boolean = false):Seq[String] ={
    if(withArrayIndicators) paths else paths.map(_.replace("[i]", ""))
  }

  def extractElementPathsAndTargetTypes(withArrayIndicators:Boolean = false):Seq[(String, String)] = {
    extractElementPaths(withArrayIndicators)
      .zip(targetTypes)
  }

  def extractElementPathsTargetTypesAndRestrictions(withArrayIndicators:Boolean = false):Seq[(String, String, Seq[(String,String)])] = {
    (extractElementPaths(withArrayIndicators = true), targetTypes, restrictions).zipped.toSeq
  }
}

/**
  * OnFhir configuration for a supported resource; (FHIR CapabilityStatement.rest.resource)
  *
  * @param resource          FHIR Resource type
  * @param profile           Base profile for all uses of resource
  * @param supportedProfiles Profiles for use cases supported
  * @param interactions      FHIR interactions supported
  * @param searchParams      Urls of SearchParameter definitions for supported search parameters
  * @param versioning        Supported versioning code i.e no-version, versioned, versioned-update
  * @param readHistory       Whether vRead can return past versions
  * @param updateCreate      If update can create a new resource
  * @param conditionalCreate If allows/uses conditional create
  * @param conditionalRead   How to support conditional read i.e. not-supported | modified-since | not-match | full-support
  * @param conditionalUpdate If allows/uses conditional update
  * @param conditionalDelete Conditional delete status code i.e. not-supported | single | multiple
  * @param searchInclude     List of supported include parameters
  * @param searchRevInclude  List of supported reverse include parameters
  * @param referencePolicies How this resource type uses FHIR references i.e. literal | logical | resolves | enforced | local
  */
case class ResourceConf(resource:String,
                        profile:Option[String],
                        supportedProfiles:Set[String],
                        interactions:Set[String],
                        searchParams:Set[String],
                        versioning:String,
                        readHistory:Boolean,
                        updateCreate:Boolean,
                        conditionalCreate:Boolean,
                        conditionalRead:String,
                        conditionalUpdate:Boolean,
                        conditionalDelete:String,
                        searchInclude:Set[String],
                        searchRevInclude:Set[String],
                        referencePolicies:Set[String] = Set.empty[String]
                       ) extends InternalEntity

/**
  * Onfhir configuration for a supported Operation parameter
  * @param name           Parameter name
  * @param min            Min cardinality
  * @param max            Max cardinality (*) or a integer
  * @param pType          parameter's FHIR type
  * @param pProfile       If this is a resource parameter, URLs of the profiles it should conform, or if type is a reference or canonical; the profile of target resource
  * @param pSearchType    If parameter is a search parameter, its type e.g. reference, date, token
  * @param parts          Child parameter definitions if exist
  * @param binding        URLs of valueset bindings
  */
case class OperationParamDef(name:String, //Parameter name
                             min:Int, //Min cardinality
                             max:String, //Max cardinality (*) or a integer
                             pType:Option[String], //Parameter type
                             pProfile:Seq[String] = Nil, //
                             pSearchType:Option[String] = None,
                             parts:Seq[OperationParamDef] = Nil,
                             binding:Option[(String,String)] = None
                            )

/**
  * Onfhir configuration for a supported FHIR operation
  * @param url            URL of the definition
  * @param name           Name of the operation to use e.g. validate --> $validate (parsed from OperationDefinition.code)
  * @param classPath      Full class path for the implementation of operation (parsed from OperationDefinition.name)
  * @param kind           Kind of operation "operation" or "query"
  * @param levels         Levels this operation is supported "system", "type", "instance"
  * @param resources      Resources this operation is supported for (for type and instance level operations)
  * @param inputParams    Input parameter definitions or profile for input as a whole (Parameters resource)
  * @param outputParams   Output parameter definitions or profile for output
  */
case class OperationConf(url:String,
                         name:String,
                         var classPath:String = "",
                         kind:String,
                         levels:Set[String],
                         resources:Set[String],
                         inputParams:Seq[OperationParamDef],
                         outputParams:Seq[OperationParamDef],
                         inputParamsProfile:Option[String] = None,
                         affectsState:Boolean = false
                        ) extends InternalEntity {
  //If HTTP Get is allowed for operation; if it does not affect state of resources and all input parameters are primitive
  def isHttpGetAllowed() = !affectsState && inputParams.forall(ip => ip.pType.isDefined &&  ip.pType.get.head.isLower)

}

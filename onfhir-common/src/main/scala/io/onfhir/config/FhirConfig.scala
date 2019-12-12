package io.onfhir.config

import ca.uhn.fhir.context.{FhirContext, FhirVersionEnum}
import ca.uhn.fhir.parser.IParser

import scala.collection.immutable.{HashMap, List}
import akka.http.scaladsl.model._
import io.onfhir.api._
import io.onfhir.api.validation.{ProfileRestrictions, ValueSetRestrictions}

/**
  * Central OnFhir configuration defining FHIR server capabilities
  * @param fhirContext HAPI FHIR context for the supported version
  */
class FhirConfig(val fhirContext:FhirContext) {
  /***
    *  Dynamic configurations for this instance of FHIR repository
    */
  /** List of supported FHIR resource types e.g. Set(Observation, MedicationRequest,...) */
  var supportedResources:Set[String] = _
  /** List of supported profiles for each resource; resource-type -> Set(profile-url) */
  var supportedProfiles:Map[String, Set[String]] = HashMap()
  /** Rest configuration for each (Resource, Profile) pair (if profile is null)*/
  var profileConfigurations:Map[String, ResourceConf] = HashMap()
  /** Supported Query Parameters for each resource-profile pair ; resource-type -> Map(parameter-name -> parameter-configuration)
    * e.g. Observation -> Map(subject -> ..., code -> ...)
    **/
  var resourceQueryParameters:Map[String, Map[String, SearchParameterConf]] = HashMap()
  /** Represents the common query parameters of all resources ; parameter-name -> parameter-configuration
    * e.g. _lastUpdated -> ...
    **/
  var commonQueryParameters:Map[String, SearchParameterConf] = HashMap()
  /** Represents the set of summary parameters for each resource; resource-type -> Set(summary-element-path)
    * e.g. Observation -> Set(code, category, ...)
    **/
  var summaryParameters:Map[String, Set[String]] = HashMap()
  /** Supported compartments and relations with other resources; parameters to link them;  compartment -> Map(resource-type-> Set(parameter-name))
    * e.g. Patient -> Map (Observation -> Set(subject, ....))
    **/
  var compartmentRelations:Map[String, Map[String, Set[String]]] = HashMap()
  /** Supported system level interactions e.g. history-system, search-system, transaction, batch*/
  var supportedInteractions:Set[String] = _
  /** FHIR perations supported; parsing Capability Statement and OperationDefinitions **/
  var supportedOperations:Seq[OperationConf] = _
  /** Supported FHIR value set urls with this server (For validation) */
  var supportedValueSets:Set[String] = _
  /** Supported FHIR code system urls with this server (For validation) */
  var supportedCodeSystems:Set[String] = _
  /** Shard Keys for FHIR resource types; resourceType -> Seq[fhir-search-parameter-name-indicating-the-shard-key] */
  var shardKeys:Map[String, Set[String]] = Map.empty[String, Set[String]]
  /** FHIR Profile definitions including the base profiles (For validation) Profile Url -> Definitions **/
  var profileRestrictions: Map[String, ProfileRestrictions] = _
  /** Supported FHIR value set urls with this server (For validation) ValueSet Url -> Map(Version ->Definitions) */
  var valueSetRestrictions:Map[String, Map[String, ValueSetRestrictions]] = _
  /***
    * Configurations specific to FHIR version
    */
  /** List of FHIR Result parameters this FHIR version support */
  var FHIR_RESULT_PARAMETERS:Seq[String] = _
  /** List of FHIR Special parameters this FHIR version support */
  var FHIR_SPECIAL_PARAMETERS:Seq[String] = _
  /** Names of foundational Resource Types for this FHIR version */
  var FHIR_RESOURCE:String = "Resource"
  var FHIR_CONFORMANCE:String = "CapabilityStatement"
  var FHIR_STRUCTURE_DEFINITION:String = "StructureDefinition"
  var FHIR_SEARCH_PARAMETER:String = "SearchParameter"
  var FHIR_COMPARTMENT_DEFINITION:String = "CompartmentDefinition"
  var FHIR_VALUE_SET:String = "ValueSet"
  var FHIR_CODE_SYSTEM:String = "CodeSystem"
  var FHIR_AUDIT_EVENT:String = "AuditEvent"
  var FHIR_OPERATION_DEFINITION:String = "OperationDefinition"
  var FHIR_DOMAIN_RESOURCE = "DomainResource"
  var FHIR_TYPES_META = "Meta"
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

  //XML and JSON parsers for the given FHIR context
  def xmlParser:IParser = fhirContext.newXmlParser().setStripVersionsFromReferences(false)
  def jsonParser:IParser = fhirContext.newJsonParser().setStripVersionsFromReferences(false)

  /**
    * Check if the profile is supported
    * @param profile
    * @return
    */
  def isProfileSupported(profile:String):Boolean = {
    supportedProfiles.flatMap(_._2).exists(_.equals(profile)) || supportedResources.exists(r => s"${FHIR_ROOT_URL_FOR_DEFINITIONS}/${FHIR_STRUCTURE_DEFINITION}/$r".equals(profile))
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

  def isDstu2() = fhirContext.getVersion.getVersion match {
    case FhirVersionEnum.DSTU2 | FhirVersionEnum.DSTU2_1 | FhirVersionEnum.DSTU2_HL7ORG => true
    case _ => false
  }


  def isResourceTypeVersioned(rtype:String):Boolean = {
    profileConfigurations.get(rtype).forall(_.versioning != FHIR_VERSIONING_OPTIONS.NO_VERSION)
  }
}

/**
  * onFHIR configuration for a Search Parameter
  * @param pname Name of the parameter
  * @param ptype FHIR search parameter type (number, date, string, token, etc)
  * @param paths Extracted element pats for the parameters
  *              - Seq[String] for normal and composite parameters --> e.g. subject, compose.include.concept.code
  *              - Seq[ Seq[ (String, String) ] ] for parameters on extension; last one actual path, others defining urls of extensions
  *              e.g. Seq(Seq( extension.url -> http://hl7.org/fhir/StructureDefinition/goal-target,
  *                            extension.extension.url -> measure,
  *                            extension.extension.valueCodeableConcept -> ""))
  * @param targets
  *                For reference parameter types, the possible target Resource types
  *                For composite parameter types, the names of the parameters to combine in the same order of query usage
  * @param modifiers Supported modifiers of the parameter
  * @param targetTypes Seq of target type for each path (indices are inline with paths)
  * @param onExtension If this is defined on an extension path
  * @param restrictions Further restriction on the search for each path e.g. For "f:OrganizationAffiliation/f:telecom[system/@value=&#39;email&#39;]" --> system -> email
  */
case class SearchParameterConf(pname:String,
                               ptype:String,
                               paths:Seq[Any],
                               targets:Seq[String],
                               modifiers:Set[String],
                               targetTypes:Seq[String] = Nil,
                               onExtension:Boolean = false,
                               restrictions:Seq[Option[(String, String)]] = Nil
                              ) {

  /**
    * Extract the possible element paths for the search parameter
    * @param withArrayIndicators If true, it returns the path with array indicators ([i]); e.g. component[i].code
    * @return
    */
  def extractElementPaths(withArrayIndicators:Boolean = false):Set[String] ={
    //if it is on extension, return the last extension element (to get both URL and value) e.g. extension.extension
    val resultPaths =
      if(onExtension)
        paths
          .asInstanceOf[Seq[Seq[(String, String)]]]
          .map(_.last._1.split('.').dropRight(1).mkString(".")).toSet //Return the last extension
      else
        paths.asInstanceOf[Seq[String]].toSet

    if(withArrayIndicators) resultPaths else resultPaths.map(_.replace("[i]", ""))
  }
}

/**
  * OnFhir configuration for a supported resource; (FHIR CapabilityStatement.rest.resource)
  *
  * @param resource          FHIR Resource type
  * @param profile           Profile contraint for the supported
  * @param interactions      FHIR interactions supported
  * @param versioning        Supported versioning code
  * @param readHistory       Whether vRead can return past versions
  * @param updateCreate      If update can create a new resource
  * @param conditionalCreate If allows/uses conditional create
  * @param conditionalUpdate If allows/uses conditional update
  * @param conditionalDelete Conditional delete status code
  * @param searchInclude     List of supported include parameters
  * @param searchRevInclude  List of supported reverse include parameters
  */
case class ResourceConf(resource:String,
                        profile:Option[String],
                        interactions:List[String],
                        versioning:String,
                        readHistory:Boolean,
                        updateCreate:Boolean,
                        conditionalCreate:Boolean,
                        conditionalUpdate:Boolean,
                        conditionalDelete:String,
                        searchInclude:Option[List[String]],
                        searchRevInclude:Option[List[String]],
                        referencePolicies:Set[String] = Set.empty[String]
                       )

/**
  * Onfhir configuration for a supported Operation parameter
  * @param name           Parameter name
  * @param min            Min cardinality
  * @param max            Max cardinality (*) or a integer
  * @param pType          parameter's FHIR type
  * @param pProfile       If this is a resource parameter, URLs of the profiles it should conform
  * @param parts          Child parameter definitions if exist
  * @param binding       URLs of valueset bindings
  */
case class OperationParamDef(name:String, //Parameter name
                             min:Int, //Min cardinality
                             max:String, //Max cardinality (*) or a integer
                             pType:String, //Parameter type
                             pProfile:Seq[String] = Nil, //
                             parts:Seq[OperationParamDef] = Nil,
                             binding:Option[String] = None
                            )

/**
  * Onfhir configuration for a supported FHIR operation
  * @param name           Name of the operation to use e.g. validate --> $validate (parsed from OperationDefinition.code)
  * @param classPath      Full class path for the implementation of operation (parsed from OperationDefinition.name)
  * @param kind           Kind of operation "operation" or "query"
  * @param levels         Levels this operation is supported "system", "type", "instance"
  * @param resources      Resources this operation is supported for (for type and instance level operations)
  * @param inputParams    Input parameter definitions
  * @param outputParams   Output parameter definitions
  */
case class OperationConf(name:String,
                         classPath:String,
                         kind:String,
                         levels:Set[String],
                         resources:Seq[String],
                         inputParams:Seq[OperationParamDef],
                         outputParams:Seq[OperationParamDef],
                         affectsState:Boolean = false
                        ) {
  //If HTTP Get is allowed for operation; if it does not affect state of resources and all input parameters are primitive
  def isHttpGetAllowed() = !affectsState && inputParams.forall(ip => FHIR_PRIMITIVE_TYPES.contains(ip.pType))

}

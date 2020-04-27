package io.onfhir.config

import akka.http.scaladsl.model.{MediaType, MediaTypes}
import io.onfhir.api.{FHIR_SEARCH_RESULT_PARAMETERS, FHIR_SEARCH_SPECIAL_PARAMETERS, Resource}
import io.onfhir.api.validation.{IFhirResourceValidator, IFhirTerminologyValidator, ProfileRestrictions, ValueSetRestrictions}
import io.onfhir.audit.IFhirAuditCreator

/**
 * Compact form for FHIR CapabilityStatement
 * @param restResourceConf        REST configurations for each supported resource
 * @param searchParamDefUrls      Common search parameter definition URLs
 * @param operationDefUrls        All operation definition URLs
 * @param systemLevelInteractions System level interactions supported (e.g. transaction, batch)
 * @param compartments            Definition url of compartments supported
 */
case class FHIRCapabilityStatement(
                                    restResourceConf:Seq[ResourceConf],
                                    searchParamDefUrls:Set[String],
                                    operationDefUrls:Set[String],
                                    systemLevelInteractions:Set[String],
                                    compartments:Set[String]
                                  )

/**
 * Compact form of a FHIR Search Parameter definition
 * @param name          Name/code of search parameter
 * @param url           URL of definition
 * @param base          Resource Types that this parameter is defined on
 * @param ptype         Search parameter type i.e. number | date | string | token | reference | composite | quantity | uri | special
 * @param expression    FHIR Expression for parameter paths
 * @param xpath         XPath expression for parameter paths
 * @param target        If type is reference, possible target Resource Types
 * @param multipleOr    If it allows multiple values per parameter (or)
 * @param multipleAnd   If it allows multiple parameters (and)
 * @param comparators   Allowed comparators used with parameter
 * @param modifiers     Allowed modifiers used with parameter
 * @param components    URL of search parameter definitions for children of composite parameters
 */
case class FHIRSearchParameter(
                                name:String,
                                url:String,
                                base:Set[String],
                                ptype:String,
                                expression:Option[String],
                                xpath:Option[String],
                                target:Set[String],
                                multipleOr:Option[Boolean],
                                multipleAnd:Option[Boolean],
                                comparators:Set[String],
                                modifiers:Set[String],
                                components:Set[String]
                              )

/**
 * Compact form of FHIR Compartment definition
 * @param url       Canonical url
 * @param code      Code for compartment (e.g. Patient, Practitioner, etc)
 * @param relations Relations with all resource types i.e. resource type -> name of search parameters
 */
case class FHIRCompartmentDefinition(url:String, code:String, relations:Map[String, Set[String]])

trait IFhirVersionConfigurator {
  //FHIR Version
  val fhirVersion:String = "R4"

  /**
   * Names of FHIR Infrastructure resources in the version of the FHIR standard
   */
  final val FHIR_RESOURCE: String = "Resource"
  final val FHIR_CONFORMANCE: String = "CapabilityStatement"
  final val FHIR_STRUCTURE_DEFINITION: String = "StructureDefinition"
  final val FHIR_SEARCH_PARAMETER: String = "SearchParameter"
  final val FHIR_COMPARTMENT_DEFINITION: String = "CompartmentDefinition"
  final val FHIR_VALUE_SET: String = "ValueSet"
  final val FHIR_CODE_SYSTEM: String = "CodeSystem"
  final val FHIR_AUDIT_EVENT: String = "AuditEvent"
  final val FHIR_OPERATION_DEFINITION: String = "OperationDefinition"
  final val FHIR_DOMAIN_RESOURCE = "DomainResource"
  final val FHIR_TYPES_META = "Meta"

  /**
   * List of FHIR Result parameters this FHIR version support
   */
  val FHIR_RESULT_PARAMETERS:Seq[String] = Seq(
    FHIR_SEARCH_RESULT_PARAMETERS.SORT,
    FHIR_SEARCH_RESULT_PARAMETERS.COUNT,
    FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY,
    FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS,
    FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE,
    FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE,
    FHIR_SEARCH_RESULT_PARAMETERS.PAGE,
    FHIR_SEARCH_RESULT_PARAMETERS.TOTAL,
    FHIR_SEARCH_RESULT_PARAMETERS.CONTAINED,
    FHIR_SEARCH_RESULT_PARAMETERS.CONTAINED_TYPE,
    FHIR_SEARCH_RESULT_PARAMETERS.SINCE,
    FHIR_SEARCH_RESULT_PARAMETERS.AT
  )

  /** List of FHIR Special parameters this FHIR version support */
  var FHIR_SPECIAL_PARAMETERS:Seq[String] = Seq(
    FHIR_SEARCH_SPECIAL_PARAMETERS.ID,
    FHIR_SEARCH_SPECIAL_PARAMETERS.LIST,
    FHIR_SEARCH_SPECIAL_PARAMETERS.QUERY,
    FHIR_SEARCH_SPECIAL_PARAMETERS.FILTER,
    FHIR_SEARCH_SPECIAL_PARAMETERS.HAS,
    FHIR_SEARCH_SPECIAL_PARAMETERS.TEXT,
    FHIR_SEARCH_SPECIAL_PARAMETERS.CONTENT
  )

  val FHIR_JSON_MEDIA_TYPE = MediaType.applicationWithOpenCharset("fhir+json")
  val FHIR_XML_MEDIA_TYPE = MediaType.applicationWithOpenCharset("fhir+xml")

  /** MediaType configurations for this FHIR version */
  // List of Supported FHIR JSON Media Types
  val FHIR_JSON_MEDIA_TYPES:Seq[MediaType] = Seq(
    MediaTypes.`application/json`,
    FHIR_JSON_MEDIA_TYPE
  )
  // List of Supported FHIR XML Media Types
  val FHIR_XML_MEDIA_TYPES:Seq[MediaType] = Seq(
    MediaTypes.`application/xml`,
    FHIR_XML_MEDIA_TYPE
  )
  // Json patch media type supported
  val FHIR_JSON_PATCH_MEDIA_TYPE:Option[MediaType] = Some(MediaType.applicationWithOpenCharset("json-patch+json"))
  //Map from _format param value to actual MediaType
  val FHIR_FORMAT_MIME_TYPE_MAP:Map[String, MediaType] = Map(
    "html" -> MediaTypes.`text/html`,
    "text/html" -> MediaTypes.`text/html`,
    "application/json" -> MediaTypes.`application/json`,
    "application/xml" -> MediaTypes.`application/xml`,
    "application/fhir+json" -> FHIR_JSON_MEDIA_TYPE,
    "application/fhir+xml" -> FHIR_XML_MEDIA_TYPE,
    "json" -> FHIR_JSON_MEDIA_TYPE,
    "xml" -> FHIR_XML_MEDIA_TYPE,
    "text/xml" -> FHIR_XML_MEDIA_TYPE
  )
  //Default media type used when no match
  val FHIR_DEFAULT_MEDIA_TYPE:MediaType = FHIR_JSON_MEDIA_TYPE

  //Code system to indicate a search result is summarized
  val FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM = "http://terminology.hl7.org/CodeSystem/v3-ObservationValue"

  /**
   * Parse the base FHIR standard bundle and provide a configuration for the server
   * @param fromConfig  If false, initialization is performed from the FHIR foundation resources stored in database
   * @return
   */
  def initializePlatform(fromConfig:Boolean = false, fhirOperationImplms:Map[String, String]):FhirConfig

  /**
   * Setup the platform (database initialization) for the first time (or updated the configurations)
   * @param fhirConfig
   */
  def setupPlatform(fhirConfig: FhirConfig):Unit

  /**
   * Get a resource validator for this FHIR version
   * @param fhirConfig
   * @return
   */
  def getResourceValidator(fhirConfig: FhirConfig):IFhirResourceValidator

  /**
   * Get a terminology validator for this FHIR version
   * @param fhirConfig
   * @return
   */
  def getTerminologyValidator(fhirConfig: FhirConfig):IFhirTerminologyValidator

  /**
   * Return a class that implements the interface to create AuditEvents conformant to the given base specification
   * @return
   */
  def getAuditCreator():IFhirAuditCreator

  /**
   * Get Resource type or Data type from a StructureDefinition resource if it is not abstract
   * @param structureDefinition
   * @return
   */
  def getTypeFromStructureDefinition(structureDefinition:Resource):Option[String]

  /**
   * Parse a FHIR Capability Statement into our compact form
   * @param capabilityStmt  CapabilityStatement resource in parsed JSON format
   * @return
   */
  def parseCapabilityStatement(capabilityStmt:Resource):FHIRCapabilityStatement

  /**
   * Parse a FHIR SearchParameter definition into our compact form
   * @param searchParameter SearchParameter resource in parsed JSON format
   * @return
   */
  def parseSearchParameter(searchParameter:Resource):FHIRSearchParameter


  /**
   * Parse a FHIR OperationDefinition  into our compact form
   * @param operationDefinition OperationDefinition resource in parsed JSON format
   * @return
   */
  def parseOperationDefinition(operationDefinition:Resource):OperationConf

  /**
   * Parse a FHIR CompartmentDefinition into our compact form
   * @param compartmentDefinition CompartmentDefinition resource in parsed JSON format
   * @return
   */
  def parseCompartmentDefinition(compartmentDefinition:Resource):FHIRCompartmentDefinition

  /**
   * Parse a FHIR StructureDefinition into our compact form
   * @param structureDefinition
   * @return
   */
  def parseStructureDefinition(structureDefinition:Resource):ProfileRestrictions

  /**
   * Parse a bundle of FHIR ValueSet and CodeSystem into a compact form for validation
   * @param valueSetOrCodeSystems
   * @return
   */
  def parseValueSetAndCodeSystems(valueSetOrCodeSystems:Seq[Resource]):Map[String, Map[String, ValueSetRestrictions]]

  /**
   * Extract certain type of resources from a FHIR Bundle
   * @param bundle
   * @param rtype
   * @return
   */
  def extractResourcesFromBundle(bundle:Resource, rtype:String):Seq[Resource]
}

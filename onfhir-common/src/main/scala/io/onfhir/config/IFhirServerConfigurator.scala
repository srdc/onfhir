package io.onfhir.config

import akka.http.scaladsl.model.{MediaType, MediaTypes}
import io.onfhir.api.parsers.IFhirFoundationResourceParser
import io.onfhir.api.validation.{IFhirResourceValidator, IFhirTerminologyValidator}
import io.onfhir.api.{FHIR_MEDIA_TYPES, FHIR_SEARCH_RESULT_PARAMETERS, FHIR_SEARCH_SPECIAL_PARAMETERS, Resource}
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.db.BaseDBInitializer

trait IFhirServerConfigurator extends IFhirVersionConfigurator {
  /**
   * List of FHIR Result parameters this FHIR version support
   */
  val FHIR_RESULT_PARAMETERS: Seq[String] = Seq(
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
  var FHIR_SPECIAL_PARAMETERS: Seq[String] = Seq(
    FHIR_SEARCH_SPECIAL_PARAMETERS.ID,
    FHIR_SEARCH_SPECIAL_PARAMETERS.LIST,
    FHIR_SEARCH_SPECIAL_PARAMETERS.QUERY,
    FHIR_SEARCH_SPECIAL_PARAMETERS.FILTER,
    FHIR_SEARCH_SPECIAL_PARAMETERS.HAS,
    FHIR_SEARCH_SPECIAL_PARAMETERS.TEXT,
    FHIR_SEARCH_SPECIAL_PARAMETERS.CONTENT
  )

  val FHIR_JSON_MEDIA_TYPE = FHIR_MEDIA_TYPES.FHIR_JSON_MEDIA_TYPE
  val FHIR_XML_MEDIA_TYPE = FHIR_MEDIA_TYPES.FHIR_XML_MEDIA_TYPE

  /** MediaType configurations for this FHIR version */
  // List of Supported FHIR JSON Media Types
  def FHIR_JSON_MEDIA_TYPES(fhirVersion: String): Seq[MediaType] = Seq(
    MediaTypes.`application/json`,
    FHIR_JSON_MEDIA_TYPE,
    FHIR_JSON_MEDIA_TYPE.withParams(Map("fhirVersion" -> fhirVersion))
  )

  // List of Supported FHIR XML Media Types
  def FHIR_XML_MEDIA_TYPES(fhirVersion: String): Seq[MediaType] = Seq(
    MediaTypes.`application/xml`,
    FHIR_XML_MEDIA_TYPE,
    FHIR_XML_MEDIA_TYPE.withParams(Map("fhirVersion" -> fhirVersion))
  )

  // Patch media types supported by onFHIR
  val FHIR_PATCH_MEDIA_TYPES: Seq[MediaType] = Seq(FHIR_MEDIA_TYPES.FHIR_JSON_PATCH_MEDIA_TYPE)
  //Map from _format param value to actual MediaType
  val FHIR_FORMAT_MIME_TYPE_MAP: Map[String, MediaType] = Map(
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
  val FHIR_DEFAULT_MEDIA_TYPE: MediaType = FHIR_JSON_MEDIA_TYPE

  //Code system to indicate a search result is summarized
  val FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM = "http://terminology.hl7.org/CodeSystem/v3-ObservationValue"


  /**
   * Parse the base FHIR standard bundle and supplied FHIR foundation resources and provide a configuration for the server
   * @param configReader        Reader for configuration files (FHIR standard, Foundation resources)
   * @param fhirOperationImplms FHIR Operation implementations (URL for OperationDefinition -> Classpath of the implementation)
   * @return
   */
  def initializePlatform(configReader: IFhirConfigReader, fhirOperationImplms: Map[String, String]): FhirServerConfig


  /**
   * Setup the platform (database initialization) for the first time (or updated the configurations)
   * @param configReader        Configuration reader
   * @param baseDBInitializer   Database initializer
   * @param fhirConfig          FHIR configuration
   */
  def setupPlatform(configReader: IFhirConfigReader,
                    baseDBInitializer: BaseDBInitializer,
                    fhirConfig: FhirServerConfig):Unit



  /**
   * Return a class that implements the interface to create AuditEvents compliant to the given base specification
   *
   * @return
   */
  def getAuditCreator(): IFhirAuditCreator
}

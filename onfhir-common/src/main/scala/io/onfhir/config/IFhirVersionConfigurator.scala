package io.onfhir.config

import io.onfhir.api.{FOUNDATION_RESOURCES_FILE_SUFFIX, Resource}
import io.onfhir.api.parsers.IFhirFoundationResourceParser

trait IFhirVersionConfigurator {
  //FHIR Version
  val fhirVersion: String = "R4"

  //Name of the files that includes the Bundle for search parameter definitions in base FHIR specification zip file
  protected val SEARCH_PARAMETERS_BUNDLE_FILE_NAME = s"search-parameters$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for Resources in base FHIR specification
  protected val PROFILES_RESOURCES_BUNDLE_FILE_NAME = s"profiles-resources$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for Types in base FHIR specification
  protected val PROFILES_TYPES_BUNDLE_FILE_NAME = s"profiles-types$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for other FHIR profiles given in base
  protected val PROFILES_OTHERS_BUNDLE_FILE_NAME = s"profiles-others$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for extensions given in base
  protected val PROFILES_EXTENSIONS_BUNDLE_FILE_NAME = s"extension-definitions$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the files that includes the Bundle for ValueSets and CodeSystems in base FHIR specification
  protected val VALUESET_AND_CODESYSTEM_BUNDLE_FILES: Seq[String] = Seq(s"valuesets$FOUNDATION_RESOURCES_FILE_SUFFIX", s"v3-codesystems$FOUNDATION_RESOURCES_FILE_SUFFIX", s"v2-tables$FOUNDATION_RESOURCES_FILE_SUFFIX")


  /**
   * Parse the base FHIR standard bundle and given profiles, valuesets and codesystems,
   * and provide a configuration for the server
   *
   * @param configReader Reader for configuration files
   * @return
   */
  def initializePlatform(configReader: IFhirConfigReader): BaseFhirConfig

  /**
   * Return the parser for foundation resources
   *
   * @param complexTypes   List of FHIR complex types defined in the standard
   * @param primitiveTypes List of FHIR primitive types defined in the standard
   * @return
   */
  def getFoundationResourceParser(complexTypes: Set[String], primitiveTypes: Set[String]): IFhirFoundationResourceParser

  /**
   * Get Resource type or Data type from a StructureDefinition resource if it is not abstract
   *
   * @param structureDefinition FHIR StructureDefinition resource
   * @return
   */
  def getTypeFromStructureDefinition(structureDefinition: Resource): Option[String]

  /**
   * Validate the given infrastructure resources and throw exception if any invalid
   *
   * @param baseFhirConfig Base FHIR configurations
   * @param rtype          FHIR Resource type
   * @param resources      Resources to validate
   * @throws InitializationException If there is a problem in given profile or value set definitions
   */
  def validateGivenInfrastructureResources(baseFhirConfig: BaseFhirConfig, rtype: String, resources: Seq[Resource]): Unit
}

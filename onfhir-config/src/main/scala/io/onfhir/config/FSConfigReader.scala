package io.onfhir.config
import io.onfhir.api.FHIR_FOUNDATION_RESOURCES.FHIR_CONFORMANCE
import io.onfhir.api.util.IOUtil
import io.onfhir.api.{DEFAULT_RESOURCE_PATHS, FHIR_FOUNDATION_RESOURCES, Resource}

/**
 * FHIR configuration reader from file system
 * @param fhirStandardZipFilePath   Path to the FHIR standard zip file (definitions.json.zip)
 * @param profilesPath              Path to the zip file or folder that supported FHIR profiles (StructuredDefinition)  are given
 * @param valueSetsPath             Path to the zip file or folder that supported FHIR ValueSet definitions are given
 * @param codeSystemsPath           Path to the zip file or folder that supported FHIR CodeSystem definitions are given
 * @param conformancePath           Path to the CapabilityStatement definition
 * @param searchParametersPath      Path to the zip file or folder that supported FHIR SearchParameter definitions are given
 * @param operationDefinitionsPath  Path to the zip file or folder that supported FHIR OperationDefinition definitions are given
 * @param compartmentDefinitionsPath  Path to the zip file or folder that supported FHIR CompartmentDefinition definitions are given
 */
class FSConfigReader(
                      fhirStandardZipFilePath:Option[String] = None,
                      profilesPath:Option[String] = None,
                      valueSetsPath:Option[String] = None,
                      codeSystemsPath:Option[String] = None,
                      conformancePath:Option[String] = None,
                      searchParametersPath:Option[String] = None,
                      operationDefinitionsPath:Option[String] = None,
                      compartmentDefinitionsPath:Option[String] = None,
                    ) extends IFhirConfigReader {
  /**
   * Read and parse FHIR Bundle zip file (JSON version) published by FHIR for specific versions and
   * return specific FHIR foundation resources e.g. StructureDefinitions defined in the standard
   * e.g. https://www.hl7.org/fhir/definitions.json.zip
   *
   * @param fileName           Name of the file in the FHIR standard definitions zip e.g. profiles-resources.json --> Base FHIR resource definitions (StructureDefinitions)
   * @param resourceTypeFilter Types of the resources to extract from the Bundle
   * @return
   */
  override def readStandardBundleFile(fileName: String, resourceTypeFilter: Set[String]): Seq[Resource] = {
    IOUtil
      .readStandardBundleFile(
        fhirStandardZipFilePath,
        DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS,
        fileName,
        resourceTypeFilter
      )
  }

  /**
   * Read FHIR infrastructure resources (profiles, value sets, etc) supplied for a FHIR configuration
   * @param rtype Resource type of the foundation resource e.g. StructureDefinition, ValueSet, etc
   * @return
   */
  override def getInfrastructureResources(rtype: String): Seq[Resource] = {
    val pathsToSearch =
      rtype match {
        case FHIR_FOUNDATION_RESOURCES.FHIR_SEARCH_PARAMETER =>
          searchParametersPath -> DEFAULT_RESOURCE_PATHS.SEARCH_PARAMETER
        case FHIR_FOUNDATION_RESOURCES.FHIR_STRUCTURE_DEFINITION =>
          profilesPath -> DEFAULT_RESOURCE_PATHS.PROFILES_FOLDER
        case FHIR_FOUNDATION_RESOURCES.FHIR_OPERATION_DEFINITION =>
          operationDefinitionsPath -> DEFAULT_RESOURCE_PATHS.OPDEFS_PATH
        case FHIR_FOUNDATION_RESOURCES.FHIR_COMPARTMENT_DEFINITION =>
          compartmentDefinitionsPath -> DEFAULT_RESOURCE_PATHS.COMPARTMENTS_PATH
        case FHIR_FOUNDATION_RESOURCES.FHIR_VALUE_SET =>
          valueSetsPath -> DEFAULT_RESOURCE_PATHS.VALUESETS_PATH
        case FHIR_FOUNDATION_RESOURCES.FHIR_CODE_SYSTEM =>
          codeSystemsPath -> DEFAULT_RESOURCE_PATHS.CODESYSTEMS_PATH
      }
    IOUtil.readResourcesInFolderOrZip(pathsToSearch._1, pathsToSearch._2)
  }

  def readCapabilityStatement():Resource =
    IOUtil.readResource(conformancePath, DEFAULT_RESOURCE_PATHS.CONFORMANCE_PATH, FHIR_CONFORMANCE)
}

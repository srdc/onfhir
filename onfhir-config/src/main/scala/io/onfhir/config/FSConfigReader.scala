package io.onfhir.config

import io.onfhir.api.FHIR_FOUNDATION_RESOURCES.FHIR_CONFORMANCE
import io.onfhir.api.util.{FHIRUtil, IOUtil}
import io.onfhir.api.{DEFAULT_RESOURCE_PATHS, DEFAULT_ROOT_FOLDER, FHIR_FOUNDATION_RESOURCES, FOUNDATION_RESOURCES_FILE_SUFFIX, Resource}

/**
 * FHIR configuration reader from file system
 *
 * @param fhirVersion                The version of the FHIR to be used (R4 or R5). For now, this affects which default base definitions will be loaded (R4 or R5) unless fhirStandardZipFilePath is given.
 * @param fhirStandardZipFilePath    Path to the FHIR standard zip file (definitions.json.zip)
 * @param profilesPath               Path to the zip file or folder that supported FHIR profiles (StructuredDefinition)  are given
 * @param valueSetsPath              Path to the zip file or folder that supported FHIR ValueSet definitions are given
 * @param codeSystemsPath            Path to the zip file or folder that supported FHIR CodeSystem definitions are given
 * @param conformancePath            Path to the CapabilityStatement definition
 * @param searchParametersPath       Path to the zip file or folder that supported FHIR SearchParameter definitions are given
 * @param operationDefinitionsPath   Path to the zip file or folder that supported FHIR OperationDefinition definitions are given
 * @param compartmentDefinitionsPath Path to the zip file or folder that supported FHIR CompartmentDefinition definitions are given
 */
class FSConfigReader(
                      fhirVersion: String,
                      fhirStandardZipFilePath: Option[String] = None,
                      profilesPath: Option[String] = None,
                      valueSetsPath: Option[String] = None,
                      codeSystemsPath: Option[String] = None,
                      conformancePath: Option[String] = None,
                      searchParametersPath: Option[String] = None,
                      operationDefinitionsPath: Option[String] = None,
                      compartmentDefinitionsPath: Option[String] = None,
                    ) extends BaseConfigReader(fhirVersion, fhirStandardZipFilePath) {
  /**
   * Read FHIR infrastructure resources (profiles, value sets, etc) supplied for a FHIR configuration
   *
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

  override def readCapabilityStatement(): Resource = {
    val conformanceFileDefaultPath = fhirVersion match {
      case "R4" | "4.0.1" => DEFAULT_RESOURCE_PATHS.CONFORMANCE_PATH_R4
      case "R5" | "5.0.0" => DEFAULT_RESOURCE_PATHS.CONFORMANCE_PATH_R5
      case oth => FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER, s"conformance-statement-${oth.toLowerCase()}$FOUNDATION_RESOURCES_FILE_SUFFIX")
    }
    IOUtil.readResource(conformancePath, conformanceFileDefaultPath, FHIR_CONFORMANCE)
  }
}

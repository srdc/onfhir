package io.onfhir.config

import io.onfhir.api.{DEFAULT_RESOURCE_PATHS, DEFAULT_ROOT_FOLDER, FOUNDATION_RESOURCES_FILE_SUFFIX, Resource}
import io.onfhir.api.util.{FHIRUtil, IOUtil}

abstract class BaseConfigReader(fhirVersion: String, fhirStandardZipFilePath: Option[String] = None) extends IFhirConfigReader {
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
    val baseDefinitionsFile = fhirVersion match {
      case "R4" | "4.0.1" => DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4
      case "R5" | "5.0.0" => DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R5
      case oth => FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER, s"definitions-${oth.toLowerCase}${FOUNDATION_RESOURCES_FILE_SUFFIX}.zip")
    }
    IOUtil
      .readStandardBundleFile(
        fhirStandardZipFilePath,
        baseDefinitionsFile,
        fileName,
        resourceTypeFilter
      )
  }

}

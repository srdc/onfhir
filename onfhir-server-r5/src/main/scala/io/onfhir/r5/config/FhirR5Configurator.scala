package io.onfhir.r5.config

import io.onfhir.api.FOUNDATION_RESOURCES_FILE_SUFFIX
import io.onfhir.api.parsers.IFhirFoundationResourceParser
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.config.BaseFhirServerConfigurator
import io.onfhir.r4.parsers.R4Parser

class FhirR5Configurator extends BaseFhirServerConfigurator {
  override val fhirVersion: String = "R5"

  override val VALUESET_AND_CODESYSTEM_BUNDLE_FILES = Seq(s"valuesets$FOUNDATION_RESOURCES_FILE_SUFFIX")

  /**
   * Return a class that implements the interface to create AuditEvents compliant to the given base specification
   *
   * @return
   */
  override def getAuditCreator(): IFhirAuditCreator = ???

  /**
   * Return the parser for foundation resources
   *
   * @param complexTypes   List of FHIR complex types defined in the standard
   * @param primitiveTypes List of FHIR primitive types defined in the standard
   * @return
   */
  override def getFoundationResourceParser(complexTypes: Set[String], primitiveTypes: Set[String]): IFhirFoundationResourceParser =
    new R4Parser(complexTypes, primitiveTypes)
}

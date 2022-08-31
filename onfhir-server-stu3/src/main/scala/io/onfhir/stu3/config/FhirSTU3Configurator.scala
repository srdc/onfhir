package io.onfhir.stu3.config

import io.onfhir.api.parsers.IFhirFoundationResourceParser
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.config.BaseFhirConfigurator
import io.onfhir.stu3.audit.STU3AuditCreator
import io.onfhir.stu3.parsers.STU3Parser


class FhirSTU3Configurator extends BaseFhirConfigurator {
  override val fhirVersion: String = "STU3"
  override val FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM = "http://hl7.org/fhir/v3/ObservationValue"
  /**
   * Return a class that implements the interface to create AuditEvents conformant to the given base specification
   *
   * @return
   */
  override def getAuditCreator(): IFhirAuditCreator = new STU3AuditCreator

  /**
   * Return the parser for foundation resources
   *
   * @param complexTypes   List of FHIR complex types defined in the standard
   * @param primitiveTypes List of FHIR primitive types defined in the standard
   * @return
   */
  override def getFoundationResourceParser(complexTypes: Set[String], primitiveTypes: Set[String]): IFhirFoundationResourceParser =
    new STU3Parser(complexTypes, primitiveTypes)
}

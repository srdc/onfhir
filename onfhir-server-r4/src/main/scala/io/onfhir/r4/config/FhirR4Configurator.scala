package io.onfhir.r4.config

import io.onfhir.api.parsers.IFhirFoundationResourceParser
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.config._
import io.onfhir.r4.audit.R4AuditCreator
import io.onfhir.r4.parsers.R4Parser



/**
 * Configurator for FHIR R4 standard (parsing R4 foundation resources, etc)
 */
class FhirR4Configurator extends BaseFhirConfigurator {
  /**
   * Return the Audit creator for R4
   *  @return
   */
  def getAuditCreator():IFhirAuditCreator = new R4AuditCreator()

  /**
   * Return the parser for R4
   * @param complexTypes    List of FHIR complex types defined in the standard
   * @param primitiveTypes  List of FHIR primitive types defined in the standard
   *  @return
   */
  def getFoundationResourceParser(complexTypes:Set[String], primitiveTypes:Set[String]):IFhirFoundationResourceParser =
    new R4Parser(complexTypes, primitiveTypes)
}

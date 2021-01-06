package io.onfhir.config

import io.onfhir.api.validation.{IFhirResourceValidator, IFhirTerminologyValidator}
import io.onfhir.audit.IFhirAuditCreator

trait IFhirConfigurationManager {
  /**
   * Return FHIR Configuration for onFhir
   * @return
   */
  def getFhirConfig:FhirConfig

  /**
   * Return FHIR content validator for onFhir
   * @return
   */
  def getFhirValidator:IFhirResourceValidator

  /**
   * Return terminologu validator for onFhir
   * @return
   */
  def getFhirTerminologyValidator:IFhirTerminologyValidator

  /**
   * Return audit creator for onFhir
   * @return
   */
  def getFhirAuditCreator:IFhirAuditCreator

}

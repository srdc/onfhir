package io.onfhir.config

import io.onfhir.api.validation.IFhirResourceValidator
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.config.IndexConfigurator.ResourceIndexConfiguration
import org.hl7.fhir.instance.model.api.IBaseResource

/**
  * Interface for OnFhir version specific FHIR capabilities configurators
  */
trait IFhirConfigurator {

  /**
    * Names of FHIR Infrastructure resources in the version of the FHIR standard
    */
  def FHIR_RESOURCE: String = "Resource"

  def FHIR_CONFORMANCE: String = "CapabilityStatement"

  def FHIR_STRUCTURE_DEFINITION: String = "StructureDefinition"

  def FHIR_SEARCH_PARAMETER: String = "SearchParameter"

  def FHIR_COMPARTMENT_DEFINITION: String = "CompartmentDefinition"

  def FHIR_VALUE_SET: String = "ValueSet"

  def FHIR_CODE_SYSTEM: String = "CodeSystem"

  def FHIR_AUDIT_EVENT: String = "AuditEvent"

  def FHIR_OPERATION_DEFINITION: String = "OperationDefinition"

  def FHIR_DOMAIN_RESOURCE = "DomainResource"

  def FHIR_TYPES_META = "Meta"

  /**
    * Initialize the platform by preparing Fhir Configuration from FHIR foundation resources (Capability Statement, StructureDefinition, etc)
    *
    * @param fromConfig
    * @return
    */
  def initializePlatform(fromConfig: Boolean = false): (FhirConfig, IBaseResource, Seq[IBaseResource], Seq[IBaseResource], Seq[IBaseResource], Seq[IBaseResource], Seq[IBaseResource], Seq[IBaseResource], Map[String, ResourceIndexConfiguration])

  /**
    * Prepare the database and store the infrastructure resources
    *
    * @param fhirConfig
    * @param conformance
    * @param profiles
    * @param searchParameters
    * @param compartmentDefinitions
    * @param valueSets
    * @param operationDefinitions
    * @param codeSystems
    * @param indexConfigurations
    */
  def setupPlatform(fhirConfig: FhirConfig,
                    conformance: IBaseResource,
                    profiles: Seq[IBaseResource],
                    searchParameters: Seq[IBaseResource],
                    compartmentDefinitions: Seq[IBaseResource],
                    operationDefinitions: Seq[IBaseResource],
                    valueSets: Seq[IBaseResource],
                    codeSystems: Seq[IBaseResource],
                    indexConfigurations: Map[String, ResourceIndexConfiguration])

  /**
    * Return the version specific FHIR AuditEvent creator
    *
    * @return
    */
  def getAuditCreator(): IFhirAuditCreator

  /**
    * Return the version specific FHIR Resource Validator
    *
    * @return
    */
  def getResourceValidator(): IFhirResourceValidator

}

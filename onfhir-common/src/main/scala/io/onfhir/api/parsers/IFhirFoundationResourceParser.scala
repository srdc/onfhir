package io.onfhir.api.parsers

import io.onfhir.api.Resource
import io.onfhir.api.validation.{ProfileRestrictions, ValueSetRestrictions}
import io.onfhir.config.{FHIRCapabilityStatement, FHIRCompartmentDefinition, FHIRSearchParameter, OperationConf, OperationParamDef}
import org.json4s.JObject

/**
 * Interface to parse FHIR foundation resources (to handle changes in different major versions of FHIR)
 */
trait IFhirFoundationResourceParser {
  /**
   * Parse CapabilityStatement definition
   * @param capabilityStmt  Parsed CapabilityStatement json content
   * @return
   */
  def parseCapabilityStatement(capabilityStmt: Resource): FHIRCapabilityStatement

  /**
   * Extract common search parameter definition URLs
   * @param capabilityStmt CapabilityStatement resource in parsed JSON format
   * @return
   */
  protected def extractCommonSearchParameterDefinitionUrls(capabilityStmt:Resource):Seq[String]

  /**
   * Extract definition URLs for system level operations
   * @param capabilityStmt    Parsed JSON object for CapabilityStatement
   * @return
   */
  protected def extractOperationDefinitionUrls(capabilityStmt:Resource):Seq[String]

  /**
   * Parse a FHIR SearchParameter definition into our compact form
   * @param searchParameter SearchParameter resource in parsed JSON format
   * @return
   */
  def parseSearchParameter(searchParameter: Resource): FHIRSearchParameter


  /**
   * Parse a FHIR OperationDefinition  into our compact form
   *
   * @param operationDefinition OperationDefinition resource in parsed JSON format
   * @return
   */
  def parseOperationDefinition(operationDefinition: Resource): OperationConf

  /**
   * Parse a Operation parameter definition object
   * @param paramDef  OperationDefinition.parameter element
   * @return
   */
  protected def parseOperationParamDefinition(paramDef:JObject):(String, OperationParamDef)

  /**
   * Parse a FHIR CompartmentDefinition into our compact form
   *
   * @param compartmentDefinition CompartmentDefinition resource in parsed JSON format
   * @return
   */
  def parseCompartmentDefinition(compartmentDefinition: Resource): FHIRCompartmentDefinition

  /**
   * Extract resources from FHIR Bundle with given resource type
   * @param bundle  FHIR Bundle
   * @param rtype   FHIR Resource type
   * @return
   */
  protected def extractResourcesFromBundle(bundle: Resource, rtype: String): Seq[Resource]


  /**
   * Parse a FHIR StructureDefinition into our compact form
   *
   * @param structureDefinition   Parsed JSON object for FHIR StructureDefinition
   * @return
   */
  def parseStructureDefinition(structureDefinition: Resource): ProfileRestrictions

  /**
   * Parse a bundle of FHIR ValueSet and CodeSystem into a compact form for validation
   *
   * @param valueSetOrCodeSystems   Parsed JSON objects for all ValueSet and CodeSystem resources that will be related with server
   * @return
   */
  def parseValueSetAndCodeSystems(valueSetOrCodeSystems: Seq[Resource]): Map[String, Map[String, ValueSetRestrictions]]
}

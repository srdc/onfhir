package io.onfhir.api.parsers

import io.onfhir.api.Resource
import io.onfhir.config.{OperationConf, ResourceConf}

/**
 * Compact form for FHIR CapabilityStatement
 * @param restResourceConf      REST configurations for each supported resource
 * @param searchParamDefUrls    Common search parameter definition URLs
 * @param operationDefUrls      All operation definition URLs
 */
case class FHIRCapabilityStatement(restResourceConf:Seq[ResourceConf], searchParamDefUrls:Set[String], operationDefUrls:Set[String])

/**
 * Compact form of a FHIR Search Parameter definition
 * @param name          Name/code of search parameter
 * @param url           URL of definition
 * @param base          Resource Types that this parameter is defined on
 * @param ptype         Search parameter type i.e. number | date | string | token | reference | composite | quantity | uri | special
 * @param expression    FHIR Expression for parameter paths
 * @param xpath         XPath expression for parameter paths
 * @param target        If type is reference, possible target Resource Types
 * @param multipleOr    If it allows multiple values per parameter (or)
 * @param multipleAnd   If it allows multiple parameters (and)
 * @param comparators   Allowed comparators used with parameter
 * @param modifiers     Allowed modifiers used with parameter
 * @param components    URL of search parameter definitions for children of composite parameters
 */
case class FHIRSearchParameter(
                                name:String,
                                url:String,
                                base:Set[String],
                                ptype:String,
                                expression:Option[String],
                                xpath:Option[String],
                                target:Set[String],
                                multipleOr:Option[Boolean],
                                multipleAnd:Option[Boolean],
                                comparators:Set[String],
                                modifiers:Set[String],
                                components:Set[String]
                              )

trait IFHIRFoundationResourceParser {
  /**
   * Parse a FHIR Capability Statement into our compact form
   * @param capabilityStmt  CapabilityStatement resource in parsed JSON format
   * @return
   */
  def parseCapabilityStatement(capabilityStmt:Resource):FHIRCapabilityStatement

  /**
   * Parse a FHIR SearchParameter definition into our compact form
   * @param searchParameter SearchParameter resource in parsed JSON format
   * @return
   */
  def parseSearchParameter(searchParameter:Resource):FHIRSearchParameter


  /**
   * Parse a FHIR OperationDefinition  into our compact form
   * @param operationDefinition OperationDefinition resource in parsed JSON format
   * @param classPath           Classpath for the operation implementation if not given in the resource itself
   * @return
   */
  def parseOperationDefinition(operationDefinition:Resource, classPath:Option[String] = None):OperationConf

  /**
   * Parse a FHIR CompartmentDefinition into our compact form
   * @param compartmentDefinition CompartmentDefinition resource in parsed JSON format
   * @return
   */
  def parseCompartmentDefinition(compartmentDefinition:Resource):(String, Map[String, Set[String]])
}

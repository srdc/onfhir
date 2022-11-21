package io.onfhir.config

/**
 * Compact form of FHIR Compartment definition
 * @param url       Canonical url
 * @param code      Code for compartment (e.g. Patient, Practitioner, etc)
 * @param relations Relations with all resource types i.e. resource type -> name of search parameters
 */
case class FHIRCompartmentDefinition(url:String, code:String, relations:Map[String, Set[String]])


package io.onfhir.mapping.model

/**
 * Represents FHIR Structure Map
 * @param url             URL of the structure map
 * @param structureDefs   Structure Definitions for target and source that this map is defined on or for
 * @param groups          Defined groups
 */
case class StructureMap(url:String, structureDefs:Seq[(String, String)], groups:Map[String, StructureMapGroup])

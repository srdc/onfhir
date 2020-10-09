package io.onfhir.mapping.engine
import io.onfhir.api.Resource
import io.onfhir.mapping.model.StructureMap
import io.onfhir.mapping.parsers.{BaseStructureMapParser, IMappingParser}

class MappingUtilityWithSuppliedMaps(structureMaps:Map[String, Resource], mappingParser: IMappingParser) extends IMappingUtility {

  val parsedMaps:Map[String, StructureMap] = structureMaps.mapValues(r => mappingParser.parseStructureMap(r))

  override def getStructureMap(url: String): Option[StructureMap] = parsedMaps.get(url)
}

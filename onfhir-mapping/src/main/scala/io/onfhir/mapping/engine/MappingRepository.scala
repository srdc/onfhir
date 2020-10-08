package io.onfhir.mapping.engine

import io.onfhir.mapping.model.StructureMap

trait IMappingRepository {

  def getStructureMap(url:String):Option[StructureMap]

  //def getConceptMap(url:String):Option[ConceptMap]
}


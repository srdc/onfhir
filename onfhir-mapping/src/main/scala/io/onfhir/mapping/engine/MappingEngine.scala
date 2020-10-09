package io.onfhir.mapping.engine

import io.onfhir.api.Resource
import io.onfhir.mapping.model.StructureMappingException
import org.json4s.JsonAST.JObject

class MappingEngine(mappingUtility:IMappingUtility) {

  /**
   * Execute the mapping defined by the given group in the StructureMap resource given by the url
   * @param url     Canonical url of StructureMap
   * @param group   Group to start execution (StructureMap.group)
   * @param inputs  Named source inputs for the execution
   * @return
   */
  def mapStructure(url:String, group:String, inputs:Map[String,Resource]):Map[String,Resource] = {
    mappingUtility.getStructureMap(url) match {
      case None =>
        throw new StructureMappingException(s"StructureMap with given url $url cannot be found!")
      case Some(structureMap) =>
        if(structureMap.groups.head.sourceInputs.length != 1)
          throw new StructureMappingException(s"Main mapping group of StructureMap with given url $url need more than input!")

        val mainGroup = structureMap.groups.find(_.name == group)
        if(mainGroup.isEmpty)
          throw new StructureMappingException(s"No such mapping group $group defined in StructureMap with given url $url!")

        val executor = new StructureMapExecutor(structureMap, inputs, mappingUtility)
        executor.executeMapping(mainGroup.get)
        executor.getMappingResults()
    }
  }

  /**
   * Execute the mapping defined in the StructureMap resource given by the url. First group will be called and it needs
   * to have a single source input
   * @param url     Canonical url of StructureMap
   * @param input   Source input for the execution (resource to be transformed)
   * @return
   */
  def mapStructure(url:String, input:Resource):Resource = {
    mappingUtility.getStructureMap(url) match {
      case None =>
        throw new StructureMappingException(s"StructureMap with given url $url cannot be found!")
      case Some(structureMap) =>
        if(structureMap.groups.head.sourceInputs.length != 1)
          throw new StructureMappingException(s"Main mapping group of StructureMap with given url $url need more than input!")

        val executor = new StructureMapExecutor(structureMap, Map(structureMap.groups.head.sourceInputs.head._1 -> input), mappingUtility)
        executor.executeMapping()
        executor.getMappingResults().head._2
    }
  }

  /**
   * Execute the mapping defined in the StructureMap resource given by the url. First group will be called and the inputs
   * will be given in order of definition.
   * @param url     Canonical url of StructureMap
   * @param inputs  Source inputs for the execution in order of defined inputs of the main group in map
   * @return
   */
  def mapStructure(url:String, inputs:Seq[Resource]):Seq[Resource] = {
    mappingUtility.getStructureMap(url) match {
      case None =>
        throw new StructureMappingException(s"StructureMap with given url $url cannot be found!")
      case Some(structureMap) =>
        if(structureMap.groups.head.sourceInputs.length != inputs.length)
          throw new StructureMappingException(s"Given inputs does not match with expected inputs by the main mapping group ${structureMap.groups.head.name} of StructureMap with given url $url!")

        val sourceInputs = structureMap.groups.head.sourceInputs.map(_._1).zip(inputs).toMap

        val executor = new StructureMapExecutor(structureMap, sourceInputs, mappingUtility)
        executor.executeMapping()
        executor.getMappingResults().values.toSeq
    }
  }
}

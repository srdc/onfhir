package io.onfhir.mapping.engine

import io.onfhir.mapping.model.StructureMap
import org.json4s.JsonAST.{JArray, JNull, JObject, JString, JValue}

trait IMappingUtility {

  def getStructureMap(url:String):Option[StructureMap]

  /**
   * Create a data type object
   * @param _type
   * @return
   */
  def createResource(_type:String, profile:Option[String] = None):JValue =
    JObject(
      (
        Seq("resourceType" -> JString(_type)) ++
        profile.toSeq.map(p =>
          "meta" -> JObject(
            "profile" -> JArray(List(JString(p)))
          )
        )
      ).toList
    )

  def createType(_type:String):JValue = {
    if(_type.head.isLower)
      JNull //Otherwise create null
    else
      JObject() //For complex data types create an empty object
  }
  //def getConceptMap(url:String):Option[ConceptMap]
}


package io.onfhir.api.client

import io.onfhir.api.FHIR_CONTENT_TYPES
import org.json4s.{JArray, JInt, JString, JValue}
import org.json4s.JsonAST.JObject

import scala.collection.mutable.ListBuffer

class FhirPathPatchRequestBuilder(patchRequestBuilder: FhirPatchRequestBuilder) extends FhirRequestBuilder(patchRequestBuilder.onFhirClient, patchRequestBuilder.request) {
  type This = FhirPathPatchRequestBuilder
  val parameters:ListBuffer[JObject] = new ListBuffer[JObject]

  protected override def compile():Unit = {
    super.compile()
    request.contentType = Some(FHIR_CONTENT_TYPES.FHIR_JSON_CONTENT_TYPE)

    request.resource = Some(
      JObject(
        "resourceType" -> JString("Parameters"),
        "parameter" -> JArray(parameters.toList)
      )
    )
  }

  /**
   * FHIR Path Patch add operation
   * @param path
   * @param name
   * @param value
   * @return
   */
  def patchAdd(path:String, name:String, value:(String, JValue)):FhirPathPatchRequestBuilder = {
    parameters.append(
      JObject(
        "name"-> JString("operation"),
        "part" -> JArray(List(
          JObject(
            "name" -> JString("type"),
            "valueCode" -> JString("add")
          ),
          JObject(
            "name" -> JString("path"),
            "valueString" -> JString(path)
          ),
          JObject(
            "name" -> JString("name"),
            "valueString" -> JString(name)
          ),
          JObject(
            "name" -> JString("value"),
            "value" + value._1.capitalize -> value._2
          )
        ))
      )
    )
    this
  }

  /**
   * FHIR Path Patch delete operation
   * @param path
   * @return
   */
  def patchDelete(path:String):FhirPathPatchRequestBuilder = {
    parameters.append(
      JObject(
        "name"-> JString("operation"),
        "part" -> JArray(List(
          JObject(
            "name" -> JString("type"),
            "valueCode" -> JString("delete")
          ),
          JObject(
            "name" -> JString("path"),
            "valueString" -> JString(path)
          )
        ))
      )
    )
    this
  }

  /**
   * FHIR Path Patch insert operation
   * @param path
   * @param index
   * @param value
   * @return
   */
  def patchInsert(path:String, index:Int, value:(String, JValue)):FhirPathPatchRequestBuilder = {
    parameters.append(
      JObject(
        "name"-> JString("operation"),
        "part" -> JArray(List(
          JObject(
            "name" -> JString("type"),
            "valueCode" -> JString("insert")
          ),
          JObject(
            "name" -> JString("path"),
            "valueString" -> JString(path)
          ),
          JObject(
            "name" -> JString("index"),
            "valueInteger" -> JInt(index)
          ),
          JObject(
            "name" -> JString("value"),
            "value" + value._1.capitalize -> value._2
          )
        ))
      )
    )
    this
  }

  /**
   * FHIR Path Patch insert operation
   * @param path
   * @param source
   * @param destination
   * @return
   */
  def patchMove(path:String, source:Int, destination:Int):FhirPathPatchRequestBuilder = {
    parameters.append(
      JObject(
        "name"-> JString("operation"),
        "part" -> JArray(List(
          JObject(
            "name" -> JString("type"),
            "valueCode" -> JString("move")
          ),
          JObject(
            "name" -> JString("path"),
            "valueString" -> JString(path)
          ),
          JObject(
            "name" -> JString("source"),
            "valueInteger" -> JInt(source)
          ),
          JObject(
            "name" -> JString("destination"),
            "valueInteger" -> JInt(destination)
          )
        ))
      )
    )
    this
  }

  def patchReplace(path:String,value:(String, JValue)):FhirPathPatchRequestBuilder = {
    parameters.append(
      JObject(
        "name"-> JString("operation"),
        "part" -> JArray(List(
          JObject(
            "name" -> JString("type"),
            "valueCode" -> JString("replace")
          ),
          JObject(
            "name" -> JString("path"),
            "valueString" -> JString(path)
          ),
          JObject(
            "name" -> JString("value"),
            "value" + value._1.capitalize -> value._2
          )
        ))
      )
    )
    this
  }

}

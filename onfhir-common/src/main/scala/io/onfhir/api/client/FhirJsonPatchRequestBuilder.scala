package io.onfhir.api.client

import org.json4s.JsonAST.{JArray, JObject, JString, JValue}

import scala.collection.mutable.ListBuffer

class FhirJsonPatchRequestBuilder(patchRequestBuilder: FhirPatchRequestBuilder) extends FhirRequestBuilder(patchRequestBuilder.onFhirClient, patchRequestBuilder.request) {
  type This = FhirJsonPatchRequestBuilder

  private val parameters:ListBuffer[JObject] = new ListBuffer[JObject]

  override protected def compile(): Unit = {
    request.contentType = Some("application/json-patch+json")

    request.resource = Some(JObject("patches" -> JArray(parameters.toList)))
  }

  def patchAdd(path:String, value:JValue):FhirJsonPatchRequestBuilder = {
    parameters.append(JObject(
      "op" -> JString("add"),
      "path" -> JString(path),
      "value"-> value
    ))
    this
  }

  def patchCopy(from:String, path:String):FhirJsonPatchRequestBuilder = {
    parameters.append(JObject(
      "op" -> JString("copy"),
      "from" -> JString(from),
      "path"-> JString(path)
    ))
    this
  }

  def patchMove(from:String, path:String):FhirJsonPatchRequestBuilder = {
    parameters.append(JObject(
      "op" -> JString("move"),
      "from" -> JString(from),
      "path"-> JString(path)
    ))
    this
  }

  def patchRemove(path:String):FhirJsonPatchRequestBuilder = {
    parameters.append(JObject(
      "op" -> JString("remove"),
      "path"-> JString(path)
    ))
    this
  }

  def patchReplace(path:String, value:JValue):FhirJsonPatchRequestBuilder = {
    parameters.append(JObject(
      "op" -> JString("replace"),
      "path"-> JString(path),
      "value" -> value
    ))
    this
  }

}

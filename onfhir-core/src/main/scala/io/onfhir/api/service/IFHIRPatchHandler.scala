package io.onfhir.api.service

import io.onfhir.api.Resource
import org.json4s.JsonAST.{JArray, JNothing, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s._
import scala.util.Try

/**
 * Defined patch operations
 */
object PATCH_OPERATIONS {
  val ADD = "add"
  val REMOVE = "remove"
  val REPLACE = "replace"
  val MOVE = "move"
  val COPY = "copy"
  val TEST = "test"
  //Addinitional from FHIR Patch
  val INSERT = "insert"
  val DELETE = "delete"
}

trait IFHIRPatchHandler {

  /**
   * Apply the patch to the resource and return the new updated content
   * @param patch      Patch content (e.g. JSON Patch, FHIR Patch, ...)
   * @param rtype      Resource type
   * @param resource   Resource content
   * @return           New resource content
   */
  def applyPatch(patch:Resource, rtype:String, resource: Resource):Resource



}

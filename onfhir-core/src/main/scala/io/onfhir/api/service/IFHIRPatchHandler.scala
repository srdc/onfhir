package io.onfhir.api.service

import io.onfhir.api.Resource
import org.json4s.JsonAST.JValue

/**
 * Represent the FHIR patch item
 * @param op     FHIR patch operation
 * @param path   Parsed FHIR patch path
 * @param from   Parsed FHOR patch path for copy/move
 * @param value  Value to patch
 */
case class PatchItem(op:String, path:Seq[(String, Option[Int])], from:Seq[(String, Option[Int])], value:Option[JValue] = None)

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
   * @param resource   Resource content
   * @return           New resource content
   */
  def applyPatch(patch:Resource, resource: Resource):Resource

}

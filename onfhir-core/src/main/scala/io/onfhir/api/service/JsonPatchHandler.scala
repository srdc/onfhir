package io.onfhir.api.service
import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.exception.{BadRequestException, PreconditionFailedException}
import io.onfhir.util.JsonFormatter._

import scala.util.Try
import org.json4s.JsonAST.{JArray, JNothing, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s._

/**
 * Represent the FHIR patch item
 * @param op     FHIR patch operation
 * @param path   Parsed FHIR patch path
 * @param from   Parsed FHOR patch path for copy/move
 * @param value  Value to patch
 */
case class PatchItem(op:String, path:Seq[(String, Option[Int])], from:Seq[(String, Option[Int])], value:Option[JValue] = None)

object JsonPatchHandler extends IFHIRPatchHandler {
  /**
   * Apply the patch to the resource and return the new updated content
   *
   * @param patch    Patch content (e.g. JSON Patch, FHIR Patch, ...)
   * @param resource Resource content
   * @return New resource content
   */
  override def applyPatch(patch: Resource, rtype:String, resource: Resource): Resource = {
    //Parse the patch request
    val patches = parsePatch(patch)
    // Apply the pathces
    val updatedResource = applyPatches(resource, patches)

    updatedResource
  }


  /**
   * Convert path element back to string path for debug purposes
   * @param path Path of patch
   * @return
   */
  private def convertPathToString(path:Seq[(String, Option[Int])]):String = {
    "/" + path.map(i=> {
      i._2 match {
        case None => i._1
        case Some(-1) => i._1 + "/-"
        case Some(o)=>   i._1 + "/"+o.toString
      }
    }).mkString("/")
  }

  /**
   * Apply all patches if test opearations are OK
   * @param originalResource Original FHIR resource
   * @param patches          Parsed Patch Items
   * @return
   */
  private def applyPatches(originalResource:Resource, patches:Seq[PatchItem]):Resource = {
    //Run the test operations
    val testResult:Boolean =
      patches
        .filter(_.op == PATCH_OPERATIONS.TEST)
        .map(t => patchTest(originalResource, t.path, t.value))
        .forall(t => t)

    var finalResource = originalResource

    if(testResult) {
      patches.foreach(pitem => {
        pitem.op match {
          case PATCH_OPERATIONS.ADD =>
            try {
              finalResource = patchAdd(finalResource, pitem.path, pitem.value.get)
            } catch {
              case e:Exception => throw new BadRequestException(Seq(OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid JSON patch operation (add); invalid path '" + convertPathToString(pitem.path) +"'"),
                Nil
              )))
            }
          case PATCH_OPERATIONS.COPY =>
            try {
              finalResource = patchCopy(finalResource, pitem.from, pitem.path)
            } catch {
              case e:Exception => throw new BadRequestException(Seq(OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid JSON patch operation (copy): invalid path '" + convertPathToString(pitem.path)+"' or invalid from '"+convertPathToString(pitem.from)+"'"),
                Nil
              )))
            }
          case PATCH_OPERATIONS.MOVE =>
            try {
              finalResource = patchMove(finalResource, pitem.from, pitem.path)
            } catch {
              case e:Exception => throw new BadRequestException(Seq(OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid JSON patch operation (move): invalid path '" + convertPathToString(pitem.path)+"' or invalid from '"+convertPathToString(pitem.from)+"'"),
                Nil
              )))
            }
          case PATCH_OPERATIONS.REPLACE =>
            try {
              finalResource = patchReplace(finalResource, pitem.path, pitem.value.get)
            }catch {
              case e:Exception => throw new BadRequestException(Seq(OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid JSON patch operation (replace); invalid path '" + convertPathToString(pitem.path) +"'"),
                Nil
              )))
            }
          case PATCH_OPERATIONS.REMOVE =>
            try {
              finalResource = patchRemove(finalResource, pitem.path)
            } catch {
              case e:Exception => throw new BadRequestException(Seq(OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid JSON patch operation (remove); invalid path '" + convertPathToString(pitem.path) +"'"),
                Nil
              )))
            }
          case PATCH_OPERATIONS.TEST =>
        }
      })
    } else {
      throw new PreconditionFailedException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.INFORMATION,
          FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
          None,
          Some(s"JSON patch tests failed, so skipping patch operation..."),
          Nil
        ))
      )
    }
    originalResource
  }

  /**
   * Parse a patch request
   * @param patches  Patch content
   * @return
   */
  def parsePatch(patches: Resource): Seq[PatchItem] ={
    patches \ "patches" match {
      case JArray(values) => parseJsonPatch(values.map(_.asInstanceOf[JObject]))
      case _ =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.INFORMATION,
            FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
            None,
            Some(s"Only supporting Json patch for now !!!"),
            Nil
          )
        ))
    }
  }


  /**
   * Parse the JsonPatch objects See http://jsonpatch.com/. e.g
    [
  { "op": "replace", "path": "/baz", "value": "boo" },
  { "op": "add", "path": "/hello", "value": ["world"] },
  { "op": "remove", "path": "/foo"}
    ]
   * @param patches  Patch contents
   * @return
   */
  def parseJsonPatch(patches:Seq[Resource]):Seq[PatchItem] = {
    patches.map(patch => {
      val path = (patch \ "path").extractOpt[String]
      val value = (patch \ "value").extractOpt[JValue]
      val from = (patch \ "from").extractOpt[String]
      val op = (patch \ "op").extractOpt[String]
      op match {
        //Op, Path, Value
        case Some(PATCH_OPERATIONS.ADD) | Some(PATCH_OPERATIONS.REPLACE) | Some(PATCH_OPERATIONS.TEST) =>
          if (path.isEmpty || value.isEmpty || !path.get.isInstanceOf[String])
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json-patch , 'path' or 'value' element is not found or invalid in '${patch.toJson}'"),
                Nil
              )
            ))
          else
            PatchItem(op.get, parseJsonPointerPath(path.get), Seq.empty, value)
        //Op, Path
        case Some(PATCH_OPERATIONS.REMOVE) =>
          if (path.isEmpty || !path.get.isInstanceOf[String])
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json-patch , 'path' element is not found or invalid in '${patch.toJson}'"),
                Nil
              )
            ))
          else PatchItem(PATCH_OPERATIONS.REMOVE, parseJsonPointerPath(path.get), Seq.empty)
        //Op, Path, From
        case Some(PATCH_OPERATIONS.COPY) | Some(PATCH_OPERATIONS.MOVE) =>
          if (path.isEmpty || from.isEmpty || !path.get.isInstanceOf[String] || !from.get.isInstanceOf[String])
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json-patch , 'path' or 'from' element is not found or invalid in '${patch.toJson}'"),
                Nil
              )
            ))
          else
            PatchItem(op.get, parseJsonPointerPath(path.get), parseJsonPointerPath(from.get))

        case _ => throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Invalid Json patch, 'op' element not found or invalid in '${patch.toJson}'"),
            Nil
          )
        ))
      }
    })
  }

  /**
   * Parse the path into elements
   * @param path  JSON path
   * @return
   */
  def parseJsonPointerPath(path:String):Seq[(String, Option[Int])] = {
    val pathActual:String = if(path.startsWith("/")) path.substring(1) + "/$" else path + "/$"

    val pairs = pathActual.split("/").toSeq.sliding(2).toSeq

    pairs.flatMap(pair => {
      val first = pair.head
      val second = pair.last
      first match {
        case "-" => Nil
        case i if Try(i.toInt).toOption.isDefined => Nil
        case o => second match {
          case "-" => Seq((o, Some(-1)))
          case _=> Seq((o, Try(second.toInt).toOption))
        }
      }
    })
  }

  /**
   * Run a JSON operation on a given path and return the updated resource back
   * @param input JObject to run the path and operation
   * @param path Remaining JSON pointer path
   * @param jsonOperation Operation on the last item on the path
   * @return
   */
  protected def runJsonPatchOperation(input:Resource, path:Seq[(String, Option[Int])], jsonOperation:(JObject, (String, Option[Int])) => JObject):Resource = {
    //If we are on the last JObject, apply the operation on it
    if(path.size == 1) {
      jsonOperation(input,path.head)
    } else {
      val nextPathItem = path.head
      input.transformField {
        //Transform the field on the path
        case (nextPathItem._1, obj) =>
          //Otherwise go deep recursively
          nextPathItem._1 ->
            (nextPathItem._2 match {
              //If element is JObject directly run on it
              case None if obj.isInstanceOf[JObject] => runJsonPatchOperation(obj.asInstanceOf[JObject], path.tail, jsonOperation)
              //If element is JArray, go deep on the given indexed element
              case Some(i: Int) if obj.isInstanceOf[JArray] =>
                val arr = obj.asInstanceOf[JArray]
                i match {
                  case -1 => JArray(arr.arr.dropRight(1) ++ Seq(runJsonPatchOperation(arr.arr.last.asInstanceOf[JObject], path.tail, jsonOperation)))
                  case p => JArray(arr.arr.slice(0, p - 1) ++ Seq(runJsonPatchOperation(arr.arr.apply(p).asInstanceOf[JObject], path.tail, jsonOperation)) ++ arr.arr.slice(p, arr.arr.length))
                }

              //Otherwise return same obj
              case _ => obj //No change
            })
      }.asInstanceOf[JObject]
    }
  }

  /**
   * Handle the Add operation of Patch
   * @param originalResource   Original resource content
   * @param path               Parsed path elements
   * @param value              JSON Value to add
   * @return
   */
  protected def patchAdd(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue):Resource = {
    //Operation definition
    def addOperation(resource:JObject, lpath:(String, Option[Int])):JObject = {
      lpath match {
        case (key, None) => resource ~ (key -> value)
        case (key, Some(i)) =>
          val updated = resource \ key match {
            case JArray(values) => i match {
              case -1 => JArray(values.:+(value))
              case 0 => JArray(values.+:(value))
              case _ =>
                val split = values.splitAt(i)
                JArray(split._1 ++ Seq(value) ++ split._2)
            }
            case any => any
          }
          resource ~ (key -> updated)
      }
    }
    runJsonPatchOperation(originalResource, path, addOperation)
  }

  /**
   * Handle patch remove
   * @param originalResource   Original FHIR resource
   * @param path               Path elements indicating the FHIR element to remove
   * @return
   */
  protected def patchRemove(originalResource:Resource, path:Seq[(String, Option[Int])]) = {
    //Operation definition
    def removeOperation(resource:JObject, lpath:(String, Option[Int])):JObject = {
      lpath match {
        case (key, None)=>
          resource.removeField {
            case (key, _) => true
            case _ => false
          }.asInstanceOf[JObject]
        case (key, Some(i)) =>
          resource \ key match {
            case JArray(values) =>
              val updated = i match {
                case -1 => JArray(values.dropRight(1))
                case 0 => JArray(values.tail)
                case _ =>
                  val split = values.splitAt(i)
                  JArray(split._1 ++ split._2.tail)
              }
              resource ~ (key -> updated)
            case _ => resource
          }
      }
    }

    runJsonPatchOperation(originalResource, path, removeOperation)
  }

  /**
   *
   * @param originalResource   Original FHIR resource content
   * @param path               Path elements indicating the FHIR element to be replaced
   * @param value              JSON value to replace
   * @return
   */
  protected def patchReplace(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue) = {
    patchRemove(originalResource, path)
    patchAdd(originalResource, path, value)
  }

  /**
   * Get a Json Value by path (if not exist return JNothing)
   * @param originalResource   Original FHIR resource content
   * @param path               Path elements indicating the FHIR element to retrieve
   * @return
   */
  private def getValueByPath(originalResource:Resource, path:Seq[(String, Option[Int])]):JValue = {
    Try(path.foldLeft(originalResource.asInstanceOf[JValue])((r,p) => {
      p._2 match {
        case None => r \ p._1
        case Some(-1) => r \ p._1 match {
          case JArray(values) => values.last
          case _ => JNothing
        }
        case Some(i) => (r \ p._1)(i)
      }
    })).toOption.getOrElse(JNothing)
  }

  /**
   *
   * @param originalResource Original FHIR resource content
   * @param from             Path elements indicating the FHIR element to be copied
   * @param path             Path elements indicating the location to copy
   * @return
   */
  protected def patchCopy(originalResource:Resource, from:Seq[(String, Option[Int])], path:Seq[(String, Option[Int])]):Resource = {
    getValueByPath(originalResource, from) match {
      case JNothing => originalResource
      case other => patchAdd(originalResource, path, other)
    }
  }

  /**
   *
   * @param originalResource Original FHIR resource content
   * @param from             Path elements indicating the FHIR element to be moved
   * @param path             Path elements indicating the location to move
   * @return
   */
  protected def patchMove(originalResource:Resource, from:Seq[(String, Option[Int])], path:Seq[(String, Option[Int])]) = {
    getValueByPath(originalResource, from) match {
      case JNothing => originalResource
      case other =>
        patchRemove(originalResource, from)
        patchAdd(originalResource, path, other)
    }
  }

  /**
   *
   * @param originalResource Original FHIR resource content
   * @param path             Path elements indicating the FHIR element to be tested
   * @param value            Json value for equality test
   */
  protected def patchTest(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue):Boolean = {
    getValueByPath(originalResource, path).equals(value)
  }
}

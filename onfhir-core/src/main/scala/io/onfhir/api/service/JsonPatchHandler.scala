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
   * Apply all patches in sequential order
   * @param originalResource Original FHIR resource
   * @param patches          Parsed Patch Items
   * @return
   */
  private def applyPatches(originalResource:Resource, patches:Seq[PatchItem]):Resource = {
    patches.zipWithIndex.foldLeft(originalResource)((r,p) => {
      val result = applyPatch(r, p._1, p._2)
      result
    })
  }

  /**
   * Apply a patch operation on resource and return updated resource content
   * @param resource
   * @param pitem
   * @param ind
   * @return
   */
  def applyPatch(resource: Resource, pitem:PatchItem, ind:Int):Resource = {
    pitem.op match {
      case PATCH_OPERATIONS.ADD => patchAdd(resource, pitem.path, pitem.value.get, ind)
      case PATCH_OPERATIONS.COPY => patchCopy(resource, pitem.from, pitem.path, ind)
      case PATCH_OPERATIONS.MOVE => patchMove(resource, pitem.from, pitem.path, ind)
      case PATCH_OPERATIONS.REPLACE => patchReplace(resource, pitem.path, pitem.value.get, ind)
      case PATCH_OPERATIONS.REMOVE => patchRemove(resource, pitem.path, ind)
      case PATCH_OPERATIONS.TEST =>
        if(!patchTest(resource, pitem.path, pitem.value, ind))
            throw new PreconditionFailedException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.INFORMATION,
                FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
                None,
                Some(s"JSON patch test failed, so skipping patch operation..."),
                Seq(s"[$ind].path")
              ))
            )
         resource
    }
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
   * Parse the JSON Pointer path into its path elements
   * @param path  JSON Pointer path
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
  protected def runJsonPatchOperation(input:Resource, path:Seq[(String, Option[Int])], jsonOperation:(JObject, (String, Option[Int])) => JObject, opInd:Int):Resource = {
    //If we are on the last JObject, apply the operation on it
    if(path.size == 1) {
      jsonOperation(input,path.head)
    } else {
      val nextPathItem = path.head
      input.obj.map {
        //Transform the field on the path
        case (nextPathItem._1, obj) =>
          //Otherwise go deep recursively
          nextPathItem._1 ->
            (nextPathItem._2 match {
              //If element is JObject directly run on it
              case None if obj.isInstanceOf[JObject] =>
                runJsonPatchOperation(obj.asInstanceOf[JObject], path.tail, jsonOperation, opInd)
              //If element is JArray, go deep on the given indexed element
              case Some(i: Int) if obj.isInstanceOf[JArray] =>
                val arr = obj.asInstanceOf[JArray].arr
                //If it is out of index
                if(i>=arr.length || arr.exists(!_.isInstanceOf[JObject]))
                  throw new BadRequestException(Seq(
                    OutcomeIssue(
                      FHIRResponse.SEVERITY_CODES.ERROR,
                      FHIRResponse.OUTCOME_CODES.INVALID,
                      None,
                      Some(s"Invalid Json Patch operation, the path does not match any element!"),
                      Seq(s"[$opInd].path")
                    )
                  ))
                JArray(arr.zipWithIndex.map(e =>
                  if(e._2 == i)
                    runJsonPatchOperation(e._1.asInstanceOf[JObject], path.tail, jsonOperation, opInd)
                  else
                    e._1
                ))
              //Otherwise it is an error
              case _ =>
                throw new BadRequestException(Seq(
                  OutcomeIssue(
                    FHIRResponse.SEVERITY_CODES.ERROR,
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some(s"Invalid Json Patch operation, the path does not match any element!"),
                    Seq(s"[$opInd].path")
                  )
                ))
            })
        case oth => oth
      }
    }
  }

  /**
   * Handle the Add operation of Patch
   * @param originalResource   Original resource content
   * @param path               Parsed path elements
   * @param value              JSON Value to add
   * @return
   */
  protected def patchAdd(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue, opInd:Int):Resource = {
    //Operation definition
    def addOperation(resource:JObject, lpath:(String, Option[Int])):JObject = {
      var isReplaced = false
      val result = JObject(resource.obj.map {
        //Insert the element to the array at given index
        case (lpath._1, JArray(arr)) if lpath._2.nonEmpty =>
          val i = lpath._2.get
          if(i > arr.length)
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json Patch operation 'add', the index must not be greater than array size ${arr.length}!"),
                Seq(s"[$opInd].path")
              )
            ))
          isReplaced = true
          if(i == -1)
            lpath._1 -> JArray(arr:+value)
          else
            lpath._1 -> JArray((arr.slice(0, i):+ value) ++ arr.drop(i))
        //If value is not an array, but index is given
        case (lpath._1, _) if lpath._2.nonEmpty =>
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid Json Patch operation 'add', the element indicated by the path is not an array although the array index is given!"),
              Seq(s"[$opInd]")
            )
          ))
        //Replace the array itself
        case (lpath._1, arr:JArray) if lpath._2.isEmpty =>
          if(value.isInstanceOf[JArray]) {
            isReplaced = true
            lpath._1 -> value
          } else
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json Patch operation 'add', the element indicated by the path was an array but given value is not an array!"),
                Seq(s"[$opInd]")
              )
            ))
        //Replace the object and values
        case (lpath._1, objOrValue) if lpath._2.isEmpty =>
          isReplaced = true
          if(value.isInstanceOf[JArray])
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json Patch operation 'add', the element indicated by the path is array but value to replace it is not!"),
                Seq(s"[$opInd]")
              )
            ))
          lpath._1 -> value
        case oth => oth
      })
      //If the element is not replaced, append it
      if(!isReplaced) {
        //Normal replace
        if (lpath._2.isEmpty)
          result ~ (lpath._1 -> value)
        //create an array, if we are appending
        else if (lpath._2.get == -1 || lpath._2.get == 0)
          result ~ (lpath._1 -> JArray(List(value)))
        else
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid Json Patch operation 'add', the index must not be greater than array size ${0}!"),
              Seq(s"[$opInd]")
            )
          ))
      } else
        result
    }
    runJsonPatchOperation(originalResource, path, addOperation, opInd)
  }

  /**
   * Handle patch remove
   * @param originalResource   Original FHIR resource
   * @param path               Path elements indicating the FHIR element to remove
   * @return
   */
  protected def patchRemove(originalResource:Resource, path:Seq[(String, Option[Int])], opInd:Int) = {
    //Operation definition
    def removeOperation(resource:JObject, lpath:(String, Option[Int])):JObject = {
      resource.obj.find(_._1 == lpath._1).map(_._2) match {
        case Some(el) =>
          //If there is no index
          if (lpath._2.isEmpty)
            JObject(resource.obj.filterNot(_._1 == lpath._1))
          else if (el.isInstanceOf[JArray]) {
            val arr = el.asInstanceOf[JArray].arr
            val i = lpath._2.get
            if (i >= arr.length)
              throw new BadRequestException(Seq(
                OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.ERROR,
                  FHIRResponse.OUTCOME_CODES.INVALID,
                  None,
                  Some(s"Invalid Json Patch operation 'remove', the index must not be greater than array size ${0}!"),
                  Seq(s"[$opInd].path")
                )
              ))
            val farr =
              if (i == -1)
                arr.dropRight(1)
              else
                arr.slice(0, i) ++ arr.drop(i + 1)
            JObject(resource.obj.map {
              case (lpath._1, _) => lpath._1 -> JArray(farr)
              case oth => oth
            })
          } else
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json Patch operation 'remove', the path does not match an array element!"),
                Seq(s"[$opInd].path")
              )
            ))
        case None =>
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid Json Patch operation 'remove', the path does not match any element!"),
              Seq(s"[$opInd].path")
            )
          ))
      }
    }
    runJsonPatchOperation(originalResource, path, removeOperation, opInd)
  }

  /**
   *
   * @param originalResource   Original FHIR resource content
   * @param path               Path elements indicating the FHIR element to be replaced
   * @param value              JSON value to replace
   * @return
   */
  protected def patchReplace(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue, opInd:Int) = {
    def replaceOperation(resource:JObject, lpath:(String, Option[Int])):JObject = {
      var isReplaced = false
      val result = JObject(resource.obj.map {
        //Insert the element to the array at given index
        case (lpath._1, JArray(arr)) if lpath._2.nonEmpty =>
          val i = lpath._2.get
          if (i > arr.length)
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json Patch operation 'replace', the index must not be greater than array size ${arr.length}!"),
                Seq(s"[$opInd].path")
              )
            ))
          isReplaced = true
          if (i == -1)
            lpath._1 -> JArray(arr.dropRight(1) :+ value)
          else
            lpath._1 -> JArray((arr.slice(0, i) :+ value) ++ arr.drop(i + 1))
        //If value is not an array, but index is given
        case (lpath._1, _) if lpath._2.nonEmpty =>
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid Json Patch operation 'replace', the element indicated by the path is not an array although the array index is given!"),
              Seq(s"[$opInd]")
            )
          ))
        //Replace the array itself
        case (lpath._1, arr: JArray) if lpath._2.isEmpty =>
          if (value.isInstanceOf[JArray]) {
            isReplaced = true
            lpath._1 -> value
          } else
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json Patch operation 'replace', the element indicated by the path was an array but given value is not an array!"),
                Seq(s"[$opInd]")
              )
            ))
        //Replace the object and values
        case (lpath._1, objOrValue) if lpath._2.isEmpty =>
          if (value.isInstanceOf[JArray])
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid Json Patch operation 'replace', the element indicated by the path is not array but replaced value is an array!"),
                Seq(s"[$opInd]")
              )
            ))
          isReplaced = true
          lpath._1 -> value
        case oth => oth
      })

      if(!isReplaced)
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Invalid Json Patch operation 'replace', the path does not match any element!"),
            Seq(s"[$opInd]")
          )
        ))
      result
    }


   runJsonPatchOperation(originalResource, path, replaceOperation, opInd)
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
  protected def patchCopy(originalResource:Resource, from:Seq[(String, Option[Int])], path:Seq[(String, Option[Int])], opInd:Int):Resource = {
    getValueByPath(originalResource, from) match {
      case JNothing =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Invalid Json Patch operation 'copy', the from path does not match any element!"),
            Seq(s"[$opInd].from")
          )))
      case other =>
        patchAdd(originalResource, path, other, opInd)
    }
  }

  /**
   *
   * @param originalResource Original FHIR resource content
   * @param from             Path elements indicating the FHIR element to be moved
   * @param path             Path elements indicating the location to move
   * @return
   */
  protected def patchMove(originalResource:Resource, from:Seq[(String, Option[Int])], path:Seq[(String, Option[Int])], opInd:Int) = {
    getValueByPath(originalResource, from) match {
      case JNothing =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Invalid Json Patch operation 'move', the from path does not match any element!"),
            Seq(s"[$opInd].from")
          )))
      case other =>
        val temp = patchRemove(originalResource, from, opInd)
        patchAdd(temp, path, other, opInd)
    }
  }

  /**
   *
   * @param originalResource Original FHIR resource content
   * @param path             Path elements indicating the FHIR element to be tested
   * @param value            Json value for equality test
   */
  protected def patchTest(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue, opInd:Int):Boolean = {
    getValueByPath(originalResource, path).equals(value)
  }
}

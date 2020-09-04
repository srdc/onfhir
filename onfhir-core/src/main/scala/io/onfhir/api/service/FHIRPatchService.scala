package io.onfhir.api.service

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.FhirConfigurationManager.fhirValidator
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.{BadRequestException, NotFoundException, PreconditionFailedException}
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JNothing, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.util.Try

/**
  * Created by tuncay on 4/28/2017.
  */
class FHIRPatchService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession) {

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

  /**
    * Validate if interaction is supported for the request
    * @param fhirRequest FHIR Request
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply {
      val validations =
        if (fhirRequest.resourceId.isDefined)
          validatePatchInteraction(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.ifNoneExist)
        else
          validateConditionalPatchInteraction(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.queryParams, fhirRequest.prefer)

      validations.map(_ =>
        //Extra business rules validations if exist
        FHIRApiValidator.validateExtraRules(fhirRequest)
      )
    }
  }

  /**
    * Perform the interaction
    *
    * @param fhirRequest    FHIR request
    * @param authzContext   Authorization context
    * @param isTesting      If this is a testing operation
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean): Future[FHIRResponse] = {
    if(fhirRequest.resourceId.isDefined)
      patchResource(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.resourceId.get, fhirRequest.ifMatch, fhirRequest.prefer, isTesting)
    else {
      conditionalPatchResource(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.queryParams, fhirRequest.ifMatch, fhirRequest.prefer, isTesting)

    }
  }


  /**
    * Check if this is a valid patch interaction
    * @param resource     Patch resource
    * @param _type        Resource type
    * @param ifNoneExist  IfNoneExist header
    * @return
    */
  private def validatePatchInteraction(resource:Resource, _type:String, ifNoneExist:Option[String]) = {
    //1.1) Validate if "create" is supported for Resource Type
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.PATCH, _type, ifNoneExist.isDefined)
  }

  /**
    * Check if conditional patch is basically valid
    * @param resource           Patch content
    * @param _type              Resource type
    * @param searchParameters   Search parameters for conditional operation
    * @param prefer             FHIR prefer header
    */
  private def validateConditionalPatchInteraction(resource: Resource, _type:String, searchParameters:List[Parameter], prefer:Option[String]) = {
    //1.1) Validate if "update" is supported for Resource Type
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.PATCH, _type, conditional=true)
  }

  /**
    * Patch a resource
    * @param patch    Patch content
    * @param _type    Resource type
    * @param _id      Resource id
    * @param ifMatch  IfMatch header
    * @param prefer   Prefer header
    * @return
    */
  private def patchResource(patch: Resource, _type:String, _id:String, ifMatch:Option[String], prefer:Option[String], isTesting:Boolean) : Future[FHIRResponse] = {
    logger.debug(s"requesting 'patch' for ${_type} with ${_id}...")

    //2) check if resource already exists
    ResourceManager.getResource(_type, _id).flatMap {
      //If no such document, return No content
      case None => Future(FHIRResponse(StatusCodes.NoContent))
      //Otherwise
      case Some(foundResource) =>
        val wasDeleted = FHIRUtil.isDeleted(foundResource)
        val currentVersion = FHIRUtil.extractVersionFromResource(foundResource)

        //2.1) Check if user requested a version aware update
        FHIRApiValidator.validateIfMatch(ifMatch, currentVersion)

        //Parse the patch request
        val patches = parsePatch(patch)
        //3) Apply the pathces
        val updatedResource = applyPatches(FHIRUtil.clearExtraFields(foundResource), patches)
        //4) Validate if new changes are valid
        fhirValidator.validateResource(updatedResource, _type) flatMap { _ =>
          //5) Perform the update operation
          new FHIRUpdateService(transactionSession).performUpdate(updatedResource, _type, Some(_id), prefer, isTesting, Some(currentVersion -> foundResource), wasDeleted)
        }
    }
  }

  /**
    * Conditional Patch (same as conditional update in terms of process)
    * @param patch              Patch content
    * @param _type              Resource type
    * @param searchParameters   Search parameters for conditional operation
    * @param ifMatch            IfMatch header
    * @param prefer             Prefer header
    * @return
    */
  private def conditionalPatchResource(patch: Resource,
                                       _type:String,
                                       searchParameters:List[Parameter],
                                       ifMatch:Option[String],
                                       prefer:Option[String],
                                       isTesting:Boolean
                                      ) : Future[FHIRResponse] = {
    logger.debug(s"requesting conditional 'patch' for ${_type}...")

    ResourceManager.queryResources(_type, searchParameters, count =1).flatMap {
      //No matching
      case (0, _) => throw new NotFoundException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"No matching resource fot her query!"),
          Nil
        )
      ))
      //Only one match
      case (1, Seq(foundResource)) =>
        val (rid, currentVersion, _) = FHIRUtil.extractBaseMetaFields(foundResource)

        //3.2.1 Check if user requested a version aware update
        FHIRApiValidator.validateIfMatch(ifMatch, currentVersion)

        //Parse the patch request
        val patches = parsePatch(patch)
        //3) Apply the pathces
        val updatedResource = applyPatches(FHIRUtil.clearExtraFields(foundResource), patches)
        //4) Validate if new changes are valid
        fhirValidator.validateResource(updatedResource, _type) flatMap { _ =>
          //5) Perform the actual update operation
          new FHIRUpdateService(transactionSession).performUpdate(updatedResource, _type, Some(rid), prefer, isTesting, Some(currentVersion -> foundResource))
        }

      //Multiple matches
      case (_, _ ) =>
        logger.debug("Multiple matches exist with given parameters, return 412 - Precondition Failed")
        throw new PreconditionFailedException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Multiple matches exist with given parameters, for the conditional update"),
            Nil
          )
        ))
    }
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
  def applyPatches(originalResource:Resource, patches:Seq[PatchItem]):Resource = {
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
  private def runJsonPatchOperation(input:Resource, path:Seq[(String, Option[Int])], jsonOperation:(JObject, (String, Option[Int])) => JObject):Resource = {
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
  private def patchAdd(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue):Resource = {
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
  private def patchRemove(originalResource:Resource, path:Seq[(String, Option[Int])]) = {
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
  private def patchReplace(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue) = {
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
  private def patchCopy(originalResource:Resource, from:Seq[(String, Option[Int])], path:Seq[(String, Option[Int])]):Resource = {
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
  private def patchMove(originalResource:Resource, from:Seq[(String, Option[Int])], path:Seq[(String, Option[Int])]) = {
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
  private def patchTest(originalResource:Resource, path:Seq[(String, Option[Int])], value:JValue):Boolean = {
    getValueByPath(originalResource, path).equals(value)
  }


}

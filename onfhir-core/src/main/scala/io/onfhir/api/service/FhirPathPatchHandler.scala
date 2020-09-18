package io.onfhir.api.service
import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.{BaseFhirProfileHandler, FHIRUtil}
import io.onfhir.exception.BadRequestException
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s._

/**
 * FHIR Path Patch operation definitions
 */
sealed trait FhirPatchPatch
case class FhirPathPatchAdd(path:Seq[(String, Option[Int])], name:String, value:JValue) extends FhirPatchPatch
case class FhirPathPatchInsert(path:Seq[(String, Option[Int])], index:Int, value:JValue) extends FhirPatchPatch
case class FhirPathPatchDelete(path:Seq[(String, Option[Int])]) extends FhirPatchPatch
case class FhirPathPatchReplace(path:Seq[(String, Option[Int])], value:JValue) extends FhirPatchPatch
case class FhirPathPatchMove(path:Seq[(String, Option[Int])], source:Int, destination:Int) extends FhirPatchPatch

class FhirPathPatchHandler(profileHandler:BaseFhirProfileHandler) extends IFHIRPatchHandler {
  val opTypes = Set("add", "insert", "delete", "replace", "move")

  /**
   * Apply the patch to the resource and return the new updated content
   *
   * @param patch    Patch content in FHIR Path Patch format (Parameters resource)
   * @param resource Resource content to apply the patches
   * @return         Updated resource content
   */
  override def applyPatch(patch: Resource, rtype:String, resource: Resource): Resource = {
    val patchParams = getPatchParams(patch)
    patchParams.zipWithIndex.foldLeft(resource)((r, p) => { //In order parse and execute the patches
      val patchOp = parseFhirPatchParameter(p._1, r, p._2)
      val result = applyPatchOp(patchOp, rtype, r, p._2)
      result
    })
  }


  /**
   * Apply a patch operation on resource
   * @param patchOp     Patch operation
   * @param resource    Resource content
   * @param ind         Index of the operation for log purposes
   * @return
   */
  private def applyPatchOp(patchOp:FhirPatchPatch, rtype:String, resource: Resource, ind:Int):Resource = {
    patchOp match {
      case FhirPathPatchAdd(path, name, value) => applyAddPatch(path, name, value, resource, rtype, ind)
      case FhirPathPatchInsert(path, index, value) => applyInsertPatch(path, index, value, resource, ind)
      case FhirPathPatchDelete(path) => applyDeletePatch(path, resource, ind)
      case FhirPathPatchReplace(path, value) => applyReplacePatch(path, value, resource, ind)
      case FhirPathPatchMove(path, source, destination) => applyMovePatch(path, source, destination, resource, ind)
    }
  }

  /**
   * Check if the element to add is an array or not
   * @param path
   * @param rtype
   * @return
   */
  private def isOperationAddTargetArray(path:Seq[(String, Option[Int])], rtype:String):Boolean = {
    val pathForAddedElem = path.map(_._1).mkString(".")
    profileHandler.findPathCardinality(pathForAddedElem, rtype)
  }

  /**
   * Supplementary method to handle updating the data by using the delegated operation while finding the path
   * @param input           Resource to update
   * @param path            Path that patch is defined
   * @param jsonOperation   Patch Operation method to handle the update
   * @return
   */
  protected def runPatchOperation(input:Resource, path:Seq[(String, Option[Int])], jsonOperation:(JObject, (String, Option[Int])) => JObject):Resource = {
    if(path.length == 1) {
      jsonOperation(input, path.head)
    } else {
      JObject(
        input.obj.map {
          case (el, value) if el == path.head._1 && value.isInstanceOf[JObject] =>
            path.head._1 -> runPatchOperation(value.asInstanceOf[JObject], path.tail, jsonOperation) //Continue from the element
          case (el, value) if el == path.head._1 && value.isInstanceOf[JArray] =>
            val arrIndex = path.head._2.getOrElse(0)
            path.head._1 ->
                JArray(
                  value.asInstanceOf[JArray]
                    .arr.zipWithIndex
                    .map(v =>
                      if(v._2 == arrIndex)
                        runPatchOperation(v._1.asInstanceOf[JObject], path.tail, jsonOperation)
                      else
                        v._1
                    )
                )
          case oth => oth
        }
      )
    }
  }


  /**
   *
   * @param path
   * @param name
   * @param value
   * @param resource
   * @param rtype
   * @param opInd
   * @return
   */
  private def applyAddPatch(path:Seq[(String, Option[Int])], name:String, value:JValue, resource: Resource, rtype:String,  opInd:Int):Resource = {
    //final path
    val fpath = if(path.length == 1 && path.head._1 == "") Seq(name -> None) else  path :+ (name -> None)

    def addOperation(resource: Resource, lastPath:(String, Option[Int])):Resource = {
        resource.obj.find(_._1 == lastPath._1).map(_._2) match {
         case Some(JArray(arr)) =>
           resource.obj.filterNot(_._1 == lastPath._1) :+
             lastPath._1 -> JArray(arr :+ value) // add the element to end of array
         case Some(_) =>
           //Replace the value if exist, if it is not array
           resource.obj.filterNot(_._1 == lastPath._1) :+
             lastPath._1 -> value
         case None =>
           //If the element does not exist
           if(isOperationAddTargetArray(fpath, rtype))
             resource ~ (lastPath._1 -> JArray(List(value)))
           else
             resource ~ (lastPath._1 -> value)
       }
      }

    runPatchOperation(resource, fpath, addOperation)
  }

  /**
   *
   * @param path
   * @param index
   * @param value
   * @param resource
   * @param opInd
   * @return
   */
  private def applyInsertPatch(path:Seq[(String, Option[Int])], index:Int, value:JValue, resource: Resource, opInd:Int):Resource = {
    def insertOperation(resource: Resource, lastPath:(String, Option[Int])):Resource = {
      JObject(
        resource.obj.map {
          case (lastPath._1, JArray(arr)) =>
            if(index > arr.size)
              throw new BadRequestException(Seq(
                OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.ERROR,
                  FHIRResponse.OUTCOME_CODES.INVALID,
                  None,
                  Some(s"Invalid FHIR Path Patch operation 'insert', given index $index is greater than the size of array!"),
                  Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')", s"Parameters.parameter[$opInd].part.where(name = 'index')")
                )
              ))
            lastPath._1 -> JArray((arr.slice(0,index) :+ value) ++ arr.drop(index))
          case (lastPath._1, _) =>
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid FHIR Path Patch operation, operation 'insert' can only be used on repetitive elements!"),
                Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')")
              )
            ))
          case oth => oth
        }
      )
    }

    if(path.last._2.nonEmpty){
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR Path Patch operation 'insert', operation 'insert' can only be used on repetitive elements"),
          Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')")
        )
      ))
    }

    runPatchOperation(resource, path, insertOperation)
  }

  /**
   *
   * @param path
   * @param resource
   * @param opInd
   * @return
   */
  private def applyDeletePatch(path:Seq[(String, Option[Int])], resource: Resource, opInd:Int):Resource = {
    def deleteOperation(resource: Resource, lastPath:(String, Option[Int])):Resource = {
      if(lastPath._2.isEmpty) //if it is not an an array, just remove the element
        JObject(resource.obj.filterNot(_._1 == lastPath._1))
      else {
        //Otherwise,it should be an array, find the element ad remove it
        val arr = resource.obj.find(_._1 == lastPath._1).get._2.asInstanceOf[JArray].arr
       JObject(
          if(arr.size == 1) //if the array will be empty, remove it
            resource.obj.filterNot(_._1 == lastPath._1)
          else {
            val farr = JArray(arr.slice(0, lastPath._2.get) ++ arr.drop(lastPath._2.get + 1))
            resource.obj.filterNot(_._1 == lastPath._1) :+ lastPath._1 -> farr
          }
       )
      }
    }
    if(path.isEmpty)
      resource
    else
      runPatchOperation(resource, path, deleteOperation)
  }

  /**
   *
   * @param path
   * @param value
   * @param resource
   * @param opInd
   * @return
   */
  private def applyReplacePatch(path:Seq[(String, Option[Int])], value:JValue, resource: Resource, opInd:Int):Resource = {
    def replaceOperation(resource: Resource, lastPath:(String, Option[Int])):Resource = {
      JObject(resource.obj.map {
        case (lastPath._1, JArray(arr)) if lastPath._2.isDefined =>
          val replacedInd = lastPath._2.get
          lastPath._1 -> JArray((arr.slice(0, replacedInd) :+ value) ++ arr.drop(replacedInd +1))
        //Replacing the array
        case (lastPath._1, JArray(arr)) if lastPath._2.isEmpty =>
          lastPath._1 -> JArray(List(value))
        //Replacing a single Json object or value
        case (lastPath._1, _) =>
          lastPath._1 -> value
        case oth => oth
      })
    }

    runPatchOperation(resource, path, replaceOperation)
  }

  /**
   *
   * @param path
   * @param source
   * @param destination
   * @param resource
   * @param opInd
   * @return
   */
  private def applyMovePatch(path:Seq[(String, Option[Int])], source:Int, destination:Int, resource: Resource, opInd:Int):Resource = {
    def moveOperation(resource: Resource, lastPath:(String, Option[Int])):Resource = {
      JObject(resource.obj.map {
        case (lastPath._1, JArray(arr)) =>
          if(source > arr.size)
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid FHIR Path Patch operation 'move', given source $source is greater than the size of array ${arr.size}!"),
                Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')", s"Parameters.parameter[$opInd].part.where(name = 'source')")
              )
            ))
          if(destination > arr.size)
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Invalid FHIR Path Patch operation 'move', given destination $destination is greater than the size of array ${arr.size}!"),
                Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')", s"Parameters.parameter[$opInd].part.where(name = 'destination')")
              )
            ))

          val elem = arr.apply(source)
          val farr =
          if(destination <= source)
            (
              arr.slice(0, destination) :+ elem //get the part before destination and add the element
              ) ++ arr.slice(destination, source) ++ //add the part between the destionation and source
                arr.drop(source+1) //get list after the destination
          else
            (
              arr.slice(0, source) ++ //Get the list before the element to move
                arr.slice(source+1, destination+1) :+ elem //Get the middle part between source and destination and add the element to the end
            ) ++ arr.drop(destination+1)  //Get list after the destination

         lastPath._1 -> JArray(farr)

        case (lastPath._1, _) =>
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid FHIR Path Patch operation 'move', the given path should indicate a repetitive element!"),
              Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')")
            )
          ))
        case oth => oth
      })
    }

    runPatchOperation(resource, path, moveOperation)
  }

  /**
   * Get the patch operation parameters (Parameters.parameter)
   * @param patch
   * @return
   */
  private def getPatchParams(patch:Resource):Seq[JObject] = {
    patch.obj
      .find(p => p._1 == "parameter" && p._2.isInstanceOf[JArray])  //Get the parameter elements that defines each patch operation
      .map(p => p._2.asInstanceOf[JArray].arr)
      .getOrElse(Nil)
      .filter(_.isInstanceOf[JObject])
      .map(_.asInstanceOf[JObject])
      .filter(p => p.obj.exists(e => e._1 == "name" && e._2.extractOpt[String].contains("operation"))) //name of the parameter should be operation
  }

  def extractOrConstructValueFromMultiParam(part:JObject):JValue = {
    part.obj
      .find(_._1.startsWith("value")) match {
      case Some(v) => v._2
      case None =>
        part.obj.find(e =>e._1 == "part")
          .map(_._2.asInstanceOf[JArray])
          .map(a =>
            JObject(
              a.arr
                .map(_.asInstanceOf[JObject])
                .map(p =>
                  FHIRUtil.extractValue[String](p, "name") ->
                    extractOrConstructValueFromMultiParam(p)
                )
            )
          ).getOrElse(JNothing)
    }
  }

  /**
   * Parse and validate a FHIR Path Patch parameter element content (Parameter.parameter)
   * @param patch       FHIR Path Patch parameter element (Parameter.parameter)
   * @param resource    Resource to patch on
   * @param ind         Index of the operation
   */
  private def parseFhirPatchParameter(patch:Resource, resource: Resource, ind:Int):FhirPatchPatch = {
    val patchParams:Map[String, JValue] =
      patch.obj
      .find(e => e._1 == "part" && e._2.isInstanceOf[JArray])
      .map(_._2.asInstanceOf[JArray].arr)
      .getOrElse(Nil)
      .filter(_.isInstanceOf[JObject])
      .map(_.asInstanceOf[Resource])
      .flatMap(part => {
         FHIRUtil.extractValueOption[String](part, "name")
           .flatMap(pname => {
             val pvalue = pname match {
               case "path" | "name" => part.obj.find(_._1 == "valueString")
               case "type" => part.obj.find(_._1 == "valueCode")
               case "index" | "source" | "destination" => part.obj.find(_._1 == "valueInteger")
               case "value" =>
                 extractOrConstructValueFromMultiParam(part) match {
                   case JNothing => None
                   case oth => Some("value" -> oth)
                 }
             }
             pvalue
               .map(_._2)
               .map(v => pname -> v)
           })
      }).toMap

    //Get operation type e.g. add, insert, etc
    val opType = patchParams.get("type").flatMap(_.extractOpt[String])
    if(!opType.exists(opTypes.contains))
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR Patch Parameter definition, missing part for 'type' or invalid operation!"),
          Seq(s"Parameters.parameter[$ind].part.where(name = 'type')")
        )
      ))

    val opPath = patchParams.get("path").flatMap(_.extractOpt[String])
    if(opPath.isEmpty)
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR Patch Parameter definition, missing part for 'path'"),
          Seq(s"Parameters.parameter[$ind].part.where(name = 'path')")
        )
      ))

    val paths = FhirPathEvaluator().evaluateToFindPaths(opPath.get, resource)
    //FHIR Patch Path should return single element so single path
    if(opType.get != "delete" && paths.length != 1 || paths.length > 1){
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR Patch Parameter definition, given path ${opPath.get} does not return a single element for the resource to be patched!"),
          Seq(s"Parameters.parameter[$ind].part.where(name = 'path')")
        )
      ))
    }
    val elNameOption = patchParams.get("name").flatMap(_.extractOpt[String])
    val elIndex = patchParams.get("index").flatMap(_.extractOpt[Int])
    val elSourceIndex = patchParams.get("source").flatMap(_.extractOpt[Int])
    val elDestinationIndex = patchParams.get("destination").flatMap(_.extractOpt[Int])

    val patchValue = patchParams.get("value")

    opType.get match {
      case "add" =>
        if(patchValue.isEmpty || elNameOption.isEmpty)
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid FHIR Patch Parameter definition for 'add' operation, parameters 'value' or 'name' is missing!"),
              Seq(s"Parameters.parameter[$ind]")
            )
          ))
        FhirPathPatchAdd(paths.head, elNameOption.get, patchValue.get)
      case "insert" =>
        if(patchValue.isEmpty || !elIndex.exists(_ >= 0))
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid FHIR Patch Parameter definition for 'insert' operation, parameters 'value' or 'index' is missing or invalid!"),
              Seq(s"Parameters.parameter[$ind]")
            )
          ))
        FhirPathPatchInsert(paths.head, elIndex.get, patchValue.get)
      case "replace" =>
        if(patchValue.isEmpty)
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid FHIR Patch Parameter definition for 'replace' operation, parameter 'value' is missing!"),
              Seq(s"Parameters.parameter[$ind]")
            )
          ))
        FhirPathPatchReplace(paths.head, patchValue.get)
      case "move" =>
        if(!elSourceIndex.exists(_ >= 0) || !elDestinationIndex.exists(_ >= 0))
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid FHIR Patch Parameter definition for 'move' operation, parameter 'source' or 'destination' is missing or invalid!"),
              Seq(s"Parameters.parameter[$ind]")
            )
          ))
        FhirPathPatchMove(paths.head, elSourceIndex.get, elDestinationIndex.get)
      case "delete" =>
        FhirPathPatchDelete(paths.headOption.getOrElse(Nil))
    }
  }

}

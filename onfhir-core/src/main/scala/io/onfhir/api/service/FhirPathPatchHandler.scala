package io.onfhir.api.service
import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.{FHIRUtil, FhirPatchUtil}
import io.onfhir.api.validation.ReferenceResolver
import io.onfhir.config.IFhirConfigurationManager
import io.onfhir.exception.BadRequestException
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.util.JsonFormatter._
import io.onfhir.validation.BaseFhirProfileHandler
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s._

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * FHIR Path Patch operation definitions
 */
sealed trait FhirPatchPatch
case class FhirPathPatchAdd(path:Seq[(String, Option[Int])], name:String, value:JValue) extends FhirPatchPatch
case class FhirPathPatchInsert(path:Seq[(String, Option[Int])], index:Int, value:JValue) extends FhirPatchPatch
case class FhirPathPatchDelete(path:Seq[(String, Option[Int])]) extends FhirPatchPatch
case class FhirPathPatchReplace(path:Seq[(String, Option[Int])], value:JValue) extends FhirPatchPatch
case class FhirPathPatchMove(path:Seq[(String, Option[Int])], source:Int, destination:Int) extends FhirPatchPatch

class FhirPathPatchHandler(fhirConfigurationManager: IFhirConfigurationManager, profileHandler:BaseFhirProfileHandler) extends IFHIRPatchHandler {
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
    //final path i.e. for paths like "Observation", our parser returns Seq("" -> None)
    val fpath = if(path.length == 1 && path.head._1 == "") Nil else path //Seq(name -> None) else  path :+ (name -> None)
    val isTargetArray = isOperationAddTargetArray(fpath:+ (name -> None), rtype)
    FhirPatchUtil.applyAddPatch(resource, fpath, name, value, isTargetArray)
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
    try {
      FhirPatchUtil.applyInsertPatch(resource, path, index, value)
    } catch {
      case iob:IndexOutOfBoundsException =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(iob.getMessage),
            Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')", s"Parameters.parameter[$opInd].part.where(name = 'index')")
          )
        ))
      case ia:IllegalArgumentException =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(ia.getMessage),
            Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')")
          )
        ))
    }
  }

  /**
   *
   * @param path
   * @param resource
   * @param opInd
   * @return
   */
  private def applyDeletePatch(path:Seq[(String, Option[Int])], resource: Resource, opInd:Int):Resource = {
    FhirPatchUtil.applyDeletePatch(resource, path)
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
    try {
      FhirPatchUtil.applyReplacePatch(resource, path, value)
    }  catch {
    case ia:IllegalArgumentException =>
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(ia.getMessage),
          Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')")
        )
      ))
    }
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
    try {
      FhirPatchUtil.applyMovePatch(resource, path, source, destination)
    } catch {
      case iob:IndexOutOfBoundsException =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(iob.getMessage),
            Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')", s"Parameters.parameter[$opInd].part.where(name = 'index')")
          )
        ))
      case ia:IllegalArgumentException =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(ia.getMessage),
            Seq(s"Parameters.parameter[$opInd].part.where(name = 'path')")
          )
        ))
    }
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

  def extractOrConstructValueFromMultiParam(part:JObject, resource: Resource, ind:Int):JValue = {
    part.obj
      .find(_._1.startsWith("value")) match {
      //Value evaluated from expression
      case Some(("valueExpression", expr:JObject)) =>
        FHIRUtil.extractValue[String](expr, "language") match {
          case "text/fhirpath" =>
            FHIRUtil.extractValueOption[String](expr, "expression") match {
              case Some(e) =>
                evaluateFhirPathExpressionToGetValue(e, resource, ind)
              case None => expr
            }
          case _ => expr
        }
      //Normal single value
      case Some((_,  v)) => v
      //value constructed from parts
      case None =>
        part.obj.find(e =>e._1 == "part")
          .map(_._2.asInstanceOf[JArray])
          .map(a =>
            JObject(
              a.arr
                .map(_.asInstanceOf[JObject])
                .map(p =>
                  FHIRUtil.extractValue[String](p, "name") ->
                    extractOrConstructValueFromMultiParam(p, resource, ind)
                )
                .groupMap(_._1)(v => v._2)
                .view
                .mapValues(l => if(l.length > 1) JArray(l) else l.head)
                .toList
            )
          ).getOrElse(JNothing)
    }
  }

  /**
   * Evaluate given FHIR path expression to extract the value to use
   * @param e
   * @param resource
   * @param ind
   * @return
   */
  private def evaluateFhirPathExpressionToGetValue(e:String, resource: Resource, ind:Int):JValue = {
    val fpe = FhirPathEvaluator(new ReferenceResolver(fhirConfigurationManager, resource))
    fpe.evaluateAndReturnJson(e, resource) match {
      case None | Some(JArray(_)) => throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR Patch, given value expression does not evaluate to any result or single JSON result!"),
          Seq(s"Parameters.parameter[$ind].part.where(name = 'value')")
        )
      ))
      case Some(oth) =>
        oth
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
                 extractOrConstructValueFromMultiParam(part, resource, ind) match {
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

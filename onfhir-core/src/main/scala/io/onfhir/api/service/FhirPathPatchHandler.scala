package io.onfhir.api.service
import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.BadRequestException
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JValue}

object FhirPathPatchHandler extends IFHIRPatchHandler {
  val opTypes = Set("add", "insert", "delete", "replace", "move")
  /**
   * Apply the patch to the resource and return the new updated content
   *
   * @param patch    Patch content (e.g. JSON Patch, FHIR Patch, ...)
   * @param resource Resource content
   * @return New resource content
   */
  override def applyPatch(patch: Resource, resource: Resource): Resource = {



  }


  private def parsePatch(patch:Resource):Seq[PatchItem] ={


  }

  /**
   * Parse and validate a FHIR Path Patch parameter element content (Parameter.parameter)
   * @param patch
   * @param resource
   * @param ind
   */
  private def parseFhirPatchParameter(patch:Resource, resource: Resource, ind:Int) = {
    val patchParams:Map[String, JValue] =
      patch.obj
      .filter(_._1 == "part")
      .map(_._2.asInstanceOf[Resource])
      .map(part => {
        FHIRUtil.extractValue[String](part, "name") ->
          part.obj.find(_._1.startsWith("value")).map(_._2).get
      }).toMap

    //Get operation type e.g. add, insert, etc
    val opType = patchParams.get("type").flatMap(_.extractOpt[String])
    if(!opType.exists(opTypes.contains))
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid FHIR Patch Parameter definition, missing part for 'type'"),
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
    if(paths.length != 1){
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

    opType.get match {
      case "add" =>
    }
  }

}

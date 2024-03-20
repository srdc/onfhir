package io.onfhir.api.service

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api.Resource
import io.onfhir.api.model._
import io.onfhir.authz.AuthzContext
import io.onfhir.exception.BadRequestException
import io.onfhir.util.JsonFormatter.formats
import org.json4s.Extraction

import scala.concurrent.Future

class FHIRBulkService extends FHIRInteractionService {

  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply {
      //Check if all resource types are ok
      val rtypes = fhirRequest.childRequests.map(_.resourceType.get).toSet
      if (rtypes.size != 1)
        throw new BadRequestException(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"For bulk import in upsert mode all resources in ndjson file should be from same FHIR resource type. Resource types identified; ${rtypes.mkString(", ")}"),
          Nil
        )))
    }
  }

  override def completeInteraction(fhirRequest:FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting:Boolean = false): Future[FHIRResponse] = {
    val resources = fhirRequest.childRequests.map(r => r.resourceId -> r.resource.get)
    performBulkUpsertRequest(fhirRequest.resourceType.get, resources)
      .map {
        case (ncreated, nupdated, errors) =>
          val response = new FHIROperationResponse(if(errors.isEmpty) StatusCodes.OK else StatusCodes.BadRequest)
          response.setPrimitiveParam("numCreated", ncreated)
          response.setPrimitiveParam("numUpdated", nupdated)
          response.setPrimitiveParam("numError", errors.length)
          errors.foreach {
            case (i, error) =>
              response.setMultiParam("error", FHIRMultiOperationParam(
                Seq(
                  "index" -> FHIROperationParam.createSimpleParam(i)
                ) ++
                  error.outcomeIssues.map(issue => "issue" -> FHIRSimpleOperationParam(Extraction.decompose(issue)))
              ))
          }
          response
      }
  }

  /**
   *
   * @param fhirRequest
   * @return
   */
  def performBulkUpsertRequest(rtype:String, resources:Seq[(Option[String],Resource)]):Future[(Int, Int, Seq[(Int, BadRequestException)])] = {
      Future
        .sequence(resources.zipWithIndex.map(r => validateResource(rtype, r._1, r._2)))
        .flatMap { validationResults =>
          val validResources = validationResults.filter(_._1.isLeft).map(_._1.swap.getOrElse(null))
          val invalidResources = validationResults.filter(_._1.isRight).map(r => r._2 -> r._1.getOrElse(null))
          if (validResources.nonEmpty) {
            fhirConfigurationManager
              .resourceManager
              .bulkUpsertResources(rtype, validResources)
              .map {
                case (nInserted, nUpdated) =>
                  (nInserted, nUpdated, invalidResources)
              }
          } else {
            Future.apply(0, 0, invalidResources)
          }
        }
  }

  private def validateResource(rtype:String, resource: (Option[String],Resource), rind:Int):Future[(Either[(Option[String],Resource), BadRequestException], Int)] = {
      fhirConfigurationManager.fhirValidator
        .validateResource(resource._2, rtype, silent = false)
        .map(_ => Left(resource) -> rind)
        .recover {
          case br:BadRequestException => Right(br) -> rind
          case oth => Right(new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some("Invalid content: " + oth.getMessage),
              Nil
            )))) -> rind
        }
  }


}

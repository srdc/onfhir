package io.onfhir.operation

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import io.onfhir.Onfhir
import io.onfhir.api.model._
import io.onfhir.api.service.FHIROperationHandlerService
import io.onfhir.async.BulkImportJobHandler
import io.onfhir.async.BulkImportJobHandler.{BulkImportJob, UriSource}
import io.onfhir.config.IFhirConfigurationManager
import io.onfhir.exception.BadRequestException
import io.onfhir.operation.BulkOperationHandler.OPERATION_IMPORT
import org.json4s.JsonAST.JString
import org.slf4j.{Logger, LoggerFactory}

import java.net.URI
import java.util.UUID
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try

/**
 *
 * @param fhirConfigurationManager
 */
class BulkOperationHandler(fhirConfigurationManager: IFhirConfigurationManager) extends FHIROperationHandlerService(fhirConfigurationManager) {
  private val logger: Logger = LoggerFactory.getLogger("BulkOperationHandler")

  /**
   * Job and actor handlers
   * TODO Design a better async job management architecture
   */
  val importJobs: mutable.Map[String, ActorRef] = new mutable.HashMap[String, ActorRef]

  /**
   * Method that implements the operation
   *
   * @param operationName    Operation name as defined after '$' symbol e.g. meta-add
   * @param operationRequest Operation Request including the parameters
   * @param resourceType     The resource type that operation is called if exists
   * @param resourceId       The resource id that operation is called if exists
   * @return The response containing the Http status code and the output parameters
   */
  override def executeOperation(operationName: String, operationRequest: FHIROperationRequest, resourceType: Option[String], resourceId: Option[String]): Future[FHIROperationResponse] = {
    operationName match {
      case OPERATION_IMPORT => executeImportOperation(operationRequest)
    }
  }

  /**
   * Implementing Bulk Import operation See https://smilecdr.com/docs/bulk/fhir_bulk_import.html
   *
   * @param operationRequest Operation request
   * @return
   */
  private def executeImportOperation(operationRequest: FHIROperationRequest): Future[FHIROperationResponse] = {
    Future.apply {
      val format = operationRequest.extractParamValue[String]("inputFormat")
      if (!format.contains("application/fhir+ndjson") && !format.contains("application/ndjson") && !format.contains("ndjson"))
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"The parameter inputFormat should exist and be 'application/fhir+ndjson'!"),
            Seq("Parameters.parameter.where(name ='inputFormat')")
          )
        ))

      val inputSourceUri =
        Try(new URI(operationRequest.extractParamValue[String]("inputSource").get))
          .getOrElse(throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"The parameter inputSource should exist!"),
              Seq("Parameters.parameter.where(name ='inputSource')")
            )
          )))


      val ndjsonSources =
        operationRequest
          .getParams("input")
          .zipWithIndex
          .map {
            case (FHIRMultiOperationParam(Seq("type" -> FHIRSimpleOperationParam(JString(rtype)), "url" -> FHIRSimpleOperationParam(JString(url)))), _) =>
              rtype -> url
            case (_, i) =>
              throw new BadRequestException(Seq(
                OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.ERROR,
                  FHIRResponse.OUTCOME_CODES.INVALID,
                  None,
                  Some(s"One of the parameter input is invalid!"),
                  Seq(s"Parameters.parameter.where(name ='input')[$i]")
                )
              ))
          }
      //Create a job identifier
      val jobId = UUID.randomUUID().toString

      operationRequest.getParam("storageDetail") match {
        case Some(FHIRMultiOperationParam(params)) =>
          params.find(_._1 == "type").map(_._2) match {
            case Some(FHIRSimpleOperationParam(JString(storageDetail))) =>
              storageDetail match {
                case "file" =>
                  val jobHandler =
                    Onfhir
                      .actorSystem
                      .actorOf(BulkImportJobHandler.props(BulkImportJob(jobId, UriSource(inputSourceUri), ndjsonSources)), s"BulkImport_$jobId")
                  jobHandler ! BulkImportJobHandler.StartImportJob
                  FHIROperationResponse(FHIRResponse.apply(StatusCodes.Accepted))
              }
            case _ =>
              throw new BadRequestException(Seq(
                OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.ERROR,
                  FHIRResponse.OUTCOME_CODES.INVALID,
                  None,
                  Some(s"The parameter storageDetail with children parameter type should exist!"),
                  Seq(s"Parameters.parameter.where(name ='storageDetail')")
                )
              ))
          }
        case _ =>
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"The parameter storageDetail should exist!"),
              Seq(s"Parameters.parameter.where(name ='storageDetail')")
            )
          ))
      }
    }
  }
}


object BulkOperationHandler {
  val OPERATION_IMPORT = "import"
  val OPERATION_EXPORT = "export"
}

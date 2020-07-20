package io.onfhir.operation

import akka.http.scaladsl.model.StatusCodes

import io.onfhir.api._
import io.onfhir.api.model.{FHIROperationRequest, FHIROperationResponse, FHIRResponse, OutcomeIssue}
import io.onfhir.api.service.FHIROperationHandlerService
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.FhirConfigurationManager.fhirValidator
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.ResourceManager
import io.onfhir.exception._
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.JObject
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.util.{Success, Try}

/**
  * Handles the FHIR $validate operation; see https://www.hl7.org/fhir/resource-operations.html#validate
  */
class ValidationOperationHandler extends FHIROperationHandlerService {
  private val logger: Logger = LoggerFactory.getLogger("ValidationOperationHandler")

  final val PARAMETER_RESOURCE = "resource"
  final val PARAMETER_MODE = "mode"
  final val PARAMETER_PROFILE = "profile"

  final val VALIDATION_MODE_GENERAL = "general"
  final val VALIDATION_MODE_CREATE = "create"
  final val VALIDATION_MODE_UPDATE = "update"
  final val VALIDATION_MODE_DELETE = "delete"

  /**
    * Execute the operation and prepare the output parameters for the operation
    * @param operationName    Operation name as defined after '$' symbol e.g. meta-add
    * @param operationRequest Operation Request including the parameters
    * @param resourceType     The resource type that operation is called if exists
    * @param resourceId       The resource id that operation is called if exists
    * @return The response containing the Http status code and the output parameters
    */
  override def executeOperation(operationName: String, operationRequest: FHIROperationRequest, resourceType: Option[String], resourceId: Option[String]): Future[FHIROperationResponse] = {
    //Validation mode; create, update, delete or default general
    val mode:String = operationRequest.extractParamValue[String](PARAMETER_MODE).getOrElse(VALIDATION_MODE_GENERAL)
    //Profile to validate against
    val profile:Option[String] = operationRequest.extractParamValue[String](PARAMETER_PROFILE)
    val resource:Option[Resource] = operationRequest.extractParamValue[Resource](PARAMETER_RESOURCE)
    //Check if resource is not empty unless validation mode is delete
    if(mode != VALIDATION_MODE_DELETE && resource.isEmpty)
      throw new BadRequestException(Seq(
        OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Empty or invalid resource body while validation mode is not '$VALIDATION_MODE_DELETE'"),
          Nil)))


    validate(resourceType.get, resourceId, mode, resource, profile).map(response => {
      val opResponse = new FHIROperationResponse(response.httpStatus)
      opResponse.setComplexOrResourceParam("return", FHIRResponse.createOperationOutcome(response.outcomeIssues))
      opResponse
    })
  }

  /**
    * Validation logic
    * @param resourceType
    * @param rid
    * @param validationMode
    * @param resourceOption
    * @param profileOption
    * @return
    */
  private def validate(resourceType:String, rid:Option[String], validationMode:String, resourceOption:Option[Resource], profileOption:Option[String]):Future[FHIRResponse] = {
    validationMode match {
      case VALIDATION_MODE_GENERAL => generalValidation(resourceType, resourceOption, profileOption)
      case VALIDATION_MODE_UPDATE | VALIDATION_MODE_DELETE => validateForUpdateDelete(validationMode, resourceType, rid, resourceOption, profileOption)
      case VALIDATION_MODE_CREATE => validateForCreate(resourceType, resourceOption, profileOption)
      case _ =>
        Future.apply(FHIRResponse.errorResponse(StatusCodes.BadRequest,
          Seq(
            OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, //fatal
              FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
              None,
              Some(s"Not supported validation mode (create, update, delete) are  supported only!"),
              Nil))
          )
        )
    }
  }


  /**
    * General mode validation (only content)
    * @param resourceType
    * @param resourceOption
    * @param profileOption
    * @return
    */
  private def generalValidation(resourceType:String, resourceOption:Option[Resource], profileOption:Option[String], validationMode:Option[String] = None):Future[FHIRResponse] ={
      resourceOption match{
        case None =>
          Future.apply(
            FHIRResponse.errorResponse(StatusCodes.BadRequest,
              Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, //fatal
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Parameter '$PARAMETER_RESOURCE' is required for validation in general mode!"),
                Nil))
              )
          )
        case Some(resource) => {
          //Validate if resource type matches
          FHIRApiValidator.validateResourceTypeMatching(resource, resourceType)
          val resultResource = if(profileOption.isDefined) {
            //Check if we support profile, if not return not supported
            if(!fhirConfig.isProfileSupported(profileOption.get))
              throw new MethodNotAllowedException(Seq(
                OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR,
                  FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
                  None,
                  Some(s"Given profile is not supported in this server. See our Conformance statement from ${OnfhirConfig.fhirRootUrl}/metadata for all supported profiles ..."),
                  Nil)))
            //else Set the profile into the resource
            FHIRUtil.setProfile(resource, profileOption.get)
          } else resource
          validateContent(resultResource, resourceType, validationMode) map { issues =>
            //Not error response, but we should return OperationOutcome so we use this
            FHIRResponse.errorResponse(StatusCodes.OK, issues)
          }
        }
      }
  }

  /**
    * Validate content of a resource
    * @param resource
    * @param validationMode
    * @return
    */
  private def validateContent(resource:Resource, resourceType:String, validationMode:Option[String]=None):Future[Seq[OutcomeIssue]] = {
    //Validate resource in silent mode to get only the issues (preventing exception throwing)
    fhirValidator.validateResource(resource, resourceType, silent = true) map { issues =>
      //If no error, add information issue that everything is ok
      if(
          !issues.exists(i => i.isError)
        )
        Seq(everythingOk) ++ issues
      else
        issues ++ validationMode.map(vm =>Seq(problemExists(vm))).getOrElse(Nil)
    }
  }

  /**
    * Validate for create interaction
    * @param resourceType
    * @param resourceOption
    * @param profileOption
    * @return
    */
  private def validateForCreate(resourceType:String, resourceOption:Option[Resource], profileOption:Option[String]) = {
    val issues = Try(FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.CREATE, resourceType, false)).recoverWith{
      case e:NotFoundException => Success(e.outcomeIssues)
    }.get
    if(issues.nonEmpty)
      Future.apply(FHIRResponse.errorResponse(StatusCodes.OK, issues :+ problemExists(VALIDATION_MODE_CREATE)))
    else {
      val resourceTypeIssues = Try(FHIRApiValidator.validateResourceType(resourceOption.get, resourceType)).recoverWith {
        case e:UnprocessableEntityException => Success(e.outcomeIssues)
      }.get
      if(resourceTypeIssues.nonEmpty)
        Future.apply(FHIRResponse.errorResponse(StatusCodes.OK, resourceTypeIssues :+ problemExists(VALIDATION_MODE_CREATE)))
      else
        generalValidation(resourceType, resourceOption, profileOption, Some(VALIDATION_MODE_CREATE))
    }
  }

  /**
    * Validation for update and delete interactions
    * @param validationMode
    * @param resourceType
    * @param rid
    * @param resourceOption
    * @param profileOption
    * @return
    */
  private def validateForUpdateDelete(validationMode:String, resourceType:String, rid:Option[String], resourceOption:Option[Resource], profileOption:Option[String]): Future[FHIRResponse] = {
    if(rid.isEmpty)
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid request for validation mode '$validationMode'."+ " Use the request scheme 'URL: [base]/[Resource]/[id]/$validate'"),
          Nil
        )))

    //Check if they are allowed for that interaction
    val issues =
      Try(
          FHIRApiValidator.validateInteractionOnResourceType(validationMode, resourceType, false)
      ).recoverWith {
          case e:NotFoundException => Success(e.outcomeIssues)
        }.get

    if(issues.nonEmpty)
      Future.apply(FHIRResponse.errorResponse(StatusCodes.OK, issues))
    else {
      ResourceManager.isResourceExist(resourceType, rid.get) flatMap {
        case false =>
        //If resource does not exist
          Future.apply(FHIRResponse.errorResponse(StatusCodes.OK,
            Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, FHIRResponse.OUTCOME_CODES.INFORMATIONAL, None, Some("Resource with the given id does not exist. So operation is not allowed!"), Nil))))
        case true=>
          if (validationMode.equals(VALIDATION_MODE_UPDATE)) {
            resourceOption match {
              case None =>
                Future.apply(FHIRResponse.errorResponse(StatusCodes.BadRequest,
                  Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, //fatal
                    FHIRResponse.OUTCOME_CODES.INVALID,
                    None,
                    Some(s"Parameter '$PARAMETER_RESOURCE' is required for validation in general mode!"),
                    Nil))
                ))
              case Some(resource) =>
                val resourceIdIssues = Try(FHIRApiValidator.validateResourceId(resource, rid.get)).recoverWith {
                  case e:BadRequestException => Success(e.outcomeIssues)
                }.get
                if(resourceIdIssues.nonEmpty)
                  Future.apply(FHIRResponse.errorResponse(StatusCodes.OK, resourceIdIssues :+ problemExists(VALIDATION_MODE_UPDATE)))
                else {
                  val resourceTypeIssues = Try(FHIRApiValidator.validateResourceType(resource, resourceType)).recoverWith {
                    case e: BadRequestException => Success(e.outcomeIssues)
                    case e: UnprocessableEntityException => Success(e.outcomeIssues)
                  }.get
                  if(resourceTypeIssues.nonEmpty)
                    Future.apply(FHIRResponse.errorResponse(StatusCodes.OK, resourceTypeIssues :+ problemExists(VALIDATION_MODE_UPDATE)))
                  else
                    generalValidation(resourceType, resourceOption, profileOption, Some(VALIDATION_MODE_UPDATE))
                }
            }
          } else {
            Future.apply(FHIRResponse.errorResponse(StatusCodes.OK, Seq(everythingOk)))
          }
      }
    }
  }

  /**
    * Informational Outcome Issue stating some problem exists
    * @param operation
    * @return
    */
  private def problemExists(operation:String):OutcomeIssue =
    OutcomeIssue(FHIRResponse.SEVERITY_CODES.INFORMATION, FHIRResponse.OUTCOME_CODES.INFORMATIONAL, None, Some(s"Problem(s) detected for your $operation, it will be not valid"), Nil)

  /**
    * Informational Outcome Issue stating everything is OK
    * @return
    */
  private def everythingOk:OutcomeIssue =
    OutcomeIssue(FHIRResponse.SEVERITY_CODES.INFORMATION, FHIRResponse.OUTCOME_CODES.INFORMATIONAL, None, Some("All OK :)"), Nil)


}


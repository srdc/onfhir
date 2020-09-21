package io.onfhir.api.service

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.util.{BaseFhirProfileHandler, FHIRUtil}
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.FhirConfigurationManager.fhirValidator
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.{NotFoundException, PreconditionFailedException}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

/**
  * Created by tuncay on 4/28/2017.
  */
class FHIRPatchService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession) {

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
        //apply the patch
        val updatedResource = getPatchHandler(patch).applyPatch(patch, _type, FHIRUtil.clearExtraFields(foundResource))

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
          Some(s"No matching resource for given query!"),
          Nil
        )
      ))
      //Only one match
      case (1, Seq(foundResource)) =>
        val (rid, currentVersion, _) = FHIRUtil.extractBaseMetaFields(foundResource)

        //3.2.1 Check if user requested a version aware update
        FHIRApiValidator.validateIfMatch(ifMatch, currentVersion)
        //apply the patch
        val updatedResource = getPatchHandler(patch).applyPatch(patch, _type, FHIRUtil.clearExtraFields(foundResource))

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
   * Based on the patch type return the handler
   * @param patch Patch content
   * @return
   */
  private def getPatchHandler(patch: Resource):IFHIRPatchHandler = {
    //If content is given with Parameters
    if(FHIRUtil.extractValueOption[String](patch, "resourceType")
      .contains("Parameters")) {
      val profileHandler = new BaseFhirProfileHandler(fhirConfig) {
        override protected val logger: Logger = LoggerFactory.getLogger(classOf[FHIRPatchService])
      }
      new FhirPathPatchHandler(profileHandler)
    } else
      JsonPatchHandler
  }
}

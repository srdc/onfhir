package io.onfhir.api.service

import akka.http.scaladsl.model.headers.{`If-Modified-Since`, `If-None-Match`}
import akka.http.scaladsl.model.{StatusCodes, Uri}
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.api.parsers.FHIRResultParameterResolver
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.NotFoundException

import scala.concurrent.Future

class FHIRReadService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession) {

  /**
    * Validate if interaction is supported for the request
    *
    * @param fhirRequest FHIRRequest object
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply(
      validateReadInteraction(fhirRequest.resourceType.get, fhirRequest.resourceId.get, fhirRequest.versionId)
    )
  }

  /**
    * Perform the interaction
    *
    * @param fhirRequest FHIRRequest object
    * @param isTesting If interaction is only for testing (not realized)
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean): Future[FHIRResponse] = {
    getResource(fhirRequest.resourceType.get, fhirRequest.resourceId.get, fhirRequest.versionId, fhirRequest.ifNoneMatch, fhirRequest.ifModifiedSince, fhirRequest.summary)
  }

  /**
    * Check if read is supported and basically ok
    * @param _type  Resource type
    * @param _id    Resource id
    * @param _vid   Version id
    * @return
    */
  def validateReadInteraction(_type:String,
                                      _id:String,
                                      _vid:Option[String]): Unit = {
    //1) Validation of operation
    //1.1) Validation of id and version id
    FHIRApiValidator.validateId(_id)
    //1.2) Validation of operation
    if(_vid.isDefined) {
      FHIRApiValidator.validateId(_vid.get)
      FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.VREAD, _type)
    }else {
      FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.READ, _type)
    }
  }

  /**
    * Get the requested resource
    * @param _type Resource type
    * @param _id Resource identifier
    * @param _vid Resource version
    * @param ifNoneMatch If-None-Match header value
    * @param ifModifiedSince If-Modified-Since header value
    * @param summary FHIR summary parameter value
    * @return
    */
  def getResource(_type:String,
                  _id:String,
                  _vid:Option[String],
                  ifNoneMatch:Option[`If-None-Match`],
                  ifModifiedSince:Option[`If-Modified-Since`],
                  summary:Option[String]):Future[FHIRResponse] = {
    logger.debug(s"requesting '${if(_vid.isDefined)"v" else ""}read' for ${_type} with id ${_id} ...")
    //If exist resolve summary parameters
    val includingExcludingFields = summary.flatMap(s => FHIRResultParameterResolver.resolveSummary(_type, s))

    //2) check if resource exists with given type, id and optional version id
    ResourceManager.getResource(_type, _id, _vid, includingExcludingFields)(transactionSession) map {
      case None =>
        logger.debug("resource not found, return 404 NotFound...")
        throw new NotFoundException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.INFORMATION,
            FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
            None,
            if(_vid.isDefined)
              Some(s"Resource with type (${_type}), id (${_id}) and version (${_vid.get}) not found...")
            else
              Some(s"Resource with type (${_type}), id (${_id}) not found...")
            ,
            Nil
          )
        ))
      case Some(foundResource) =>
        val alreadyDeleted = FHIRUtil.isDeleted(foundResource)
        val ( _, currentVersion, lastModified) = FHIRUtil.extractBaseMetaFields(foundResource)
        if(alreadyDeleted){
          logger.debug("resource already deleted, returning 410 Gone...")
          FHIRResponse.errorResponse(
            StatusCodes.Gone, //Status code
            Seq(OutcomeIssue(//Issues
              FHIRResponse.SEVERITY_CODES.INFORMATION,
              FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
              None,
              if (_vid.isDefined)
                Some(s"Resource with type (${_type}), id (${_id}) and version (${_vid.get}) has been deleted...")
              else
                Some(s"Resource with type (${_type}), id (${_id}) has been deleted...")
              ,
              Nil
            )),
            Some(currentVersion))

        } else {
          //2.1.2.1) check If-None-Match and If-Modified-Since HTTP headers
          FHIRApiValidator.validateIfNoneMatch(ifNoneMatch, currentVersion)
          FHIRApiValidator.validateIfModifiedSince(ifModifiedSince, lastModified)

          //2.1.2.2) post process the resource
          var resource = FHIRUtil.clearExtraFields(foundResource)
          if(summary.isDefined && summary.get != "false")
            resource = FHIRUtil.indicateSummarization(resource)
          logger.debug("resource found, returning...")
          FHIRResponse(
            StatusCodes.OK, //HTTP Status code
            Some(resource), //HTTP body
            Some(Uri(FHIRUtil.resourceLocationWithVersion(_type, _id, currentVersion))),
            Some(lastModified), //HTTP Last-Modified header
            Some(currentVersion)) //HTTP Etag header
        }
    }
  }


}

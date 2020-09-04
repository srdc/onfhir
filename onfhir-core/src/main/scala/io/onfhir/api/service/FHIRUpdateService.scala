package io.onfhir.api.service

import akka.http.scaladsl.model.{StatusCodes, Uri}
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.FhirConfigurationManager.fhirValidator
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.{BadRequestException, PreconditionFailedException}

import scala.concurrent.Future
import scala.util.Try

class FHIRUpdateService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession)  {
  /**
    * Validate if interaction is supported for the request
    *
    * @param fhirRequest FHIR Request
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
     val validations =
       if(fhirRequest.resourceId.isDefined)
        validateUpdateInteraction(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.resourceId.get, fhirRequest.ifMatch)
       else
         validateConditionalUpdateInteraction(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.queryParams, fhirRequest.prefer)

    validations.map(_ =>
      //Extra business rules validations if exist
      FHIRApiValidator.validateExtraRules(fhirRequest)
    )
  }

  /**
    * Perform the interaction
    *
    * @param fhirRequest    FHIR Request
    * @param authzContext   Authorization Context
    * @param isTesting      If this interaction is just for testing
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean): Future[FHIRResponse] = {
    if(fhirRequest.resourceId.isDefined)
      updateResource(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.resourceId.get, fhirRequest.ifMatch, fhirRequest.prefer, isTesting)
    else
      conditionalUpdateResource(fhirRequest.resource.get, fhirRequest.resourceType.get, fhirRequest.queryParams, fhirRequest.ifMatch, fhirRequest.prefer, isTesting)
  }

  /**
    * Check if update is basically valid
    * @param resource   FHIR Resource
    * @param _type      Resource type
    * @param _id        Resource id
    */
  private def validateUpdateInteraction(resource: Resource, _type:String, _id:String, ifmatch:Option[String]):Future[Unit] = {
    //1.1) Validate if "update" is supported for Resource Type
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.UPDATE, _type)
    //1.2) Check the id field and id in URL (e.g. is matching, conformant to ID format)
    FHIRApiValidator.validateResourceId(resource, _id)
    //1.3) Check the resource type consistency
    FHIRApiValidator.validateResourceType(resource, _type) //validate resource type
    //1.4) Check if only versioned update is supported, then ifMatch should exist
    FHIRApiValidator.validateVersionedUpdate(_type, ifmatch)
    //1.5) Validate the conformance of resource
    fhirValidator.validateResource(resource, _type).map(_ => Unit)
  }


  /**
    * Check if conditional update is basically valid
    * @param resource         FHIR resource
    * @param _type            Resource type
    * @param searchParameters Search parameters
    * @param prefer           Prefer header
    */
  private def validateConditionalUpdateInteraction(resource: Resource, _type:String, searchParameters:List[Parameter], prefer:Option[String]):Future[Unit] = {
    //1.1) Validate if "update" is supported for Resource Type
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.UPDATE, _type, conditional = true)
    //1.2) Validate if the provided search parameters are supported
    //val invalidSet = FHIRApiValidator.validateSearchParameters(_type, searchParameters.keySet, prefer)
    //1.3) Check the resource type consistency
    FHIRApiValidator.validateResourceType(resource, _type)
    //1.4) Validate the conformance of resource
    fhirValidator.validateResource(resource, _type).map(_ => Unit)
  }

  /**
    * Update the resource with the provided content if condition holds (See https://www.hl7.org/fhir/http.html#update)
    * @param resource Content of the resource
    * @param _type Resource type
    * @param searchParameters Search parameters representing the condition (parsed)
    * @param ifMatch If-Match header
    * @param prefer Prefer header
    * @return
    */
  private def conditionalUpdateResource(resource: Resource,
                                        _type:String,
                                        searchParameters:List[Parameter],
                                        ifMatch:Option[String],
                                        prefer:Option[String],
                                        testUpdate:Boolean = false
                                        ) : Future[FHIRResponse] = {
    logger.debug(s"Requesting conditional 'update' for ${_type}...")

    //1) Search with given condition update parameters (we need only id and other extra elements
    ResourceManager.queryResources(_type, searchParameters, count=1).flatMap {
      //If there is no match
      case (0, _) =>
        //First check if there is a supplied resource id
        Try(FHIRUtil.extractIdFromResource(resource)).toOption match {
          //Case: No matches, no id provided: it means it is create with update
          case None =>
            performUpdate(resource, _type, None, prefer, testUpdate)
          //Case: No matches, id provided
          case Some(rid) =>
            //we should check if this resource is deleted already or not
            ResourceManager.getResource(_type, rid).flatMap {
              //If there is no such resource, just create the resource
              case None =>
                performUpdate(resource, _type, Some(rid), prefer, testUpdate)
              case Some(foundResource) =>
                //If deleted create a new version
                if(FHIRUtil.isDeleted(foundResource)) {
                  val oldVersion = FHIRUtil.extractVersionFromResource(foundResource)
                  performUpdate(resource, _type, Some(rid), prefer, testUpdate, Some(oldVersion -> foundResource), wasDeleted = true)
                }else { //Otherwise it is problematic
                  logger.debug("Supplied resource id matches another FHIR resource which does not satisfy the given query!")
                  throw new BadRequestException(Seq(
                    OutcomeIssue(
                      FHIRResponse.SEVERITY_CODES.ERROR,
                      FHIRResponse.OUTCOME_CODES.INVALID,
                      None,
                      Some(s"Supplied resource id matches another FHIR resource which does not satisfy the given query!"),
                      Nil
                    )
                  ))
                }
            }
        }

      //If one matched
      case (1, Seq(foundResource)) =>
        val (rid, oldVersion,_ ) = FHIRUtil.extractBaseMetaFields(foundResource)
        //Case: One Match, no resource id provided OR (resource id provided and it matches the found resource)
        if(Try(FHIRUtil.extractIdFromResource(resource)).toOption.forall(_ == rid))
          performUpdate(resource, _type, Some(rid), prefer, testUpdate, Some(oldVersion->foundResource))
        //Case: One Match, resource id provided but does not match resource found
        else {
          logger.debug("There is one match in conditional update query, but resource id provided but does not match the resource found")
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"There is one match in conditional update query, but resource id provided but does not match the resource found!"),
              Nil
            )
          ))
        }
      //Multiple matches
      case (_, _) =>
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
    * Update the resource with the provided content (See https://www.hl7.org/fhir/http.html#update)
    * @param _type Resource type
    * @param _id The id of resource to be updated
    * @param ifMatch If-Match header
    * @param prefer Prefer header
    * @param testUpdate - Indicates if it is the real update operation
    * @return
    */
  def updateResource(resource: Resource, _type:String, _id:String, ifMatch:Option[String], prefer:Option[String], testUpdate:Boolean = false) : Future[FHIRResponse] = {
    logger.debug(s"requesting 'update' for ${_type} with ${_id}...")

    //1) check if resource already exists
    ResourceManager.getResource(_type, _id) flatMap {
      case None =>

        //1.a) Check if user requested a version aware update
        FHIRApiValidator.validateIfMatch(ifMatch, 0L)
        performUpdate(resource, _type, Some(_id), prefer, testUpdate)
      case Some(foundResource) =>
        val oldVersion = FHIRUtil.extractVersionFromResource(foundResource)
        val wasDeleted = FHIRUtil.isDeleted(foundResource)
        //1.b) Check if user requested a version aware update
        FHIRApiValidator.validateIfMatch(ifMatch, oldVersion)
        //Perform update
        performUpdate(resource, _type, Some(_id), prefer, testUpdate, Some(oldVersion -> foundResource), wasDeleted)
    }
  }

  /**
    * Perform the update
    * @param resourceIn   Resource content
    * @param rtype        Resource Type
    * @param rid          Resource id
    * @param prefer       Prefer header
    * @param testUpdate   If this is a test
    * @param oldVersion   Old version of updated resource
    * @param wasDeleted   If this resource was deleted previously
    * @return
    */
  def performUpdate(resourceIn:Resource, rtype:String, rid:Option[String], prefer:Option[String], testUpdate:Boolean = false, oldVersion:Option[(Long, Resource)] = None, wasDeleted:Boolean = false):Future[FHIRResponse] = {
    oldVersion.foreach(ov => FHIRApiValidator.validateContentChanges(rtype, ov._2, resourceIn))

    if (!testUpdate) {
      oldVersion match {
        case None =>
          FHIRApiValidator.validateUpdateCreate(rtype)
          logger.debug("creating a new document in database as it does not exist")
          ResourceManager.createResource(rtype, resourceIn, rid, withUpdate = true)(transactionSession).map {
            case (newId, newVersion, lastModified, createdResource) =>
              FHIRResponse(
                StatusCodes.Created ,
                FHIRUtil.getResourceContentByPreference(createdResource, prefer), //HTTP Body
                Some(Uri(FHIRUtil.resourceLocationWithVersion(rtype, newId, newVersion))), //HTTP Location header
                Some(lastModified), //HTTP Last-Modified header
                Some(newVersion) //HTTP ETag header
              )
          }
        case Some(ov) =>
          //Check if update create is supported for the resource type
          if(wasDeleted)
            FHIRApiValidator.validateUpdateCreate(rtype)
          logger.debug("updating (creating a new version) document in database as it already exists")
          //Update the resource
          ResourceManager.updateResource(rtype, rid.get, resourceIn, ov, wasDeleted)(transactionSession).map {
            case (newVersion, lastModified, updatedResource) =>
              FHIRResponse(
                if(ov._1 > 0 && !wasDeleted) StatusCodes.OK else StatusCodes.Created,
                FHIRUtil.getResourceContentByPreference(updatedResource, prefer), //HTTP Body
                Some(Uri(FHIRUtil.resourceLocationWithVersion(rtype, rid.get, newVersion))), //HTTP Location header
                Some(lastModified), //HTTP Last-Modified header
                Some(newVersion) //HTTP ETag header
              )
          }
      }
    } else Future(FHIRResponse(StatusCodes.OK))
  }
}

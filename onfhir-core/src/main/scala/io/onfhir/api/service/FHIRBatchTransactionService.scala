package io.onfhir.api.service

import java.time.Instant
import java.util.concurrent.TimeUnit

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.api.{FHIR_BUNDLE_FIELDS, _}
import io.onfhir.authz.AuthzContext
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.{BadRequestException, InternalServerException, NotFoundException}
import io.onfhir.server.ErrorHandler
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonDSL._
import org.json4s.JsonAST.{JObject, JString}


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}

/**
  * Created by ozan on 21.02.2017.
  */

class FHIRBatchTransactionService extends FHIRInteractionService {

  /**
    * Validate if interaction is supported for the request
    *
    * @param fhirRequest Parsed FHIRRequest object
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply {
      // Extract bundle type from the resource
      val bundleType = (fhirRequest.resource.get \ FHIR_COMMON_FIELDS.TYPE).extractOrElse[String]("")
      // Validate resource type
      FHIRApiValidator.validateResourceTypeMatching(fhirRequest.resource.get, FHIR_BUNDLE_TYPES.BUNDLE)
      // Validate interaction
      FHIRApiValidator.validateSystemLevelInteraction(bundleType)
      //Set the bundle type
      fhirRequest.interaction = bundleType
    }
  }

  /**
    * Perform the interaction
    * Delegates bundle execution to respective processes
    * wrt the bundle type
    *
    * @param fhirRequest Parsed FHIRRequest object
    * @param isTesting   If interaction is not performed actually
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean): Future[FHIRResponse] = {
    // Process request according to the bundle
    fhirRequest.interaction match {
      case FHIR_BUNDLE_TYPES.BATCH => performBatchRequest(fhirRequest, authzContext)
      case FHIR_BUNDLE_TYPES.TRANSACTION => performTransactionRequest(fhirRequest, authzContext)
      case anyType => throw new NotFoundException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Invalid bundle type $anyType, please use one of ${FHIR_BUNDLE_TYPES.BATCH} or ${FHIR_BUNDLE_TYPES.TRANSACTION}."),
          Nil
        )
      ))
    }
  }

  /**
    * No need to copy the whole response to FHIR request back so we override it
    *
    * @param fhirRequest  Parsed FHIR Request object
    * @param authzContext Authorization context if needed (only for transaction and batch)
    * @param isTesting    If we are only testing the interaction or realy perform it
    * @return
    */
  override def executeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean = false): Future[FHIRResponse] = {
    validateInteraction(fhirRequest) flatMap { _ =>
      completeInteraction(fhirRequest, authzContext, isTesting)
    }
  }

  /**
    * Priority of interactions in batch interaction
    *
    * @param interaction FHIR interaction
    * @return priority numbering
    */
  private def operationPriority(interaction: String) = interaction match {
    case FHIR_INTERACTIONS.DELETE => 1
    case FHIR_INTERACTIONS.CREATE => 2
    case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.PATCH => 3
    case op if op.startsWith("$") => 4
    case _ => 5
  }

  /**
    * Exceute a FHIRRequest
    *
    * @param bundleType   Type of Bundle request - batch or interaction
    * @param childRequest - The child request for an entry
    * @param authzContext Authz context
    * @param uuids        UUIDs for all requests to check interdependencies
    * @param isTesting    if this is only testing the execution
    * @return
    */
  private def executeChildInteraction(bundleType: String, childRequest: FHIRRequest, authzContext: Option[AuthzContext], uuids: Seq[String], transactionSession: Option[TransactionSession] = None, isTesting: Boolean = false): FHIRResponse = {
    try {
      val fhirInteractionService = FHIRServiceFactory.getFHIRService(childRequest, transactionSession)
      //If it is batch validate if there is no interdependency with other resources
      if (bundleType == FHIR_INTERACTIONS.BATCH && childRequest.resource.isDefined)
        FHIRApiValidator.validateInterdependencies(childRequest.resource.get, uuids)
      //If there is already a response (it means it is authorization or parsing error) return it
      if (childRequest.response.isDefined)
        childRequest.response.get
      else {
        // Perform the interaction
        val fresponse = fhirInteractionService.executeInteraction(childRequest, authzContext, isTesting)
        //Wait the result, handle the exceptions
        Await.result(fresponse, FiniteDuration(OnfhirConfig.fhirRequestTimeout.getSeconds, TimeUnit.SECONDS))
      }
    } catch {
      //Handle exceptions
      case e: Exception =>
        //logger.debug("Exception in child interaction execution;", e)
        val response = ErrorHandler.fhirErrorHandlerToResponse(e)
        childRequest.setResponse(response)
        response
    }
  }

  /**
    * Only used for transactions - to realize the interactions that is already validated, authorized, etc in test execution
    *
    * @param childRequest Parsed child FHIR request of a transaction/batch
    * @return
    */
  def realizeChildInteraction(childRequest: FHIRRequest): FHIRResponse = {
    try {
      // Perform the interaction
      val fresponse = FHIRServiceFactory.getFHIRService(childRequest).completeInteraction(childRequest)
      Await.result(fresponse, FiniteDuration(OnfhirConfig.fhirRequestTimeout.getSeconds, TimeUnit.SECONDS))
    } catch {
      //We still handle exceptions because errors in Read/Search operations are not critical we need to handle them
      case e: Exception =>
        val response = ErrorHandler.fhirErrorHandlerToResponse(e)
        childRequest.setResponse(response)
        response
    }
  }

  /**
    * Perform the batch request - sequential execution
    *
    * @param fhirRequest  Parsed batch request object
    * @param authzContext Authorization context
    * @return
    */
  def performBatchRequest(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext]): Future[FHIRResponse] = {
    Future.apply {
      //Actual order of requests
      val actualOrderOfRequests = fhirRequest.childRequests.map(_.id)
      //Sort the requests according to priority, execution order
      val sortedFhirRequests = fhirRequest.childRequests.sortWith((fr1: FHIRRequest, fr2: FHIRRequest) => {
        operationPriority(fr1.interaction) < operationPriority(fr2.interaction)
      })

      //Execute the interactions sequentially
      sortedFhirRequests.foreach(childRequest => {
        //Wait the result, as we want a sequential execution
        childRequest.requestTime = Instant.now()
        childRequest.setResponse(executeChildInteraction(fhirRequest.interaction, childRequest, authzContext, actualOrderOfRequests))
      })
      //Sort the responses back to original order
      //val sortedResponses = responses.sortWith((r1, r2) => actualOrderOfRequests.indexOf(r1._1) < actualOrderOfRequests.indexOf(r2._1)).map(_._2)
      //Set the responses within request (Need for auditing)
      //fhirRequest.childResponses = sortedResponses
      //Convert the FHIRResponses to Bundle.entry list
      val responseEntries = createEntriesForChildResponses(sortedFhirRequests)
      //Create the full Bundle response from response entries
      generateBundleResponse(responseEntries, FHIR_BUNDLE_TYPES.BATCH_RESPONSE)
    }
  }

  /**
    * Filter Transaction critical errors
    *
    * @param requests FHIR requests ( with a response inside)
    * @return
    */
  private def filterTransactionCriticalErrors(requests: Seq[FHIRRequest]): Seq[FHIRRequest] = {
    requests.filter(request => {
      request.response.exists(_.isError)
      /*requestResponse.interaction match {
        case FHIR_INTERACTIONS.CREATE | FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.DELETE => requestResponse.response.exists(_.httpStatus.isFailure)
        case _ => false
      }*/
    })
  }

  /**
    * Get All issues for a failed transcation
    * @param requests
    * @return
    */
  private def getIssuesForTransaction(requests:Seq[FHIRRequest]):Seq[OutcomeIssue] = {
    Seq(OutcomeIssue(
      FHIRResponse.SEVERITY_CODES.ERROR,
      FHIRResponse.OUTCOME_CODES.INVALID,
      None,
      Some("Some of the requests are failed! Transaction can not be performed ..."),
      requests.filter(_.response.exists(_.isError)).map(childRequest => childRequest.getRequestLocation())
    )) ++
      requests
      .flatMap(r =>
        r.response
          .map(_.outcomeIssues.map(oi => oi.copy(expression = oi.expression :+ r.getRequestLocation()))).getOrElse(Nil))
  }

  /**
    * Replace UUIDs with actual references in a resource including links to other resources in transaction
    *
    * @param resource
    * @param uuidMappingsMap
    * @return
    */
  private def replaceUUIDs(resource: JObject, uuidMappingsMap: Map[String, String]): JObject = {
    resource.mapField {
      //Check every reference value
      case ("reference", org.json4s.JsonAST.JString(s)) if uuidMappingsMap.contains(s) =>
        "reference" -> JString(uuidMappingsMap(s))
      case other => other
    }.asInstanceOf[JObject]
  }


  private def performTransactionRequest(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext]): Future[FHIRResponse] = {
    //Check if FHIR requests are valid
    val requestsWithError = fhirRequest.childRequests.filter(_.response.exists(_.isError))
    if (requestsWithError.nonEmpty) {
      throw new BadRequestException(getIssuesForTransaction(requestsWithError))
    }

    //Map given uuids to our own logical ids for resources
    urnIdToLogicalIdMapping(fhirRequest.childRequests).flatMap(uuidMappings => {
      //Check if there is identity overlapping in resources
      if (uuidMappings.map(_._1).distinct.size != uuidMappings.size)
        throw new BadRequestException(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some("Identity overlapping, check your 'uuid's given to the entries! Transaction can not be performed ..."),
          Nil
        )))

      val uuidMappingsMap: Map[String, String] = uuidMappings.toMap
      if (uuidMappingsMap.nonEmpty)
      //Replace the references, urls, etc in resources with logical ids
        fhirRequest.childRequests.foreach(childRequest => {
          if (childRequest.resource.isDefined)
            childRequest.resource = Some(replaceUUIDs(childRequest.resource.get, uuidMappingsMap))
        })

      //If MongoDB database is suitable, use Mongo transaction
      if (OnfhirConfig.mongoUseTransaction)
        performTransactionRequestByActual(fhirRequest, authzContext)
      else
      //Otherwise, simulate transaction
        performTransactionRequestBySimulation(fhirRequest, authzContext)
    })
  }

  /**
    * Perform the FHIR transaction by using MongoDB transaction capabilities
    *
    * @param fhirRequest  Parsed transaction request object
    * @param authzContext Authorization context
    * @return
    */
  def performTransactionRequestByActual(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext]): Future[FHIRResponse] = {
    val transactionSession = new TransactionSession(fhirRequest.id)
    val actualOrderOfRequests = fhirRequest.childRequests.map(_.id)
    //Sort the requests according to priority, execution order
    val sortedFhirRequests = fhirRequest.childRequests.sortWith((fr1: FHIRRequest, fr2: FHIRRequest) => {
      operationPriority(fr1.interaction) < operationPriority(fr2.interaction)
    })
    //Test the interactions sequentially (Test excecution - nothing changed in persistence)
    sortedFhirRequests.foreach(childReq =>
      //Execute with the transaction
      childReq.setResponse(executeChildInteraction(fhirRequest.interaction, childReq, authzContext, actualOrderOfRequests, Some(transactionSession)))
    )
    //Check if there is any error, if so return the errors directly
    val responseWithErrors = filterTransactionCriticalErrors(sortedFhirRequests)
    if (responseWithErrors.nonEmpty) {
      transactionSession.abort().flatMap(_ =>
        throw new BadRequestException(getIssuesForTransaction(sortedFhirRequests))
      )
    } else {
      try {
        //Realize the database operations as transaction
        transactionSession.commit().map(completed => {
          //Convert the FHIRResponses to Bundle.entry list
          val responseEntries = createEntriesForChildResponses(fhirRequest.childRequests)
          //Create the full Bundle response from response entries
          generateBundleResponse(responseEntries, FHIR_BUNDLE_TYPES.TRANSACTION_RESPONSE)
        })
      } catch {
        case e: Exception =>
          throw new InternalServerException("Problem in persistency layer while committing transaction!")
      }
    }
  }

  /**
    * Perform the FHIR transaction by simulating a transaction
    *
    * @param fhirRequest  Parsed transaction request object
    * @param authzContext Authorization context
    * @return
    */
  def performTransactionRequestBySimulation(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext]): Future[FHIRResponse] = {
    Future.apply {
      val actualOrderOfRequests = fhirRequest.childRequests.map(_.id)
      //Sort the requests according to priority, execution order
      val sortedFhirRequests = fhirRequest.childRequests.sortWith((fr1: FHIRRequest, fr2: FHIRRequest) => {
        operationPriority(fr1.interaction) < operationPriority(fr2.interaction)
      })
      //Test the interactions sequentially (Test excecution - nothing changed in persistence)
      sortedFhirRequests.foreach(childReq =>
        childReq.setResponse(executeChildInteraction(fhirRequest.interaction, childReq, authzContext, actualOrderOfRequests, isTesting = true))
      )
      //Check if there is any error, if so return the errors directly
      val responseWithErrors = filterTransactionCriticalErrors(sortedFhirRequests)
      if (responseWithErrors.nonEmpty)
        throw new BadRequestException(getIssuesForTransaction(sortedFhirRequests))

      //Clear the side effects of test execution
      sortedFhirRequests.foreach(fr => if (fr.resource.isDefined) fr.resource = Some(FHIRUtil.clearExtraFields(fr.resource.get)))
      //Perform the interactions
      sortedFhirRequests.foreach(childRequest =>
        childRequest.setResponse(realizeChildInteraction(childRequest))
      )
      //Convert the FHIRResponses to Bundle.entry list
      val responseEntries = createEntriesForChildResponses(fhirRequest.childRequests)
      //Create the full Bundle response from response entries
      generateBundleResponse(responseEntries, FHIR_BUNDLE_TYPES.TRANSACTION_RESPONSE)
    }
  }


  /**
    * Construct full URL from request response pair
    *
    * @param requestResponse
    * @return
    */
  private def constructFullUrl(requestResponse: FHIRRequest): Option[String] = {
    requestResponse.interaction match {
      case FHIR_INTERACTIONS.CREATE | FHIR_INTERACTIONS.UPDATE =>
        //If location is defined we use it
        if (requestResponse.response.get.location.isDefined)
          Some(requestResponse.response.get.location.get.path.toString())
        else if (requestResponse.response.get.httpStatus.isSuccess) {
          //Otherwise we try to construct the url from the request
          val rtype: String = requestResponse.resourceType.map(rt => "/" + rt).getOrElse("")
          val rid: String = requestResponse.resourceId.map(r => "/" + r).getOrElse("")
          Some(OnfhirConfig.fhirRootUrl + rtype + rid)
        } else {
          None
        }
      case _ => None
    }
  }

  /**
    * Create the Bundle Entries for child responses
    *
    * @param requestsResponses
    * @param isTransaction
    * @return
    */
  def createEntriesForChildResponses(requestsResponses: Seq[FHIRRequest], isTransaction: Boolean = false): Seq[Resource] = {
    requestsResponses.map(requestResponse => {
      var entry: Resource = JObject()
      //Put the body of the resource into the 'resource' element
      if (requestResponse.response.get.responseBody.isDefined)
        entry = entry ~ (FHIR_BUNDLE_FIELDS.RESOURCE -> requestResponse.response.get.responseBody.get)
      //Handle the 'fullUrl' element
      if (!requestResponse.isIdGenerated)
        entry = entry ~ (FHIR_BUNDLE_FIELDS.FULL_URL -> requestResponse.id)
      else {
        //Construct the URL from request and response
        constructFullUrl(requestResponse).foreach(fullUrl => entry = entry ~ (FHIR_BUNDLE_FIELDS.FULL_URL -> fullUrl))
      }
      //Construct the response element
      entry = entry ~ (FHIR_BUNDLE_FIELDS.RESPONSE -> convertFHIRResponseToMap(requestResponse.response.get))
      //Return the entry
      entry
    })
  }

  /**
    * Convert FHIRResponse object format --> Bundle entry.response
    *
    * @param fhirResponse FHIRResponse
    * @return Resource
    */
  private def convertFHIRResponseToMap(fhirResponse: FHIRResponse): Resource = {
    var response: Resource = FHIR_BUNDLE_FIELDS.STATUS -> fhirResponse.httpStatus.value

    if (fhirResponse.lastModified.isDefined)
      response = response ~ ("lastModified" -> fhirResponse.lastModified.get.toIsoDateTimeString)
    if (fhirResponse.location.isDefined)
      response = response ~ ("location" -> fhirResponse.location.get.toString())
    if (fhirResponse.newVersion.isDefined)
      response = response ~ ("etag" -> ("W/\"" + fhirResponse.newVersion.get + "\""))
    if (fhirResponse.outcomeIssues.nonEmpty)
      response = response ~ ("outcome" -> FHIRResponse.createOperationOutcome(fhirResponse.outcomeIssues))
    response
  }


  /**
    * Generates bundle response
    *
    * @param responseEntries bundle entries
    * @param responseType    type of response
    * @return FHIR response
    */
  def generateBundleResponse(responseEntries: Seq[Resource], responseType: String): FHIRResponse = {
    val responseBody: Resource =
      (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> FHIR_BUNDLE_TYPES.BUNDLE) ~
        (FHIR_COMMON_FIELDS.ID -> FHIRUtil.generateResourceId()) ~
        (FHIR_COMMON_FIELDS.TYPE -> responseType) ~
        (FHIR_COMMON_FIELDS.ENTRY -> responseEntries)
    FHIRResponse(StatusCodes.OK, responseBody = Some(responseBody))
  }

  /**
    * Retrieve resource id from database for conditional update
    *
    * @param fhirRequest
    * @return
    */
  private def getResourceIdForConditionalUpdate(fhirRequest: FHIRRequest): Future[Option[String]] = {
    ResourceManager
      .queryResources(fhirRequest.resourceType.get, fhirRequest.queryParams, count = 1, elementsIncludedOrExcluded = Some(true, Set.empty), excludeExtraFields = true)
      .map {
        case (1, Seq(foundResource)) => Some(FHIRUtil.extractIdFromResource(foundResource))
        case _ => None
      }
  }

  /**
    * Converts each local uuid to logical ids and returns the mapping
    *
    * @param fhirRequests
    * @return
    */
  def urnIdToLogicalIdMapping(fhirRequests: Seq[FHIRRequest]): Future[Seq[(String, String)]] = {
    Future.sequence(
      fhirRequests.map { fhirRequest =>
        val fullUrl = if (fhirRequest.isIdGenerated) None else Some(fhirRequest.id)
        fhirRequest.interaction match {
          //For FHIR create
          case FHIR_INTERACTIONS.CREATE =>
            fullUrl match {
              //If they give us a uuid, create an id for resource, return the mapping
              case Some(uuid) =>
                // Generate new resource Id, reference, url
                val rid = FHIRUtil.generateResourceId()
                val newReference = fhirRequest.resourceType.get + "/" + rid
                val newUrl = OnfhirConfig.fhirRootUrl + newReference
                //Put the identifier into the resource, as generated id
                fhirRequest.resourceId = Some(rid)
                Future.apply(Some((uuid, newReference)))
              case None =>
                (fhirRequest.resource.get \ FHIR_COMMON_FIELDS.ID).extractOpt[String] match {
                  //If there is no uuid, but they are trying to create the resource with a given id
                  case Some(oldId) =>
                    val rid = FHIRUtil.generateResourceId()
                    val oldReference = fhirRequest.resourceType.get + "/" + oldId
                    val newReference = fhirRequest.resourceType.get + "/" + rid
                    //Put the identifier into the resource, as generated id
                    fhirRequest.resourceId = Some(rid)
                    Future.apply(Some((oldReference, newReference)))
                  case None =>
                    //Otherwise no mapping
                    Future.apply(None)
                }
            }
          case FHIR_INTERACTIONS.UPDATE =>
            //If it is a conditional update and they give a uuid
            if (fullUrl.isDefined && fhirRequest.resourceId.isEmpty) {
              getResourceIdForConditionalUpdate(fhirRequest).map(_.map(rid => {
                val newReference = fhirRequest.resourceType.get + "/" + rid
                (fullUrl.get, newReference)
              }))
            } else Future.apply(None)
          case _ => Future.apply(None)
        }
      }
    ).map(_.flatten)
  }


}

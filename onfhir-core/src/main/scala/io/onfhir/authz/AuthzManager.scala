package io.onfhir.authz

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.directives.FutureDirectives.onComplete
import akka.http.scaladsl.server.directives.{BasicDirectives, RouteDirectives}
import akka.http.scaladsl.server.{Directive0, Rejection}
import io.onfhir.Onfhir
import io.onfhir.api.{FHIR_INTERACTIONS, FHIR_OPERATIONS, FHIR_PARAMETER_CATEGORIES}
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, Parameter}
import io.onfhir.api.util.{FHIRUtil, ResourceChecker}
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.ResourceManager
import io.onfhir.exception.AuthorizationFailedException
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.util.Success

/**
  * Created by tuncay on 19/12/2018.
  * Authorization Manager that authorizes the FHIR interactions
  */
object AuthzManager {
  //Execution context
  implicit val executionContext = Onfhir.actorSystem.dispatcher
  //Logger
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  //Rejection object for our authorization
  case class FhirAuthorizationFailedRejection(authzResult: AuthzResult) extends Rejection

  /**
    * Our authorization directive
    * @param authzContext Authorization context if any resolved
    * @param fhirRequest FHIR request object
    * @return
    */
  def authorize(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest):Directive0 = {

    onComplete(authorizeF(authzContext, fhirRequest)).flatMap {
      case Success(ar) =>
        if(ar.isAuthorized) BasicDirectives.pass else RouteDirectives.reject(FhirAuthorizationFailedRejection(ar))
      case scala.util.Failure(ex) =>
        logger.error("Exception while processing authorization", ex)
        RouteDirectives.reject(FhirAuthorizationFailedRejection(AuthzResult.failureInvalidRequest("Cannot process request for authorization!")))
    }
  }

  /**
    * Authorization decision for any FHIR interaction
    * @param authzContext Authorization context if any resolved
    * @param fhirRequest FHIR request object
    * @return throws AuthorizationFailedException if authorization failed
    */
  private def authorizeF(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest):Future[AuthzResult] = {
    //If authorization is disabled or interaction is public, everything is OK
    if (isPublic(fhirRequest,authzContext))
      Future.apply(AuthzResult.success())
    else {
      logger.debug(s"Authorizing request for context $authzContext ...")
      forceAuthorization(authzContext, fhirRequest)
    }
  }

  /**
    * Decide on authorization for the request
    * @param authzContext Authorization Context
    * @param fhirRequest FHIRRequest to be authorized
    * @return
    */
  def forceAuthorization(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest):Future[AuthzResult] = {
    //Get authorization decision
    val authzResult = getAuthorizationDecision(authzContext, fhirRequest)

    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.TRANSACTION | FHIR_INTERACTIONS.BATCH =>
        handleBatchAndTransaction(authzContext, fhirRequest, authzResult)
      case _ =>
        if (authzResult.result == AuthzResult.FILTERING) //If we have resource restrictions
          authorizeForSimpleInteraction(fhirRequest, authzResult) //Check them
        else Future.apply(authzResult) //otherwise return the decision
    }
  }

  /**
    * Special handling of batch and transaction interactions
    * @param authzContext Authorization Context
    * @param fhirRequest FHIRRequest to be authorized
    * @param authzResult General authorization result for batch/transaction
    * @return
    */
  private def handleBatchAndTransaction(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest, authzResult: AuthzResult):Future[AuthzResult] = {
    authzResult.result match {
      //If it is unauthorized, throw the related exception
      case AuthzResult.UNAUTHORIZED => throw new AuthorizationFailedException(authzResult)
      //If authorized or undecided, we go on in default
      case _ =>
        fhirRequest.interaction match {
          case FHIR_INTERACTIONS.BATCH =>
            Future.sequence(
              fhirRequest.childRequests
                .filter(_.response.isEmpty)
                .map(cr => {
                  val childAuthzResult = getAuthorizationDecision(authzContext, cr)
                  authorizeForSimpleInteraction(cr, childAuthzResult).map(r => cr -> r)
                })).map(results => {
                  results
                    .filterNot(_._2.isAuthorized)
                      .foreach(r => r._1.setResponse(FHIRResponse.errorResponse(StatusCodes.Unauthorized, r._2.toOutcomeIssue.toSeq)))
                  /*results.foreach(r => {
                    //Set the child requests, if unauthorized
                    r._1.isUnauthorized = ! r._2.isAuthorized
                    r._1.error = r._1.error ++ r._2.toOutcomeIssue.toSeq
                  })*/
                  AuthzResult.success()
                })
          case FHIR_INTERACTIONS.TRANSACTION =>
            //If any child request fails within transaction for authorization, return failure
            val results = fhirRequest.childRequests.to[scala.collection.immutable.Iterable]
              .map(cr => {
                val childAuthzResult = getAuthorizationDecision(authzContext, cr)
                authorizeForSimpleInteraction(cr, childAuthzResult).map(r => cr -> r)
              })
            Future.find(results)(!_._2.isAuthorized).map {
              case None => AuthzResult.success() //If there is no failed authorization in child requests
              case Some(az) =>
                fhirRequest.setResponse(FHIRResponse.errorResponse(StatusCodes.Unauthorized, az._2.toOutcomeIssue.map(o => o.copy(expression = o.expression ++ Seq(s"Request Uri:${az._1.requestUri}"))).toSeq))
                az._2 //If there is one failed, return it
            }
        }
    }
  }


  /**
    * Authorize for simple interactions apart from batch or transaction
    * @param fhirRequest FHIR request
    * @param authzResult Authorization result
    * @return
    */
  private def authorizeForSimpleInteraction(fhirRequest:FHIRRequest, authzResult: AuthzResult):Future[AuthzResult] = {
    if(authzResult.result == AuthzResult.FILTERING && authzResult.resourceRestrictions.nonEmpty ) {
      //One of the restriction set should be satifided
      authorizeAgainstGivenContent(fhirRequest, authzResult.resourceRestrictions).flatMap {
          case true => authorizeAgainstResourceContent(fhirRequest, authzResult.resourceRestrictions).map {
            case true => authzResult
            case false => AuthzResult.failureInsufficientScope("User is not authorized to execute the interaction on this resource instance")
          }
          case false => Future.apply(AuthzResult.failureInsufficientScope("User is not authorized to supply the given content"))
        }
    } else Future.apply(authzResult)
  }



  /**
    * Authorize the interaction for a given content
    * @param fhirRequest FHIR request
    * @param resourceRestrictions Resource restrictions coming from authorization
    * @return
    */
  private def authorizeAgainstGivenContent(fhirRequest:FHIRRequest, resourceRestrictions:List[Parameter]):Future[Boolean] = {
    Future.apply {
      fhirRequest.interaction match {
        //If we have the resource content for the interaction, we check it if it is OK with resource restrictions
        case FHIR_INTERACTIONS.CREATE | FHIR_INTERACTIONS.UPDATE =>
          ResourceChecker.checkIfResourceSatisfies(fhirRequest.resourceType.get, resourceRestrictions, fhirRequest.resource.get)
        //TODO Check patch items if the given values satisfies resource restrictions
        case  FHIR_INTERACTIONS.PATCH => true
        //For all other resources including the operations we don't care (For operations, detailed authorization should be done on content within the implementations)
        case _ => true
      }
    }
  }

  /**
    * Authorize according to the referenced resource content in db
    * @param fhirRequest FHIR request
    * @param resourceRestrictions Resource restrictions coming from authorization
    * @return
    */
  private def authorizeAgainstResourceContent(fhirRequest:FHIRRequest, resourceRestrictions:List[Parameter]):Future[Boolean] = {
    //If this is an instance interaction
    //           FHIR_INTERACTIONS.READ |
    //           FHIR_INTERACTIONS.UPDATE |
    //           FHIR_INTERACTIONS.DELETE |
    //           FHIR_INTERACTIONS.VREAD |
    //           FHIR_INTERACTIONS.HISTORY_INSTANCE |
    //           FHIR_INTERACTIONS.PATCH =>
    // or operation
    if (fhirRequest.resourceId.isDefined) {
      //Only include the required parameter paths to minimize parsing, etc
      val includedElements =
        resourceRestrictions
          .map(p=> FHIRUtil.extractElementPaths(fhirRequest.resourceType.get, p))
          .reduce((s1,s2) => s1 ++ s2)
      //Retrieve the mentioned document
      ResourceManager
        .getResource(fhirRequest.resourceType.get, fhirRequest.resourceId.get, fhirRequest.versionId, includingOrExcludingFields = Some(true -> includedElements), excludeExtraFields = true)
        .map(_.forall(r => ResourceChecker.checkIfResourceSatisfies(fhirRequest.resourceType.get, resourceRestrictions, r)))

    }
    else if(fhirRequest.resourceType.isDefined){
      fhirRequest.interaction match {
        //Search like operations (conditional update, delete)
        case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.DELETE | FHIR_INTERACTIONS.SEARCH | FHIR_INTERACTIONS.HISTORY_TYPE =>
          authorizeAgainstCompartmentSearch(fhirRequest, resourceRestrictions) match {
            case (false, _) => Future.apply(false)
            case (true, filteredResourceRestrictions) =>
              //Merge the parameters
              fhirRequest.queryParams = fhirRequest.queryParams ++ filteredResourceRestrictions
              Future.apply(true)
          }
        case _ => Future.apply(true)
      }
    } else
      Future.apply(true)
  }

  /**
    * If this is already a compartment search, check if the expected compartment id is same if resourceRestrictions also has a compartment query
    * @param fhiRequest FHIR Request
    * @param resourceRestrictions Resource restrictions coming from authorization
    * @return
    */
  private def authorizeAgainstCompartmentSearch(fhiRequest: FHIRRequest, resourceRestrictions:List[Parameter]):(Boolean, List[Parameter]) = {
    fhiRequest.compartmentType match {
      //If this is not a compartment we will not filter resource restrictions
      case None => true -> resourceRestrictions
      //If this is a compartment search
      case Some(compartment) =>
        val relatedCompartment =
          resourceRestrictions.find(rr =>
                rr.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT &&
                  rr.valuePrefixList.head._1 == compartment
            )

        val expectedCompartmentId:Option[String] =   relatedCompartment.map(rr => rr.valuePrefixList.head._2)
        //Check if the expected compartment id is equal to given compartment id
        expectedCompartmentId.forall(_ == fhiRequest.compartmentId.get) ->
          //Also filter the resource restrictions at the same time in case result is positive
          (
            if(relatedCompartment.nonEmpty)
              resourceRestrictions.filterNot(rr => rr .paramCategory != FHIR_PARAMETER_CATEGORIES.COMPARTMENT)
            else
              resourceRestrictions
            )
    }
  }


  /**
    * Get authorization decision from specific authorization handler
    * @param authzContext Authorization Context
    * @param fhirRequest FHIR Request to be authorized
    * @return
    */
  private def getAuthorizationDecision(authzContext: Option[AuthzContext], fhirRequest: FHIRRequest): AuthzResult = {
    authzContext match {
        case None =>
          //If there is no token check if users are publicly can access the interaction
          val authzResult = AuthzConfigurationManager.authorizationHandler.authorizeForPublic(fhirRequest.interaction, fhirRequest.resourceType, fhirRequest.resourceId)
          if(!authzResult.isAuthorized) //If not return missing access token
            AuthzResult.failureInvalidRequest("Missing access token!") //failure as token does not exist
          else
            authzResult
        case Some(ac) if ac.isActive => //if token is active check if the operation is allowed
          AuthzConfigurationManager.authorizationHandler.authorize(ac, fhirRequest.interaction, fhirRequest.resourceType, fhirRequest.resourceId)
        //If token is invalid return false
        case _ => AuthzResult.failureInvalidToken("Invalid access token or we cannot resolve it...")
     }
  }

  /**
    * Check if interaction is public in default
    * @param fhirRequest FHIR Request to be authorized
    * @param authzContext Authorization Context
    * @return
    */
  private def isPublic(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext]):Boolean = {
    !OnfhirConfig.authzConfig.isSecure() ||
      (fhirRequest.interaction match {
        //Public anyway, anonymous
        case FHIR_INTERACTIONS.CAPABILITIES => true
        //If user is authenticated, so all users
        case FHIR_OPERATIONS.VALIDATION => authzContext.isDefined
        //Otherwise no
        case _ => false
      })
  }
}

package io.onfhir.authz

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.directives.{BasicDirectives, RouteDirectives}
import io.onfhir.Onfhir
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, Parameter}
import io.onfhir.api.util.{FHIRUtil, ResourceChecker}
import io.onfhir.api.{FHIR_HTTP_OPTIONS, FHIR_INTERACTIONS, FHIR_OPERATIONS, FHIR_PARAMETER_CATEGORIES, FHIR_PARAMETER_TYPES}
import io.onfhir.config.{IFhirConfigurationManager, OnfhirConfig}
import io.onfhir.exception.{AuthorizationFailedException, AuthorizationFailedRejection}
import io.onfhir.path.FhirPathEvaluator
import org.json4s.{JArray, JNull, JObject, JString}
import org.json4s.JsonAST.JValue
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContextExecutor
import scala.util.Try

/**
  * Created by tuncay on 19/12/2018.
  * Authorization Manager that authorizes the FHIR interactions
  */
class AuthzManager(fhirConfigurationManager: IFhirConfigurationManager) {
  //Execution context
  implicit val executionContext: ExecutionContextExecutor = Onfhir.actorSystem.dispatcher
  //Logger
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val resourceChecker = new ResourceChecker(fhirConfigurationManager.fhirConfig)

  /**
    * Our authorization directive
    * @param authzContext Authorization context if any resolved
    * @param fhirRequest FHIR request object
    * @return
    */
  def authorize(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest):Directive0 = {
    checkAuthorization(authzContext, fhirRequest) match {
      case az if az.isAuthorized => BasicDirectives.pass
      case failed => RouteDirectives.reject(AuthorizationFailedRejection(failed))
    }
  }

  /**
    * Authorization decision for any FHIR interaction
    * @param authzContext Authorization context if any resolved
    * @param fhirRequest FHIR request object
    * @return throws AuthorizationFailedException if authorization failed
    */
  private def checkAuthorization(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest):AuthzResult = {
    //If authorization is disabled or interaction is public, everything is OK
    if (isPublic(fhirRequest,authzContext))
      AuthzResult.success()
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
  def forceAuthorization(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest):AuthzResult = {
    //Get authorization decision
    val authzResult = getAuthorizationDecision(authzContext, fhirRequest)
    //Prepare FHIR Path context for constraint evaluations
    val fhirPathContext = authzContext.map(ac =>AuthzManager.getFhirPathContext(fhirRequest, ac)).getOrElse(Map.empty)
    val fhirPathEvaluator = FhirPathEvaluator.apply(fhirPathContext)

    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.TRANSACTION | FHIR_INTERACTIONS.BATCH =>
        handleBatchAndTransaction(authzContext, fhirRequest, authzResult, fhirPathEvaluator)
      case _ =>
        if (authzResult.result == AuthzResult.FILTERING) //If we have resource restrictions
          authorizeForSimpleInteraction(fhirRequest, authzResult, fhirPathEvaluator) //Check them
        else
          authzResult //otherwise return the decision
    }
  }

  /**
    * Special handling of batch and transaction interactions
    * @param authzContext Authorization Context
    * @param fhirRequest FHIRRequest to be authorized
    * @param authzResult General authorization result for batch/transaction
    * @return
    */
  private def handleBatchAndTransaction(authzContext: Option[AuthzContext], fhirRequest:FHIRRequest, authzResult: AuthzResult, fhirPathEvaluator: FhirPathEvaluator):AuthzResult = {
    authzResult.result match {
      //If it is unauthorized, throw the related exception
      case AuthzResult.UNAUTHORIZED => throw new AuthorizationFailedException(authzResult)
      //If authorized or undecided, we go on in default
      case _ =>
        fhirRequest.interaction match {
          case FHIR_INTERACTIONS.BATCH =>
            fhirRequest
              .childRequests
              .filter(_.response.isEmpty)
              //Evaluate authorization for child request
              .map(cr => {
                val childAuthzResult = getAuthorizationDecision(authzContext, cr)
                cr -> authorizeForSimpleInteraction(cr, childAuthzResult, fhirPathEvaluator)
              })
              //Filter the unauthorized ones
              .filter(!_._2.isAuthorized)
              //Set the response for them
              .foreach {
                case (cr, ar) => cr.setResponse(FHIRResponse.errorResponse(StatusCodes.Unauthorized, ar.toOutcomeIssue.toSeq))
              }
            AuthzResult.success()
          case FHIR_INTERACTIONS.TRANSACTION =>
            //If any child request fails within transaction for authorization, return failure
            val results =
              fhirRequest
                .childRequests
                .map(cr => {
                  val childAuthzResult = getAuthorizationDecision(authzContext, cr)
                  cr -> authorizeForSimpleInteraction(cr, childAuthzResult, fhirPathEvaluator)
                })

            results.find(!_._2.isAuthorized) match {
              case None => AuthzResult.success()
              case Some(cr -> az) =>
                fhirRequest
                  .setResponse(
                    FHIRResponse
                      .errorResponse(
                        StatusCodes.Unauthorized,
                        az.toOutcomeIssue.map(o => o.copy(expression = o.expression ++ Seq(s"Request Uri:${cr.requestUri}"))).toSeq)
                      )
                az
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
  private def authorizeForSimpleInteraction(fhirRequest:FHIRRequest, authzResult: AuthzResult, fhirPathEvaluator: FhirPathEvaluator):AuthzResult = {
    if(authzResult.result == AuthzResult.FILTERING && authzResult.resourceRestrictions.nonEmpty ) {
      //One of the restriction set should be satifided
      authorizeAgainstGivenContent(fhirRequest, authzResult.resourceRestrictions.get, fhirPathEvaluator) match {
          case true =>
            authorizeAgainstResourceContent(fhirRequest, authzResult.resourceRestrictions.get, fhirPathEvaluator) match {
              case true => authzResult
              case false => AuthzResult.failureInsufficientScope("User is not authorized to execute the interaction on this resource instance")
            }
          case false =>
            AuthzResult.failureInsufficientScope("User is not authorized to supply the given content based on specified constraints!")
        }
    } else
      authzResult
  }



  /**
    * Authorize the interaction for a given content
    * @param fhirRequest FHIR request
    * @param authzConstraints Constraints for authorization
    * @param fhirPathEvaluator  FHIRPath evaluator initialized with authorization context
    * @return
    */
  private def authorizeAgainstGivenContent(fhirRequest:FHIRRequest, authzConstraints: AuthzConstraints, fhirPathEvaluator: FhirPathEvaluator):Boolean = {
    fhirRequest.interaction match {
        //If we have the resource content for the interaction, we check it if it is OK with resource restrictions
        case FHIR_INTERACTIONS.CREATE | FHIR_INTERACTIONS.UPDATE =>
          (
            authzConstraints.filters.isEmpty ||
            authzConstraints
              .filters
              .exists(query =>
                resourceChecker.checkIfResourceSatisfies(fhirRequest.resourceType.get, query, fhirRequest.resource.get)
              )
            ) && //And all constraints should be satisfied
              authzConstraints
                .contentConstraints
                .forall(fhirPathConstraint => fhirPathEvaluator.satisfies(fhirPathConstraint, fhirRequest.resource.get))

        //TODO Check patch items if the given values satisfies resource restrictions
        case  FHIR_INTERACTIONS.PATCH => true
        //For operations, there may be extra content constraints
        case op if op.startsWith("$") =>
          authzConstraints
            .contentConstraints
            .forall(fhirPathConstraint =>
              fhirPathEvaluator.satisfies(fhirPathConstraint, fhirRequest.resource.get)
            )

        //For all other resources including the operations we don't care (For operations, detailed authorization should be done on content within the implementations)
        case _ => true
    }
  }

  /**
    * Authorize according to the referenced resource content in db
    * @param fhirRequest FHIR request
    * @param authzConstraints Constraints
    * @return
    */
  private def authorizeAgainstResourceContent(fhirRequest:FHIRRequest, authzConstraints: AuthzConstraints, fhirPathEvaluator: FhirPathEvaluator):Boolean = {
    //If this is an instance interaction
    //           FHIR_INTERACTIONS.READ |
    //           FHIR_INTERACTIONS.UPDATE |
    //           FHIR_INTERACTIONS.DELETE |
    //           FHIR_INTERACTIONS.VREAD |
    //           FHIR_INTERACTIONS.HISTORY_INSTANCE |
    //           FHIR_INTERACTIONS.PATCH =>
    // or instance level operation
    if (fhirRequest.resourceId.isDefined) {
      fhirRequest
        .getResolvedSecurityContext  //If the target resource is resolved, check the constraints
        .forall(targetResource =>
          authzConstraints
            .filters
            .exists(q =>
              resourceChecker.checkIfResourceSatisfies(fhirRequest.resourceType.get, q, targetResource)
            ) &&
            authzConstraints
              .contentConstraints
              .forall(constraint => fhirPathEvaluator.satisfies(constraint, targetResource))
        )
    }
    else if(fhirRequest.resourceType.isDefined){
      fhirRequest.interaction match {
        //Search like operations (conditional update, delete)
        case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.DELETE | FHIR_INTERACTIONS.SEARCH | FHIR_INTERACTIONS.HISTORY_TYPE =>
          authorizeForSearchLike(fhirRequest, authzConstraints)
        case _ => true
      }
    } else
      true
  }

  /**
   * Handle authorization enforcement for search and searh like FHIR interactions
   * @param fhirRequest          FHIR request details
   * @param authzConstraints    Authorization constraints
   * @return
   */
  private def authorizeForSearchLike(fhirRequest: FHIRRequest, authzConstraints: AuthzConstraints):Boolean = {
    val uncoveredFilterConstraints =
      authzConstraints
        .filters
        .map(restrictions =>
          restrictions.filterNot(p => isCoveredInRequest(p, fhirRequest))
        )
        .filter(_.nonEmpty)

    uncoveredFilterConstraints match {
      case Nil => true
      //If the request is compartment search, and the compartment constraint is not satisfied just return false
      case Seq(constraint) if fhirRequest.compartmentType.nonEmpty && constraint.exists(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT) => false
      //If the request is not a compartment search, and there is an uncovered compartment constraint and
      // strict-compartment-authorization is set, we reject i.e enforcing user to search only for the specific entities
      case Seq(constraint) if
        OnfhirConfig.authzConfig.strictCompartmentAuthorizationEnforcement &&
          constraint.exists(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT) => false
      //Otherwise if there are other constraints, add them to query to filter the results further according to what user is authorized for
      case Seq(constraint) =>
        //Add the constraint to request to filter further and return authorized
        fhirRequest.addParsedQueryParams(constraint)
        true
      //Multiple constraints exist
      //e.g. user is authorized for category=laboratory or code=xxx  and query is more generic and does not filter for any of this
      case _ =>
        throw new AuthorizationFailedException(AuthzResult.undecided("Multiple result set constraints in authorization layer is not supported currently, please arrange your query accordingly!"))
    }
  }

  /**
   * Whether the given search like restriction is already covered in original request (query)
   * e.g. comp(patient) = 123    --> query: comp(patient)=123  --> true
   * e.g. category=laboratory    --> query: category=laboratory,vitalsign -->  true
   *
   * @param parameter   Restriction given as search parameter
   * @param request     FHIR request
   * @return
   */
  private def isCoveredInRequest(parameter: Parameter, request: FHIRRequest):Boolean = {
    def getAliasParams(param:String):Seq[String] =
      param match {
        case "subject" => Seq("patient")
        case _ => Nil
      }

    parameter.paramCategory match {
      case FHIR_PARAMETER_CATEGORIES.COMPARTMENT =>
        //If searching over same compartment
        (request.compartmentType.contains(parameter.valuePrefixList.head._1) && request.compartmentId.contains(parameter.valuePrefixList.head._2)) ||
          //Or one of the parameter is filtering for the same entity as the compartment
          //e.g. restriction: Patient/123   query= ?patient=Patient/123  --> true
          //e.g. restriction: Patient/123   query= ?catetory=...  --> false
          //e.g. restriction: Patient/123   query= ?patient=Patient/245  --> false
          //e.g. restriction: Patient/123   query= ?patient=Patient/123,Patient/245  --> false
          getParsedQueryParams(request)
            .exists(sp =>
              sp.paramCategory == FHIR_PARAMETER_CATEGORIES.NORMAL &&
                sp.paramType == FHIR_PARAMETER_TYPES.REFERENCE &&
                  parameter.chain.map(_._2).flatMap(p => p +: getAliasParams(p)).contains(sp.name) &&
                    sp.valuePrefixList.length == 1 && sp.valuePrefixList.exists {
                      case ("", referenceValue) =>
                        Try(FHIRUtil.parseReferenceValue(referenceValue))
                          .toOption
                          .exists(pr =>
                            pr._2 == parameter.valuePrefixList.head._1 && pr._3 == parameter.valuePrefixList.head._2
                          )
                    }
            )
      case FHIR_PARAMETER_CATEGORIES.NORMAL =>
        getParsedQueryParams(request)
          .exists(sp =>
            //Either parameters are equal (same filtering)
            sp.equals(parameter) ||
              //Or the query focuses on the subset
              (sp.paramCategory == FHIR_PARAMETER_CATEGORIES.NORMAL &&
                sp.name == parameter.name &&
                  sp.valuePrefixList.nonEmpty && sp.valuePrefixList.toSet.subsetOf(parameter.valuePrefixList.toSet))
          )
    }
  }

  /**
   * Parse the query params and return
   * @param fhirRequest FHIR request
   * @return
   */
  private def getParsedQueryParams(fhirRequest:FHIRRequest):List[Parameter] = {
    fhirConfigurationManager
      .fhirSearchParameterValueParser
      .parseSearchParameters(fhirRequest.resourceType.get, fhirRequest.queryParams, Some(FHIR_HTTP_OPTIONS.FHIR_SEARCH_LENIENT)) //Lenient parsing as validation/parsing will be done later again
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
          val authzResult = AuthzConfigurationManager.authorizationHandler.authorizeForPublic(fhirRequest)
          if(!authzResult.isAuthorized) //If not return missing access token
            AuthzResult.failureInvalidRequest("Missing access token!") //failure as token does not exist
          else
            authzResult
        case Some(ac) if ac.isActive => //if token is active check if the operation is allowed
          AuthzConfigurationManager.authorizationHandler
            .authorize(ac, fhirRequest)
        case Some(ac) if !ac.isActive =>
          AuthzResult.failureInvalidToken(ac.reasonNotActive.getOrElse("Invalid access token or we cannot resolve it..."))
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

object AuthzManager {
  /**
   * Constructor for authorization manager
   * @param fhirConfigurationManager FHIR configuration manager
   * @return
   */
  def apply(fhirConfigurationManager: IFhirConfigurationManager):AuthzManager = new AuthzManager(fhirConfigurationManager)
  /**
   * Get FHIRPath context for authorization context
   *
   * @param request         FHIR request details
   * @param authzContext    Authorization context
   * @return
   */
  def getFhirPathContext(request: FHIRRequest, authzContext: AuthzContext): Map[String, JValue] = {
    //Add the claims and scopes about the current user as context variables for FHIR Path evaluations
    val claimContext =
      JObject(
        (authzContext
          .furtherParams ++ //Further Claims e.g. patient, fhirUser
          //Base claims
          authzContext.sub.map(sub => "sub" -> JString(sub)).toMap ++
          authzContext.clientId.map(clientId => "client_id" -> JString(clientId)).toMap)
          .toList
      )

    val scopeContext = JArray(authzContext.scopes.map(JString).toList)

    //Add some request details as context variables for FHIR Path evaluations
    val requestContext =
      JObject(
        //Resource identifier
        "resourceId" -> request.resourceId.map(JString).getOrElse(JNull),
        //List of query params if this is a search
        "queryParams" -> (if (request.queryParams.nonEmpty) JArray(request.queryParams.keySet.toList.map(JString)) else JNull)
      )

    //Prepare all context for FHIR Path evaluations
    val fhirContext = Map("claims" -> claimContext, "request" -> requestContext, "scopes" -> scopeContext)
    //TODO may add others headers, etc if needed
    fhirContext
  }
}
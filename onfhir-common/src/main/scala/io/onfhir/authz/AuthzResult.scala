package io.onfhir.authz

import io.onfhir.api.FHIR_HTTP_OPTIONS
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser

/**
  * Created by tuncay on 3/3/2017.
  */
case class AuthzResult(
                        result:String, //Result of Authorization (AUTHORIZED, UNAUTHORIZED, FILTERING)
                        resourceRestrictions:List[Parameter] = List.empty, //List of List of Search parameters to define alternative resource restrictions for authorization (OR the results of the restrictions)
                        errorCode:Option[String] = None, //Error code if authorization failed
                        errorDesc:Option[String] = None) { //Description of error
  //If the authorization is ongoing still we should continue
  def isAuthorized: Boolean = result.equals(AuthzResult.AUTHORIZED) || result.equals(AuthzResult.FILTERING)

  /**
    * Convert the result to Outcome Issues
    * @return
    */
  def toOutcomeIssue:Option[OutcomeIssue] =
    if(!isAuthorized)
      Some(OutcomeIssue(
        FHIRResponse.SEVERITY_CODES.ERROR,
        FHIRResponse.OUTCOME_CODES.SECURITY,
        None,
        Some(s"Error: ${errorCode.get}; ${errorDesc.get}"),
        Seq("Header: Authorization")
      ))
    else None
}

/**
  *
  */
object AuthzResult{
  /**
    * Authorization result codes
    */
  final val AUTHORIZED = "authorized" //Authorized for the operation or query
  final val UNAUTHORIZED = "unauthorized" // Unauthorized for the operation or query
  final val FILTERING = "filtering" //Have partial authorization so results will be filtered or restricted with the given resource restrictions
  final val UNDECIDED = "undecided" //Cannot authorize the given interaction

  /**
    * Failure error codes
    */
  final val FAILURE_INVALID_REQUEST = "invalid_request"
  final val FAILURE_INVALID_TOKEN = "invalid_token"
  final val FAILURE_INSUFFICIENT_SCOPE = "insufficient_scope"

  /**
    * Create an authoriation result indicating success
    * @return
    */
  def success(): AuthzResult = new AuthzResult(AUTHORIZED)

  /**
    * Create an authorization result indicating that decision cannot be taken
    * @param msg Related msg
    * @return
    */
  def undecided(msg:String):AuthzResult = new AuthzResult(UNDECIDED, errorDesc = Some(msg))

  /**
    * Create a conditional authorization result which means authorize if the given restrictions are satisfied
    * @param rtype Related resource type
    * @param comparmentFilters Restrictions indicated as Compartment query e.g. records that belong to a specific Patient --> Compartment Patient -> 356589
    * @param authorizationFilterParams Restrictions idnicated as FHIR query
    * @return
    */
  def filtering(rtype:String, comparmentFilters:List[(String, String)], authorizationFilterParams: List[(String, String)]):AuthzResult =
    new AuthzResult(
      FILTERING,
      //Parse the compartment search
      comparmentFilters
        .map(cf => FHIRSearchParameterValueParser.constructCompartmentSearchParameter(cf._1, cf._2, rtype)) ++
        //Parse the other search parameters strictly
        FHIRSearchParameterValueParser
          .parseSearchParameters(
            rtype,
            authorizationFilterParams.groupBy(_._1).map(g => g._1 -> g._2.map(_._2)),
            Some(FHIR_HTTP_OPTIONS.FHIR_SEARCH_STRICT))
    )


  /**
    * Create an authorization result indicating failure (unauthorized case)
    * @param errorCode Failure Error code
    * @param errorDesc Description of error
    * @return
    */
  private def failure(errorCode:String, errorDesc:String): AuthzResult =
    new AuthzResult(result = UNAUTHORIZED, errorCode = Some(errorCode), errorDesc = Some(errorDesc))

  def failureInvalidToken(errorDesc:String): AuthzResult = failure(FAILURE_INVALID_TOKEN, errorDesc)
  def failureInsufficientScope(errorDesc:String): AuthzResult =failure(FAILURE_INSUFFICIENT_SCOPE, errorDesc)
  def failureInvalidRequest(errorDesc:String):AuthzResult=failure(FAILURE_INVALID_REQUEST, errorDesc)
}

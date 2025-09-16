package io.onfhir.authz

import io.onfhir.api.FHIR_HTTP_OPTIONS
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser

/**
  * Created by tuncay on 3/3/2017.
  */

/**
 * Authorization result created by authorizer
 * @param result                    The code indicating the result of Authorization (AUTHORIZED, UNAUTHORIZED, FILTERING)
 *                                  - AUTHORIZED:   Request is authorized (no need to change anything)
 *                                  - UNAUTHORIZED: Request is unauthorized
 *                                  - FILTERING:    Partial authorization (need to comply or result set should be restricted with the given restrictions)
 *                                  - UNDECIDED:    Authorization cannot be decided e.g. scopes conflict, unresolvable
 * @param resourceRestrictions      Restrictions
 * @param errorCode                 Error code if authorization failed
 * @param errorDesc                 Description of authorization failure
 */
case class AuthzResult(
                        result:String, //Result of Authorization (AUTHORIZED, UNAUTHORIZED, FILTERING)
                        resourceRestrictions:Option[AuthzConstraints] = None,
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
 * Constraints to apply for partial authorization results
 * @param filters             Filtering constraints given with parsed query statements (one of them should be satisfied to access/update the result)
 *                            e.g. Patient level smart scopes
 *                            patient/Observation.rs with patient=1234 --> patient=Patient/1234
 *                            patient/Observation.rs?category=laboratory --> patient=Patient/1234&category=laboratory
 *
 * @param contentConstraints  FHIR Path statements to be satisfied for the supplied content for FHIR create, update, etc
 *                            e.g. User can only create observations authored by himself
 *                            Observation.practitioner.reference = 'Practitioner/' & %claims.sub
 */
case class AuthzConstraints(filters:Seq[List[Parameter]] = Nil, contentConstraints:Seq[String] = Nil)

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
    * @param constraints Constraints for filtering
    * @return
    */
  def filtering(constraints:AuthzConstraints):AuthzResult = {
    new AuthzResult(FILTERING, Some(constraints))
  }


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

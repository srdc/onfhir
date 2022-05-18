package io.onfhir.authz

/**
  * Created by tuncay on 2/27/2017.
  * Interface for authorization modules for onFHIR for FHIR REST API
  */
trait IAuthorizer {
  /**
    * Further parameters required by the Authorizer within token or introspection response for policy enforcement
    * @return
    */
  def furtherParamsInAuthzContext:List[String]

  /**
    * Policy enforcer for all interactions
    * @param authzContext           resolved Authorization Context
    * @param interaction            Name of FHIR  interaction (all fhir interactions + custom or fhir operations as $...
    * @param resourceType           Type of the resource to be accessed for FHIR type and instance base interactions
    * @param resourceId             Id of the resource to be accessed for FHIR instance base interactions
    * @return Either rejection or success with a list of restrictions in the format of FHIR search query
    */
  def authorize(authzContext: AuthzContext,
                interaction:String,
                resourceType:Option[String],
                resourceId:Option[String]
               ):AuthzResult

  /**
    * Policy enforcer for public resources (if there is no token)
    * @param interaction Name of FHIR  interaction (all fhir interactions + custom or fhir operations as $...
    * @param resourceType Type of the resource to be accessed for FHIR type and instance base interactions
    * @param resourceId Id of the resource to be accessed for FHIR instance base interactions
    * @return
    */
  def authorizeForPublic(interaction:String, resourceType:Option[String], resourceId:Option[String]):AuthzResult

}

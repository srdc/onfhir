package io.onfhir.authz

/**
 * Authorization handler for basic authentication
 */
class BasicAuthorizer extends IAuthorizer {
  def furtherParamsInAuthzContext:List[String] = List.empty

  override def authorize(authzContext: AuthzContext,
                interaction: String,
                resourceType: Option[String],
                resourceId: Option[String]
               ): AuthzResult = AuthzResult.success()

  override def authorizeForPublic(interaction:String, resourceType:Option[String], resourceId:Option[String]):AuthzResult =
    AuthzResult.success()
}

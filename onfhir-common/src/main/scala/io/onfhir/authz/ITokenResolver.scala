package io.onfhir.authz

import scala.concurrent.Future

/**
  * Created by tuncay on 2/27/2017.
  * Interface for modules that resolve the meaning of a access token sent by the request
  */
trait ITokenResolver {
  /**
    * Somehow resolve the token and extract the Authorization context (scopes, etc)
    * Possible methods; OAuth2 Token Introspection or JWT token, or ?
    * @param accessToken Bearer access token
    * @param furtherParams Name of further parameters outside the scope of OAuth2 standard
    * @return
    */
  def resolveToken(accessToken:String, furtherParams:List[String] = List.empty):Future[AuthzContext]
}

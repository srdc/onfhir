package io.onfhir.authz

import java.time.Instant

import com.nimbusds.oauth2.sdk.auth.{ClientSecretBasic, Secret}
import com.nimbusds.oauth2.sdk.id.ClientID
import com.nimbusds.oauth2.sdk.{ClientCredentialsGrant, Scope, TokenRequest}
import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by tuncay on 10/24/2017.
  * Client utility to retrieve access token for OAuth Token service with client authentication
  */
class TokenClient(clientId:String, clientSecret:String, requiredScopes:Seq[String], authzServerMetadata:AuthorizationServerMetadata) extends Serializable {
  private val logger: Logger = LoggerFactory.getLogger(classOf[TokenClient])
  //Cached token and expiration time
  private var token:Option[(String, Instant)] = None

  /**
    * Get the Bearer access token stored
    * @return
    */
  def getToken:Option[String] = {
    if(token.isEmpty || token.get._2.isBefore(Instant.now())){
      token = getBearerToken()
    }
    token.map(_._1)
  }

  /**
    * Call the Token service of Authorization Server to retrieve the Access Token
    * @return
    */
  private def getBearerToken(): Option[(String, Instant)] = {
    try {
      logger.debug(s"Retrieving access token for client '${clientId}' ...")
      val uri = authzServerMetadata.token_endpoint.get
      //TODO Other client authentication options are not implemented yet
      val authentication = new ClientSecretBasic(new ClientID(clientId), new Secret(clientSecret))
      val grant = new ClientCredentialsGrant
      val tokenRequest = new TokenRequest(uri, authentication, grant, new Scope(requiredScopes:_*))
      //TODO Replace with comment part
      val tokenResponse = tokenRequest.toHTTPRequest.send.getContentAsJSONObject
      Some(tokenResponse.get("access_token").asInstanceOf[String] -> Instant.now().plusSeconds(tokenResponse.get("expires_in").asInstanceOf[Long] - 60))

      /*val tokenResponse = TokenResponse.parse(tokenRequest.toHTTPRequest.send)
      tokenResponse match {
        case accessTokenResponse:AccessTokenResponse =>
          logger.debug("Access Token retrieved successfully ...")
          val bearerToken = accessTokenResponse.getTokens.getBearerAccessToken
          Some(bearerToken.getValue -> Instant.now().plusSeconds(bearerToken.getLifetime - 60))
        case errorResponse:TokenErrorResponse =>
          logger.error(s"Error while obtaining access token! error: ${errorResponse.getErrorObject.getCode} - ${errorResponse.getErrorObject.getDescription}")
          None
      }*/
    }catch {
      case e:Exception =>
        logger.error("Unexpected error while returning access token", e)
        None
    }
  }
}

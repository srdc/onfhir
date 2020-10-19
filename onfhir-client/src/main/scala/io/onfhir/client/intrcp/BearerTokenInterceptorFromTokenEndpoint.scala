package io.onfhir.client.intrcp

import java.net.URI
import java.time.Instant

import akka.http.scaladsl.model.HttpRequest
import com.nimbusds.jose.JWSAlgorithm
import com.nimbusds.oauth2.sdk.auth.{ClientSecretBasic, ClientSecretJWT, ClientSecretPost, Secret}
import com.nimbusds.oauth2.sdk.id.ClientID
import com.nimbusds.oauth2.sdk.{AccessTokenResponse, ClientCredentialsGrant, Scope, TokenErrorResponse, TokenRequest, TokenResponse}
import com.typesafe.config.Config
import io.onfhir.api.client.FhirClientException
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by tuncay on 10/16/2020.
  * Client utility to retrieve OAuth2 bearer access token from OAuth Token Endpoint service with given client authentication method
  */
class BearerTokenInterceptorFromTokenEndpoint(clientId:String, clientSecret:String, requiredScopes:Seq[String], authzServerTokenEndpoint:String, clientAuthenticationMethod:String = "client_secret_basic")
  extends BearerTokenInterceptor with Serializable {
  private val logger: Logger = LoggerFactory.getLogger(classOf[BearerTokenInterceptorFromTokenEndpoint])
  //Cached token and expiration time
  private var token:Option[(String, Instant)] = None

  private val tokenEndpointUri = new URI(authzServerTokenEndpoint)

  private val authentication = clientAuthenticationMethod match {
    case "client_secret_basic" => new ClientSecretBasic(new ClientID(clientId), new Secret(clientSecret))
    case "client_secret_post" => new ClientSecretPost(new ClientID(clientId), new Secret(clientSecret))
    case "client_secret_jwt" => new ClientSecretJWT(new ClientID(clientId), tokenEndpointUri, JWSAlgorithm.HS512, new Secret(clientSecret))
    case oth => throw new FhirClientException(s"Client authentication method $oth not supported by this interceptor!")
  }
  //val authentication = new ClientSecretBasic(new ClientID(clientId), new Secret(clientSecret))
  private val grant = new ClientCredentialsGrant

  /**
   * Intercept and update the http request according to interceptor's own logic e.g. adding headers
   *
   * @param httpRequest
   * @return
   */
  override def processRequest(httpRequest: HttpRequest)(implicit ex: ExecutionContext): Future[HttpRequest] = {
    Future.apply {
      val bearerToken = getToken.get
      addHeader(httpRequest, bearerToken)
    }
  }

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
      val tokenRequest = new TokenRequest(tokenEndpointUri, authentication, grant, new Scope(requiredScopes:_*))
      //TODO Replace with comment part
      //val tokenResponse = tokenRequest.toHTTPRequest.send.getContentAsJSONObject
      //Some(tokenResponse.get("access_token").asInstanceOf[String] -> Instant.now().plusSeconds(tokenResponse.get("expires_in").asInstanceOf[Long] - 60))
      val tokenResponse = TokenResponse.parse(tokenRequest.toHTTPRequest.send)
      tokenResponse match {
        case accessTokenResponse:AccessTokenResponse =>
          logger.debug("Access Token retrieved successfully ...")
          val bearerToken = accessTokenResponse.getTokens.getBearerAccessToken
          Some(bearerToken.getValue -> Instant.now().plusSeconds(bearerToken.getLifetime - 60))
        case errorResponse:TokenErrorResponse =>
          logger.error(s"Error while obtaining access token! error: ${errorResponse.getErrorObject.getCode} - ${errorResponse.getErrorObject.getDescription}")
          None
      }
    }catch {
      case e:Exception =>
        logger.error("Unexpected error while returning access token", e)
        None
    }
  }
}

object BearerTokenInterceptorFromTokenEndpoint {

  def getFromConfig(config:Config, requiredScopes:Seq[String]): BearerTokenInterceptorFromTokenEndpoint = {
    val authzConfig = config.getConfig("onfhir.client.authz")
    val clientId = authzConfig.getString("client_id")
    val clientSecret = authzConfig.getString("client_secret")
    val authzServerTokenEndpoint = authzConfig.getString("token_endpoint")
    val clientAuthenticationMethod = authzConfig.getString("token_endpoint_auth_method")
    new BearerTokenInterceptorFromTokenEndpoint(clientId, clientSecret, requiredScopes, authzServerTokenEndpoint, clientAuthenticationMethod)
  }

  def apply(clientId: String, clientSecret: String, requiredScopes: Seq[String], authzServerTokenEndpoint: String, clientAuthenticationMethod: String): BearerTokenInterceptorFromTokenEndpoint = new BearerTokenInterceptorFromTokenEndpoint(clientId, clientSecret, requiredScopes, authzServerTokenEndpoint, clientAuthenticationMethod)
}
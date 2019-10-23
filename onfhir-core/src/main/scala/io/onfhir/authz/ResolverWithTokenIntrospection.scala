package io.onfhir.authz

import java.util.Date

import com.nimbusds.jose.jwk.RSAKey
import com.nimbusds.oauth2.sdk._
import com.nimbusds.oauth2.sdk.auth.{ClientAuthentication, ClientAuthenticationMethod, ClientSecretBasic, PrivateKeyJWT}
import com.nimbusds.oauth2.sdk.token.BearerAccessToken
import io.onfhir.Onfhir
import io.onfhir.config.AuthzConfig
import io.onfhir.exception.InternalServerException
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.util.Try

/**
  * Created by tuncay on 2/27/2017.
  */
class ResolverWithTokenIntrospection(authzConfig:AuthzConfig) extends ITokenResolver{
  implicit val executionContext = Onfhir.actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Prepare Client Authentication according to configurations
    * @return
    */
  private def prepareClientAuthentication():Option[ClientAuthentication] = {
    authzConfig.protectedResourceInformation.getMetadata.getTokenEndpointAuthMethod  match {
      case ClientAuthenticationMethod.CLIENT_SECRET_BASIC => Some(new ClientSecretBasic(authzConfig.protectedResourceInformation.getID, authzConfig.protectedResourceInformation.getSecret))
      case ClientAuthenticationMethod.PRIVATE_KEY_JWT =>
        Some(new PrivateKeyJWT(
          authzConfig.protectedResourceInformation.getID, //client ID
          authzConfig.authzServerMetadata.introspection_endpoint.get, // Introspection Endpoint URI
          authzConfig.protectedResourceInformation.getMetadata.getTokenEndpointAuthJWSAlg,
          authzConfig.protectedResourceJWKSet.getKeyByKeyId(authzConfig.protectedResourceCurrentSignerKeyId).asInstanceOf[RSAKey].toRSAPrivateKey,
          authzConfig.protectedResourceCurrentSignerKeyId,
          null
        ))
      case ClientAuthenticationMethod.NONE => None
    }
  }

  /**
    * Call the OAuth2 Token Introspection endpoint of the configured Authorization Server and validate the access token and extract the Authorization context
    * @param accessToken Bearer access token
    * @param furtherParams Name of further parameters outside the scope of OAuth2 standard
    * @return
    */
  override def resolveToken(accessToken: String, furtherParams:List[String] = Nil): Future[AuthzContext] = {
    Future.apply {
      //Prepare Token Introspection request based on client authentication method preferred
      val introspectionRequest = prepareClientAuthentication() match {
        case None => new TokenIntrospectionRequest(authzConfig.authzServerMetadata.introspection_endpoint.get, new BearerAccessToken(accessToken))
        case Some(clientAuthentication) => new TokenIntrospectionRequest(authzConfig.authzServerMetadata.introspection_endpoint.get, clientAuthentication, new BearerAccessToken(accessToken))
      }
      //Send the introspection and receive it
      val introspectionResponse = Try(TokenIntrospectionResponse.parse(introspectionRequest.toHTTPRequest.send()))
        .getOrElse(new TokenIntrospectionErrorResponse(new ErrorObject("500", "Invalid token introspection response!")))

      introspectionResponse match {
        //The nimbus library throws Parse Exception when response is invalid
        //If the result is success
        case s: TokenIntrospectionSuccessResponse =>
          //Check if the token is valid
          if (
            Try(s.isActive).getOrElse(false) &&
            Try(s.getNotBeforeTime.getTime < new Date().getTime).getOrElse(true) && //Check if the client use the token before nbt (not before time), if there is no nbt it is ok
              Try(s.getExpirationTime.getTime > new Date().getTime).getOrElse(true)) {
            //Check if it is not expired
            val paramObject = s.toJSONObject
            logger.debug(s"TokenIntrospection response:${paramObject.toString}")
            //Construct and return Authorization Context
            AuthzContext(
              isActive = true,
              Try(s.getClientID.getValue).toOption, //clientId, can be null
              Try(s.getScope.iterator().asScala.toSeq.map(_.getValue)).toOption.getOrElse(Nil), //scopes
              Option(s.getExpirationTime), //expiration time
              Try(s.getAudience.asScala.toList.map(_.getValue)).toOption.getOrElse(Nil), //audience
              Try(s.getSubject.getValue).toOption, //subject
              furtherParams.map(pn => pn -> paramObject.get(pn)).toMap, //further parameters (additional to Oauth2 by other specs)
              Option(s.getUsername) //Username
            )
          } else
            AuthzContext(isActive = false, reasonNotActive = Some("Token is not valid, expired or used before nbt!")) //Return if not valid
        //If error response, this is related with our own authentication to Authorization Server
        case e: TokenIntrospectionErrorResponse =>
          logger.error(s"Error during authentication for token introspection; ${e.getErrorObject.getCode}: ${e.getErrorObject.getDescription}")
          throw new InternalServerException("Problem accessing to Authorization server for authorization, please try again later :)")
      }
    }
  }
}

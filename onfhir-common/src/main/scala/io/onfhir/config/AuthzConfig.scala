package io.onfhir.config

import java.util.concurrent.TimeUnit

import akka.http.scaladsl.model.{HttpCharsets, MediaType}
import com.nimbusds.jose.JWSAlgorithm
import com.nimbusds.jose.jwk.JWKSet
import com.nimbusds.oauth2.sdk.client.ClientInformation
import com.typesafe.config.Config

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try
import io.onfhir.api.{AUTHZ_DISCOVERY_NONE, AUTHZ_METHOD_NONE, TOKEN_RESOLUTION_JWT}
import io.onfhir.authz.AuthorizationServerMetadata
import io.onfhir.exception.InitializationException
/**
  * Created by tuncay on 2/28/2017.
  * All configurations related with the authorization of FHIR server
  * @param authzConfig authorization config part
  */
class AuthzConfig(authzConfig:Config) {
  /**
    * Constants
    */
  //JWKS media type registration
  final val JWKSET_MEDIA_TYPE = MediaType.applicationWithFixedCharset("jwk-set+json", HttpCharsets.`UTF-8`, "jwkset")

  /**
    * Input configurations for Authorization
    */
  //Authorization method configured, default none
  lazy val authorizationMethod:String = Try(authzConfig.getString("method")).toOption.getOrElse(AUTHZ_METHOD_NONE)

  //Extra claim names used in access tokens
  lazy val authorizationExtraClaimsRole:Option[String] = Try(authzConfig.getString("claims.role")).toOption
  lazy val authorizationExtraClaimsFhirSubjectReference:Option[String] = Try(authzConfig.getString("claims.fhir-subject")).toOption
  lazy val authorizationExtraClaimsClientName:Option[String] = Try(authzConfig.getString("claims.client-name")).toOption

  //Token resolution method configured
  lazy val tokenResolutionMethod:String = Try(authzConfig.getString("token-resolution")).toOption.getOrElse(TOKEN_RESOLUTION_JWT)

  //Signature algorithm if token resolution is by JWT
  lazy val jwtSignatureAlgorithm:Option[JWSAlgorithm] = Try(authzConfig.getString("token-resolution-signature-algorithm")).toOption.map(alg => JWSAlgorithm.parse(alg))

  //If signature algorithm is symmetric , the secret key
  lazy val jwtSignatureSecretKey:Option[String] = Try(authzConfig.getString("token-resolution-signature-secret")).toOption

  //Whether the registration of this resource provider to AuthorizationServer will be dynamic or already registered manually
  lazy val isAuthorizationServerRegistrationDynamic:Boolean = Try(authzConfig.getBoolean("authz-server-dynamic-registration")).toOption.getOrElse(false)

  //File path for the Our Resource Provider metadata configurations
  lazy val protectedResourceMetadataPath:Option[String] = Try(authzConfig.getString("resource-server-metadata-path")).toOption

  //File path for the JWKS file (including signing keys) for our resource provider
  lazy val protectedResourceJWKSPath:Option[String] = Try(authzConfig.getString("resource-server-jwks-path")).toOption

  //Whether we will discover the Authorization Server or its metadata is given by configuration (See authorizationServerMetadataPath)
  lazy val authorizationServerDiscovery:String = Try(authzConfig.getString("authz-server-discovery")).toOption.getOrElse(AUTHZ_DISCOVERY_NONE)
  //Whether we will discover the Authorization Server or its metadata is given by configuration (See authorizationServerMetadataPath)
  lazy val authorizationServerDiscoveryURL:String =
    if(!authorizationServerDiscovery.equals(AUTHZ_DISCOVERY_NONE))
      Try(authzConfig.getString("authz-server-discovery-url")).toOption match {
        case None=> throw new InitializationException("The configuration param 'authz-server-discovery-url' should not be null if 'authz-server-discovery' is true!")
        case Some(p) => p
      }
    else ""

  // Root URL for Authorization Server (the paths given in the authorizationServerMetadataPath is relative with placeholders)
  lazy val authorizationServerUrl:Option[String] =
    //if(authorizationMethod != AUTHZ_METHOD_NONE)
      Try(authzConfig.getString("authz-server-url")).toOption /*match {
        case None => throw new InitializationException("The configuration param 'authz-server-url' should not be null if 'method' is not 'none'!")
        case Some(url) => Some(url)
      }
    else
      None*/

  //Path to the Authorization Server metadata
  lazy val authorizationServerMetadataPath:Option[String] =
    if(authorizationServerDiscovery.equals(AUTHZ_DISCOVERY_NONE))
        Try(authzConfig.getString("authz-server-metadata-path")).toOption match {
          case None => throw new InitializationException("The configuration param 'authz-server-metadata' should not be null if 'authz-server-discovery' is false!")
          case Some(x) => Some(x)
        }
    else None


   lazy val tokenCachingMaxCapacity: Int = Try(authzConfig.getInt("token-caching-max-capacity")).toOption.getOrElse(1000)
   lazy val tokenCachingInitialCapacity: Int = Try(authzConfig.getInt("token-caching-initial-capacity")).toOption.getOrElse(100)
   lazy val tokenCachingTTL: FiniteDuration = Try(Duration(authzConfig.getLong("token-caching-time-to-live"), TimeUnit.MINUTES)).toOption.getOrElse(Duration(30, TimeUnit.MINUTES))
   lazy val tokenCachingIdle: FiniteDuration = Try(Duration(authzConfig.getLong("token-caching-time-to-idle"), TimeUnit.MINUTES)).toOption.getOrElse(Duration(10, TimeUnit.MINUTES))

  /**
    * Resulting Runtime configurations
    */
  //Metadata of the Authorization Server
  var authzServerMetadata:AuthorizationServerMetadata = _

  //Metadata of fhir server as Protected Resource Server
  var protectedResourceInformation:ClientInformation = _

  //Fhir server's JWK set
  var protectedResourceJWKSet:JWKSet = _

  //Our default signing key id
  var protectedResourceCurrentSignerKeyId:String = _



  /**
    * If our server will be secure or not
    * @return
    */
  def isSecure():Boolean = ! authorizationMethod.equals(AUTHZ_METHOD_NONE)
}

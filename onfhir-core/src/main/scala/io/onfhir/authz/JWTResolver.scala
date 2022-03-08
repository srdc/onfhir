package io.onfhir.authz

import java.text.ParseException

import com.nimbusds.jose.JWSAlgorithm.Family
import com.nimbusds.jose.jwk.source.{ImmutableSecret, RemoteJWKSet}
import com.nimbusds.jose.proc.{BadJOSEException, JWSVerificationKeySelector, SecurityContext}
import com.nimbusds.jose.{JOSEException, JWSAlgorithm}
import com.nimbusds.jwt.proc.{ConfigurableJWTProcessor, DefaultJWTProcessor}
import io.onfhir.Onfhir
import io.onfhir.config.AuthzConfig

import scala.jdk.CollectionConverters._
import scala.concurrent.Future
import scala.util.Try

/**
  * Created by tuncay on 2/27/2017.
  */
class JWTResolver(authzConfig:AuthzConfig) extends ITokenResolver {
  implicit val executionContext = Onfhir.actorSystem.dispatcher
  /**
    * Initialize JWT Processor on setup
    */
  lazy val jwtProcessor:ConfigurableJWTProcessor[SecurityContext]  = {
    val myJwtProcessor = new DefaultJWTProcessor[SecurityContext]()
    val signatureAlgorithm:JWSAlgorithm = authzConfig.jwtSignatureAlgorithm.get

    if(Family.RSA.contains(signatureAlgorithm))
      myJwtProcessor.setJWSKeySelector(new JWSVerificationKeySelector(signatureAlgorithm, new RemoteJWKSet[SecurityContext](authzConfig.authzServerMetadata.jwks_uri.get.toURL)))
    else if(Family.HMAC_SHA.contains(signatureAlgorithm)){
      myJwtProcessor.setJWSKeySelector(new JWSVerificationKeySelector(signatureAlgorithm, new ImmutableSecret[SecurityContext](authzConfig.jwtSignatureSecretKey.get.getBytes())))
    }

    myJwtProcessor
  }

  /**
    * Resolve the authorization context from JWT encoded access token
    * @param accessToken Bearer access token
    * @param furtherParams Name of further parameters outside the scope of OAuth2 standard
    * @return
    */
  override def resolveToken(accessToken: String, furtherParams:List[String] = Nil): Future[AuthzContext] = {
    Future.apply {
      try {
        val claimSet = jwtProcessor.process(accessToken, null)
        val aud = claimSet.getAudience.asScala
        //Check if it is valid (other checks like expiration are done in process)
        if(
          aud.exists(_.equals(authzConfig.protectedResourceInformation.getID.getValue)) && //If our(resource-server's) client id is within the audience
          claimSet.getIssuer.equals(authzConfig.authzServerMetadata.issuer) // If the issuer of the token is our Authorization Server
        ){
          AuthzContext(
            isActive = true,
            clientId = aud.find(! _.equals(authzConfig.protectedResourceInformation.getID.getValue)),
            scopes = Try(Option(claimSet.getStringClaim("scope"))).toOption.flatten.map(_.split(" ").toSeq).getOrElse(Seq.empty), //scopes
            expirationTime = Option(claimSet.getExpirationTime),
            aud = aud.toSeq,
            sub = Option(claimSet.getSubject),
            username = Try(Option(claimSet.getStringClaim("username"))).toOption.flatten,
            furtherParams = furtherParams.map(pname => pname -> claimSet.getClaim(pname)).toMap
          )
        } else {
          AuthzContext(isActive = false, reasonNotActive = Some("Token is not valid, wrong issuer id or target aud !")) //Return if not valid
        }
      } catch {
        case p: ParseException => AuthzContext(isActive = false, reasonNotActive = Some("Token is not a valid JWT! " +p.getMessage))
        case bj: BadJOSEException => AuthzContext(isActive = false, reasonNotActive = Some("Token is not a valid! " + bj.getMessage))
        case j: JOSEException => AuthzContext(isActive = false, reasonNotActive = Some("Token is not a valid! " + j.getMessage))
      }
    }
  }
}

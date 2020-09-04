package io.onfhir.authz

import akka.Done
import akka.http.caching.LfuCache
import akka.http.caching.scaladsl.{Cache, CachingSettings}
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.directives.{BasicDirectives, Credentials, HeaderDirectives, MiscDirectives}
import io.onfhir.Onfhir
import io.onfhir.api.AUTHZ_METHOD_NONE
import io.onfhir.config.OnfhirConfig
import io.onfhir.config.OnfhirConfig.authzConfig
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

/**
  * Created by tuncay on 2/27/2017.
  * Authentication Manager
  */
object AuthManager {
  implicit val executionContext = Onfhir.actorSystem.dispatcher
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  final val PARAM_NETWORK_ADDRESS = "networkAddress"

  /**
    * Cache Settings
    */
  private val defaultCachingSettings = CachingSettings(Onfhir.actorSystem)
  private val lfuCacheSettings =
    defaultCachingSettings.lfuCacheSettings
      .withInitialCapacity(authzConfig.tokenCachingInitialCapacity)
      .withMaxCapacity(authzConfig.tokenCachingMaxCapacity)
      .withTimeToLive(authzConfig.tokenCachingTTL)
      .withTimeToIdle(authzConfig.tokenCachingIdle)
  private val cachingSettings = defaultCachingSettings.withLfuCacheSettings(lfuCacheSettings)
  /**
    * Cache for the resolved access tokens
    */
  val authzContextCache:Cache[String, AuthzContext] = LfuCache[String, AuthzContext](cachingSettings)


  /**
   * Authenticate the requests for internal api
    * @param credentials
   * @return
   */
 def authenticateForInternalApi(credentials: Credentials):Option[Done] = {
   if(OnfhirConfig.internalApiAuthenticate) {
     credentials match {
       case p@Credentials.Provided(token) =>
         None //TODO implement
       case _ =>
         None
     }
   } else
     Some(Done)
 }

  /**
    * Return the Authentication and Authorization Context
    * @return
    */
  def authenticate():Directive1[(AuthContext, Option[AuthzContext])] = {
    HeaderDirectives.optionalHeaderValueByType[Authorization](()).flatMap(authorizationHeader => {
      clientIP.flatMap(networkAddress => {
        val accessToken = getToken(authorizationHeader)

        if(authzConfig.authorizationMethod == AUTHZ_METHOD_NONE)
          BasicDirectives.provide[(AuthContext, Option[AuthzContext])](AuthContext(accessToken, networkAddress), None)
        else {
          logger.debug("Authenticating the request ...")
          //Resolve Authorization Context from access token
          val authzContext = accessToken.map(ac => resolveToken(ac, AuthzConfigurationManager.authorizationHandler.furtherParamsInAuthzContext))
          BasicDirectives.provide[(AuthContext, Option[AuthzContext])](AuthContext(accessToken, networkAddress), authzContext)
        }
      })
    })
  }

  /**
    * Retrieve clientIP, if not exist return unknown (normal directive is problematic when tests run)
    * @return
    */
  private def clientIP:Directive1[String] =
      MiscDirectives.extractClientIP.flatMap(remoteAddress => {
        BasicDirectives.provide[String](remoteAddress.toOption.map(_.getHostAddress).getOrElse("unknown"))
      }).recoverPF {
        case _ => BasicDirectives.provide[String]("unknown")
      }



  /**
    * Resolve token by a token resolver and cache it and when it is in cache serve it from there
    * @param accessToken access token
    * @param furtherRequiredParams extra parameters
    * @return
    */
  private def resolveToken(accessToken:String, furtherRequiredParams:List[String]):AuthzContext = {
    Try {
      Await.result(
        //Try to retrieve from cache
        authzContextCache.get(accessToken) match {
          //if there is no in cache, resolve it and cache it
          case None =>
            logger.debug(s"Resolving access token ...")
            authzContextCache.apply(accessToken, () => AuthzConfigurationManager.tokenResolver.resolveToken(accessToken, furtherRequiredParams))
          //if there is, check if it is expired, if yes remove it and return token expired, if not expired return it
          case Some(fac) => fac.flatMap { ac =>
            if(ac.isExpired) {
              logger.debug(s"Access token expired ...")
              authzContextCache.remove(accessToken)
              Future(AuthzContext(isActive = false, reasonNotActive = Some("Token expired...")))
            } else{
              Future(ac)
            }
          }
        },
        Duration(3000, MILLISECONDS)
      )
    }.getOrElse(AuthzContext(isActive = false, reasonNotActive = Some("Problem while introspecting/processing the access token...")))
  }


  /**
    * Parse Authorization header and retrieve the Bearer token
    * @param authorizationHeader authorization header
    * @return
    */
  private def getToken(authorizationHeader:Option[Authorization]):Option[String] = {
    authorizationHeader.flatMap(header => header.credentials match {
      case bt: OAuth2BearerToken => Some(bt.token)
      case _ => None
    })
  }


}

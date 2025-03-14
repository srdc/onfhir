package io.onfhir.api.endpoint

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.nimbusds.jose.jwk.JWKSet
import io.onfhir.config.OnfhirConfig

trait SecurityEndpoint {
  /**
    * Routes related with security issues
    */
  val securityRoute:Route =
    get {
      //Returning our JWKSet
      path(".well-known" / "jwks") {
        jwks
      } ~
      path("jwks") {
        //TODO
        //respondWithMediaType(AuthzConfig.JWKSET_MEDIA_TYPE) {
        jwks
        //}
      }
    }

  private def jwks = {
    complete {
      HttpResponse
        .apply(StatusCodes.OK)
        .withEntity(OnfhirConfig.authzConfig.protectedResourceJWKSet.toPublicJWKSet.toString)
    }
  }

}

package io.onfhir.api.endpoint

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.onfhir.config.OnfhirConfig

trait SecurityEndpoint {
  /**
    * Routes related with security issues
    */
  val securityRoute:Route =
    get {
      //Returning our JWKSet
      path(OnfhirConfig.baseUri / "jwks") {
        //TODO
        //respondWithMediaType(AuthzConfig.JWKSET_MEDIA_TYPE) {
        complete {
          HttpResponse
            .apply(StatusCodes.OK)
            .withEntity(OnfhirConfig.authzConfig.protectedResourceJWKSet.toPublicJWKSet.toString)
        }
        //}
      }
    }

}

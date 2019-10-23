package io.onfhir.exception

import akka.http.scaladsl.server.Rejection
import io.onfhir.authz.AuthzResult

/**
  * Rejection of the route for authorization failure
  * @param authzResult
  */
case class AuthorizationFailedRejection(authzResult: AuthzResult) extends Rejection

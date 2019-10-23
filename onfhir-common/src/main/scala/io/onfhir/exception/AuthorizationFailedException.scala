package io.onfhir.exception

import io.onfhir.authz.AuthzResult

/**
  * Created by tuncay on 5/16/2017.
  */
class AuthorizationFailedException(ar: AuthzResult) extends Exception {
  val authzResult = ar
}

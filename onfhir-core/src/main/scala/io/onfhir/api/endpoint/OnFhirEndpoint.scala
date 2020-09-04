package io.onfhir.api.endpoint
import akka.http.scaladsl.server.Directives._

trait OnFhirEndpoint extends OnFhirInternalEndpoint {
  //Combine FHIR routes with internal route
  val routes = internalRoute
}

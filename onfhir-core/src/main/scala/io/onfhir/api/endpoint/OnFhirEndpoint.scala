package io.onfhir.api.endpoint

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.onfhir.Onfhir

trait OnFhirEndpoint extends FHIREndpoint {
  //Combine FHIR routes with other internal onFhir module routes (currently we have only cds)
  val routes:Route = Onfhir().cdsHooksRoute match {
    case None => fhirRoute
    case Some(cdsRoute) =>
      concat(cdsRoute, fhirRoute)
  }
}

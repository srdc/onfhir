package io.onfhir.r4

import io.onfhir.Onfhir
import io.onfhir.r4.config.{FhirR4Configurator}

object Boot extends App {
  //Initialize onfhir for R4
  var onfhir = Onfhir.apply(new FhirR4Configurator())
  //Start it
  onfhir.start
}

package io.onfhir.r5

import io.onfhir.Onfhir
import io.onfhir.r5.config.FhirR5Configurator

object Boot extends App {
  //Initialize onfhir for R4
  var onfhir = Onfhir.apply(new FhirR5Configurator())
  //Start it
  onfhir.start
}

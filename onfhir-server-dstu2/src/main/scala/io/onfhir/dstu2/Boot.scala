package io.onfhir.dstu2

import io.onfhir.Onfhir
import io.onfhir.dstu2.config.DSTU2Configurator

object Boot extends  App {
  //Initialize onfhir for R4
  var onfhir = Onfhir.apply(new DSTU2Configurator())
  //Start it
  onfhir.start
}

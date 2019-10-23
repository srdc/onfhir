package io.onfhir.stu3

import io.onfhir.Onfhir
import io.onfhir.stu3.config.STU3Configurator

object Boot extends App {
  //Initialize onfhir for DSTU3
  var onfhir = Onfhir.apply(new STU3Configurator())
  //Start it
  onfhir.start
}

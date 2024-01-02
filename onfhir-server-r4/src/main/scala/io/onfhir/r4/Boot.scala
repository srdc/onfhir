package io.onfhir.r4

import io.onfhir.Onfhir
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.EmbeddedMongo
import io.onfhir.r4.config.FhirR4Configurator

object Boot extends App {

  // Start an embedded mongo if it is configured before any other processing.
  if (OnfhirConfig.mongoEmbedded) {
    // If it is configured to use an embedded Mongo instance
    val firstHostConfig = OnfhirConfig.mongodbHosts.head.split(':')
    EmbeddedMongo.start(OnfhirConfig.serverName, firstHostConfig(0), firstHostConfig(1).toInt, withTemporaryDatabaseDir = false)
  }

  //Initialize onfhir for R4
  var onfhir = Onfhir.apply(new FhirR4Configurator())
  //Start it
  onfhir.start
}

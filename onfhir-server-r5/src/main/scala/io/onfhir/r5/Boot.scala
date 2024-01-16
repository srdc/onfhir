package io.onfhir.r5

import io.onfhir.Onfhir
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.EmbeddedMongo
import io.onfhir.r5.config.FhirR5Configurator

object Boot extends App {

  // Start an embedded mongo if it is configured before any other processing.
  if (OnfhirConfig.mongoEmbedded) {
    // If it is configured to use an embedded Mongo instance
    val firstHostConfig = OnfhirConfig.mongodbHosts.head.split(':')
    EmbeddedMongo.start(OnfhirConfig.serverName, firstHostConfig(0), firstHostConfig(1).toInt, withTemporaryDatabaseDir = false)
  }

  //Initialize onfhir for R5
  var onfhir = Onfhir.apply(new FhirR5Configurator())
  //Start it
  onfhir.start
}

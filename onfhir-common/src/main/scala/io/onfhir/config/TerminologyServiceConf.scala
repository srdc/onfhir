package io.onfhir.config

import scala.concurrent.duration.Duration

/**
 * Metadata for a terminology service
 * TODO Extend the definition
 *
 * @param name                Name of the service
 * @param supportedValueSets  Supported ValueSet urls and versions if supplied
 */
case class TerminologyServiceConf(
                                   name:String,
                                   timeout: Duration,
                                   supportedValueSets:Map[String, Option[Set[String]]]
                                 )

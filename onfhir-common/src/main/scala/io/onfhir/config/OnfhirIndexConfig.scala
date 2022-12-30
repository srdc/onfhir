package io.onfhir.config

/**
 * onFHIR index configuration model
 * @param createIndexForCompartments  Whether automatically create indexes for target compartment search parameters
 * @param resources                   Index configuration for each FHIR resource type
 */
case class OnfhirIndexConfig(createIndexForCompartments:Boolean = false, resources:Seq[ResourceIndexConfiguration])

/**
 * Index configuration for a resource type
 * @param resource FHIR resource type e.g. Observation
 * @param shardKey FHIR search parameter(s) to use as shard key. Given parameters should target a single path and should
 *                 exist for all resources. e.g. subject --> patient based sharding
 * @param indexes  Indexes to be created for the resource type
 */
case class ResourceIndexConfiguration(resource:String, shardKey:Option[Seq[String]], indexes:Seq[OnfhirIndex])

case class OnfhirIndex(parameters:Seq[String] = Nil, description:Option[String] = None)
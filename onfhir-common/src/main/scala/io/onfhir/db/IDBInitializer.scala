package io.onfhir.db

import io.onfhir.api.Resource
import io.onfhir.config.{OnfhirIndex, ResourceIndexConfiguration, SearchParameterConf}

import scala.concurrent.Future

trait IDBInitializer {
  /**
   * Pre-initialization for the database
   * e.g. configure sharding, etc.
   */
  def prepareSharding():Future[Unit]

  /**
   * List existing collections for persisting resource types
   * @return
   */
  def listExistingCollections(history:Boolean = false):Future[Seq[String]]
  /**
   * Create a table for maintaining latest version of resources for a specific FHIR Resource type
   * Note: If already a collection exist, nothing is changed for that collection
   *
   * @param rtype FHIR resource type
   */
  def createCollection(rtype:String):Future[String]

  /**
   * Create a table/collection for maintaining historic versions of resources for a specific FHIR Resource type
   * @param rtype FHIR resource type
   * @return
   */
  def createHistoryCollection(rtype:String):Future[String]

  /**
   * Create configured indexes for a resource type
   * @param resourceType                  FHIR Resource type
   * @param searchParameterConfigurations All supported search parameters for resource type
   * @param parameterNamesToBeIndexed     Search parameters to create index for
   */
  def createIndexForResourceType(resourceType:String, searchParameterConfigurations:Seq[SearchParameterConf], indexes:Seq[OnfhirIndex]):Future[Unit]

  /**
   * Enable sharding for resource type based on configuration
   *
   * @param resourceType                  Resource type
   * @param indexConfiguration            Index configurations
   * @param searchParameterConfigurations Search parameters supported
   * @return
   */
  def enableSharding(resourceType:String, indexConfiguration:ResourceIndexConfiguration, searchParameterConfigurations:Seq[SearchParameterConf]):Future[Unit]

  /**
   * Enable sharding on resource type for history instances (default on id field)
   *
   * @param resourceType Resource type
   */
  def enableShardingForHistory(resourceType: String):Future[Unit]

  /**
   * Refresh db config after major changes (like sharding)
   * @return
   */
  def refreshDbConfig():Future[Unit]

  /**
   * Store the infrastructure resources (Conformance, StructureDefinition, SearchParameter, ValueSet, etc)
   * if not exist in DB
   *
   * @param resourceType FHIR Resource type (e.g. Condition, Observation)
   * @param resources    The content of the infrastructure resources
   */
  def storeInfrastructureResources(resourceType: String, resources: Seq[Resource]):Unit
}

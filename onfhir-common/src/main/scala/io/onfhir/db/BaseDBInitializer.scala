package io.onfhir.db

import io.onfhir.api.FHIR_VERSIONING_OPTIONS
import io.onfhir.config.IndexConfigurator.ResourceIndexConfiguration
import io.onfhir.config.SearchParameterConf
import io.onfhir.exception.InitializationException
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

abstract class BaseDBInitializer(shardingEnabled:Boolean) extends IDBInitializer {
  private val logger:Logger = LoggerFactory.getLogger(this.getClass)


  def prepareDatabase():Unit = {
    logger.info("Preparing database ...")
    if(shardingEnabled) {
      logger.info("Enable sharding for database...")
      Await.result(prepareSharding(), 60 seconds)
    }
  }
  /**
   * Create the collections/tables for all supported FHIR resources
   * Note: If already a collection exist, nothing is changed for that collection
   *
   * @param supportedResources List of supported resource names (e.g. Observation, Condition, etc) -> Versioning mechanism (no-version | versioned | versioned-update)
   * @return
   */
  def createCollections(supportedResources:Map[String, String]): Unit = {
    logger.info("Creating collections for supported resources ...")
    try {
      // Get the list of collections for FHIR resource types (for storing current versions) present at the database
      val collectionList = Await.result(listExistingCollections(), 60 seconds)

      // For each resource in Supported Resources not in Mongo yet
      supportedResources.keySet.diff(collectionList.toSet)
        .foreach(eachResource =>
          Await.result(createCollection(eachResource), 60 seconds)
        )
    }
    catch {
      case e: Exception => logger.error("Failure in creating collections for supported resources: " + e.getMessage)
        throw new InitializationException(e.getMessage)
    }

    try {
      // Get the list of collections for FHIR resource types (for storing current versions) present at the database
      val historyCollectionList = Await.result(listExistingCollections(history = true), 60 seconds)

      // For each resource in Supported Resources not in Mongo yet
      supportedResources
        .filterNot(_._2 == FHIR_VERSIONING_OPTIONS.NO_VERSION) //Only create history collection if we support versioning for resource type
        .keySet
        .diff(historyCollectionList.toSet) //Only create if the collection is not created yet
        .foreach(eachResource =>
          Await.result(createHistoryCollection(eachResource), 60 seconds)
        )

    } catch {
      case e: Exception =>
        logger.error("Failure in creating history collections for supported resources: " + e.getMessage)
        throw new InitializationException(e.getMessage)
    }
  }

  /**
   * Create Database indexes for all collections by going over the Resource Query parameter and common query parameter configurations
   *
   * @param supportedResources    All supported resources and versioning supported for resource type
   * @param allParameters         All search parameters defined for resource types
   * @param commonQueryParameters Common query parameters
   * @param indexConfigurations   Configurations for database indexing
   */
  def createIndexes(
                     supportedResources: Map[String, String],
                     allParameters: Map[String, Map[String, SearchParameterConf]],
                     commonQueryParameters: Map[String, SearchParameterConf],
                     indexConfigurations: Map[String, ResourceIndexConfiguration]): Unit = {
    logger.info("Creating database indexes from configurations ...")

    allParameters.foreach(resourceQueryParameters => {
      val resourceType = resourceQueryParameters._1
      logger.info(s"Creating indexes for $resourceType ...")

      //All parameters supported for the resource
      val searchParameterConfigurations: Seq[SearchParameterConf] = resourceQueryParameters._2.values.toSeq ++
        commonQueryParameters.values //Add the common parameters

      //Get the search parameters to be indexed from the configurations and filter the parameters to be indexed
      var parameterNamesToBeIndexed = indexConfigurations.get(resourceType).map(_.indexes).getOrElse(Set.empty)
      //If sharding is not enabled also add the shard keys to indexes
      if (!shardingEnabled) {
        parameterNamesToBeIndexed =
          parameterNamesToBeIndexed ++
            indexConfigurations
              .get(resourceType)
              .flatMap(_.shardKey)
              .map(_.filterNot(_.startsWith("_")))
              .getOrElse(Set.empty)
      }
      val job = createIndexForResourceType(resourceType, searchParameterConfigurations, parameterNamesToBeIndexed)
      Await.result(job, 300 seconds)

      //Enable sharding for resource type
      if (shardingEnabled && indexConfigurations.contains(resourceType)) {
        logger.info(s"Handling sharding for $resourceType ...")
        val job = enableSharding(resourceType, indexConfigurations.apply(resourceType), searchParameterConfigurations)
        Await.result(job, 300 seconds)
        //Enable sharding for history collection on id field if we shard this collection and if we support versioning
        if (indexConfigurations(resourceType).shardKey.getOrElse(Nil).nonEmpty && supportedResources.apply(resourceType) != FHIR_VERSIONING_OPTIONS.NO_VERSION) {
          val job = enableShardingForHistory(resourceType)
          Await.result(job, 300 seconds)
        }
      }
    })

    if (shardingEnabled)
      Await.result(refreshDbConfig(), 60 seconds)
  }


}

package io.onfhir.db

import java.time.Instant

import akka.http.scaladsl.model.{DateTime, StatusCodes}
import com.mongodb.client.model.IndexOptions
import io.onfhir.api._
import io.onfhir.Onfhir
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.IndexConfigurator.ResourceIndexConfiguration
import io.onfhir.config.{IndexConfigurator, OnfhirConfig, SearchParameterConf}
import io.onfhir.exception.InitializationException
import org.bson.conversions.Bson
import org.mongodb.scala.model.{Filters, IndexModel, Indexes}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

/**
  * Created by tuncay on 11/15/2016.
  * This class is used during setup of the platform (first run) to initialize Mongo database;
  * i) create collections,
  * ii) create indexes,
  * ii) and store the infrastructure resources into the database
  * The class is also used to retrieve infrastructure resources from database during initialization (for other runs)
  */
object DBInitializer {
  implicit val executionContext = Onfhir.actorSystem.dispatcher
  private val logger:Logger = LoggerFactory.getLogger(DBInitializer.getClass)

  val MONGO_SINGLE_FIELD_INDEX="single"
  val MONGO_COMPOUND_FIELD_INDEX="compound"
  val MONGO_TEXT_INDEX="text"

  /**
    * Basic preparation for database e.g. sharding
    * @return
    */
  def prepareDatabase():Unit = {
    logger.info("Preparing database ...")
    if(OnfhirConfig.mongoShardingEnabled) {
      logger.info("Enable sharding for database...")
      Await.result(MongoDB.enableSharding(), 60 seconds)
    }
  }

  /**
    * Create the Mongo collections for all supported FHIR resource
    * Note: If already a collection exist, nothing is changed for that collection
    * @param supportedResources  list of supported resource names (e.g. Observation, Condition, etc) -> Versioning mechanism
    * @return
    */
  def createCollections(supportedResources:Map[String, String]):Set[String] = {
    logger.info("Creating MongoDB collections for supported resources ...")
    // Get the list of collections for FHIR resource types (for storing current versions) present at the database
    val job = MongoDB.listCollections().flatMap{ collectionList =>
      Future.sequence(
        // For each resource in Supported Resources not in Mongo yet
        supportedResources.keySet.diff(collectionList.toSet).map(eachResource => {
          createCollection(eachResource)
        })
      )
    }.recoverWith {
      case e:Exception =>  logger.error("Failure in creating collections for supported resources: "+ e.getMessage)
        throw new InitializationException(e.getMessage)
    }
    Await.result(job, 60 seconds)

    // Get the list of collections for FHIR resource types (for storing current versions) present at the database
    val jobHistory = MongoDB.listCollections(history = true).flatMap{ collectionList =>
      Future.sequence(
        // For each resource in Supported Resources not in Mongo yet
        supportedResources
          .filterNot(_._2 == FHIR_VERSIONING_OPTIONS.NO_VERSION) //Only create history collection if we support versioning for resource type
          .keySet
          .diff(collectionList.toSet) //Only create if the collection is not created yet
          .map(eachResource => {
          createHistoryCollection(eachResource)
        })
      )
    }.recoverWith {
      case e:Exception =>  logger.error("Failure in creating history collections for supported resources: "+ e.getMessage)
        throw new InitializationException(e.getMessage)
    }
    Await.result(jobHistory, 60 seconds)
  }

  /**
    * Create a current collection for resource type
    * @param rtype Resource type
    * @return
    */
  private def createCollection(rtype:String) = {
    //Create the collection and basic indexes
    MongoDB.getDatabase.createCollection(rtype).toFuture
      .flatMap { unit =>
        Future.sequence(
          Seq(
            //Create a unique compound index on resource id and version
            IndexModel(
                Indexes.ascending(FHIR_COMMON_FIELDS.ID),
                new IndexOptions().name("onfhir_rid")),
            //Index on Mongo id
            IndexModel(Indexes.ascending(FHIR_COMMON_FIELDS.MONGO_ID)),
            //Index on Last Updated
            IndexModel(
              Indexes.descending(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
              new IndexOptions().name("onfhir_lastUpdated"))
          ).map(indModel =>
            MongoDB.getCollection(rtype).createIndexes(Seq(indModel)).toFuture
          )
        )
      }.flatMap { unit =>
      logger.debug(s"MongoDB collection created for '$rtype' ...")
      Future.successful(rtype)
    }
  }

  /**
    * Create a collec
    * @param rtype
    * @return
    */
  private def createHistoryCollection(rtype:String) = {
    //Create the collection and basic indexes
    MongoDB.getDatabase.createCollection(rtype+"_history").toFuture
      .flatMap { unit =>
        Future.sequence(
          Seq(
            //Create a unique compound index on resource id and version
            IndexModel(
              Indexes.compoundIndex(
                Indexes.ascending(FHIR_COMMON_FIELDS.ID),
                Indexes.descending(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}")),
              new IndexOptions().name("onfhir_rid")),
            //Index on Mongo id
            IndexModel(Indexes.ascending(FHIR_COMMON_FIELDS.MONGO_ID)),
            //Index on Last Updated
            IndexModel(
              Indexes.descending(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
              new IndexOptions().name("onfhir_lastUpdated"))
          ).map(indModel =>
            MongoDB.getCollection(rtype, history = true).createIndexes(Seq(indModel)).toFuture
          )
        )
      }.flatMap { unit =>
      logger.debug(s"MongoDB history collection created for '$rtype' ...")
      Future.successful(rtype)
    }
  }


  /**
    * Store the infrastructure resources (Conformance, StructureDefinition, SearchParameter, ValueSet, etc)
    * if not exist in DB
    * @param resourceType FHIR Resource type (e.g. Condition, Observation)
    * @param resources The content of the infrastructure resources
    */
  def storeInfrastructureResources(resourceType: String, resources: Seq[Resource]):Unit = {
    if (resources.nonEmpty) {
      logger.debug(s"Storing $resourceType resources ...")

      //Get the resource ids and versions
      val resourceIdsAndVersions: Map[String, (Long, Resource)] =
        //If it is conformance
        if(resources.length ==1 && FHIRUtil.extractValueOption[String](resources.head, FHIR_COMMON_FIELDS.ID).contains(SERVER_CONFORMANCE_STATEMENT_ID))  {
          Map(SERVER_CONFORMANCE_STATEMENT_ID -> (
            FHIRUtil
              .extractValueOptionByPath[String](resources.head, s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}")
              .map(_.toLong).getOrElse(1L),
            resources.head
          ))
        } else {
          resources.map(resource => {
            FHIRUtil.extractValueOption[String](resource, FHIR_COMMON_FIELDS.URL) match {
              case None => throw new InitializationException(s"One of the $resourceType infrastructure resources does not have url!")
              case Some(url) =>
                val rid = url.split('/').last
                rid ->
                  (
                    FHIRUtil
                      .extractValueOptionByPath[String](resource, s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}")
                      .map(_.toLong).getOrElse(1L),
                    FHIRUtil.setId(resource, rid)
                  )
            }
          }).toMap
        }

      val job =
        ResourceManager
          .getResourcesWithIds(resourceType, resourceIdsAndVersions.keySet)
          .map(resources => {
            val idsAndVersionsExist:Map[String, (Long, Resource)] = resources.map(r => FHIRUtil.extractIdFromResource(r) -> (FHIRUtil.extractVersionFromResource(r) -> r)).toMap
            //Find out which resources are new
            val resourcesDoesNotExist = resourceIdsAndVersions.filterNot(r => idsAndVersionsExist.keySet.contains(r._1)).map(r => r._1 -> r._2._2)

            //Create the non existant infrastructure resources
            val createFuture = if(resourcesDoesNotExist.nonEmpty) ResourceManager.createResources(resourceType, resourcesDoesNotExist).map(_ => Unit) else Future(Unit)

            // Wait for the creation of the resources
            Await.result(createFuture, 5 seconds)

            //Filter resources that exist
            val resourcesExist =
              resourceIdsAndVersions
                .filter(r => idsAndVersionsExist.keySet.contains(r._1)) //Get the resources that exist
            //Filter resources that exist but does not have a newer version in db, replace the documents
            val resourcesToUpdate = resourcesExist.filter(r => r._2._1 >= idsAndVersionsExist.apply(r._1)._1)

            // Group the resourcesToUpdate into chunks so that within each chunk processing occurs in parallel,
            // but the next chunk can only start after the previous chunk joins all parallel threads and completes.
            resourcesToUpdate.grouped(128).map { chunk =>
              //Replace those
              val updatesFutureChunk = chunk.map(r => {
                //If versions are same, replace the document
                if(r._2._1 == idsAndVersionsExist.apply(r._1)._1) {
                  var ur = FHIRUtil.populateResourceWithMeta(r._2._2, Some(r._1), r._2._1, Instant.now)
                  ur = FHIRUtil.populateResourceWithExtraFields(ur, FHIR_METHOD_NAMES.METHOD_PUT, StatusCodes.OK)
                  ResourceManager.replaceResource(resourceType, r._1, ur)
                } else { //Otherwise update, so we can keep the versions
                  ResourceManager.updateResource(resourceType, r._1, r._2._2, idsAndVersionsExist.apply(r._1)._1 -> idsAndVersionsExist.apply(r._1)._2)
                }
              })
              val chunkJob = Future.sequence(updatesFutureChunk)
              Await.result(chunkJob, 5 seconds) // Wait for the update operations of this chunk to be completed
            }

            logger.info(s"${resourcesDoesNotExist.size} resources stored for $resourceType ...")
            logger.info(s"${resourcesToUpdate.size} resources updated for $resourceType ...")
            if(resourcesExist.size - resourcesToUpdate.size > 0)
              logger.info(s"${resourcesExist.size - resourcesToUpdate.size} resources skipped for $resourceType, as newer versions exist in database ...")
          }
        ).recoverWith {
          case e =>
            logger.error(s"Problem in storing $resourceType !!!")
            logger.error(e.getMessage, e)
            throw new InitializationException(s"Problem in storing $resourceType !!!")
        }
        Await.result(job, 60 seconds)
      } else
        logger.info(s"No resources for $resourceType, skipping storage...")
  }


  /**
    * Create Database indexes for all collections by going over the Resource Query parameter and common query parameter configurations
    * @param supportedResources     All supported resources and versioning supported for resource type
    * @param allParameters          All search parameters defined for resource types
    * @param commonQueryParameters  Common query parameters
    * @param indexConfigurations    Configurations for database indexing
    */
  def createIndexes(supportedResources:Map[String, String], allParameters:Map[String, Map[String, SearchParameterConf]], commonQueryParameters:Map[String, SearchParameterConf], indexConfigurations:Map[String, ResourceIndexConfiguration]):Unit = {
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
      if(!OnfhirConfig.mongoShardingEnabled){
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
      if(OnfhirConfig.mongoShardingEnabled && indexConfigurations.contains(resourceType)){
        logger.info(s"Handling sharding for $resourceType ...")
        val job = enableSharding(resourceType, indexConfigurations.apply(resourceType), searchParameterConfigurations)
        Await.result(job, 300 seconds)
        //Enable sharding for history collection on id field if we shard this collection and if we support versioning
        if(indexConfigurations(resourceType).shardKey.getOrElse(Nil).nonEmpty && supportedResources.apply(resourceType) != FHIR_VERSIONING_OPTIONS.NO_VERSION){
          val job = enableShardingForHistory(resourceType)
          Await.result(job, 300 seconds)
        }
      }
    })

    if(OnfhirConfig.mongoShardingEnabled)
      Await.result(MongoDB.refreshDBConfig(), 60 seconds)
  }

  /**
    * Create configured indexes for a resource type
    * @param resourceType                     Resource type
    * @param searchParameterConfigurations    All supported search parameters for resource type
    * @param parameterNamesToBeIndexed        Search parameters to create index for
    */
  private def createIndexForResourceType(resourceType:String, searchParameterConfigurations:Seq[SearchParameterConf], parameterNamesToBeIndexed:Set[String]):Future[Unit] = {
    // find corresponding search parameters to be indexed over
    val parametersToBeIndexed = searchParameterConfigurations.filter(sp => parameterNamesToBeIndexed.contains(sp.pname))
    //Create Indexes for all search parameters defined for the Resource (INDEX_TYPE, ELEMENT PATH, BSON FILTER FOR PATH, IsSparseIndex)
    val indexes: Seq[(String,String,Bson, Boolean)] = parametersToBeIndexed.flatMap(searchParameterConf => {
      searchParameterConf.ptype match {
        case FHIR_PARAMETER_TYPES.COMPOSITE => Nil //Indexes are created for child paths, so nothing to do
        //For string and URIs use text index
        case FHIR_PARAMETER_TYPES.STRING | FHIR_PARAMETER_TYPES.URI =>
          searchParameterConf.extractElementPaths().map(path => (MONGO_TEXT_INDEX, path, Indexes.text(path), false))
        case FHIR_PARAMETER_TYPES.TOKEN |
             FHIR_PARAMETER_TYPES.DATE |
             FHIR_PARAMETER_TYPES.REFERENCE |
             FHIR_PARAMETER_TYPES.QUANTITY |
             FHIR_PARAMETER_TYPES.NUMBER =>
          createIndexesForComplexType(resourceType, searchParameterConf)

        case _ => Seq.empty[(String, String, Bson, Boolean)]
      }
    })

    //Eliminate the indexes with the same path (Some search parameters ara just aliases)
    val uniqueIndexes:Seq[(String, Bson, Boolean)] = indexes.groupBy(_._2).values.map(ind => (ind.head._1, ind.head._3, ind.head._4)).toSeq

    // Convert the indexes into IndexModel objects by merging test indexes into one compound index
    // As MongoDB allows only one text index
    // Set language_override="_language" to change the default value from "language", as this can appear in any FHIR resource
    val mongoTextIndexes = uniqueIndexes.filter(ind => ind._1.equals(MONGO_TEXT_INDEX)).map(_._2)
    val indexModels =
      if(mongoTextIndexes.isEmpty)
        uniqueIndexes
          .filterNot(_._1 == MONGO_TEXT_INDEX)
          .map(ind =>
            IndexModel(ind._2, new IndexOptions().sparse(ind._3))
          )
      else
        uniqueIndexes
          .filterNot(_._1 == MONGO_TEXT_INDEX)
          .map(ind => IndexModel(ind._2, new IndexOptions().sparse(ind._3))) :+
          IndexModel(
            Indexes.compoundIndex(mongoTextIndexes:_*),
            new IndexOptions().name(resourceType+"_all_text").languageOverride(INDEX_LANGUAGE_OVERRIDE)
          )

    // Get the collection for the resource type
    val collection = MongoDB.getCollection(resourceType)

    //Create the indexes
    /* Due to a bug in new version. See https://jira.mongodb.org/browse/SCALA-431?focusedCommentId=1940384&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel
      val job = collection.createIndexes(indexModels).toFuture().flatMap( created => {
      logger.info(s"${created.size} indexes created for the collection $resourceType ...")
      Future.apply(Unit)
    })*/
    Future.sequence(
      indexModels
        .map(indexModel => collection.createIndexes(Seq(indexModel)).toFuture())
    ).map(createdIndexes => {
      logger.info(s"${createdIndexes.size} indexes ensured for the collection $resourceType ...")
    }).recoverWith {
        case e =>
          logger.error(s"Problem in index creation for $resourceType !!!")
          logger.error(e.getMessage)
          throw new InitializationException(s"Problem in index creation for $resourceType !!!")
      }
  }

  /**
    * Enable sharding for resource type based on configuration
    * @param resourceType                   Resource type
    * @param indexConfiguration             Index configurations
    * @param searchParameterConfigurations  Search parameters supported
    * @return
    */
  def enableSharding(resourceType:String, indexConfiguration:ResourceIndexConfiguration, searchParameterConfigurations:Seq[SearchParameterConf]):Future[Unit] = {
    //TODO Currently we are supporting single shard keys, so we take the first one
    indexConfiguration.shardKey
      .flatMap(_.headOption)
      .flatMap(sk => {
        //If shard key is '_id', return the path
        if(sk == FHIR_SEARCH_SPECIAL_PARAMETERS.ID)
          Some(FHIR_COMMON_FIELDS.ID)
        else
          //Otherwise try to find the path from search parameters
          searchParameterConfigurations
            .find(spc => spc.pname == sk) //Check if there is search parameter defined
            .flatMap(spc=>
              spc.ptype match {
                //We only support sharding on Reference parameters (e.g. Observation.subject)
                case FHIR_PARAMETER_TYPES.REFERENCE =>
                      Some(s"${spc.extractElementPaths().head}.${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}")
                case _ =>
                  logger.warn(s"Sharding failure for $resourceType! OnFhir does not support sharding on ${spc.ptype} parameters. Only sharding on 'Resource.id' or reference parameters are supported!")
                  None
              }
            )
      })
      .map(path =>
        MongoDB.shardCollection(resourceType,path).map(d =>
          logger.info(s"Sharding enabled for collection $resourceType on path $path...")
        )
      )
      .getOrElse(Future.unit)
      .recoverWith {
        case e =>
          logger.error(s"Problem in enabling sharding for $resourceType !!!")
          logger.error(e.getMessage)
          throw new InitializationException(s"Problem in sharding for $resourceType !!!")
      }
  }

  /**
    * Enable sharding on resource type for history instances (default on id field)
    * @param resourceType Resource type
    */
  def enableShardingForHistory(resourceType:String) = {
    MongoDB.shardCollection(resourceType+"_history", FHIR_COMMON_FIELDS.ID).map(d =>
      logger.info(s"Sharding enabled for history collection $resourceType on path ${FHIR_COMMON_FIELDS.ID} ...")
    ).recoverWith {
      case e =>
        logger.error(s"Problem in enabling sharding for $resourceType history collection !!!")
        logger.error(e.getMessage)
        throw new InitializationException(s"Problem in sharding for $resourceType history collection !!!")
    }
  }

  /**
    * Create index for complex search paramter types (all other than string and uri)
    * @param rtype Resource type
    * @param searchParameterConf Search parameter configuration
    * @return IndexType, Path for the index, Index  object, IsSparse
    */
  private def createIndexesForComplexType(rtype:String, searchParameterConf: SearchParameterConf):Set[(String, String, Bson, Boolean)] = {
    val pathsAndTargetTypes = searchParameterConf.extractElementPathsAndTargetTypes()
    //if there are alternative paths, index should be sparse
    var isSparse = searchParameterConf.paths.size > 1
    // if it is on date, create
    val isDescending = searchParameterConf.ptype == FHIR_PARAMETER_TYPES.DATE

    pathsAndTargetTypes.flatMap { case (path, ttype) =>
      val subPaths =
        INDEX_SUBPATHS
          .getOrElse(ttype, Seq("")) //Get the possible subpaths
      //If there is multiple paths here, again index should be sparse
      if(subPaths.length > 1)
        isSparse = true
      // Based on the subpaths, create single or compound index
      subPaths.map {
        case simplePath:String =>
          (
            MONGO_SINGLE_FIELD_INDEX,
            simplePath,
            if(isDescending) Indexes.descending(path + simplePath) else Indexes.ascending(path + simplePath),
            isSparse
          )

        case compoundPath:(String, String) @unchecked =>
          (
            MONGO_COMPOUND_FIELD_INDEX,
            path + compoundPath._1 + compoundPath._2,
            Indexes.compoundIndex(
              if(isDescending) Indexes.descending(path + compoundPath._1) else Indexes.ascending(path + compoundPath._1),
              if(isDescending) Indexes.descending(path + compoundPath._2) else Indexes.ascending(path + compoundPath._2)
            ),
            isSparse
          )
      }
    }.toSet
  }
/*
  /**
    * Create database index for token type search parameter
    * @param searchParameterConf Configuration for the search parameter
    * @return Sequence of indexes -> (INDEX_TYPE, ELEMENT-PATH, MONGODB-INDEX)
    */
  private def createIndexForTokenType(searchParameterConf: SearchParameterConf):Seq[(String, String, Bson, Boolean)] = {
    val isSparse = searchParameterConf.paths.size > 1
    //Create the index models for each sub-entry
    // e.g. For Observation.code search path -> Observation.code.coding.system and Observation.code.coding.code indexes
    searchParameterConf.paths.zip(searchParameterConf.targetTypes).flatMap(pathTypePair => {
      val path = pathTypePair._1.asInstanceOf[String]
      val targetType = pathTypePair._2

      val pathsForType = INDEX_TOKEN_PATHS.get(targetType)
      pathsForType
        .map(_.map(subPath => (MONGO_SINGLE_FIELD_INDEX, path + subPath, Indexes.ascending(path + subPath), isSparse)))
        .getOrElse(Nil)
    })
  }

  /**
    * Create database index for date type search parameter
    * @param searchParameterConf Configuration for the search parameter
    * @return Sequence of indexes -> (INDEX_TYPE, ELEMENT-PATH, MONGODB-INDEX, isSparseIndex)
    */
  private def createIndexForDateType(searchParameterConf: SearchParameterConf):Seq[(String, String, Bson, Boolean)]  = {
    //If there is more than one path for index, we create a sparse index
    val isSparse = searchParameterConf.paths.size > 1

    searchParameterConf.paths.zip(searchParameterConf.targetTypes).flatMap(pathTypePair => {
      val path = pathTypePair._1.asInstanceOf[String]
      val targetType = pathTypePair._2

      INDEX_DATE_PATHS
        .apply(targetType) //Based on target type decide on index paths
        .map(subPath => (MONGO_SINGLE_FIELD_INDEX, path + subPath, Indexes.descending(path + subPath), isSparse))
    })
  }
*/

  /**
    * Read the Conformance statement from DB and return
    * @param resourceType Name of the conformance statement (e.g. Conformance in DSTU2)
    * @return
    */
  def getConformance(resourceType:String):Resource = {
    logger.info(s"Reading $resourceType of server from database ...")
    val job = ResourceManager.getResource(resourceType, SERVER_CONFORMANCE_STATEMENT_ID, None, excludeExtraFields = true) map {
      //No conformance statement
      case None =>
        throw new InitializationException(s"$resourceType not exist in database, please check if you correctly setup the platform !!!")
      case Some(conformance) =>
        conformance
    }
    Await.result(job, 1 seconds)
  }

  /**
    * Read the other infrastructure resources from DB
    * @param resourceType Resource type
    * @return
    */
  def getInrastructureResources(resourceType:String):Seq[Resource] = {
    logger.info(s"Reading $resourceType definitions from database ...")
    val job =
      ResourceManager
        .queryResources(resourceType, List.empty, excludeExtraFields = true)
        .map(_._2)
    Await.result(job, 5 seconds)
  }

}

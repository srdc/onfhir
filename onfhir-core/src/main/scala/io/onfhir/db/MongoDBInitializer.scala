package io.onfhir.db

import akka.http.scaladsl.model.StatusCodes
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.IndexOptions

import java.time.Instant
import io.onfhir.Onfhir
import io.onfhir.api._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.{OnfhirConfig, OnfhirIndex, ResourceIndexConfiguration, SearchParameterConf}
import io.onfhir.exception.InitializationException
import org.mongodb.scala.model.{IndexModel, Indexes}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.language.postfixOps

/**
  * Created by tuncay on 11/15/2016.
  * This class is used during setup of the platform (first run) to initialize Mongo database;
  * i) create collections,
  * ii) create indexes,
  * ii) and store the infrastructure resources into the database
  * The class is also used to retrieve infrastructure resources from database during initialization (for other runs)
  */
class MongoDBInitializer(resourceManager: ResourceManager) extends BaseDBInitializer(OnfhirConfig.mongoShardingEnabled){
  implicit val executionContext: ExecutionContextExecutor = Onfhir.actorSystem.dispatcher
  private val logger:Logger = LoggerFactory.getLogger(this.getClass)

  val MONGO_SINGLE_FIELD_INDEX="single"
  val MONGO_COMPOUND_FIELD_INDEX="compound"
  val MONGO_TEXT_INDEX="text"

  /**
    * Basic preparation for database e.g. sharding
    * @return
    */
  override def prepareSharding():Future[Unit] = {
      MongoDB
        .enableSharding()
        .map(_ => ())
  }

  /**
   * Create a MongoDB collection for maintaining latest version of resources for a specific FHIR Resource type
   * Note: If already a collection exist, nothing is changed for that collection
   *
   * @param rtype FHIR resource type
   */
  override def createCollection(rtype:String):Future[String] = {
    //Create the collection and basic indexes
    MongoDB.getDatabase
      .createCollection(rtype)
      .toFuture()
      .flatMap { unit =>
        Future.sequence(
          Seq(
            //Index on Mongo id
            IndexModel(Indexes.ascending(FHIR_COMMON_FIELDS.MONGO_ID)),
            //Create a index on resource id
            IndexModel(
                Indexes.hashed(FHIR_COMMON_FIELDS.ID),
                new IndexOptions().name("onfhir_rid")
            ),
            //Index on Last Updated
            IndexModel(
              Indexes.descending(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
              new IndexOptions().name("onfhir_lastUpdated"))
          ).map(indModel =>
            MongoDB.getCollection(rtype).createIndexes(Seq(indModel)).toFuture()
          )
        )
      }
      .flatMap { unit =>
        logger.debug(s"MongoDB collection created for '$rtype' ...")
        Future.successful(rtype)
      }
  }

  /**
    * Create a collec
    * @param rtype
    * @return
    */
  override def createHistoryCollection(rtype:String):Future[String] = {
    //Create the collection and basic indexes
    MongoDB.getDatabase.createCollection(rtype+"_history").toFuture()
      .flatMap { unit =>
        Future.sequence(
          Seq(
            //Create a unique compound index on resource id and version
            IndexModel(
              Indexes.compoundIndex(
                Indexes.hashed(FHIR_COMMON_FIELDS.ID),
                Indexes.descending(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}")),
              new IndexOptions().name("onfhir_rid")),
            //Index on Mongo id
            IndexModel(Indexes.ascending(FHIR_COMMON_FIELDS.MONGO_ID)),
            //Index on Last Updated
            IndexModel(
              Indexes.descending(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
              new IndexOptions().name("onfhir_lastUpdated"))
          ).map(indModel =>
            MongoDB.getCollection(rtype, history = true).createIndexes(Seq(indModel)).toFuture()
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
  override def storeInfrastructureResources(resourceType: String, resources: Seq[Resource]):Unit = {
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
        resourceManager
          .getResourcesWithIds(resourceType, resourceIdsAndVersions.keySet)
          .map(resources => {
            val idsAndVersionsExist:Map[String, (Long, Resource)] = resources.map(r => FHIRUtil.extractIdFromResource(r) -> (FHIRUtil.extractVersionFromResource(r) -> r)).toMap
            //Find out which resources are new
            val resourcesDoesNotExist = resourceIdsAndVersions.filterNot(r => idsAndVersionsExist.keySet.contains(r._1)).map(r => r._1 -> r._2._2)

            //Create the non existant infrastructure resources
            val createFuture = if(resourcesDoesNotExist.nonEmpty) resourceManager.createResources(resourceType, resourcesDoesNotExist).map(_ => ()) else Future(())

            // Wait for the creation of the resources
            Await.result(createFuture, 30 seconds)

            //Filter resources that exist
            val resourcesExist =
              resourceIdsAndVersions
                .filter(r => idsAndVersionsExist.keySet.contains(r._1)) //Get the resources that exist
            //Filter resources that exist but does not have a newer version in db, replace the documents
            val resourcesToUpdate = resourcesExist.filter(r => r._2._1 >= idsAndVersionsExist.apply(r._1)._1)

            // Group the resourcesToUpdate into chunks so that within each chunk processing occurs in parallel,
            // but the next chunk can only start after the previous chunk joins all parallel threads and completes.
            resourcesToUpdate.grouped(128).toArray.map { chunk =>
              //Replace those
              val updatesFutureChunk = chunk.map(r => {
                //If versions are same, replace the document
                if(r._2._1 == idsAndVersionsExist.apply(r._1)._1) {
                  var ur = FHIRUtil.populateResourceWithMeta(r._2._2, Some(r._1), r._2._1, Instant.now)
                  ur = FHIRUtil.populateResourceWithExtraFields(ur, FHIR_METHOD_NAMES.METHOD_PUT, StatusCodes.OK)
                  resourceManager.replaceResource(resourceType, r._1, ur)
                } else { //Otherwise update, so we can keep the versions
                  resourceManager.updateResource(resourceType, r._1, r._2._2, idsAndVersionsExist.apply(r._1)._1 -> idsAndVersionsExist.apply(r._1)._2)
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
   * Construct the MongoDB index models for single parameter index definitions
   * @param resourceType                      FHIR resource type
   * @param searchParameterConfigurations     Search parameter configurations
   * @param indexDefs                         All index definitions for resource type
   * @return
   */
  def constructSingleParameterIndexesForResourceType(resourceType:String, searchParameterConfigurations:Seq[SearchParameterConf], indexDefs:Seq[OnfhirIndex]):Seq[IndexModel] = {
    val indexes: Seq[(String,String,Seq[Bson], Boolean)]  =
      indexDefs
      .filter(_.parameters.length == 1) //Filter the index definitions on single search parameter
      .flatMap(indexDef =>
        searchParameterConfigurations
          .find(_.pname == indexDef.parameters.head)
          .map(paramConf => constructIndexForParam(resourceType, paramConf)) match {
            case None => throw new InitializationException(s"Search parameter ${indexDef.parameters.head} set for index configuration for $resourceType is not defined!")
            case Some(ind) => ind
          }
      )

    //Eliminate the indexes with the same path (Some search parameters ara just aliases)
    val uniqueIndexes: Seq[(String, Seq[Bson], Boolean)] = indexes.groupBy(_._2).values.map(ind => (ind.head._1, ind.head._3, ind.head._4)).toSeq

    // Convert the indexes into IndexModel objects by merging test indexes into one compound index
    // As MongoDB allows only one text index
    // Set language_override="_language" to change the default value from "language", as this can appear in any FHIR resource
    val mongoTextIndexes = uniqueIndexes.filter(ind => ind._1.equals(MONGO_TEXT_INDEX)).flatMap(_._2)
    val indexModels =
      if (mongoTextIndexes.isEmpty)
        uniqueIndexes
          .filterNot(_._1 == MONGO_TEXT_INDEX)
          .map(ind =>
            IndexModel(ind._2 match {
              case Seq(i) => i
              case oth => Indexes.compoundIndex(oth:_*)
            },
              new IndexOptions().sparse(ind._3)
            )
          )
      else
        uniqueIndexes
          .filterNot(_._1 == MONGO_TEXT_INDEX)
          .map(ind => IndexModel(
            ind._2 match {
              case Seq(i) => i
              case oth => Indexes.compoundIndex(oth: _*)
            },
            new IndexOptions().sparse(ind._3))
          ) :+
          IndexModel(
            Indexes.compoundIndex(mongoTextIndexes: _*),
            new IndexOptions().name(resourceType + "_all_text").languageOverride(INDEX_LANGUAGE_OVERRIDE)
          )
    indexModels
  }

  /**
   * Construct all multi parameter indexes on the given resource type
   * @param resourceType                    FHIR resource type
   * @param searchParameterConfigurations   All search parameter configurations for resource type
   * @param indexDefs                       All index definitions
   * @return
   */
  def constructMultiParameterIndexesForResourceType(resourceType:String, searchParameterConfigurations:Seq[SearchParameterConf], indexDefs:Seq[OnfhirIndex]):Seq[IndexModel] = {
    indexDefs
      .filter(_.parameters.length > 1) //Filter the index definitions on single search parameter
      .flatMap(indexDef => {
        val searchParamConfs =
          indexDef.parameters.map {
            //MongoDb _id parameter
            case "__id" => Some(SearchParameterConf("__id", FHIR_PARAMETER_TYPES.TOKEN, Seq("_id"), targetTypes = Seq("id")))
            //FHIR search parameter
            case p =>  searchParameterConfigurations.find(_.pname == p)
          }
        if (searchParamConfs.forall(_.isDefined)) {
          val indexesForEachParam =
            searchParamConfs
              .flatten
              .map(paramConf => constructIndexForParam(resourceType, paramConf))

          if(indexesForEachParam.flatten.exists(_._1 == MONGO_TEXT_INDEX))
            throw new InitializationException(s"Compound indexes cannot be defined on the search parameters (string, url) needing text search!")
          //Cartesian product of all indexes
          indexesForEachParam.tail
            .foldLeft(indexesForEachParam.head.map(Seq(_)))((i1, i2) => i1.flatMap(i => i2.map(j => i ++ Seq(j))))
            .map(combinedIndexes =>
              IndexModel(
                Indexes.compoundIndex(combinedIndexes.flatMap(_._3): _*),
                new IndexOptions().sparse(combinedIndexes.exists(_._4))
              )
            )
        } else
          throw new InitializationException(s"Search parameter(s) ${indexDef.parameters.apply(searchParamConfs.indexWhere(_.isEmpty))} set for index configuration for $resourceType is not defined!")
      })
  }

  /**
    * Create configured indexes for a resource type
    * @param resourceType                     Resource type
    * @param searchParameterConfigurations    All supported search parameters for resource type
    * @param indexDefs                        Index definitions
    */
  override def createIndexForResourceType(resourceType:String, searchParameterConfigurations:Seq[SearchParameterConf], indexDefs:Seq[OnfhirIndex]):Future[Unit] = {
    val indexModels =
      constructSingleParameterIndexesForResourceType(resourceType,searchParameterConfigurations, indexDefs) ++ //Indexes defined on single parameters
        constructMultiParameterIndexesForResourceType(resourceType, searchParameterConfigurations, indexDefs) //Indexes defined on multiple search parameter

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
   * Construct the suitable MongoDB index for parameter
   * @param resourceType          FHIR resource type
   * @param searchParameterConf   Search parameter configuration
   * @return                      Mongo Index type, path, index element(s), isSparse
   */
  private def constructIndexForParam(resourceType:String,searchParameterConf:SearchParameterConf):Seq[(String, String, Seq[Bson], Boolean)] = {
    searchParameterConf.ptype match {
      case FHIR_PARAMETER_TYPES.COMPOSITE => Nil //Indexes are created for child paths, so nothing to do
      //For string and URIs use text index
      case FHIR_PARAMETER_TYPES.STRING  =>
        searchParameterConf
          .extractElementPaths()
          .map(path => (MONGO_TEXT_INDEX, path, Seq(Indexes.text(path)), false))
      case
           FHIR_PARAMETER_TYPES.URI |
           FHIR_PARAMETER_TYPES.TOKEN |
           FHIR_PARAMETER_TYPES.DATE |
           FHIR_PARAMETER_TYPES.REFERENCE |
           FHIR_PARAMETER_TYPES.QUANTITY |
           FHIR_PARAMETER_TYPES.NUMBER =>
        constructIndexesForComplexType(resourceType, searchParameterConf).toSeq
      case _ =>
       Nil
    }
  }

  /**
    * Enable sharding for resource type based on configuration
    * @param resourceType                   Resource type
    * @param indexConfiguration             Index configurations
    * @param searchParameterConfigurations  Search parameters supported
    * @return
    */
  override def enableSharding(resourceType:String, indexConfiguration:ResourceIndexConfiguration, searchParameterConfigurations:Seq[SearchParameterConf]):Future[Unit] = {
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
  override def enableShardingForHistory(resourceType:String) = {
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
   *
   *  @return
   */
  def refreshDbConfig():Future[Unit] = MongoDB.refreshDBConfig()

  /**
    * Create index for complex search paramter types (all other than string and uri)
    * @param rtype Resource type
    * @param searchParameterConf Search parameter configuration
    * @return IndexType, Path for the index, Index or compound index elements, IsSparse
    */
  private def constructIndexesForComplexType(rtype:String, searchParameterConf: SearchParameterConf):Set[(String, String, Seq[Bson], Boolean)] = {
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
            if(isDescending) Seq(Indexes.descending(path + simplePath)) else Seq(Indexes.ascending(path + simplePath)),
            false//isSparse
          )

        case compoundPath:(String, String) @unchecked =>
          (
            MONGO_COMPOUND_FIELD_INDEX,
            path + compoundPath._1 + compoundPath._2,
            Seq(
              if(isDescending) Indexes.descending(path + compoundPath._1) else Indexes.ascending(path + compoundPath._1),
              if(isDescending) Indexes.descending(path + compoundPath._2) else Indexes.ascending(path + compoundPath._2)
            ),
            false//isSparse
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
   * List existing collections for persisting resource types
   *
   * @return
   */
  override def listExistingCollections(history: Boolean): Future[Seq[String]] = MongoDB.listCollections(history)
}

package io.onfhir.db


import io.onfhir.Onfhir
import io.onfhir.config.OnfhirConfig
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.connection.{ClusterSettings, ConnectionPoolSettings}
import org.mongodb.scala.{ClientSession, MongoClient, MongoClientSettings, MongoCollection, MongoCredential, MongoDatabase, Observable, ReadConcern, ReadPreference, ServerAddress, TransactionOptions, WriteConcern}

import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters._
import scala.concurrent.Future

/**
  * MongoDB client object
  */
object MongoDB {
  implicit val executionContext = Onfhir.actorSystem.dispatcher

  private val SYSTEM_INDEXES = "system.indexes"

  private val dbHosts = OnfhirConfig.mongodbHosts

  val transactionOptions:TransactionOptions =
    TransactionOptions
    .builder()
    .readPreference(ReadPreference.primary())
    .readConcern(ReadConcern.LOCAL)
    .writeConcern(WriteConcern.ACKNOWLEDGED)
    .build()


  // Base connection settings for MongoDB
  private val mongoClient: MongoClient = {
    var clientSettingsBuilder = MongoClientSettings.builder()
    //Set hostnames
    clientSettingsBuilder = clientSettingsBuilder
      .applicationName("onFhir.io")
      .applyToClusterSettings(b => b.applySettings(
        ClusterSettings
          .builder()
          .hosts(dbHosts.toList.map(h => new ServerAddress(h)).asJava)
          //.mode(if(dbHosts.length > 1) ClusterConnectionMode.MULTIPLE else ClusterConnectionMode.SINGLE)
          .build()
      ))

    //If database is secure
    if (OnfhirConfig.mongodbUser.isDefined && OnfhirConfig.mongodbPassword.isDefined && OnfhirConfig.mongoAuthDbName.isDefined){
      clientSettingsBuilder = clientSettingsBuilder.credential(MongoCredential.createCredential(OnfhirConfig.mongodbUser.get, OnfhirConfig.mongoAuthDbName.get, OnfhirConfig.mongodbPassword.get.toCharArray))
    }

    //If pooling is configured
    if(OnfhirConfig.mongodbPooling.isDefined)
      clientSettingsBuilder = clientSettingsBuilder.applyToConnectionPoolSettings( b => b.applySettings(
          ConnectionPoolSettings
            .builder()
            .minSize(OnfhirConfig.mongodbPoolingMinSize.getOrElse(5))
            .maxSize(OnfhirConfig.mongodbPoolingMaxSize.getOrElse(200))
            .maxWaitTime(OnfhirConfig.mongodbPoolingMaxWaitTime.getOrElse(180), TimeUnit.SECONDS) //3 minutes default
            .maxConnectionLifeTime(OnfhirConfig.mongodbPoolingMaxConnectionLifeTime.getOrElse(1200), TimeUnit.SECONDS) // 20 minutes default
            .build()
          )
        )

    MongoClient(clientSettingsBuilder.build())
  }

  /*if (OnfhirConfig.mongodbUser.isDefined && OnfhirConfig.mongodbPassword.isDefined && OnfhirConfig.mongoAuthDbName.isDefined) {
    val username = OnfhirConfig.mongodbUser.get
    val password = OnfhirConfig.mongodbPassword.get
    val authdb = OnfhirConfig.mongoAuthDbName.get
    MongoClient(s"mongodb://$username:$password@${dbHosts.mkString(",")}/?authSource=$authdb")
  } else {
    MongoClient(s"mongodb://${dbHosts.mkString(",")}")
  }*/


  //FHIR database
  private val database: MongoDatabase = mongoClient.getDatabase(OnfhirConfig.mongodbName)


  /**
    * Get the database
    * @return
    */
  def getDatabase: MongoDatabase = database

  /**
    * Create a transaction session
    * @return
    */
  def createSession():Observable[ClientSession] = mongoClient.startSession()

  /**
    * Get a specific collection for current version of FHIR resources of a specific FHIR Resource type (e.g. Observation)
    * @param name Name of collection
    * @return
    */
  def getCollection(name: String, history:Boolean = false): MongoCollection[Document] = if(history) database.getCollection(name+"_history") else database.getCollection(name)

  /**
    * Enable sharding on FHIR database
    * @return
    */
  def enableSharding():Future[Document] = {
    mongoClient
      .getDatabase(OnfhirConfig.mongoAuthDbName.getOrElse("admin")) //This should run on admin database
      .runCommand(Document("enableSharding" -> OnfhirConfig.mongodbName)).toFuture()
  }

  /**
    * Shard a collection
    * @param collectionName Name of the collection
    * @param key The field (or index name for compound) name to be used for sharding
    * @return
    */
  def shardCollection(collectionName:String, key:String):Future[Document] = {
    mongoClient
      .getDatabase(OnfhirConfig.mongoAuthDbName.getOrElse("admin")) //This should run on admin database
      .runCommand(Document("shardCollection" -> s"${OnfhirConfig.mongodbName}.$collectionName", "key" -> Document(key -> "hashed"))).toFuture()
  }

  /**
    * Refresh Mongo configs on config servers
    * @return
    */
  def refreshDBConfig():Future[Unit] ={
    Future.sequence(
      OnfhirConfig.mongodbHosts.map( _ =>
        mongoClient
          .getDatabase(OnfhirConfig.mongoAuthDbName.getOrElse("admin"))
          .runCommand(Document("flushRouterConfig" -> OnfhirConfig.mongodbName))
          .toFuture()
      )
    ).map(docs =>
      ()
    )
  }

  /**
    * List collections
    */
  def listCollections(history:Boolean = false):Future[Seq[String]] = {
    if(history)
      listHistoryCollections()
    else
      listCurrentCollections()
  }

  /**
    * List all FHIR current collections in FHIR database
    * @return
    */
  private def listCurrentCollections():Future[Seq[String]] = database.listCollectionNames().toFuture() map { list =>
    list.filterNot(collection => collection == SYSTEM_INDEXES || collection.endsWith("_history"))
  }

  /**
    * List all collections created for string FHIR history resources for each resource type
    * @return
    */
  private def listHistoryCollections():Future[Seq[String]] = database.listCollectionNames().toFuture() map { list =>
    list.filter(collection => collection.endsWith("_history")).map(_.replace("_history", ""))
  }
}

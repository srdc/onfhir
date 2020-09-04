package io.onfhir.config

import java.time.Duration

import com.typesafe.config.ConfigFactory

import io.onfhir.api.{FHIR_VALIDATION_ALTERNATIVES, DEFAULT_FHIR_VERSION, FHIR_HTTP_OPTIONS}

import scala.collection.JavaConverters._
import scala.util.Try

/**
  * OnFhir application configurations
  */
object OnfhirConfig {

  /** Application config object. */
  val config:com.typesafe.config.Config = ConfigFactory.load()

  /** Name of the server */
  lazy val serverName:String = Try(config.getString("spray.can.server.server-header")).getOrElse("onFHIR Repository")
  /** Host name/address to start service on. */
  lazy val serverHost:String = Try(config.getString("server.host")).getOrElse("localhost")

  /** Port to start service on. */
  lazy val serverPort:Int = Try(config.getInt("server.port")).getOrElse(8080)

  /** SSL settings if server will be server over SSL */
  lazy val serverSsl:Boolean = Try(config.getConfig("server.ssl").isEmpty || config.getString("server.ssl.keystore") != null).getOrElse(false)
  lazy val sslKeystorePath: Option[String] = Try(config.getString("server.ssl.keystore")).toOption
  lazy val sslKeystorePasword: Option[String] = Try(config.getString("server.ssl.password")).toOption

  /** Protocol of the server. Either http or https and the value is determined from spray properties */
  lazy val serverProtocol:String = if(serverSsl) "https" else "http"

  /** Full address of the server */
  lazy val serverLocation:String =  serverProtocol + "://" + serverHost + ":" + serverPort

  /** Base URI for FHIR Services to be served from */
  lazy val baseUri:String = Try(config.getString("server.base-uri")).getOrElse("fhir")

  /** Default return preference when Prefer HTTP header not set */
  lazy val fhirDefaultReturnPreference:String = "return=" + Try(config.getString("fhir.default.return-preference")).getOrElse("representation")

  /** Default page count when count parameter not set  */
  lazy val fhirDefaultPageCount:Int = Try(config.getInt("fhir.default.page-count")).getOrElse(50)

  /** Default value for [CapabilityStatement|Conformance].rest.resource.versioning when not present */
  lazy val fhirDefaultVersioning:String = Try(config.getString("fhir.default.versioning")).getOrElse("versioned")

  /** Default value for [CapabilityStatement|Conformance].rest.resource.readHistory when not present */
  lazy val fhirDefaultReadHistory:Boolean = Try(config.getBoolean("fhir.default.read-history")).getOrElse(false)

  /** Default value for [CapabilityStatement|Conformance].rest.resource.updateCreate when not present */
  lazy val fhirDefaultUpdateCreate:Boolean = Try(config.getBoolean("fhir.default.update-create")).getOrElse(false)

  /** Default value for [CapabilityStatement|Conformance].rest.resource.conditionalCreate when not present */
  lazy val fhirDefaultConditionalCreate:Boolean = Try(config.getBoolean("fhir.default.conditional-create")).getOrElse(false)

  /** Default value for [CapabilityStatement|Conformance].rest.resource.conditionalRead when not present */
  lazy val fhirDefaultConditionalRead:String = Try(config.getString("fhir.default.conditional-read")).getOrElse("full-support")

  /** Default value for [CapabilityStatement|Conformance].rest.resource.conditionalUpdate when not present */
  lazy val fhirDefaultConditionalUpdate:Boolean = Try(config.getBoolean("fhir.default.conditional-update")).getOrElse(false)

  /** Default value for [CapabilityStatement|Conformance].rest.resource.conditionalDelete when not present */
  lazy val fhirDefaultConditionalDelete:String = Try(config.getString("fhir.default.conditional-delete")).getOrElse("not-supported")

  /** Whether to start an embedded MongoDB instance */
  lazy val mongoEmbedded: Boolean = Try(config.getBoolean("mongodb.embedded")).getOrElse(false)

  /** Database host name/address and ports. */
  lazy val mongodbHosts:Seq[String] = Try(config.getStringList("mongodb.host")).map(l => l.asScala).getOrElse(Seq("localhost"))

  /** Database host port number. */
  //lazy val mongodbPort = Try(config.getInt("mongodb.port")).getOrElse(27017)

  /** Service database name. */
  lazy val mongodbName:String = Try(config.getString("mongodb.db")).getOrElse("fhir")

  /** Service database name. */
  lazy val mongoAuthDbName:Option[String] = Try(config.getString("mongodb.authdb")).toOption

  /** User name used to access database. */
  lazy val mongodbUser:Option[String] = Try(config.getString("mongodb.username")).toOption

  /** Password for specified user and database. */
  lazy val mongodbPassword:Option[String] = Try(config.getString("mongodb.password")).toOption

  /** Pooling parameters */
  lazy val mongodbPooling:Option[com.typesafe.config.Config] = Try(config.getConfig("mongodb.pooling")).toOption
  lazy val mongodbPoolingMinSize:Option[Int] = mongodbPooling.flatMap(p => Try(p.getInt("minSize")).toOption)
  lazy val mongodbPoolingMaxSize:Option[Int] = mongodbPooling.flatMap(p => Try(p.getInt("maxSize")).toOption)
  lazy val mongodbPoolingMaxWaitTime:Option[Long] = mongodbPooling.flatMap(p => Try(p.getLong("maxWaitTime")).toOption)
  lazy val mongodbPoolingMaxConnectionLifeTime:Option[Long] = mongodbPooling.flatMap(p => Try(p.getLong("maxConnectionLifeTime")).toOption)

  /** If sharding to be enabled for mongodb **/
  lazy val mongoShardingEnabled:Boolean = Try(config.getBoolean("mongodb.sharding")).toOption.getOrElse(false)

  /** If true, onfhir will use MongoDB Transaction which requires Mongo to be a replication set or sharded cluster */
  lazy val mongoUseTransaction:Boolean = Try(config.getBoolean("mongodb.transaction")).toOption.getOrElse(false)

  /** Path to the zip file of definitions for validation(Default DSTU2 within resources) */
  lazy val baseDefinitions:Option[String] = Try(config.getString("fhir.initialization.base-definitions-path")).toOption

  /** File path to the conformance statement */
  lazy val conformancePath:Option[String] = Try(config.getString("fhir.initialization.conformance-path")).toOption

  /** Directory path to the definitions of supported structures */
  lazy val profilesPath:Option[String] = Try(config.getString("fhir.initialization.profiles-path")).toOption

  /** Directory path to the definitions of supported search parameters */
  lazy val searchParametersPath:Option[String] = Try(config.getString("fhir.initialization.parameters-path")).toOption

  lazy val valueSetsPath:Option[String] = Try(config.getString("fhir.initialization.valuesets-path")).toOption

  lazy val codeSystemsPath:Option[String] = Try(config.getString("fhir.initialization.codesystems-path")).toOption

  lazy val compartmentDefinitionsPath:Option[String] = Try(config.getString("fhir.initialization.compartments-path")).toOption

  lazy val operationDefinitionsPath:Option[String] = Try(config.getString("fhir.initialization.operations-path")).toOption

  lazy val dbIndexConfigurationPath:Option[String] = Try(config.getString("fhir.initialization.index-conf-path")).toOption


  lazy val fhirInitialize:Boolean = Try(config.getBoolean("fhir.initialize")).toOption.getOrElse(false)

  lazy val fhirRootUrl:String = Try(config.getString("fhir.root-url")).toOption.getOrElse(s"$serverLocation/$baseUri")

  lazy val fhirDefinitionsUrl:String = Try(config.getString("fhir.definitions-url")).toOption.getOrElse(fhirRootUrl)

  lazy val fhirValidation:String = Try(config.getString("fhir.validation")).toOption.getOrElse(FHIR_VALIDATION_ALTERNATIVES.PROFILE)

  /** Indicates how to handle erroneous search requests*/
  lazy val fhirSearchHandling:String = Try(config.getString("fhir.search-handling")).toOption.getOrElse(FHIR_HTTP_OPTIONS.FHIR_SEARCH_STRICT)

  /** Which Foundation resource types we should persist into database from base standard */
  lazy val fhirPersistBaseDefinitions:Set[String] = Try(config.getStringList("fhir.persisted-base-definitions")).toOption.map(_.asScala.toSet).getOrElse(Set.empty[String])

  /** Auditing related configurations */
  lazy val fhirAuditingRepository:String = Try(config.getString("fhir.auditing.repository")).toOption.getOrElse("local")
  lazy val fhirAuditingRepositoryUrl:Option[String] = Try(config.getString("fhir.auditing.repository-url")).toOption
  lazy val fhirAuditingRepositoryIsSecure:Boolean = Try(config.getBoolean("fhir.auditing.is-secure")).toOption.getOrElse(false)
  lazy val fhirAuditingRemoteBatchInterval:Int = Try(config.getInt("fhir.auditing.batch-interval")).toOption.getOrElse(5)
  lazy val fhirAuditingRemoteBatchSize:Int = Try(config.getInt("fhir.auditing.batch-size")).toOption.getOrElse(50)

  lazy val fhirRequestTimeout:Duration = Try(config.getDuration("akka.http.server.request-timeout")).toOption.getOrElse(Duration.ofSeconds(30))

  /**
    * Authorization configurations
    */
  lazy val authzConfig:AuthzConfig = new AuthzConfig(OnfhirConfig.config.getConfig("fhir.authorization"))

  /**
   * FHIR Subscription related configuration
   */
  //Enables sending FHIR subscription events to kafka so onfhir-subscription module can work
  lazy val fhirSubscriptionActive = Try(config.getBoolean("fhir.subscription.active")).toOption.getOrElse(false)
  lazy val fhirSubscriptionAllowedResources = Try(config.getStringList("fhir.subscription.allowed-resources")).toOption.map(_.asScala.toSeq)
  /**
   * Internal API configurations
   */
  lazy val internalApiActive:Boolean = Try(config.getBoolean("server.internal.active")).toOption.getOrElse(false)
  lazy val internalApiPort:Int = Try(config.getInt("server.internal.port")).toOption.getOrElse(8081)
  lazy val internalApiAuthenticate:Boolean = Try(config.getBoolean("server.internal.authenticate")).toOption.getOrElse(false)
}

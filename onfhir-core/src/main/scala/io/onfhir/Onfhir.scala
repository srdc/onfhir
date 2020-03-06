package io.onfhir

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import org.slf4j.{Logger, LoggerFactory}
import akka.http.scaladsl.server.{HttpApp, Route}
import akka.http.scaladsl.settings.ServerSettings
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.api.model.FHIRRequest
import io.onfhir.audit.AuditManager
import io.onfhir.authz._
import io.onfhir.config.{FhirConfigurationManager, IFhirVersionConfigurator, OnfhirConfig, SSLConfig}
import io.onfhir.db.DBConflictManager
import io.onfhir.db.DBConflictManager.ACTOR_NAME
import io.onfhir.event.{FhirEvent, FhirEventBus, FhirEventSubscription}
import io.onfhir.event.kafka.KafkaEventProducer

import scala.concurrent.{ExecutionContext, Future, Promise, blocking}
import scala.util.Try
import scala.io.StdIn

/**
  * Created by tuncay on 10/16/2017.
  * Instance of an OnFhir server
  *
  * @param fhirConfigurator      Module that will configure the FHIR capabilities of the server based on the base FHIR version
  * @param customAuthorizer      Module to handle authorization with a custom protocol
  * @param customTokenResolver   Module to handle access token resolution with a custom way
  * @param customAuditHandler    Module to handle auditing with a custom strategy
  * @param externalRoutes        External non-fhir routes for the server
  */
class Onfhir(
              val fhirConfigurator:IFhirVersionConfigurator,
              val customAuthorizer:Option[IAuthorizer],
              val customTokenResolver:Option[ITokenResolver],
              val customAuditHandler:Option[ICustomAuditHandler],
              val externalRoutes:Seq[(FHIRRequest, (AuthContext, Option[AuthzContext])) => Route]
            ) extends SSLConfig {

  private val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Akka HTTP rest server for FHIR endpoint
    */
  private object OnfhirServer extends HttpApp with FHIREndpoint {
    /**
      * Callback for server shutdown
      * @param attempt
      * @param system
      */
    override def postServerShutdown(attempt: Try[Done], system: ActorSystem): Unit = {
      logger.info("Closing OnFhir server...")
      implicit val executionContext = Onfhir.actorSystem.dispatcher
      Onfhir.actorSystem.terminate().map( _ => System.exit(0))
    }

    /**
      * In order to close the server
      * @param actorSystem
      * @param executionContext
      * @return
      */
    override def waitForShutdownSignal(actorSystem: ActorSystem)(implicit executionContext: ExecutionContext): Future[Done] = {
      val promise = Promise[Done]()
      sys.addShutdownHook {
        promise.trySuccess(Done)
      }
      Future {
        blocking {
          if (StdIn.readLine("Write 'quit' to stop the server...\n").equalsIgnoreCase("quit"))
            promise.trySuccess(Done)
        }
      }
      promise.future
    }
  }

  /* Setup or Configure the platform and prepare it for running */
  FhirConfigurationManager.initialize(fhirConfigurator)

  /* Setup the authorization module and prepare it for running */
  AuthzConfigurationManager.initialize(customAuthorizer, customTokenResolver)

  //Create audit manager actor, if auditing is enabled
  val auditManager =
    if(customAuditHandler.isDefined || OnfhirConfig.fhirAuditingRepository != AuditManager.AUDITING_METHOD_NONE)
      Some(Onfhir.actorSystem.actorOf(AuditManager.props(customAuditHandler), AuditManager.ACTOR_NAME))
    else
      None
  //Create db conflict manager actor, if transaction is not enabled
  val dbConflictManager =
    if(!OnfhirConfig.mongoUseTransaction)
      Some(Onfhir.actorSystem.actorOf(DBConflictManager.props(), DBConflictManager.ACTOR_NAME))
    else
      None


  //Create Kafka producer if enabled
  val kafkaEventProducer =
    if(KafkaEventProducer.kafkaConfig.kafkaEnabled) {
      val actorRef = Onfhir.actorSystem.actorOf(KafkaEventProducer.props(), KafkaEventProducer.ACTOR_NAME)
      FhirEventBus.subscribe(actorRef, FhirEventSubscription(classOf[FhirEvent], KafkaEventProducer.kafkaConfig.kafkaEnabledResources))
      actorRef
    }

  /**
    * Start the server
    */
  def start = {
    /* and bind to Akka's I/O interface */
    logger.info("Starting OnFhir server on host {} and port {}", OnfhirConfig.serverHost, OnfhirConfig.serverPort)
    //Read server settings from config
    val settings = ServerSettings(OnfhirConfig.config).withVerboseErrorMessages(true)
    if(OnfhirConfig.serverSsl) {
      logger.info("Configuring SSL context...")
      Http()(Onfhir.actorSystem).setDefaultServerHttpContext(https)
    }
    OnfhirServer.startServer(OnfhirConfig.serverHost, OnfhirConfig.serverPort, settings, Onfhir.actorSystem)
    //IO(Http) ! Http.Bind(service, Config.serverHost, Config.serverPort)
  }
}

/**
  * Companion object to initialize Akka Actor System and OnFhir
  */
object Onfhir {
  //Base Akka Actor system for the whole system
  implicit val actorSystem = ActorSystem("onfhir")
  //Singleton onfhir server instance
  private var _instance:Onfhir = null


  def apply(): Onfhir =  _instance

  /**
    * Initialize OnFhir
    * @param fhirConfigurator     Module that will configure the FHIR capabilities of the server based on the version
    * @param customAuthorizer     Module to handle authorization with a custom protocol, if not supplied decided based on configurations
    * @param customTokenResolver  Module to handle access token resolution with a custom way, if not supplied decided based on configurations
    * @param customAuditHandler   Module to handle auditing with a custom strategy, if not supplied decided based on configurations
    * @param externalRoutes       External non-fhir routes for the server
    * @return
    */
  def apply(
             fhirConfigurator:IFhirVersionConfigurator,
             customAuthorizer:Option[IAuthorizer] = None,
             customTokenResolver:Option[ITokenResolver] = None,
             customAuditHandler:Option[ICustomAuditHandler] = None,
             externalRoutes:Seq[(FHIRRequest, (AuthContext, Option[AuthzContext])) => Route] = Nil): Onfhir = {

    if(_instance == null)
      _instance = new Onfhir(fhirConfigurator, customAuthorizer, customTokenResolver,customAuditHandler,externalRoutes)
    _instance
  }
}
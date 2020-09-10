package io.onfhir

import java.time.temporal.ChronoUnit

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import org.slf4j.{Logger, LoggerFactory}
import akka.http.scaladsl.server.{HttpApp, Route}
import akka.http.scaladsl.settings.ServerSettings
import io.onfhir.api.endpoint.{FHIREndpoint, OnFhirEndpoint}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.audit.AuditManager
import io.onfhir.authz._
import io.onfhir.config.{FhirConfigurationManager, IFhirVersionConfigurator, OnfhirConfig, SSLConfig}

import io.onfhir.db.{DBConflictManager, EmbeddedMongo}
import io.onfhir.event.{FhirEvent, FhirEventBus, FhirEventSubscription}
import io.onfhir.event.kafka.KafkaEventProducer

import scala.concurrent.{ExecutionContext, Future, Promise, blocking}
import scala.util.{Try}
import scala.io.StdIn

/**
  * Created by tuncay on 10/16/2017.
  * Instance of an OnFhir server
  *
  * @param fhirConfigurator      Module that will configure the FHIR capabilities of the server based on the base FHIR version
  * @param fhirOperationImplms   Map for FHIR operation implementations; URL of FHIR Operation -> Class path for the implementation of operation
  * @param customAuthorizer      Module to handle authorization with a custom protocol
  * @param customTokenResolver   Module to handle access token resolution with a custom way
  * @param customAuditHandler    Module to handle auditing with a custom strategy
  * @param externalRoutes        External non-fhir routes for the server
  */
class Onfhir(
              val fhirConfigurator:IFhirVersionConfigurator,
              val fhirOperationImplms:Map[String, String],
              val customAuthorizer:Option[IAuthorizer],
              val customTokenResolver:Option[ITokenResolver],
              val customAuditHandler:Option[ICustomAuditHandler],
              val externalRoutes:Seq[(FHIRRequest, (AuthContext, Option[AuthzContext])) => Route]
            ) extends SSLConfig {

  private val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Akka HTTP rest server for FHIR endpoint
    */
  private object FhirServer extends HttpApp with FHIREndpoint {
    /**
      * Callback for server shutdown
      * @param attempt
      * @param system
      */
    override def postServerShutdown(attempt: Try[Done], system: ActorSystem): Unit = {
      logger.info("Closing OnFhir server...")
      if (OnfhirConfig.mongoEmbedded) {
        EmbeddedMongo.stop()
      }
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

    override def postHttpBinding(binding: Http.ServerBinding) = {
      logger.info("OnFhir FHIR server started on host {} and port {}", OnfhirConfig.serverHost, OnfhirConfig.serverPort)
      if(OnfhirConfig.internalApiActive)
        OnFhirServer.startServer(OnfhirConfig.serverHost, OnfhirConfig.internalApiPort, ServerSettings(OnfhirConfig.config).withVerboseErrorMessages(true), Onfhir.actorSystem)
    }
  }

  private object OnFhirServer extends HttpApp with OnFhirEndpoint {
    override def postServerShutdown(attempt: Try[Done], system: ActorSystem): Unit = {
      logger.info("Closing OnFhir internal server...")
    }

    override def postHttpBinding(binding: Http.ServerBinding) = {
      logger.info("OnFhir internal server is started on host {} and port {}...", OnfhirConfig.serverHost, OnfhirConfig.internalApiPort)
    }

    override def waitForShutdownSignal(actorSystem: ActorSystem)(implicit executionContext: ExecutionContext): Future[Done] = {
      val promise = Promise[Done]()
      sys.addShutdownHook {
        promise.trySuccess(Done)
      }
      promise.future
    }
  }

  /* Setup or Configure the platform and prepare it for running */
  FhirConfigurationManager.initialize(fhirConfigurator, fhirOperationImplms)

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


  //Create Kafka producer if enabled or subscription is active
  val kafkaEventProducer =
    if(KafkaEventProducer.kafkaConfig.kafkaEnabled || OnfhirConfig.fhirSubscriptionActive) {
      val actorRef = Onfhir.actorSystem.actorOf(KafkaEventProducer.props(), KafkaEventProducer.ACTOR_NAME)

      val fhirSubscriptionAllowedResources = if(OnfhirConfig.fhirSubscriptionActive) OnfhirConfig.fhirSubscriptionAllowedResources else Some(Nil)
      val kafkaEnabledResources = KafkaEventProducer.kafkaConfig.kafkaEnabledResources
      val resourcesToSendToKafka = (fhirSubscriptionAllowedResources, kafkaEnabledResources) match {
        case (Some(l1), Some(l2)) =>
          if(OnfhirConfig.fhirSubscriptionActive)
            Some(l1 ++ l2 :+ "Subscription")
          else
            Some(l1 ++ l2)
        case (None, _) => None
        case (_, None) => None
      }

      FhirEventBus.subscribe(actorRef, FhirEventSubscription(classOf[FhirEvent], resourcesToSendToKafka))
      actorRef
    }

  /**
    * Start the server
    */
  def start = {
    //Read server settings from config
    val settings = ServerSettings(OnfhirConfig.config).withVerboseErrorMessages(true)
    if(OnfhirConfig.serverSsl) {
      logger.info("Configuring SSL context...")
      Http()(Onfhir.actorSystem).setDefaultServerHttpContext(https)
    }

    //Start FHIR server
    FhirServer.startServer(OnfhirConfig.serverHost, OnfhirConfig.serverPort, settings, Onfhir.actorSystem)
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
    * @param fhirOperationImplms  Map of own FHIR Operation implementation configurations: (operation url -> operation implementation class name)
    * @param customAuthorizer     Module to handle authorization with a custom protocol, if not supplied decided based on configurations
    * @param customTokenResolver  Module to handle access token resolution with a custom way, if not supplied decided based on configurations
    * @param customAuditHandler   Module to handle auditing with a custom strategy, if not supplied decided based on configurations
    * @param externalRoutes       External non-fhir routes for the server
    * @return
    */
  def apply(
             fhirConfigurator:IFhirVersionConfigurator,
             fhirOperationImplms:Map[String, String] = Map.empty[String, String],
             customAuthorizer:Option[IAuthorizer] = None,
             customTokenResolver:Option[ITokenResolver] = None,
             customAuditHandler:Option[ICustomAuditHandler] = None,
             externalRoutes:Seq[(FHIRRequest, (AuthContext, Option[AuthzContext])) => Route] = Nil): Onfhir = {

    if(_instance == null)
      _instance = new Onfhir(fhirConfigurator, fhirOperationImplms, customAuthorizer, customTokenResolver,customAuditHandler,externalRoutes)
    _instance
  }
}

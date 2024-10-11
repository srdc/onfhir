package io.onfhir

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.concat
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.ServerSettings
import io.onfhir.api.endpoint.{FHIREndpoint, OnFhirInternalEndpoint}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.audit.{AuditManager, RequestLogManager}
import io.onfhir.authz._
import io.onfhir.config.{FhirConfigurationManager, IFhirServerConfigurator, OnfhirConfig, SSLConfig}
import io.onfhir.db.{DBConflictManager, EmbeddedMongo}
import io.onfhir.event.kafka.KafkaEventProducer
import io.onfhir.event.{FhirDataEvent, FhirEventSubscription}
import org.slf4j.{Logger, LoggerFactory}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent._
import scala.io.StdIn
import scala.util.{Failure, Success}

/**
  * Created by tuncay on 10/16/2017.
  * Instance of an OnFhir server
  *
  * @param fhirConfigurator      Module that will configure the FHIR capabilities of the server based on the base FHIR version
  * @param fhirOperationImplms   Map for FHIR operation implementations; URL of FHIR Operation -> Class path for the implementation of operation
  * @param customAuthorizer      Module to handle authorization with a custom protocol
  * @param customTokenResolver   Module to handle access token resolution with a custom way
  * @param customAuditHandler    Module to handle auditing with a custom strategy
  * @param externalRoutes        External non-fhir routes for the server that uses marshalling and authentication
  * @param cdsHooksRoute         CDS-Hooks compliant CDS route (using onfhir-cds)
  */
class Onfhir(
              val fhirConfigurator:IFhirServerConfigurator,
              val fhirOperationImplms:Map[String, String],
              val customAuthorizer:Option[IAuthorizer],
              val customTokenResolver:Option[ITokenResolver],
              val customAuditHandler:Option[ICustomAuditHandler],
              val externalRoutes:Seq[(FHIRRequest, (AuthContext, Option[AuthzContext])) => Route],
              val cdsHooksRoute:Option[Route]
            )(implicit actorSystem:ActorSystem) extends SSLConfig with FHIREndpoint with OnFhirInternalEndpoint{

  private val logger:Logger = LoggerFactory.getLogger(this.getClass)

  implicit val ec:ExecutionContext = actorSystem.dispatcher
  // FHIR server binding
  private var fhirServerBinding:Http.ServerBinding = _
  // Internal server binding
  private var internalOnFhirServerBinding:Http.ServerBinding = _

  /* Setup or Configure the platform and prepare it for running */
  FhirConfigurationManager.initialize(fhirConfigurator, fhirOperationImplms)

  /* Setup the authorization module and prepare it for running */
  AuthzConfigurationManager.initialize(customAuthorizer, customTokenResolver)

  //Create audit manager actor, if auditing is enabled
  val auditManager =
    if(customAuditHandler.isDefined || OnfhirConfig.fhirAuditingRepository != AuditManager.AUDITING_METHOD_NONE)
      Some(Onfhir.actorSystem.actorOf(AuditManager.props(FhirConfigurationManager, customAuditHandler), AuditManager.ACTOR_NAME))
    else
      None

  val requestLogManager =
    Onfhir.actorSystem.actorOf(RequestLogManager.props(), "request-response-logger")

  //Create db conflict manager actor, if transaction is not enabled
  val dbConflictManager =
    if(!OnfhirConfig.mongoUseTransaction)
      Some(Onfhir.actorSystem.actorOf(DBConflictManager.props(), DBConflictManager.ACTOR_NAME))
    else
      None


  //Create Kafka producer if enabled or subscription is active
  val kafkaEventProducer =
    if(KafkaEventProducer.kafkaConfig.kafkaEnabled || OnfhirConfig.fhirSubscriptionActive) {
      val actorRef = Onfhir.actorSystem.actorOf(KafkaEventProducer.props(FhirConfigurationManager.fhirConfig), KafkaEventProducer.ACTOR_NAME)

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

      FhirConfigurationManager.eventManager.subscribe(actorRef, FhirEventSubscription(classOf[FhirDataEvent], resourcesToSendToKafka))
      actorRef
    }

  /**
    * Start the server
    */
  def start = {
    // FHIR server definition
    var fhirServer =
      Http()
        .newServerAt(OnfhirConfig.serverHost, OnfhirConfig.serverPort)
        .withSettings(
          ServerSettings(OnfhirConfig.config)
            .withVerboseErrorMessages(true)
        )

    if(OnfhirConfig.serverSsl) {
      logger.info("Configuring SSL context...")
      fhirServer = fhirServer.enableHttps(https)
    }
    //Final FHIR route
    val finalRoute =
      cdsHooksRoute match {
        case None => fhirRoute
        case Some(cdsRoute) => concat(cdsRoute, fhirRoute)
      }

    fhirServer
      .bind(finalRoute) onComplete {
        case Success(binding) =>
          fhirServerBinding = binding
          fhirServerBinding.addToCoordinatedShutdown(FiniteDuration.apply(60L, TimeUnit.SECONDS))
          fhirServerBinding.whenTerminated onComplete {
            case Success(t) =>
              logger.info("Closing OnFhir server...")
              if (OnfhirConfig.mongoEmbedded) {
                EmbeddedMongo.stop()
              }
              actorSystem.terminate()
              logger.info("OnFhir server is gracefully terminated...")
            case Failure(exception) => logger.error("Problem while gracefully terminating OnFhir server!", exception)
          }
          logger.info("OnFhir FHIR server started on host {} and port {}", OnfhirConfig.serverHost, OnfhirConfig.serverPort)
          //Wait for a shutdown signal
          Await.ready(waitForShutdownSignal(), Duration.Inf)
          fhirServerBinding.terminate(FiniteDuration.apply(60L, TimeUnit.SECONDS))
        case Failure(ex) =>
          logger.error("Problem while binding to the onFhir FHIR server address and port!", ex)
    }

    //If we have internal onFhir api active
    if(OnfhirConfig.internalApiActive){
      val onFhirInternalServer = Http()
        .newServerAt(OnfhirConfig.serverHost, OnfhirConfig.internalApiPort)
        .withSettings(
          ServerSettings(OnfhirConfig.config)
            .withVerboseErrorMessages(true)
        )

      onFhirInternalServer
        .bind(onFhirInternalRoutes) onComplete {
          case Success(binding) =>
            logger.info("OnFhir internal server is started on host {} and port {}...", OnfhirConfig.serverHost, OnfhirConfig.internalApiPort)
            internalOnFhirServerBinding = binding
            internalOnFhirServerBinding.addToCoordinatedShutdown(FiniteDuration.apply(60L, TimeUnit.SECONDS))
            internalOnFhirServerBinding.whenTerminated onComplete {
              case Success(t) =>
                logger.info("OnFhir internal server is gracefully terminated...")
              case Failure(exception) =>
                logger.error("Problem while gracefully terminating OnFhir internal server!", exception)
            }
          case Failure(ex) =>
            logger.error("Problem while binding to the onFhir internal server address and port!", ex)
        }
    }
  }

  /**
   *
   * @return
   */
  protected def waitForShutdownSignal(): Future[Done] = {
    val promise = Promise[Done]()
    sys.addShutdownHook {
      promise.trySuccess(Done)
    }
    Future {
      blocking {
        do {
          val line = StdIn.readLine("Write 'quit' to stop the server...\n")
          if (line.equalsIgnoreCase("quit"))
            promise.trySuccess(Done)
        } while (!promise.isCompleted)
      }
    }
    promise.future
  }
}

/**
  * Companion object to initialize Akka Actor System and OnFhir
  */
object Onfhir {
  //Base Akka Actor system for the whole system
  implicit val actorSystem: ActorSystem = ActorSystem("onfhir")
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
    * @param cdsRoute             CDS-Hooks compliant CDS route (using onfhir-cds and repository together)
    * @return
    */
  def apply(
             fhirConfigurator:IFhirServerConfigurator,
             fhirOperationImplms:Map[String, String] = Map.empty[String, String],
             customAuthorizer:Option[IAuthorizer] = None,
             customTokenResolver:Option[ITokenResolver] = None,
             customAuditHandler:Option[ICustomAuditHandler] = None,
             externalRoutes:Seq[(FHIRRequest, (AuthContext, Option[AuthzContext])) => Route] = Nil,
             cdsRoute:Option[Route] = None
           ): Onfhir = {

    if(_instance == null)
      _instance = new Onfhir(fhirConfigurator, fhirOperationImplms, customAuthorizer, customTokenResolver,customAuditHandler,externalRoutes, cdsRoute)
    _instance
  }
}

package io.onfhir.subscription

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.cluster.typed.{Cluster, SelfUp, Subscribe}
import akka.http.scaladsl.Http.ServerBinding
import com.typesafe.config.ConfigFactory
import io.onfhir.api.SubscriptionChannelTypes
import io.onfhir.subscription.cache.{AkkaBasedSubscriptionCache, DistributedSearchParameterConfCache, FhirSearchParameterCache}
import io.onfhir.subscription.channel.WebSocketHandler.GetWebSocketHandler
import io.onfhir.subscription.channel.{RestChannelManager, WebSocketChannelManager, WebSocketHandler, WebSocketHttpServer}
import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.subscription.model.ClusterRoles

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Guardian actor for onfhir-subscription
 */
object Guardian {

  sealed trait Command
  case object NodeMemberUp extends Command
  //Shard events
  final case class FhirResourceShardingStarted(region: ActorRef[SubscriptionEvaluator.Command]) extends Command
  final case class NotificationShardingStarted(region: ActorRef[FhirNotificationHandler.Command]) extends Command

  //Event to indicate synchronization of already existing subscriptions completed or failed
  final case object FhirSubscriptionSynchronizationCompleted extends Command

  final case class InitializationFailed(msg:String, reason:Throwable) extends Command

  final case class BindingFailed(reason: Throwable) extends Command
  final case class BindingSuccess(binding:ServerBinding) extends Command

  /**
   * Initialization of the onfhir-subscription system
   */
  def apply(webSocketPort:Option[Int] = None):ActorSystem[Command] = {
    ActorSystem(
      Behaviors.setup[Command] { ctx =>
        implicit val system = ctx.system
        //Read configuration
        val subscriptionConfig = new SubscriptionConfig(ctx.system)
        //Set port if given outside
        webSocketPort.foreach(p => subscriptionConfig.webSocketPort = p)
        //Setup akka cluster
        val cluster = Cluster(ctx.system)
        //Setup to the node up event
        val upAdapter = ctx.messageAdapter[SelfUp](_ => NodeMemberUp)
        cluster.subscriptions ! Subscribe(upAdapter, classOf[SelfUp])

        //Initialize the actor that caches and manages the subscriptions
        val subscriptionCache = ctx.spawn[io.onfhir.subscription.cache.Command](AkkaBasedSubscriptionCache.apply(subscriptionConfig), "subscription-cache")

        //Initialize the push based channel manager actors (only Rest for now)
        val restChannelHandler = ctx.spawn[io.onfhir.subscription.channel.Command](RestChannelManager.apply, "rest-channel-handler")
        val pushBasedChannelHandlers = Map(SubscriptionChannelTypes.RestHook -> restChannelHandler)

        val pullBasedNotificationHandler = new PullBasedNotificationHandler()

        //Initiate classes to interact with onFhir repo
        val onFhirClient = new OnFhirClient(subscriptionConfig)
        val searchParameterCache = ctx.spawn[DistributedSearchParameterConfCache.Command](DistributedSearchParameterConfCache.apply(onFhirClient), "search-parameter-cache")
        val subscriptionManager = new SubscriptionManager(onFhirClient, searchParameterCache, subscriptionConfig)

        val isInitializer = cluster.selfMember.roles.contains(ClusterRoles.Initializer)

        //If this is the coordinator node
        if(isInitializer)
          ctx.pipeToSelf(subscriptionManager.initializeSubscriptions(subscriptionCache)){
            case Success(_) => FhirSubscriptionSynchronizationCompleted
            case Failure(ex) => InitializationFailed("Problem while retrieving existing FHIR Subscriptions!" ,ex)
          }

        //Initiate notification handler actors (sharding)
        ctx.pipeToSelf(FhirNotificationHandler.init(system, subscriptionConfig, subscriptionCache, subscriptionManager, pushBasedChannelHandlers, pullBasedNotificationHandler)) {
          case Success(notificationHandler) => NotificationShardingStarted(notificationHandler)
          case Failure(ex) => InitializationFailed("Problem while initializing akka cluster sharding for notifications!", ex)
        }

        //Initiate subscription evaluation actors (sharding)
        ctx.pipeToSelf(SubscriptionEvaluator.init(system,searchParameterCache,subscriptionCache, subscriptionConfig)) {
          case Success(subscriptionEvaluator) => FhirResourceShardingStarted(subscriptionEvaluator)
          case Failure(ex) => InitializationFailed("Problem while initializing akka cluster sharding for resource evaluation!", ex)
        }

        starting(ctx, None, None, false, !isInitializer,  subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      },
      "onfhir-subscription", ConfigFactory.load())
  }

  /**
   * Waiting for all sharding and cluster joining to finish
   * @param ctx
   * @param resourceSharding
   * @param notificationSharding
   * @param joinedCluster
   * @param subscriptionConfig
   * @return
   */
  def starting(ctx: ActorContext[Command],
               resourceSharding: Option[ActorRef[SubscriptionEvaluator.Command]],
               notificationSharding: Option[ActorRef[FhirNotificationHandler.Command]],
               joinedCluster: Boolean,
               subscriptionsSynchronized:Boolean,
               subscriptionConfig: SubscriptionConfig,
               subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
               searchParameterCache:ActorRef[DistributedSearchParameterConfCache.Command],
               subscriptionManager:SubscriptionManager
              ): Behavior[Command] =
    Behaviors
    .receive[Command] {
      case (ctx, FhirResourceShardingStarted(region)) if joinedCluster && notificationSharding.isDefined && subscriptionsSynchronized  =>
        ctx.log.info("FHIR Resource sharding has started...")
        start(ctx, region, notificationSharding.get, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      case (_, FhirResourceShardingStarted(region)) =>
        ctx.log.info("FHIR Resource sharding has started...")
        starting(ctx, Some(region), notificationSharding, joinedCluster, subscriptionsSynchronized, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      case (ctx, NotificationShardingStarted(region)) if joinedCluster && resourceSharding.isDefined && subscriptionsSynchronized  =>
        ctx.log.info("FHIR Notification sharding has started...")
        start(ctx, resourceSharding.get, region, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      case (_, NotificationShardingStarted(region)) =>
        ctx.log.info("FHIR Notification sharding has started...")
        starting(ctx, resourceSharding, Some(region), joinedCluster, subscriptionsSynchronized, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      case (ctx, NodeMemberUp) if resourceSharding.isDefined && notificationSharding.isDefined && subscriptionsSynchronized =>
        ctx.log.info("Member has joined the cluster")
        start(ctx, resourceSharding.get, notificationSharding.get, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      case (_, NodeMemberUp)  =>
        ctx.log.info("Member has joined the cluster")
        starting(ctx, resourceSharding, notificationSharding, joinedCluster = true, subscriptionsSynchronized, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      case (ctx, FhirSubscriptionSynchronizationCompleted) if joinedCluster && resourceSharding.isDefined && notificationSharding.isDefined  =>
        ctx.log.info("All active FHIR subscriptions synchronized from onFhir.io repository...")
        start(ctx, resourceSharding.get, notificationSharding.get, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      case (_, FhirSubscriptionSynchronizationCompleted)=>
        ctx.log.info("All active FHIR subscriptions synchronized from onFhir.io repository...")
        starting(ctx, resourceSharding, notificationSharding, joinedCluster, subscriptionsSynchronized = true, subscriptionConfig, subscriptionCache, searchParameterCache, subscriptionManager)
      //Stop if there is a failure in initialization
      case (_, InitializationFailed(msg, ex)) =>
        ctx.log.error(msg, ex)
        Behaviors.stopped
    }

  /**
   * Preinitialization is ok, start the system
   * @param ctx
   * @param resourceSharding
   * @param notificationSharding
   * @param subscriptionConfig
   * @param subscriptionCache
   * @param searchParameterCache
   * @param subscriptionManager
   * @return
   */
  def start(ctx: ActorContext[Command],
            resourceSharding: ActorRef[SubscriptionEvaluator.Command],
            notificationSharding: ActorRef[FhirNotificationHandler.Command],
            subscriptionConfig: SubscriptionConfig,
            subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
            searchParameterCache:ActorRef[DistributedSearchParameterConfCache.Command],
            subscriptionManager:SubscriptionManager
            ): Behavior[Command] = {
    import ctx.executionContext
    implicit val system = ctx.system
    ctx.log.info("Initialization and cluster setup is completed...")

    val subscriptionKafkaProcessor = ctx.spawn[FhirSubscriptionKafkaProcessor.Command](FhirSubscriptionKafkaProcessor.apply(subscriptionCache, subscriptionManager, searchParameterCache, subscriptionConfig), "subscription-kafka-processor")
    val resourceKafkaProcessor = ctx.spawn[FhirResourceKafkaProcessor.Command](FhirResourceKafkaProcessor.apply(resourceSharding, subscriptionConfig), "resource-kafka-processor")
    val notificationKafkaProcessor = ctx.spawn[FhirNotificationKafkaProcessor.Command](FhirNotificationKafkaProcessor.apply(notificationSharding, subscriptionConfig), "notification-kafka-processor")

    ctx.watch(subscriptionKafkaProcessor)
    ctx.watch(resourceKafkaProcessor)
    ctx.watch(notificationKafkaProcessor)

    //Initialize web socket server and channel manager and handler
    val webSocketHandler = ctx.spawn[WebSocketHandler.Command](WebSocketHandler.apply(subscriptionConfig, notificationSharding), "websocket-handler")
    val webSocketChannelManager= new WebSocketChannelManager(subscriptionConfig, subscriptionCache, webSocketHandler)
    val serverBinding = WebSocketHttpServer.start(webSocketChannelManager.websocketRoute, subscriptionConfig.webSocketPort, system)

    serverBinding.onComplete {
      case Failure(ex) => ctx.self ! BindingFailed(ex)
      case Success(binding) => ctx.self ! BindingSuccess(binding)
    }

    running(ctx, subscriptionKafkaProcessor, resourceKafkaProcessor, notificationKafkaProcessor, serverBinding)
  }

  /**
   * System is running
   * @param ctx
   * @param subscriptionKafkaProcessor
   * @param resourceKafkaProcessor
   * @param notificationKafkaProcessor
   * @param serverBinding
   * @return
   */
  def running(ctx: ActorContext[Command],
              subscriptionKafkaProcessor:ActorRef[FhirSubscriptionKafkaProcessor.Command],
              resourceKafkaProcessor: ActorRef[FhirResourceKafkaProcessor.Command],
              notificationKafkaProcessor: ActorRef[FhirNotificationKafkaProcessor.Command],
              serverBinding: Future[ServerBinding]
             ):Behavior[Command] = {
      Behaviors.receiveMessagePartial[Command] {
        case BindingFailed(ex) =>
          ctx.log.error("Failed to bind HTTP endpoint for web socket endpoint, terminating system", ex)
          Behaviors.stopped
        case BindingSuccess(binding) =>
          val address = binding.localAddress
          ctx.log.info(
            "OnFhir Subscription Web Socket Server is online at http://{}:{}/",
            address.getHostString,
            address.getPort
          )
          Behaviors.same
      }.receiveSignal {
        case (ctx, Terminated(`subscriptionKafkaProcessor`)) =>
          ctx.log.warn("Subscription kafka processor stopped. Shutting down...")
          serverBinding.map(_.unbind())(ctx.executionContext)
          Behaviors.stopped

        case (ctx, Terminated(`resourceKafkaProcessor`)) =>
          ctx.log.warn("Resource kafka processor stopped. Shutting down...")
          serverBinding.map(_.unbind())(ctx.executionContext)
          Behaviors.stopped

        case (ctx, Terminated(`notificationKafkaProcessor`)) =>
          ctx.log.warn("Notification kafka processor stopped. Shutting down...")
          serverBinding.map(_.unbind())(ctx.executionContext)
          Behaviors.stopped
      }

  }


}

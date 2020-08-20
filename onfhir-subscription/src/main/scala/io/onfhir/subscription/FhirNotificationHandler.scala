package io.onfhir.subscription


import akka.{Done, NotUsed}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.sharding.external.ExternalShardAllocationStrategy
import akka.cluster.sharding.typed.scaladsl.{ClusterSharding, Entity, EntityTypeKey}
import akka.kafka.cluster.sharding.KafkaClusterSharding
import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.subscription.model.{FhirNotification, FhirSubscription, InternalKafkaTopics}
import akka.actor.typed.scaladsl.AskPattern._
import akka.stream.{ActorMaterializer, Materializer, OverflowStrategy, SourceRef}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete, StreamRefs}
import io.onfhir.subscription.cache.{GetSubscription, RemoveSubscription, Response, SetSubscriptionStatus, UpdateSubscriptionResponse}
import io.onfhir.subscription.model._
import org.reactivestreams.Publisher
import io.onfhir.subscription.channel.{Command, IPushBasedChannelManager, SendNotification}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

object FhirNotificationHandler {
  sealed trait Command {
    val sid:String
    def getEntityId:String = sid
  }

  case class SendFhirNotification(sid:String, resourceContent:String, replyTo:ActorRef[Done]) extends Command

  case class GetFhirNotificationStream(sid:String, replyTo:ActorRef[SourceRef[FhirNotification]]) extends Command

  /**
   * Result of notification sending
   * @param sid           Subscription id
   * @param result        Is notification successfull
   * @param latestStatus  Result of Previous notification attempt
   */
  case class NotificationResult(sid:String, result:Boolean, latestStatus:Int) extends Command


  case class AdaptedCacheUpdateResponse(sid:String, result:Boolean, f:Option[Throwable] = None) extends Command

  /**
   * Initialization of this actor with sharding
   * @param system                Typed actor system
   * @param subscriptionConfig    Configuration
   * @param subscriptionCache     Subscription cache
   * @return
   */
  def init(system: ActorSystem[_], subscriptionConfig: SubscriptionConfig, subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command], pushBasedNotificationChannelActors:Map[String, ActorRef[io.onfhir.subscription.channel.Command]]): Future[ActorRef[Command]] = {
    import system.executionContext

    KafkaClusterSharding(subscriptionConfig.system)
      .messageExtractorNoEnvelope(
        timeout = 10.seconds,
        topic = InternalKafkaTopics.NotificationTopic,
        entityIdExtractor = (msg: Command) => msg.getEntityId,
        settings = subscriptionConfig.kafkaConsumerSettings(ConsumerGroupIds.kafkaConsumerGroupIdForNotifications)
      ).map(messageExtractor => {
        system.log.info("Message extractor created. Initializing sharding for group {}", ConsumerGroupIds.kafkaConsumerGroupIdForNotifications)
        ClusterSharding(system).init(
          Entity(subscriptionConfig.entityTypeKeyForNotification)(createBehavior = _ => FhirNotificationHandler(subscriptionConfig, subscriptionCache, pushBasedNotificationChannelActors, system))
          .withAllocationStrategy(new ExternalShardAllocationStrategy(system, subscriptionConfig.entityTypeKeyForNotification.name))
          .withMessageExtractor(messageExtractor))
      })
  }

  //Actor behaviour setup
  def apply(subscriptionConfig: SubscriptionConfig, subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command], pushBasedNotificationChannelActors:Map[String, ActorRef[io.onfhir.subscription.channel.Command]], system: ActorSystem[_]): Behavior[Command] = {
    //Create pull based notification queue and publisher for channels like web socket
    implicit val materializer = Materializer.matFromSystem(system)
    val (pullBasedNotificationQueue, notificationPublisher) = Source
      .queue[FhirNotification](10, OverflowStrategy.backpressure)
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    running(subscriptionConfig, subscriptionCache, pullBasedNotificationQueue, notificationPublisher, pushBasedNotificationChannelActors)
  }

  /**
   * Actor behaviour
   * @param subscriptionConfig                  Subscription configuration
   * @param subscriptionCache                   Subscription cache
   * @param pullBasedNotificationQueue          Queue for pull based notification handlers (e.g. web socket)
   * @param notificationPublisher               Publisher for pull based notification handlers (e.g. web socket)
   * @param pushBasedNotificationChannelActors  Push based notification handlers by channel type
   * @return
   */
  private def running(
                       subscriptionConfig: SubscriptionConfig,
                       subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
                       pullBasedNotificationQueue:SourceQueueWithComplete[FhirNotification],
                       notificationPublisher:Publisher[FhirNotification],
                       pushBasedNotificationChannelActors:Map[String, ActorRef[io.onfhir.subscription.channel.Command]]
                     ): Behavior[Command] = {
    Behaviors.setup { ctx =>
      implicit val materializer: Materializer = Materializer.matFromSystem(ctx.system)
      implicit val executionContext: ExecutionContextExecutor = ctx.executionContext

      implicit val responseTimeout = subscriptionConfig.processorAskTimeout
      Behaviors.receiveMessage[Command] {
        //Handle the
        case SendFhirNotification(sid, resource, replyTo) =>
          subscriptionCache
            .ask[Option[FhirSubscription]](replyTo => GetSubscription(sid, replyTo))(subscriptionConfig.processorAskTimeout, ctx.system.scheduler)
              .map {
                case None =>
                  ctx.log.warn("Cannot handle notification, no such subscription {}", sid)
                  replyTo ! Done
                case Some(fs) =>
                  fs.channel.channelType match {
                    //Web socket is pull based, so we add to our queue
                    case ChannelTypes.WebSocket =>
                      pullBasedNotificationQueue.offer(FhirNotification(fs.id)) //For web sockets we only send a ping message to subscription id
                    case _ =>
                      pushBasedNotificationChannelActors
                        .get(fs.channel.channelType) match {
                          case None => ctx.log.warn(s"Communication channel ${fs.channel.channelType} is not supported in onFhir yet!")
                          case Some(cha) =>
                            //TODO handle Resource conversion based on mime type
                            val fhirNotification = FhirNotification(sid, fs.channel.endpoint, fs.channel.payload.map(mimeType => resource), fs.channel.headers, fs.status)
                            cha.tell(SendNotification(fhirNotification, ctx.self))
                        }
                  }
                  replyTo ! Done
              }
          Behaviors.same

        //Return the notification stream for specific subscription
        case GetFhirNotificationStream(sid, replyTo) =>
          val streamRef =
            Source
            .fromPublisher(notificationPublisher)
            .filter(_.sid == sid) //Get the stream for the specific subscription
            .runWith(StreamRefs.sourceRef())

          replyTo.tell(streamRef) //return the stream ref
          Behaviors.same

        case NotificationResult(sid, result, previousStatus) =>
          def updateSubscriptionStatus(status:Int) =
            ctx.ask[io.onfhir.subscription.cache.Command, io.onfhir.subscription.cache.Response](subscriptionCache, replyTo => SetSubscriptionStatus(sid, status, replyTo)){
              case Success(UpdateSubscriptionResponse(r)) => AdaptedCacheUpdateResponse(sid, r)
              case Failure(f) => AdaptedCacheUpdateResponse(sid, false, Some(f))
            }

          var status:Option[String] = None
          (result, previousStatus) match {
              //Notification is sent, it was also successful before
              case (true, 1) =>  //DO nothing
              case (true, _) =>
                status = Some(SubscriptionStatusCodes.active)
                updateSubscriptionStatus(1)
              //Otherwise
              case (false, i) =>
                i match {
                  //It was Ok before, this is the first error
                  case 1  =>
                    status = Some(SubscriptionStatusCodes.error)
                    updateSubscriptionStatus(-1)
                  //Second error
                  case r if r < -2 =>
                    updateSubscriptionStatus(r- 1)
                  //If still problematic, remove it
                  case _ =>
                    status = Some(SubscriptionStatusCodes.off)
                    ctx.ask[io.onfhir.subscription.cache.Command, io.onfhir.subscription.cache.Response](subscriptionCache, replyTo => RemoveSubscription(sid, replyTo)){
                      case Success(UpdateSubscriptionResponse(r)) => AdaptedCacheUpdateResponse(sid, r)
                      case Failure(f) => AdaptedCacheUpdateResponse(sid, false, Some(f))
                    }
                }
            }

          status

          Behaviors.same
      }
    }
  }

}

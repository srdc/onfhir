package io.onfhir.subscription

import akka.Done
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.adapter._
import io.onfhir.subscription.config.SubscriptionConfig
import akka.actor.{ActorSystem, Scheduler}
import akka.kafka.ConsumerMessage.CommittableOffset
import akka.kafka.ProducerMessage.Envelope
import akka.kafka.cluster.sharding.KafkaClusterSharding
import akka.kafka.scaladsl.Consumer.DrainingControl
import akka.kafka.{CommitterSettings, ConsumerMessage, ProducerMessage, Subscriptions}
import akka.kafka.scaladsl.{Committer, Consumer, Producer}
import io.onfhir.subscription.cache.{AddOrUpdateSubscription, DistributedSearchParameterConfCache, FhirSearchParameterCache, RemoveSubscription, Response, UpdateSubscriptionResponse}
import io.onfhir.subscription.model.ConsumerGroupIds
import akka.pattern.retry
import io.onfhir.api.SubscriptionStatusCodes
import io.onfhir.api.model.FhirSubscription
import io.onfhir.subscription.cache.DistributedSearchParameterConfCache.{SyncSearchParameterConfs, SyncSearchParameterConfsResponse}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Try
import io.onfhir.util.JsonFormatter._
import org.slf4j.LoggerFactory

import scala.concurrent.duration._

/**
 * Kafka Processor for FHIR Subscriptions (create or delete) that caches them and prepare for criteria handling
 */
object FhirSubscriptionKafkaProcessor {
  sealed trait Command

  private case class KafkaConsumerStopped(reason: Try[Any]) extends Command

  val log= LoggerFactory.getLogger("FhirSubscriptionKafkaProcessor")

  /**
   * Kafka FHIR Subscription processor
   * @param subscriptionCache   Subscription cache
   * @param subscriptionManager FHIR Subscription resource manager (interface with onFhir repo)
   * @param subscriptionConfig  Subscription config
   * @return
   */
  def apply(
             subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
             subscriptionManager: SubscriptionManager,
             searchParameterCache:ActorRef[DistributedSearchParameterConfCache.Command],
             subscriptionConfig: SubscriptionConfig): Behavior[Command] = {
    Behaviors
      .setup[Command] { ctx =>
        implicit val classic: ActorSystem = ctx.system.toClassic
        implicit val ec: ExecutionContextExecutor = ctx.executionContext
        implicit val scheduler: Scheduler = classic.scheduler

        //Cluster rebalancing listener
        val rebalanceListener =
          KafkaClusterSharding(classic)
            .rebalanceListener(subscriptionConfig.entityTypeKeyForNotification)

        //Kafka topic subscription
        val subscription =
          Subscriptions
            .topics(subscriptionConfig.kafkaFhirSubscriptionTopic)
            .withRebalanceListener(rebalanceListener.toClassic)

        val stream =
          Consumer
            .committableSource(subscriptionConfig.kafkaConsumerSettings(ConsumerGroupIds.kafkaConsumerGroupIdForSubscriptions), subscription)
            .mapAsync(5) { msg =>
              log.debug(s"Consumed kafka partition ${msg.record.key()}->${msg.committableOffset.partitionOffset} for FHIR Subscriptions...")
              //Get the subscription key from the record key
              val subsId = msg.record.key()

              val subsHandling:Future[Envelope[String, String, CommittableOffset]] =
                msg.record.value() match {
                  //Empty content means deletion of subscription
                  case "" =>
                    subscriptionCache
                      .ask[Response](replyTo => RemoveSubscription(subsId, replyTo))(subscriptionConfig.processorAskTimeout, ctx.system.scheduler)
                      .map {
                        //if everything is Ok, commit
                        case UpdateSubscriptionResponse(true) =>
                          log.debug("FHIR Subscription '{}' is processed successfully !", subsId)
                          ProducerMessage.passThrough(msg.committableOffset)
                        case UpdateSubscriptionResponse(false) =>
                          log.error("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
                          throw new RuntimeException("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
                      }
                  //Otherwise insertion of subscription
                  case sc =>
                    val subscription = sc.parseJson.extract[FhirSubscription]
                    handleSubscription(subscription, subscriptionCache, subscriptionConfig, searchParameterCache, ctx.system)
                      .flatMap {
                        case true =>
                          handleNormalCase(subscriptionManager, subscription, msg)
                        case false =>
                          handleErrorCase(subscriptionManager, subscription, msg)
                      }
              }
              //Retry 3 times
              val result = retry(() => subsHandling, attempts = 3, delay = 1.second)

              result
            }
            .via(Producer.flexiFlow(subscriptionConfig.kafkaProducerSettings))
            .map(_.passThrough)
            .toMat(Committer.sink(CommitterSettings(classic)))(DrainingControl.apply)
            .run()

        stream.streamCompletion.onComplete { result =>
          ctx.self ! KafkaConsumerStopped(result)
        }
        Behaviors.receiveMessage[Command] {
          case KafkaConsumerStopped(reason) =>
            ctx.log.info("Fhir Subscription Kafka Consumer stopped {}", reason)
            Behaviors.stopped
        }
      }.narrow
  }

  /**
   * Handle insertion/update of subscription
   * @param subscription
   * @param subscriptionCache
   * @param subscriptionConfig
   * @param searchParameterCache
   * @param system
   * @return
   */
  def handleSubscription(
                          subscription:FhirSubscription,
                          subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
                          subscriptionConfig:SubscriptionConfig,
                          searchParameterCache:ActorRef[DistributedSearchParameterConfCache.Command],
                          system:akka.actor.typed.ActorSystem[_]):Future[Boolean] = {
    implicit val executionContext: ExecutionContextExecutor = system.executionContext
    //If the subscription is set to off by the client himself
    if(subscription.status == SubscriptionStatusCodes.off){
      subscriptionCache
        .ask[Response](replyTo => RemoveSubscription(subscription.id, replyTo))(subscriptionConfig.processorAskTimeout, system.scheduler)
        .map {
          //If subscription is removed
          case UpdateSubscriptionResponse(true) => true
          case UpdateSubscriptionResponse(false) => false
        }
    } else {
      subscriptionCache
        .ask[Response](replyTo => AddOrUpdateSubscription(subscription, replyTo))(subscriptionConfig.processorAskTimeout, system.scheduler)
        .flatMap {
          //If subscription is updated
          case UpdateSubscriptionResponse(true) =>
            //Synchronize search parameter definitions from onFhir repo to handle this subscription query
            searchParameterCache
              .ask[DistributedSearchParameterConfCache.Response](replyTo => SyncSearchParameterConfs(subscription.rtype, subscription.criteria.map(_.name).toSet, replyTo))(subscriptionConfig.processorAskTimeout, system.scheduler)
              .map {
                  case SyncSearchParameterConfsResponse(result) => result
              }
          case UpdateSubscriptionResponse(false) =>
            Future.apply(false)
        }
    }
  }

  /**
   * Handle error case for new FHIR subscription
   *
   * @param subscriptionManager
   * @param subscription
   * @param msg
   * @param executionContext
   * @param scheduler
   * @return
   */
  def handleErrorCase(subscriptionManager: SubscriptionManager, subscription:FhirSubscription, msg:ConsumerMessage.CommittableMessage[String, String])(implicit executionContext:ExecutionContext, scheduler: Scheduler):Future[Envelope[String, String, CommittableOffset]] = {
    log.error("Problem while handling FHIR subscription '{}'", subscription.id)
    if(subscription.status != SubscriptionStatusCodes.off) {
      //Update the status of subscription to off
      subscriptionManager
        .updateSubscriptionStatus(subscription.id, SubscriptionStatusCodes.off, Some("Internal error in onfhir-subscription module while handling subscription!"))
        .map {
          //If that is also not possible, exit from stream
          case false =>
            log.error("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
            throw new RuntimeException("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
          //Otherwise commit (subscription is processed but is not active)
          case true => ProducerMessage.passThrough(msg.committableOffset)
        }
    } else
      Future.apply(ProducerMessage.passThrough(msg.committableOffset))
  }

  /**
   * Handle normal case
   * @param subscriptionManager
   * @param subscription
   * @param msg
   * @param executionContext
   * @param scheduler
   * @return
   */
  def handleNormalCase(subscriptionManager: SubscriptionManager, subscription:FhirSubscription, msg:ConsumerMessage.CommittableMessage[String, String])(implicit executionContext:ExecutionContext, scheduler: Scheduler):Future[Envelope[String, String, CommittableOffset]] = {
    log.debug("FHIR Subscription '{}' is processed successfully !", subscription.id)
    if(subscription.status == SubscriptionStatusCodes.requested)
      retry( () =>
        subscriptionManager
          .updateSubscriptionStatus(subscription.id, SubscriptionStatusCodes.active, None),
        attempts = 3,
        delay = 1.second
      ).map {
         case true => ProducerMessage.passThrough(msg.committableOffset)
         case false =>
           log.error(s"Problem while setting status of subscription ${subscription.id} as active in onFhir.io repo !, Ignoring the error and continue working...")
           ProducerMessage.passThrough(msg.committableOffset)
      }
    else //Otherwise commit
      Future.apply(ProducerMessage.passThrough(msg.committableOffset))
  }
}

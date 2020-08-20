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
import io.onfhir.subscription.cache.{AddOrUpdateSubscription, FhirSearchParameterCache, RemoveSubscription, Response, UpdateSubscriptionResponse}
import io.onfhir.subscription.model.{ConsumerGroupIds, FhirSubscription, SubscriptionStatusCodes}
import akka.pattern.retry

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Try
import io.onfhir.util.JsonFormatter._

import scala.concurrent.duration._

/**
 * Kafka Processor for FHIR Subscriptions (create or delete) that caches them and prepare for criteria handling
 */
object FhirSubscriptionKafkaProcessor {
  sealed trait Command

  private case class KafkaConsumerStopped(reason: Try[Any]) extends Command

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
             searchParameterCache:FhirSearchParameterCache,
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
              ctx.log.debug(s"Consumed kafka partition ${msg.record.key()}->${msg.committableOffset.partitionOffset} for FHIR Subscriptions...")
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
                          ctx.log.debug("FHIR Subscription '{}' is processed successfully !", subsId)
                          ProducerMessage.passThrough(msg.committableOffset)
                        case UpdateSubscriptionResponse(false) =>
                          ctx.log.error("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
                          throw new RuntimeException("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
                      }
                  //Otherwise insertion of subscription
                  case sc =>
                    val subscription = sc.parseJson.extract[FhirSubscription]
                    subscriptionCache
                      .ask[Response](replyTo => AddOrUpdateSubscription(subscription, replyTo))(subscriptionConfig.processorAskTimeout, ctx.system.scheduler)
                      .flatMap {
                        //If subscription is updated
                        case UpdateSubscriptionResponse(true) =>
                          //Synchronize search parameter definitions from onFhir repo to handle this subscription query
                          searchParameterCache
                            .syncSearchParameterConfs(subscription.rtype, subscription.criteria.map(_.name).toSet)
                            .flatMap {
                              //if everything is Ok, commit
                              case true =>
                                ctx.log.debug("FHIR Subscription '{}' is processed successfully !", subsId)
                                Future.apply(ProducerMessage.passThrough(msg.committableOffset))
                              case false =>
                                handleErrorCase(ctx,subscriptionManager, subsId, msg)
                            }
                        case UpdateSubscriptionResponse(false) =>
                          handleErrorCase(ctx,subscriptionManager, subsId, msg)
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
   * Handle error case for new FHIR subscription
   * @param ctx
   * @param subscriptionManager
   * @param subsId
   * @param msg
   * @param executionContext
   * @return
   */
  def handleErrorCase(ctx:ActorContext[Command], subscriptionManager: SubscriptionManager, subsId:String, msg:ConsumerMessage.CommittableMessage[String, String])(implicit executionContext:ExecutionContext):Future[Envelope[String, String, CommittableOffset]] = {
    ctx.log.error("Problem while handling FHIR subscription '{}'", subsId)
    //Update the status of subscription to off
    subscriptionManager
      .updateSubscriptionStatus(subsId, SubscriptionStatusCodes.off, Some("Internal error in onfhir-subscription module while handling subscription!"))
      .map {
        //If that is also not possible, exit from stream
        case false =>
          ctx.log.error("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
          throw new RuntimeException("Problem while writing the status of a subscription back to onFhir repository, exiting from subscription stream!")
        //Otherwise commit (subscription is processed but is not active)
        case true => ProducerMessage.passThrough(msg.committableOffset)
      }
  }
}

package io.onfhir.subscription

import akka.{Done, NotUsed}
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.{ActorSystem, Scheduler}
import akka.kafka.cluster.sharding.KafkaClusterSharding
import akka.kafka.scaladsl.{Committer, Consumer}
import akka.kafka.{CommitterSettings, ProducerMessage, Subscriptions}
import akka.pattern.retry

import io.onfhir.subscription.SubscriptionEvaluator.GetSatisfiedSubscriptionsForResource
import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.event.FhirEventUtil
import org.apache.kafka.clients.producer.ProducerRecord
import io.onfhir.subscription.model.InternalKafkaTopics


import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Try
import scala.concurrent.duration._

/**
 * Reads the FHIR resources from Kafka, delegate subscription evaluations to SubscriptionEvaluator and write possible notifications back to Kafka
 */
object FhirResourceKafkaProcessor {

  sealed trait Command

  private case class KafkaConsumerStopped(reason: Try[Any]) extends Command

  /**
   * Actor behaviour
   * @param shardRegion           Sharded SubscriptionEvaluator actor
   * @param subscriptionConfig    Subscription configuration
   * @return
   */
  def apply(shardRegion: ActorRef[SubscriptionEvaluator.Command], subscriptionConfig: SubscriptionConfig): Behavior[Nothing] = {
    Behaviors
      .setup[Command] { ctx =>
        implicit val classic: ActorSystem = ctx.system.toClassic
        implicit val ec: ExecutionContextExecutor = ctx.executionContext
        implicit val scheduler: Scheduler = classic.scheduler

        //Cluster rebalancing listener
        val rebalanceListener = KafkaClusterSharding(classic).rebalanceListener(subscriptionConfig.entityTypeKey)

        //Kafka topic subscription
        val subscription = Subscriptions
          .topics(subscriptionConfig.kafkaFhirTopic)
          .withRebalanceListener(rebalanceListener.toClassic)

        val stream: Future[Done] = Consumer.committableSource(subscriptionConfig.kafkaConsumerSettings, subscription)
          // MapAsync and Retries can be replaced by reliable delivery
          .mapAsync(20) { msg =>
            ctx.log.info(s"Consumed kafka partition ${msg.record.key()}->${msg.committableOffset.partitionOffset} for FHIR resources")
            //Extract resource type and id from Kafka record key
            val (rtype, rid) = FhirEventUtil.parseTopicKey(msg.record.key())
            //Get the resource content
            val resource = msg.record.value()

            //If there is no content it means it is a delete, ignore it
            if(resource == "")
              Future.apply(ProducerMessage.passThrough(msg.committableOffset))
            else {
              //Evaluate the resource against subscriptions
              val subsIdsFuture =
                retry(() =>
                  shardRegion.ask[Seq[String]](replyTo => {
                    GetSatisfiedSubscriptionsForResource(rtype,rid, resource, replyTo)
                  })(subscriptionConfig.processorAskTimeout, ctx.system.scheduler),
                  attempts = 3,
                  delay = 1.second
                )

              subsIdsFuture.map(sids =>
                sids.length match {
                  case 0 => ProducerMessage.passThrough(msg.committableOffset) //No subscription
                  case 1 => ProducerMessage.single( new ProducerRecord(InternalKafkaTopics.NotificationTopic, sids.head, resource), msg.committableOffset) //only one subscription
                  case _ =>
                    val notifications = sids.map(sid => new ProducerRecord(InternalKafkaTopics.NotificationTopic, sid, resource)).to[collection.immutable.Seq]
                    ProducerMessage.multi(
                      notifications,
                      msg.committableOffset
                    )
                }
              )
            }
          }
          .runWith(Committer.sinkWithOffsetContext(CommitterSettings(classic)))

        stream.onComplete { result =>
          ctx.self ! KafkaConsumerStopped(result)
        }
        Behaviors.receiveMessage[Command] {
          case KafkaConsumerStopped(reason) =>
            ctx.log.info("Fhir Kafka Consumer stopped {}", reason)
            Behaviors.stopped
        }
      }
      .narrow
  }
}

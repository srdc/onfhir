package io.onfhir.subscription

import akka.Done
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import io.onfhir.subscription.config.SubscriptionConfig
import akka.actor.{ActorSystem, Scheduler}
import akka.kafka.Subscriptions
import akka.kafka.scaladsl.Consumer

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Try


object FhirSubscriptionKafkaProcessor {
  sealed trait Command

  private case class KafkaConsumerStopped(reason: Try[Any]) extends Command

  /**
   * Kafka FHIR Subscription processor
   * @param subscriptionCache   Subscription cache
   * @param subscriptionConfig  Subscription config
   * @return
   */
  def apply(subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command], subscriptionConfig: SubscriptionConfig): Behavior[Nothing] = {
    Behaviors
      .setup[Command] { ctx =>
        implicit val classic: ActorSystem = ctx.system.toClassic
        implicit val ec: ExecutionContextExecutor = ctx.executionContext
        implicit val scheduler: Scheduler = classic.scheduler

        //Kafka topic subscription
        val subscription = Subscriptions.topics(subscriptionConfig.kafkaFhirSubscriptionTopic)

        val stream: Future[Done] =
          Consumer.committableSource(subscriptionConfig.kafkaConsumerSettings, subscription)
            .mapAsync(5) { msg =>
              ctx.log.info(s"Consumed kafka partition ${msg.record.key()}->${msg.committableOffset.partitionOffset} for FHIR Subscriptions...")
              //Get the subscription key from the record key
              val subsId = msg.record.key()

              msg.record.value()

            }
      }
  }
}

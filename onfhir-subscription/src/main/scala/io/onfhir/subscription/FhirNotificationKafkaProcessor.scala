package io.onfhir.subscription


import akka.Done
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import akka.actor.{ActorSystem, Scheduler}
import akka.actor.typed.scaladsl.adapter._
import akka.kafka.{CommitterSettings, Subscriptions}
import akka.kafka.cluster.sharding.KafkaClusterSharding
import akka.kafka.scaladsl.{Committer, Consumer}
import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.subscription.model.{ConsumerGroupIds, InternalKafkaTopics}
import akka.actor.typed.scaladsl.AskPattern._
import akka.kafka.scaladsl.Consumer.DrainingControl
import akka.pattern.retry
import akka.stream.ActorAttributes.SupervisionStrategy
import akka.stream.scaladsl.RestartSource
import io.onfhir.subscription.FhirNotificationHandler.SendFhirNotification
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContextExecutor
import scala.util.Try
import scala.concurrent.duration._

object FhirNotificationKafkaProcessor {
  sealed trait Command

  private case class KafkaConsumerStopped(reason: Try[Any]) extends Command

  val log = LoggerFactory.getLogger("FhirNotificationKafkaProcessor")
  /**
   * Actor behaviour
   * @param shardRegion           Sharded SubscriptionEvaluator actor
   * @param subscriptionConfig    Subscription configuration
   * @return
   */
  def apply(shardRegion: ActorRef[FhirNotificationHandler.Command], subscriptionConfig: SubscriptionConfig): Behavior[Command] = {
    processNotifications(shardRegion, subscriptionConfig)
    /*Behaviors
      .supervise(processNotifications(shardRegion, subscriptionConfig))
      .onFailure(SupervisorStrategy.restartWithBackoff(1 seconds, 20 seconds, 0.2))*/
  }

  def processNotifications(shardRegion: ActorRef[FhirNotificationHandler.Command], subscriptionConfig: SubscriptionConfig): Behavior[Command] = {
    Behaviors
      .setup[Command] { ctx =>
        implicit val classic: ActorSystem = ctx.system.toClassic
        implicit val ec: ExecutionContextExecutor = ctx.executionContext
        implicit val scheduler: Scheduler = classic.scheduler

        //Cluster rebalancing listener
        val rebalanceListener = KafkaClusterSharding(classic).rebalanceListener(subscriptionConfig.entityTypeKeyForNotification)

        //Kafka topic subscription
        val subscription = Subscriptions
          .topics(InternalKafkaTopics.NotificationTopic)
          .withRebalanceListener(rebalanceListener.toClassic)

        Consumer
          .committableSource(subscriptionConfig.kafkaConsumerSettings(ConsumerGroupIds.kafkaConsumerGroupIdForNotifications), subscription)
          .mapAsync(20)(cr => {
            log.debug(s"Consumed kafka partition ${cr.record.key()}->${cr.record.partition()} for FHIR notification")
            val sid = cr.record.key()
            val content = cr.record.value()
            //Send notification
            retry(() =>
              shardRegion
                .ask[Done](replyTo => SendFhirNotification(sid, content, replyTo))(subscriptionConfig.processorAskTimeout, ctx.system.scheduler),
              attempts = 3,
              delay = 1.second
            ).
              map(_ => cr.committableOffset)
          })
          .toMat(Committer.sink(CommitterSettings(classic)))(DrainingControl.apply)
          .run()

        Behaviors.receiveMessage[Command] {
          case KafkaConsumerStopped(reason) =>
            ctx.log.info("Fhir Notification Kafka Consumer stopped {}", reason)
            Behaviors.stopped
        }
      }.narrow
  }
}

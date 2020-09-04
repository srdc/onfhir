package io.onfhir.subscription.config


import akka.actor.typed.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import akka.kafka.{ConsumerSettings, ProducerSettings}
import akka.util.Timeout
import io.onfhir.subscription.SubscriptionEvaluator
import io.onfhir.subscription.FhirNotificationHandler
import io.onfhir.subscription.model.ConsumerGroupIds
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, StringDeserializer, StringSerializer}

import scala.concurrent.duration._
import scala.util.Try
import collection.JavaConverters._
class SubscriptionConfig(val system: ActorSystem[_]) {
  //Configuration for onfhir system related issues
  val onFhirConf = system.settings.config.getConfig("onfhir")

  //Comma seperated list of host:port information for kafka brokers
  val kafkaBootstrapServers = onFhirConf.getStringList("subscription.kafka.bootstrap-servers").asScala.toSeq
  //Topic pattern for Fhir resource stream (create, update, delete)
  val kafkaFhirTopic = onFhirConf.getString("subscription.kafka.fhir-topic")
  val kafkaFhirSubscriptionTopic = onFhirConf.getString("subscription.kafka.fhir-subscription-topic")

  var webSocketPort = Try(onFhirConf.getInt("subscription.websocket.port")).toOption.getOrElse(8081)

  //Onfhir hosts to connect for getting search parameter configurations and updating Subscription's
  val onFhirHosts = onFhirConf.getStringList("hosts").asScala.toSet

  val onFhirInternalSecretKey = Try(onFhirConf.getString("internal-api-secret-key")).toOption

  //Entity type keys for each sharded streams
  val entityTypeKeyForResources: EntityTypeKey[SubscriptionEvaluator.Command] = EntityTypeKey(ConsumerGroupIds.kafkaConsumerGroupIdForResources)
  val entityTypeKeyForNotification:EntityTypeKey[FhirNotificationHandler.Command] = EntityTypeKey(ConsumerGroupIds.kafkaConsumerGroupIdForNotifications)

  //Timeout for subscription evaluators
  val processorAskTimeout = Timeout.create(onFhirConf.getDuration("subscription.ask-timeout"))

  //Dimension parameter names for each resource type (if requested)
  /*val dimensionsForResourceTypes:Map[String, Seq[String]] =
    onFhirConf
    .getConfig("subscription.dimensions")
    .entrySet().asScala
    .map(e => e.getKey -> e.getValue.unwrapped().asInstanceOf[java.util.List[String]].asScala)
    .toMap
*/

  /**
   * Return kafka consumer settings
   * @return
   */
  def kafkaConsumerSettings(groupId:String): ConsumerSettings[String, String] = {
    ConsumerSettings(system, new StringDeserializer, new StringDeserializer)
      .withBootstrapServers(kafkaBootstrapServers.mkString(","))
      .withGroupId(groupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
      .withStopTimeout(0.seconds)
  }

  def kafkaProducerSettings:ProducerSettings[String, String] = {
    ProducerSettings(system, new StringSerializer, new StringSerializer)
      .withBootstrapServers(kafkaBootstrapServers.mkString(","))
  }




}

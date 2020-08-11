package io.onfhir.subscription.config

import akka.actor.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import akka.kafka.ConsumerSettings
import akka.util.Timeout
import io.onfhir.subscription.SubscriptionEvaluator
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, StringDeserializer}

import scala.concurrent.duration._
import scala.util.Try
import collection.JavaConverters._
class SubscriptionConfig(val system: ActorSystem) {
  //Configuration for onfhir system related issues
  val onFhirConf = system.settings.config.getConfig("onfhir")

  //Comma seperated list of host:port information for kafka brokers
  val kafkaBootstrapServers = onFhirConf.getString("subscription.kafka.bootstrap-servers")
  //Topic pattern for Fhir resource stream (create, update, delete)
  val kafkaFhirTopic = onFhirConf.getString("subscription.kafka.fhir-topic")
  val kafkaFhirSubscriptionTopic = onFhirConf.getString("subscription.kafka.fhir-subscription-topic")

  val kafkaConsumerGroupId = "onfhir-subscription"

  //Timeout for subscription evaluators
  val processorAskTimeout = Timeout.create(onFhirConf.getDuration("subscription.ask-timeout"))

  //Dimension parameter names for each resource type (if requested)
  val dimensionsForResourceTypes:Map[String, Seq[String]] =
    onFhirConf
    .getConfig("subscription.dimensions")
    .entrySet().asScala
    .map(e => e.getKey -> e.getValue.unwrapped().asInstanceOf[java.util.List[String]].asScala)
    .toMap


  /**
   * Return kafka consumer settings
   * @return
   */
  def kafkaConsumerSettings: ConsumerSettings[String, String] = {
    ConsumerSettings(system, new StringDeserializer, new StringDeserializer)
      .withBootstrapServers(kafkaBootstrapServers)
      .withGroupId(kafkaConsumerGroupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
      .withStopTimeout(0.seconds)
  }

  val entityTypeKey: EntityTypeKey[SubscriptionEvaluator.Command] = EntityTypeKey(kafkaConsumerGroupId)
}

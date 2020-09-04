package io.onfhir.event.kafka

import com.typesafe.config.Config

import scala.util.Try
import scala.collection.JavaConverters._

class KafkaConfig(config:Config) {
  /**
   * List of Kafka brokers
   */
  lazy val kafkaBootstrapServers = Try(config.getStringList("kafka.bootstrap-servers")).toOption.map(_.asScala).getOrElse(Seq("localhost:9092"))

  /** Kafka topic for FHIR CUD events for resources  */
  lazy val kafkaTopic:String = Try(config.getString("kafka.fhir-topic")).getOrElse("fhir")

  /**Special FHIR topic for FHIR Subscription events **/
  lazy val kafkaSubscriptionTopic:String = Try(config.getString("kafka.fhir-subscription-topic")).getOrElse("onfhir.subscription")

  /** Kafka Client Id  */
  lazy val kafkaClientId:String = Try(config.getString("kafka.client.id")).getOrElse("onfhir")

  /** Determines if kafka producer is enabled or not */
  lazy val kafkaEnabled :Boolean= Try(config.getBoolean("kafka.enabled")).getOrElse(false)

  /** If given kafka sending is only available for these resources **/
  lazy val kafkaEnabledResources:Option[Seq[String]] = Try(config.getStringList("kafka.enabled-resources").asScala).toOption

  /** Check if Kafka resource sending is enabled for this resource type **/
  def isKafkaEnabled(rtype:String):Boolean = {
    kafkaEnabled && (kafkaEnabledResources.isEmpty || kafkaEnabledResources.get.contains(rtype))
  }
}

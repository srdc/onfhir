package io.onfhir.event.kafka

import com.typesafe.config.Config

import scala.util.Try
import scala.collection.JavaConverters._

class KafkaConfig(config:Config) {
  /** Kafka host name or ip  */
  lazy val kafkaHost:String = Try(config.getString("kafka.host")).getOrElse("localhost")

  /** Kafka port  */
  lazy val kafkaPort:Int = Try(config.getInt("kafka.port")).getOrElse(9092)

  /** Kafka topic   */
  lazy val kafkaTopic:String = Try(config.getString("kafka.topic")).getOrElse("raw.fhir")

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

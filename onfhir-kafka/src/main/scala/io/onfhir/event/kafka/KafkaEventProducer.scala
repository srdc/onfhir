package io.onfhir.event.kafka

import java.util.Properties

import akka.actor.{Actor, Props}
import io.onfhir.config.OnfhirConfig
import io.onfhir.event.{ResourceCreated, ResourceDeleted, ResourceUpdated}
import io.onfhir.util.JsonFormatter._

import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.slf4j.{Logger, LoggerFactory}

/**
  * Kafka event producer for FHIR events
  * @param kafkaConfig Configuration for Kafka integration
  */
class KafkaEventProducer(kafkaConfig:KafkaConfig) extends Actor {
  private val logger:Logger = LoggerFactory.getLogger("KafkaEventProducer")
  /**
    * Properties for Kafka connection
    */
  private lazy val kafkaProducerProperties = {
    val props = new Properties()
    props.put("bootstrap.servers", kafkaConfig.kafkaHost + ":" + kafkaConfig.kafkaPort) // Server address and port
    props.put("key.serializer",   "org.apache.kafka.common.serialization.StringSerializer")
    props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    props.put("client.id", kafkaConfig.kafkaClientId)
    props.put("acks", "0")

    props
  }

  /**
    * Kafka producer
    */
  lazy val producer:KafkaProducer[String, String] = {
    logger.info("Initializing Kafka producer for host {} and port {}", kafkaConfig.kafkaHost, kafkaConfig.kafkaPort)
    new KafkaProducer[String, String](kafkaProducerProperties)
  }

  /**
    * Events expected
    * @return
    */
  override def receive: Receive = {
    case rc:ResourceCreated =>
      sendString(kafkaConfig.kafkaTopic, rc.getTopicKey(), rc.resource.toJson)
    case ru:ResourceUpdated =>
      sendString(kafkaConfig.kafkaTopic, ru.getTopicKey(), ru.resource.toJson)
    case rd:ResourceDeleted =>
      sendString(kafkaConfig.kafkaTopic, rd.getTopicKey(), "")
  }


  /**
    * Sends key:value in String for given topic
    * @param topic Kafka topic
    * @param key Kafka key
    * @param value Kafka value
    */
  def sendString(topic:String, key:String, value:String) = {
    try {
      val record = new ProducerRecord[String, String](topic, key, value)
      producer.send(record)
    } catch {
      case t:Throwable => logger.warn("Problem in sending resource to Kafka!")
    }
  }
}

object KafkaEventProducer {
  final val ACTOR_NAME = "kafka-event-producer"

  lazy val kafkaConfig = new KafkaConfig(OnfhirConfig.config)

  def props() = Props(new KafkaEventProducer(kafkaConfig))
}
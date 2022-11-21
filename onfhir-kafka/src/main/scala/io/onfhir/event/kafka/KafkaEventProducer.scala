package io.onfhir.event.kafka

import java.util.Properties
import akka.actor.{Actor, Props}
import io.onfhir.api.util.SubscriptionUtil
import io.onfhir.config.{FhirServerConfig, OnfhirConfig}
import io.onfhir.event.{FhirDataEvent, ResourceCreated, ResourceDeleted, ResourceUpdated}
import io.onfhir.util.InternalJsonMarshallers
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.slf4j.{Logger, LoggerFactory}

/**
  * Kafka event producer for FHIR events
  * @param kafkaConfig Configuration for Kafka integration
  * @param fhirConfig FHIR server configuration
  */
class KafkaEventProducer(kafkaConfig:KafkaConfig, fhirConfig:FhirServerConfig) extends Actor {
  private val logger:Logger = LoggerFactory.getLogger("KafkaEventProducer")
  /**
    * Properties for Kafka connection
    */
  private lazy val kafkaProducerProperties = {
    val props = new Properties()
    props.put("bootstrap.servers", kafkaConfig.kafkaBootstrapServers.mkString(",")) // Server address and port
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
    logger.info(s"Initializing Kafka producer for ${kafkaConfig.kafkaBootstrapServers.mkString(",")}")
    new KafkaProducer[String, String](kafkaProducerProperties)
  }

  /**
    * Events expected
    * @return
    */
  override def receive: Receive = {
    case fhirEvent: FhirDataEvent =>
      if(fhirEvent.rtype == "Subscription" && OnfhirConfig.fhirSubscriptionActive)
        handleSubscription(fhirEvent)
      else
        sendString(kafkaConfig.kafkaTopic, fhirEvent.getTopicKey(),  InternalJsonMarshallers.serializeToJson(fhirEvent))
  }

  /**
   * If resource is a subscription, we also push kafka a special stream of subscription events
   * @param event
   * @return
   */
  def handleSubscription(event:FhirDataEvent) = {
       event match {
         case rc:ResourceCreated =>
           val parsedSubscription = new SubscriptionUtil(fhirConfig).parseFhirSubscription(rc.resource)
           sendString(kafkaConfig.kafkaSubscriptionTopic, rc.rid, InternalJsonMarshallers.serializeToJson(parsedSubscription))
         case ru:ResourceUpdated =>
           val parsedSubscription = new SubscriptionUtil(fhirConfig).parseFhirSubscription(ru.resource)
           sendString(kafkaConfig.kafkaSubscriptionTopic, ru.rid, InternalJsonMarshallers.serializeToJson(parsedSubscription))
         case rd:ResourceDeleted =>
           sendString(kafkaConfig.kafkaSubscriptionTopic, rd.rid, "")
       }
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

  /**
   * Generate the actor
   * @param fhirConfig
   * @return
   */
  def props(fhirConfig:FhirServerConfig) = Props(new KafkaEventProducer(kafkaConfig, fhirConfig))
}
package io.onfhir.subscription

package object model {

  /**
   * FHIR Subscription channel type
   */
  object ChannelTypes {
    val WebSocket = "websocket"
    val RestHook = "rest-hook"
    val Email = "email"
    val Sms = "sms"
    val Message = "message"
  }

  object InternalKafkaTopics {
    val NotificationTopic = "onfhir.notification"
  }

  //Consumer Group ids for each kafka stream
  object ConsumerGroupIds {
    val kafkaConsumerGroupIdForResources = "onfhir-subscription-resource-consumer"
    val kafkaConsumerGroupIdForSubscriptions = "onfhir-subscription-subscription-consumer"
    val kafkaConsumerGroupIdForNotifications = "onfhir-subscription-notification-consumer"
  }

  object SubscriptionStatusCodes {
    val error = "error"
    val active = "active"
    val requested = "requested"
    val off = "off"
  }
}

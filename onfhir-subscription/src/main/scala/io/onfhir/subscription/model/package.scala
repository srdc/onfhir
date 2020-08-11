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
    val SubscriptionTopic = "fhir.subscription"
  }

}

package io.onfhir.subscription

package object model {



  object InternalKafkaTopics {
    val NotificationTopic = "onfhir.notification"
  }

  //Consumer Group ids for each kafka stream
  object ConsumerGroupIds {
    val kafkaConsumerGroupIdForResources = "onfhir-subscription-resource-consumer"
    val kafkaConsumerGroupIdForSubscriptions = "onfhir-subscription-subscription-consumer"
    val kafkaConsumerGroupIdForNotifications = "onfhir-subscription-notification-consumer"
  }


}

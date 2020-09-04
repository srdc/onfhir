package io.onfhir.subscription

package object model {

  object ClusterRoles {
    /**
     * When starting the cluster from scratch, role that should be assigned to the first node to initialize with existing active subscriptions in onFHIR.io repository database
     */
    val Initializer = "initializer"
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


}

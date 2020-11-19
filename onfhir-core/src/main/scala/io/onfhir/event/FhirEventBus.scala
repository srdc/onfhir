package io.onfhir.event

import akka.actor.ActorRef
import io.onfhir.api.util.ResourceChecker

/**
  * FHIR Event Bus to handle event subscription for actors
  */
object FhirEventBus extends IFhirEventBus {
  /**
    * For ordering subscriptions
    * @param a
    * @param b
    * @return
    */
  override protected def compareClassifiers(a: FhirEventSubscription, b: FhirEventSubscription): Int = a.compare(b)

  /**
    * For ordering subscribers
    * @param a
    * @param b
    * @return
    */
  override protected def compareSubscribers(a: ActorRef, b: ActorRef): Int = a.compareTo(b)


  /**
    * Check if an event matches to the subscription
    * @param classifier Fhir event subscription
    * @param event Fhir event
    * @return
    */
  override protected def matches(classifier: FhirEventSubscription, event: FhirDataEvent): Boolean = {
    classifier.eventType.isAssignableFrom(event.getClass) && // the event type should be matched
      classifier.rtype.forall( _.contains(event.rtype)) && //If subscription is on a resource type, it should be matched
        classifier.rid.forall(_ == event.rid) &&  //If subscription is on a specific resource instance, it should be matched
          classifier.query.forall(parameters => //If there is a query given, it should match
            event match {
              case rc: ResourceCreated => ResourceChecker.checkIfResourceSatisfies(event.rtype, parameters, rc.resource)
              case ru: ResourceUpdated => ResourceChecker.checkIfResourceSatisfies(event.rtype, parameters, ru.resource)
              case rd: ResourceDeleted => true
            }
          )
  }

  /**
    * Send the matching event to subscriber
    * @param event
    * @param subscriber
    */
  override protected def publish(event: FhirDataEvent, subscriber: ActorRef): Unit = subscriber ! event
}

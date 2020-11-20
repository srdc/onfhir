package io.onfhir.event

import akka.actor.ActorRef
import akka.event.{EventBus, ScanningClassification}

trait IFhirEventBus  extends EventBus with ScanningClassification {
  override type Event = FhirDataEvent
  override type Classifier = FhirEventSubscription
  override type Subscriber = ActorRef
}

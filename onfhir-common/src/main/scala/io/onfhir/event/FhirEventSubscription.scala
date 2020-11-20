package io.onfhir.event

import io.onfhir.api.model.Parameter

/**
  * Subscription class for FHIR events for OnFhir
  *
  * @param eventType  Class of Event subscribed
  * @param rtype      Resource types that are interested
  * @param rid        Resource id that is interested
  * @param query      FHIR query parameters indicating the resources interested
  */
case class FhirEventSubscription(eventType: Class[FhirDataEvent] = classOf[FhirDataEvent], rtype:Option[Seq[String]] = None, rid:Option[String] = None, query:Option[List[Parameter]] = None) extends Ordered[FhirEventSubscription]  {
  override def compare(that: FhirEventSubscription): Int = -1
}

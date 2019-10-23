package io.onfhir.event

import io.onfhir.api.Resource

abstract class FhirEvent {
  val rtype:String
  val rid:String

  def getTopicKey():String = rtype.toLowerCase + ":" + rid
}

/**
  * Event for a FHIR Resource created
  * @param rtype    Resource type
  * @param rid      Resource id
  * @param resource Resource content
  */
case class ResourceCreated(rtype:String, rid:String, resource:Resource) extends FhirEvent

/**
  * Event for a FHIR Resource updated
  * @param rtype    Resource type
  * @param rid      Resource id
  * @param resource Resource content
  */
case class ResourceUpdated(rtype:String, rid:String, resource:Resource) extends FhirEvent

/**
  * Event for a FHIR Resource deleted
  * @param rtype    Resource type
  * @param rid      Resource id
  */
case class ResourceDeleted(rtype:String, rid:String) extends FhirEvent

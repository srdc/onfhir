package io.onfhir.event

import io.onfhir.api.Resource
import io.onfhir.api.model.InternalEntity

object FhirEventUtil {

  def getTopicKey(rtype:String, rid:String):String = rtype + ":" + rid

  def parseTopicKey(topic:String):(String, String) = {
    val parts = topic.split(':')
    parts.head -> parts.last
  }
}

abstract class FhirDataEvent extends InternalEntity {
  val rtype:String
  val rid:String

  def getTopicKey():String = FhirEventUtil.getTopicKey(rtype, rid)

  override def toString():String = s"$getEvent[$rtype/$rid]"
  /**
   * Get event name
   * @return
   */
  def getEvent:String

  /**
   * Get the main resource content
   * @return
   */
  def getResource:Resource
}

/**
  * Event for a FHIR Resource created
  * @param rtype    Resource type
  * @param rid      Resource id
  * @param resource Resource content
  */
case class ResourceCreated(rtype:String, rid:String, resource:Resource) extends FhirDataEvent {
  override def getEvent: String = "data-added"

  override def getResource: Resource = resource
}

/**
  * Event for a FHIR Resource updated
  * @param rtype    Resource type
  * @param rid      Resource id
  * @param resource Resource content (updated)
  * @param previous Previous resource content (before update)
  */
case class ResourceUpdated(rtype:String, rid:String, resource:Resource, previous:Resource) extends FhirDataEvent {
  override def getEvent: String = "data-modified"

  override def getResource: Resource = resource
}

/**
  * Event for a FHIR Resource deleted
  * @param rtype    Resource type
  * @param rid      Resource id
  * @param previous Previous resource content (before delete)
  */
case class ResourceDeleted(rtype:String, rid:String, previous:Resource) extends FhirDataEvent {
  override def getEvent: String = "data-removed"

  override def getResource: Resource = previous
}

/**
 * Event triggered when a FHIR Resource accessed
 * @param rtype
 * @param rid
 * @param resource
 */
case class ResourceAccessed(rtype:String, rid:String, resource: Resource) extends FhirDataEvent {
  override def getEvent: String = "data-accessed"

  override def getResource: Resource = resource
}
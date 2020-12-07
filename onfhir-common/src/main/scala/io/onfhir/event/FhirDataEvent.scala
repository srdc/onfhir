package io.onfhir.event

import io.onfhir.api.Resource
import io.onfhir.api.model.InternalEntity
import org.json4s.{JNothing, JValue}

object FhirEventUtil {

  def getTopicKey(rtype:String, rid:String):String = rtype + ":" + rid

  def parseTopicKey(topic:String):(String, String) = {
    val parts = topic.split(':')
    parts.head -> parts.last
  }
}

trait IFhirEvent extends InternalEntity {
  def getContent:JValue = JNothing
  def getContextParams:Map[String, JValue] = Map.empty
}

abstract class FhirDataEvent extends IFhirEvent {
  val rtype:String
  val rid:String

  def getTopicKey():String = FhirEventUtil.getTopicKey(rtype, rid)

  override def toString():String = s"$getEvent[$rtype/$rid]"
  /**
   * Get event name
   * @return
   */
  def getEvent:String

}

/**
  * Event for a FHIR Resource created
  * @param rtype    Resource type
  * @param rid      Resource id
  * @param resource Resource content
  */
case class ResourceCreated(rtype:String, rid:String, resource:Resource) extends FhirDataEvent {
  override def getEvent: String = "data-added"

  override def getContent: JValue = resource
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

  override def getContent: JValue = resource

  override def getContextParams:Map[String, JValue] = Map("previous" -> previous)
}

/**
  * Event for a FHIR Resource deleted
  * @param rtype    Resource type
  * @param rid      Resource id
  * @param previous Previous resource content (before delete)
  */
case class ResourceDeleted(rtype:String, rid:String, previous:Resource) extends FhirDataEvent {
  override def getEvent: String = "data-removed"

  override def getContextParams:Map[String, JValue] = Map("previous" -> previous)
}

/**
 * Event triggered when a FHIR Resource accessed
 * @param rtype
 * @param rid
 * @param resource
 */
case class ResourceAccessed(rtype:String, rid:String, resource: Resource) extends FhirDataEvent {
  override def getEvent: String = "data-accessed"

  override def getContent: JValue = resource
}
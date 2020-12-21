package io.onfhir.event

import java.time.{Instant, ZonedDateTime}

import io.onfhir.api.Resource
import io.onfhir.api.model.InternalEntity
import io.onfhir.util.DateTimeUtil
import org.json4s.{JNothing, JObject, JString, JValue}

object FhirEventUtil {

  def getTopicKey(rtype:String, rid:String):String = rtype + ":" + rid

  def parseTopicKey(topic:String):(String, String) = {
    val parts = topic.split(':')
    parts.head -> parts.last
  }
}

/**
 * An event in OnFhir ecosystem
 */
trait IFhirEvent extends InternalEntity {
  /**
   * Get the JSON content of the event
   * @return
   */
  def getContent:JValue = JNothing

  /**
   * Get context parameters about the event
   * @return
   */
  def getContextParams:Map[String, JValue] = Map.empty
}

/**
 * A named event in onFhir ecosystem
 * @param eventName Name of the event
 * @param event     Event itself
 */
case class FhirNamedEvent(eventName:String, event:IFhirEvent) extends IFhirEvent {
  override def getContent: JValue = event.getContent

  override def getContextParams: Map[String, JValue] = event.getContextParams + ("eventName" -> JString(eventName))
}

/**
 * Event used for time based triggers
 * @param scheduledTime   Actual scheduled time for this trigger
 * @param context         Attached context to the trigger
 */
case class FhirTimeEvent(scheduledTime:Instant, context:Map[String, String] = Map.empty[String, String]) extends IFhirEvent {
  override def getContent: JValue = JNothing
  override def getContextParams: Map[String, JValue] = {
    Map("eventScheduledTime" -> JString(DateTimeUtil.serializeInstant(scheduledTime))) ++ context.mapValues(JString(_))
  }
}

/**
 * Base class for Data related event
 */
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
 * @param rtype     Resource type
 * @param rid       Resource id
 * @param resource  Resource content accessed
 */
case class ResourceAccessed(rtype:String, rid:String, resource: Resource) extends FhirDataEvent {
  override def getEvent: String = "data-accessed"

  override def getContent: JValue = resource
}
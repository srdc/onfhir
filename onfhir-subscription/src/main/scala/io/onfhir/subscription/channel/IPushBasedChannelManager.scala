package io.onfhir.subscription.channel

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import io.onfhir.subscription.FhirNotificationHandler
import io.onfhir.subscription.model.FhirNotification

trait Command

/**
 * Command to send the notification
 * @param fhirNotification
 */
case class SendNotification(fhirNotification:FhirNotification, replyTo:ActorRef[FhirNotificationHandler.Command]) extends Command



trait IPushBasedChannelManager {

  def apply(implicit actorSystem:ActorSystem[_]):Behavior[Command]
}

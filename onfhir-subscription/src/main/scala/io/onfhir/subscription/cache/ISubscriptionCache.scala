package io.onfhir.subscription.cache

import akka.actor.typed.{ActorRef, Behavior}
import io.onfhir.config.SearchParameterConf
import io.onfhir.subscription.model.{CriteriaSubscriptions, FhirSubscription}


trait Command
/**
 * Return all subscription dimensions in priority order for the resource type
 * e.g. for Observation we can keep patient specific subscriptions separately (dimensions can only be defined on reference type search parameters)
 * @param rtype Resource type
 * @return
 */
case class GetSubscriptionDimensions(rtype:String, replyTo:ActorRef[Seq[SearchParameterConf]]) extends Command

/**
 * Return all criteria subscriptions for a resource type that is not specific to a dimension or given dimension e.g. subscriptions to Observation for a specific patient
 * @param rtype   FHIR resource type e.g. Observation
 * @param dimensionParamAndValue  Parameter name for dimension e.g. patient - Value of the reference that indicates the dimension e.g. patient identifier Patient/45413
 * @return
 */
case class GetCriteriaSubscriptions(rtype:String, dimensionParamAndValue:Option[(String, String)] = None, replyTo:ActorRef[Seq[CriteriaSubscriptions]]) extends Command

/**
 * Add or update the subscription details in cache based on incoming FHIR Subscription resource
 *
 * @param fhirSubscription parsed FHIR Subscription resource
 * @return
 */
case class AddOrUpdateSubscription(fhirSubscription: FhirSubscription, replyTo:ActorRef[Boolean]) extends Command

/**
 * Return subscription details by id
 * @param id  Subscription id
 * @return
 */
case class GetSubscription(id:String, replyTo:ActorRef[Option[FhirSubscription]]) extends Command

/**
 * Activate the subscription in cache
 * @param id  Subscription id
 * @return
 */
case class ActivateSubscription(id:String, replyTo:ActorRef[Boolean]) extends Command

/**
 * Deactivate the subscription in cache (remove from criteria subscriptions, and set status = 0)
 * @param id Subscription id
 * @return
 */
case class DeactivateSubscription(id:String, replyTo:ActorRef[Boolean]) extends Command

/**
 * Delete all the subscription details
 * @param id Subscription id
 * @return
 */
case class RemoveSubscription(id:String, replyTo:ActorRef[Boolean]) extends Command

/**
 * Subscription cache behaviour
 */
trait ISubscriptionCacheBehavior extends Behavior[Command]

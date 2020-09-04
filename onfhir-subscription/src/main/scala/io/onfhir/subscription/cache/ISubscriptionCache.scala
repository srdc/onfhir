package io.onfhir.subscription.cache

import akka.actor.typed.{ActorRef, Behavior}
import io.onfhir.api.model.FhirSubscription
import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.subscription.model.{CborSerializable, CriteriaSubscriptions, DDFhirSubscription}

trait ISubscriptionCache {
  // Return cache behavior
  def apply(subscriptionConfig: SubscriptionConfig, fhirSearchParameterCache: FhirSearchParameterCache): Behavior[Command]
}

trait Command extends CborSerializable

/**
 * Response
 */
trait Response
case class GetCriteriaSubscriptionsResponse(criteriaSubscriptions: Seq[CriteriaSubscriptions]) extends Response
case class GetSubscriptionResponse(subscription:Option[DDFhirSubscription]) extends Response
case class UpdateSubscriptionResponse(result:Boolean) extends Response

/**
 * Return all criteria subscriptions for a resource type that is not specific to a dimension or given dimension e.g. subscriptions to Observation for a specific patient
 * @param rtype   FHIR resource type e.g. Observation
 * @param dimensionParamAndValue  Parameter name for dimension e.g. patient - Value of the reference that indicates the dimension e.g. patient identifier Patient/45413
 * @return
 */
case class GetCriteriaSubscriptions(rtype:String, dimensionParamAndValue:Option[(String, String)] = None, replyTo:ActorRef[Response]) extends Command

/**
 * Add or update the subscription details in cache based on incoming FHIR Subscription resource
 *
 * @param fhirSubscription parsed FHIR Subscription resource
 * @return
 */
case class AddOrUpdateSubscription(fhirSubscription: FhirSubscription, replyTo:ActorRef[Response]) extends Command

/**
 * Return subscription details by id
 * @param id  Subscription id
 * @return
 */
case class GetSubscription(id:String, replyTo:ActorRef[Response]) extends Command

/**
 * Activate the subscription in cache
 * @param id  Subscription id
 * @return
 */
case class ActivateSubscription(id:String, replyTo:ActorRef[Response]) extends Command

/**
 * Deactivate the subscription in cache (remove from criteria subscriptions, and set status = 0)
 * @param id Subscription id
 * @return
 */
case class DeactivateSubscription(id:String, replyTo:ActorRef[Response]) extends Command

/**
 * Set the status of subscription
 * @param id
 * @param status
 * @param replyTo
 */
case class SetSubscriptionStatus(id:String, status:Int, replyTo:ActorRef[Response]) extends Command

/**
 * Delete all the subscription details
 * @param id Subscription id
 * @return
 */
case class RemoveSubscription(id:String, replyTo:ActorRef[Response]) extends Command

/**
 * Subscription cache behaviour
 */
trait ISubscriptionCacheBehavior extends Behavior[Command]

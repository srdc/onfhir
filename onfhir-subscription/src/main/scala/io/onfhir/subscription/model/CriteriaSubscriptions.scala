package io.onfhir.subscription.model

import io.onfhir.api.model.Parameter
import io.onfhir.subscription.util.SubscriptionUtil

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
 * Active subscription ids for same criteria for a resource type
 * @param resourceType      Resource type subscribed
 * @param criteria          Parsed search parameters
 * @param subscriptionIds   Set of subscription ids that subscribe to this same criteria
 */
case class CriteriaSubscriptions(resourceType:String, criteria:Seq[Parameter], subscriptionIds:mutable.Set[String]) {
  /**
   * Return a hash string for the criteria to find two queries are same
   * @return
   */
  def getCriteriaHash = SubscriptionUtil.getCriteriaHash(resourceType, criteria)
}

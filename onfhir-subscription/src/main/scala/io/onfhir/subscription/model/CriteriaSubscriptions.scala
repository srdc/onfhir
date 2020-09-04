package io.onfhir.subscription.model

import akka.cluster.ddata.{DeltaReplicatedData, ORSet, ReplicatedData}
import io.onfhir.api.model.Parameter
import io.onfhir.subscription.util.SubscriptionUtil

/**
 * Active subscription ids for same criteria for a resource type
 * @param resourceType      Resource type subscribed
 * @param criteria          Parsed search parameters
 * @param subscriptionIds   Set of subscription ids that subscribe to this same criteria
 */
case class CriteriaSubscriptions(resourceType:String, criteria:Seq[Parameter], subscriptionIds:ORSet[String]) extends DeltaReplicatedData with ProtoSerializable {
  /**
   * Return a hash string for the criteria to find two queries are same
   * @return
   */
  def getCriteriaHash = SubscriptionUtil.getCriteriaHash(resourceType, criteria)

  override type T = CriteriaSubscriptions

  override def merge(that: CriteriaSubscriptions): CriteriaSubscriptions = {
    copy(subscriptionIds = this.subscriptionIds.merge(that.subscriptionIds))
  }

  override type D = ORSet.DeltaOp

  override def delta: Option[ORSet.DeltaOp] = subscriptionIds.delta

  override def mergeDelta(thatDelta: ORSet.DeltaOp): CriteriaSubscriptions = this.copy(subscriptionIds = subscriptionIds.mergeDelta(thatDelta))

  override def resetDelta: CriteriaSubscriptions = this.copy(subscriptionIds = subscriptionIds.resetDelta)
}

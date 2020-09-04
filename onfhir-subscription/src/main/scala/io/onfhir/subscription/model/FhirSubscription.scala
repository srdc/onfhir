package io.onfhir.subscription.model

import akka.cluster.ddata.{DeltaReplicatedData, PNCounter, ReplicatedData}
import io.onfhir.api.model.{FhirSubscriptionChannel, Parameter}
import io.onfhir.subscription.util.SubscriptionUtil

/**
 * Replicated data object to keep FHIR Subscription data in Akka Distributed data
 * @param id
 * @param rtype
 * @param channel
 * @param criteria
 * @param status
 * @param expiration
 */
case class DDFhirSubscription(id:String, rtype:String, channel:FhirSubscriptionChannel, criteria:Seq[Parameter] = Nil, status:PNCounter, expiration:Option[String]) extends DeltaReplicatedData with ProtoSerializable {
  override type T = DDFhirSubscription

  override def merge(that: DDFhirSubscription): DDFhirSubscription = {
    copy(status = status.merge(that.status))
  }

  override type D = PNCounter

  override def delta: Option[PNCounter] = status.delta

  override def mergeDelta(thatDelta: PNCounter):DDFhirSubscription = this.copy(status = status.mergeDelta(thatDelta))

  override def resetDelta: DDFhirSubscription = this.copy(status= status.resetDelta)

  def getCriteriaHash = SubscriptionUtil.getCriteriaHash(rtype, criteria)
}
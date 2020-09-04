package io.onfhir.subscription.model

import akka.actor.ExtendedActorSystem

class CriteriaSubscriptionsProtoSerializer(system: ExtendedActorSystem)  extends SubscriptionMessagesSerializer(system) {
  override def identifier = 99998
  override def toBinary(obj: AnyRef): Array[Byte] = obj match {
    case c:CriteriaSubscriptions => criteriaSubscriptionsToProto(c).toByteArray
    case _              => throw new IllegalArgumentException(s"Can't serialize object of type ${obj.getClass}")
  }

  override def fromBinary(bytes: Array[Byte], clazz: Option[Class[_]]): AnyRef = {
    criteriaSubscriptionsFromBinary(bytes)
  }
}

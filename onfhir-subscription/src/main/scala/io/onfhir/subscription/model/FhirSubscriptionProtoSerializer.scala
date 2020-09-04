package io.onfhir.subscription.model

import akka.actor.ExtendedActorSystem

class FhirSubscriptionProtoSerializer(system: ExtendedActorSystem)  extends SubscriptionMessagesSerializer(system) {
  override def identifier = 99999
  override def toBinary(obj: AnyRef): Array[Byte] = obj match {
    case m: DDFhirSubscription => fhirSubscriptionToProto(m).toByteArray
    case _              => throw new IllegalArgumentException(s"Can't serialize object of type ${obj.getClass}")
  }

  override def fromBinary(bytes: Array[Byte], clazz: Option[Class[_]]): AnyRef = {
    fhirSubscriptionFromBinary(bytes)
  }
}

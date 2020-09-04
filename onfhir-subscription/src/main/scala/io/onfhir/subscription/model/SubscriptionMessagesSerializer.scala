package io.onfhir.subscription.model

import akka.actor.ExtendedActorSystem
import akka.cluster.ddata.protobuf.ReplicatedDataSerializer
import io.onfhir.api.model.{FhirSubscriptionChannel, Parameter}
import akka.cluster.ddata.{GSet, ORSet, PNCounter}
import akka.cluster.ddata.protobuf.SerializationSupport
import akka.serialization.Serializer
import com.google.protobuf.ByteString
import io.onfhir.api.model
import io.onfhir.subscription.model.msg.SubscriptionMessages.FhirSubscriptionChannel

import collection.JavaConverters._
abstract class SubscriptionMessagesSerializer(val system: ExtendedActorSystem) extends Serializer with SerializationSupport {

    override def includeManifest: Boolean = false

    val replicatedDataSerializer = new ReplicatedDataSerializer(system)

    override def toBinary(obj: AnyRef): Array[Byte] = obj match {
      case m: DDFhirSubscription => fhirSubscriptionToProto(m).toByteArray
      case c:CriteriaSubscriptions => criteriaSubscriptionsToProto(c).toByteArray
      case _              => throw new IllegalArgumentException(s"Can't serialize object of type ${obj.getClass}")
    }

    override def fromBinary(bytes: Array[Byte], clazz: Option[Class[_]]): AnyRef = {

      clazz match {
        case Some(fs) if fs == classOf[DDFhirSubscription] =>
          fhirSubscriptionFromBinary(bytes)
        case Some(c) if c == classOf[CriteriaSubscriptions] =>
          criteriaSubscriptionsFromBinary(bytes)
        case _ =>
          throw new IllegalArgumentException(s"Can't deserialize object of type ${clazz}")
      }
    }

    def tupleToProto(t:(String, String)):io.onfhir.subscription.model.msg.SubscriptionMessages.Tuple = {
      val b = io.onfhir.subscription.model.msg.SubscriptionMessages.Tuple.newBuilder()
      b.setItem1(t._1)
      b.setItem2(t._2)
      b.build()
    }

    def parameterToProto(p:Parameter):io.onfhir.subscription.model.msg.SubscriptionMessages.Parameter = {
      val b = io.onfhir.subscription.model.msg.SubscriptionMessages.Parameter.newBuilder()
      b.setParamCategory(p.paramCategory)
      b.setParamType(p.paramType)
      b.setName(p.name)
      p.valuePrefixList.foreach(v => b.addValuePrefixList(tupleToProto(v)))
      b.setSuffix(p.suffix)
      p.chain.foreach(c => b.addChain(tupleToProto(c)))
      b.build()
    }

    def fhirSubscriptionToProto(fs:DDFhirSubscription): io.onfhir.subscription.model.msg.SubscriptionMessages.DDFhirSubscription = {
      val cb = io.onfhir.subscription.model.msg.SubscriptionMessages.FhirSubscriptionChannel.newBuilder()
      cb.setChannelType(fs.channel.channelType)
      fs.channel.endpoint.foreach(e => cb.setEndpoint(e))
      fs.channel.headers.foreach(h => cb.addHeaders(h))
      fs.channel.payload.foreach(p => cb.setPayload(p))

      val b =  io.onfhir.subscription.model.msg.SubscriptionMessages.DDFhirSubscription.newBuilder()
      b.setId(fs.id)
      b.setRtype(fs.rtype)
      b.setChannel(cb.build())
      fs.criteria.foreach(c => b.addCriteria(parameterToProto(c)))
      b.setStatus(ByteString.copyFrom(otherMessageToProto(fs.status).toByteArray))
      fs.expiration.foreach(e => b.setExpiration(e))

      b.build()
    }

  def criteriaSubscriptionsToProto(c:CriteriaSubscriptions): io.onfhir.subscription.model.msg.SubscriptionMessages.CriteriaSubscriptions = {
    val b =  io.onfhir.subscription.model.msg.SubscriptionMessages.CriteriaSubscriptions.newBuilder()
    b.setResourceType(c.resourceType)
    c.criteria.foreach(c=> b.addCriteria(parameterToProto(c)))
    b.setSubscriptionIds(ByteString.copyFrom(otherMessageToProto(c.subscriptionIds).toByteArray))
    b.build()
  }

    def fhirSubscriptionFromBinary(bytes: Array[Byte]): DDFhirSubscription = {
      val msg = io.onfhir.subscription.model.msg.SubscriptionMessages.DDFhirSubscription.parseFrom(bytes)
      DDFhirSubscription(
        id = msg.getId,
        rtype = msg.getRtype,
        channel = prtotoToFhirSubscriptionChannel(msg.getChannel),
        criteria = msg.getCriteriaList.asScala.map(c => protoToParameter(c)),
        status = otherMessageFromBinary(msg.getStatus.toByteArray).asInstanceOf[PNCounter],
        expiration = if(msg.hasExpiration) Some(msg.getExpiration) else None
      )
    }

  def criteriaSubscriptionsFromBinary(bytes: Array[Byte]): CriteriaSubscriptions = {
    val msg = io.onfhir.subscription.model.msg.SubscriptionMessages.CriteriaSubscriptions.parseFrom(bytes)
    CriteriaSubscriptions(
      resourceType = msg.getResourceType,
      criteria= msg.getCriteriaList.asScala.map(c => protoToParameter(c)),
      subscriptionIds = otherMessageFromBinary(msg.getSubscriptionIds.toByteArray).asInstanceOf[ORSet[String]]
    )
  }

  def prtotoToFhirSubscriptionChannel(proto: io.onfhir.subscription.model.msg.SubscriptionMessages.FhirSubscriptionChannel):model.FhirSubscriptionChannel = {
    model.FhirSubscriptionChannel(
      channelType = proto.getChannelType,
      endpoint = if(proto.hasEndpoint) Some(proto.getEndpoint) else None,
      payload = if(proto.hasPayload) Some(proto.getPayload) else None,
      headers = (0 until  proto.getHeadersCount).map(i => proto.getHeaders(i))
    )
  }

  def protoToParameter(proto:io.onfhir.subscription.model.msg.SubscriptionMessages.Parameter):model.Parameter = {
    model.Parameter(
      paramCategory = proto.getParamCategory,
      paramType = proto.getParamType,
      name = proto.getName,
      valuePrefixList = proto.getValuePrefixListList.asScala.map(v => protoToTuple(v)),
      suffix = proto.getSuffix,
      chain = proto.getChainList.asScala.map(c => protoToTuple(c))
    )
  }

  def protoToTuple(proto:io.onfhir.subscription.model.msg.SubscriptionMessages.Tuple):(String, String) = {
    proto.getItem1 -> proto.getItem2
  }


}

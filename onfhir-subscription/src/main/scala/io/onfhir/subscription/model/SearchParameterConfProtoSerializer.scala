package io.onfhir.subscription.model

import akka.actor.ExtendedActorSystem
import io.onfhir.config.SearchParameterConf
import io.onfhir.subscription.model.msg.SubscriptionMessages

import collection.JavaConverters._
class SearchParameterConfProtoSerializer(system: ExtendedActorSystem) extends SubscriptionMessagesSerializer(system) {
  override def identifier = 99997
  override def toBinary(obj: AnyRef): Array[Byte] = obj match {
    case c:SearchParameterConfWrapper => searchParameterConfToProto(c.sp).toByteArray
    case _              => throw new IllegalArgumentException(s"Can't serialize object of type ${obj.getClass}")
  }

  override def fromBinary(bytes: Array[Byte], clazz: Option[Class[_]]): AnyRef = {
    SearchParameterConfWrapper(searchParameterConfFromBinary(bytes))
  }

  def searchParameterConfFromBinary(bytes: Array[Byte]):SearchParameterConf = {
    val msg = io.onfhir.subscription.model.msg.SubscriptionMessages.SearchParameterConf.parseFrom(bytes)

    SearchParameterConf(
      pname = msg.getPname,
      ptype = msg.getPtype,
      paths =  msg.getPathsList.listIterator().asScala.toSeq,
      targets = msg.getTargetsList.listIterator().asScala.toSeq,
      modifiers = msg.getModifiersList.listIterator().asScala.toSet,
      targetTypes = msg.getTargetTypesList.listIterator().asScala.toSeq,
      restrictions = msg.getRestrictionsList.asScala.map(r => restrictionsFromProto(r)),
      multipleOr = msg.getMultipleOr,
      multipleAnd = msg.getMultiAnd,
      comparators = msg.getComparatorsList.listIterator().asScala.toSet
    )
  }



  def searchParameterConfToProto(spc:SearchParameterConf):SubscriptionMessages.SearchParameterConf = {
    val b = io.onfhir.subscription.model.msg.SubscriptionMessages.SearchParameterConf.newBuilder()
    b.setPname(spc.pname)
    b.setPtype(spc.ptype)
    spc.paths.foreach(p => b.addPaths(p))
    spc.targets.foreach(t => b.addTargets(t))
    spc.modifiers.foreach(m => b.addModifiers(m))
    spc.targetTypes.foreach(t => b.addTargetTypes(t))
    spc.restrictions.foreach(r => b.addRestrictions(restrictionSeqToProto(r)))
    b.setMultipleOr(spc.multipleOr)
    b.setMultiAnd(spc.multipleAnd)
    spc.comparators.foreach(c => b.addComparators(c))
    b.build()
  }

  def restrictionSeqToProto(restrictions:Seq[(String, String)]):SubscriptionMessages.Restrictions = {
    val b = io.onfhir.subscription.model.msg.SubscriptionMessages.Restrictions.newBuilder()
   /* if(restrictions.isEmpty)
      b.addValues(tupleToProto("" -> ""))
    else*/
      restrictions.foreach(r => b.addValues(tupleToProto(r)))
    b.build()
  }

  def restrictionsFromProto(rs: SubscriptionMessages.Restrictions):Seq[(String, String)] = {
    rs.getValuesList.asScala.map(v => v.getItem1 -> v.getItem2)
    /* rs.getValuesList.asScala.toSeq match {
      case Seq(t) if t.getItem1 == "" && t.getItem2 == "" => Nil
      case oth => oth.map(v => v.getItem1 -> v.getItem2)
    }*/
  }

}


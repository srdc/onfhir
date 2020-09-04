package io.onfhir.subscription.model

import akka.cluster.ddata.ReplicatedData
import io.onfhir.config.SearchParameterConf

case class SearchParameterConfWrapper(sp:SearchParameterConf) extends ReplicatedData with ProtoSerializable {
  override type T = SearchParameterConfWrapper

  override def merge(that: SearchParameterConfWrapper): SearchParameterConfWrapper = that
}

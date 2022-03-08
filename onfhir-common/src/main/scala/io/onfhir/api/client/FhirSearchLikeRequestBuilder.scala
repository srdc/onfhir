package io.onfhir.api.client

import io.onfhir.api.model.FHIRRequest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class FhirSearchLikeRequestBuilder(onFhirClient: IOnFhirClient, request: FHIRRequest) extends FhirRequestBuilder(onFhirClient, request) {

  protected val conditionalParams:mutable.ListBuffer[(String, Seq[String])] = new ListBuffer[(String, Seq[String])]

  def where(parsedQuery:Map[String, List[String]]):This = {
    val normalizedParams=
      parsedQuery
        .flatMap(q => q._2.map(pv => q._1 -> pv))
        .map(q => q._1 -> q._2.split(',').toSeq)
        .toSeq

    normalizedParams.foreach(np => conditionalParams.append(np._1 -> np._2))
    this.asInstanceOf[This]
  }

  def where(param:String, value:String*):This = {
    conditionalParams.append(param-> value)
    this.asInstanceOf[This]
  }

  override protected def compile():Unit = {
    if(conditionalParams.nonEmpty)
      request.queryParams =
        conditionalParams
          .groupMap(_._1)(_._2.mkString(","))
          .map(g => g._1 -> g._2.toList)

  }
}

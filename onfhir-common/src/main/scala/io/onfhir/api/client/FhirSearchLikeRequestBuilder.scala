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
  /*
    private def addSearchParams(fhirSearchRequestBuilder: FhirSearchRequestBuilder, finalQueryParams:Map[String, List[String]]):FhirSearchRequestBuilder = {
      val normalizedParams=
        finalQueryParams
          .flatMap(q => q._2.map(pv => q._1 -> pv))
          .map(q => q._1 -> q._2.split(',').toSeq)
          .toSeq

      val resultingSearch =
        if(normalizedParams.nonEmpty)
          normalizedParams.foldLeft(fhirSearchRequestBuilder)((rb, p) => rb.where(p._1, p._2:_*))
        else
          fhirSearchRequestBuilder

      resultingSearch
    }*/

  def where(param:String, value:String*):This = {
    conditionalParams.append(param-> value.toSeq)
    this.asInstanceOf[This]
  }

  override protected def compile():Unit = {
    if(conditionalParams.nonEmpty)
      request.queryParams = conditionalParams.groupBy(_._1).mapValues(v => v.map(_._2.mkString(",")).toList)
  }
}

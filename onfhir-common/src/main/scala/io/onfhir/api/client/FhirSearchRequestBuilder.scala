package io.onfhir.api.client

import akka.http.scaladsl.model.HttpMethods
import io.onfhir.api.{FHIR_HTTP_OPTIONS, FHIR_INTERACTIONS}
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * Request builder for FHIR search-type interaction
 *
 * @param onFhirClient  OnFhir client instance
 * @param rtype         FHIR resource type
 * @param count         Number of resources to return per page
 */
class FhirSearchRequestBuilder(onFhirClient: IOnFhirClient, rtype: String, count: Option[Int] = None)
  extends FHIRSearchSetReturningRequestBuilder(onFhirClient,
      FHIRRequest(
        interaction = FHIR_INTERACTIONS.SEARCH,
        requestUri = s"${onFhirClient.getBaseUrl()}/$rtype",
        resourceType = Some(rtype))) {
  type This = FhirSearchRequestBuilder
  // Sort parameters where true indicate descending sort
  protected val sortParams: mutable.ListBuffer[(String, Boolean)] = new ListBuffer[(String, Boolean)]
  /**
   * Pagination parameter explicitly set
   */
  var page:Option[(String, String)] = None
  /**
   * Use HTTP Post for the search
   * @return
   */
  def byHttpPost(): FhirSearchRequestBuilder = {
    request.httpMethod = Some(HttpMethods.POST)
    this
  }

  /**
   * Initialize compartment search
   * @param ctype Compartment type e.g. Patient
   * @param cid   Compartment identifier
   * @return
   */
  def forCompartment(ctype: String, cid: String): FhirSearchRequestBuilder = {
    request.compartmentType = Some(ctype)
    request.compartmentId = Some(cid)
    this
  }

  /**
   * Parameter names to sort the result set in descending order
   * @param params  Name of the search parameters
   * @return
   */
  def sortOnDesc(params: String*): FhirSearchRequestBuilder = {
    sortParams.appendAll(params.map(p => p -> true))
    this
  }

  /**
   * Parameter names to sort the result set in ascending order
   * @param params  Name of the search parameters
   * @return
   */
  def sortOnAsc(params: String*): FhirSearchRequestBuilder = {
    sortParams.appendAll(params.map(p => p -> false))
    this
  }

  /**
   * Strict search handling
   * @return
   */
  def strictHandling():FhirSearchRequestBuilder = {
    request.prefer = Some(FHIR_HTTP_OPTIONS.FHIR_SEARCH_STRICT)
    this.asInstanceOf[This]
  }

  /**
   * Lenient search handling
   * @return
   */
  def lenientHandling(): FhirSearchRequestBuilder = {
    request.prefer = Some(FHIR_HTTP_OPTIONS.FHIR_SEARCH_LENIENT)
    this.asInstanceOf[This]
  }

  /**
   * Set pagination parameter for this query
   * @param paramName Pagination parameter name e.g. _page, _searchafter
   * @param page      Pagination parameter value
   * @return
   */
  def setPaginationParam(paramName:String, pageValue:String):FhirSearchRequestBuilder = {
    page = Some(paramName -> pageValue)
    this.asInstanceOf[This]
  }
  def setPaginationParam(paramName: String, pageValue: Int): FhirSearchRequestBuilder = {
    page = Some(paramName, ""+pageValue)
    this.asInstanceOf[This]
  }
  def setPage(pageValue:Int):FhirSearchRequestBuilder = {
    setPaginationParam("_page", pageValue)
  }
  def setSearchAfter(offset:String):FhirSearchRequestBuilder =
    setPaginationParam("_searchafter", offset)
  def setSearchBefore(offset:String):FhirSearchRequestBuilder =
    setPaginationParam("_searchbefore", offset)

  override protected def compile(): Unit = {
    super.compile()
    if (sortParams.nonEmpty)
      request.queryParams = request.queryParams ++ Map("_sort" -> sortParams.map(sp => s"${if (sp._2) "-" else ""}${sp._1}").toList)
    if (page.isDefined)
      request.queryParams = request.queryParams ++ Map(page.get._1 -> List(page.get._2))
    //Also add the compartment as param
    if (count.isDefined)
      request.queryParams = request.queryParams ++ Map("_count" -> List(s"${count.get}"))
  }
}


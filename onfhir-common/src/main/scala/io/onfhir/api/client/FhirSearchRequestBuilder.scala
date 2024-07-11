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
 * @param onFhirClient
 * @param rtype
 * @param count
 */
class FhirSearchRequestBuilder(onFhirClient: IOnFhirClient, rtype: String, count: Option[Int] = None, var page: Option[Long] = None)
  extends FhirSearchLikeRequestBuilder(onFhirClient, FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = s"${onFhirClient.getBaseUrl()}/$rtype", resourceType = Some(rtype)))
    with IFhirBundleReturningRequestBuilder {
  type This = FhirSearchRequestBuilder
  // Sort parameters where true indicate descending sort
  protected val sortParams: mutable.ListBuffer[(String, Boolean)] = new ListBuffer[(String, Boolean)]

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
   * @param paramName Paginatipn parameter name e.g. _page, _searchafter
   * @param page      Pagination parameter value
   * @return
   */
  def setPaginationParam(paramName:String, page:String):FhirSearchRequestBuilder = {
    request.queryParams = request.queryParams + (paramName -> List(page))
    this.asInstanceOf[This]
  }


  override protected def compile(): Unit = {
    super.compile()
    if (sortParams.nonEmpty)
      request.queryParams = request.queryParams ++ Map("_sort" -> sortParams.map(sp => s"${if (sp._2) "-" else ""}${sp._1}").toList)
    if (page.isDefined)
      request.queryParams = request.queryParams ++ Map("_page" -> List(s"${page.get}"))
    //Also add the compartment as param
    if (count.isDefined)
      request.queryParams = request.queryParams ++ Map("_count" -> List(s"${count.get}"))
  }

  /**
   * Send the FHIR search request and return the FHIR Bundle returned in the response parsed as FHIRSearchSetBundle if successfull, otherwise throw FhirClientException
   * @param executionContext  Execution context
   * @return
   */
  @throws[FhirClientException]
  def executeAndReturnBundle()(implicit executionContext: ExecutionContext): Future[FHIRSearchSetBundle] = {
    execute()
      .map(r => {
        if (r.httpStatus.isFailure() || r.responseBody.isEmpty)
          throw FhirClientException("Problem in FHIR search!", Some(r))
        constructBundle(r)
      })
  }

  /**
   * Returns Scala iterator where you can iterate over search results page by page
   * @param executionContext Execution context
   * @return
   */
  def toIterator()(implicit executionContext: ExecutionContext): Iterator[Future[FHIRSearchSetBundle]] = {
    new SearchSetIterator(this)
  }

  /**
   * Send the FHIR request and paginate over the whole result set by retrieving next page until there is no further and merge them into FHIRSearchSetBundle
   * @param executionContext
   * @return
   */
  def executeAndMergeBundle()(implicit executionContext: ExecutionContext): Future[FHIRSearchSetBundle] = {
    getMergedBundle(executeAndReturnBundle())
  }

  /**
   *
   * @param bundle
   * @param ec
   * @return
   */
  private def getMergedBundle(bundle: Future[FHIRSearchSetBundle])(implicit ec: ExecutionContext): Future[FHIRSearchSetBundle] = {
    bundle.flatMap {
      case r if r.hasNext() =>
        getMergedBundle(onFhirClient.next(r))
          .map(r2 =>
            r2.mergeResults(r)
          )
      case r =>
        Future.apply(r)
    }
  }

  /**
   *
   * @param fhirResponse
   * @return
   */
  override def constructBundle(fhirResponse: FHIRResponse): FHIRSearchSetBundle = {
    try {
      new FHIRSearchSetBundle(fhirResponse.responseBody.get, this)
    } catch {
      case e: Throwable =>
        throw FhirClientException("Invalid search result bundle!", Some(fhirResponse))
    }
  }

  /**
   *
   */
  override def nextPage(): Unit = {
    this.page = Some(this.page.getOrElse(1L) + 1)
  }
}

/**
 *
 * @param rb
 * @param executionContext
 */
class SearchSetIterator(rb: FhirSearchRequestBuilder)(implicit executionContext: ExecutionContext) extends Iterator[Future[FHIRSearchSetBundle]] {
  var latestBundle: Option[FHIRSearchSetBundle] = None

  override def hasNext: Boolean = latestBundle.forall(_.hasNext())

  override def next(): Future[FHIRSearchSetBundle] = {
    latestBundle match {
      case None =>
        val temp = rb.executeAndReturnBundle()
        temp onComplete {
          case Success(v) => latestBundle = Some(v)
          case Failure(exception) =>
        }
        temp
      case Some(b) =>
        val temp = rb.onFhirClient.next(b)
        temp onComplete {
          case Success(v) => latestBundle = Some(v)
          case Failure(exception) =>
        }
        temp
    }
  }
}

package io.onfhir.api.client

import io.onfhir.api.model.{FHIRRequest, FHIRResponse}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

abstract class FHIRSearchSetReturningRequestBuilder(onFhirClient: IOnFhirClient, request: FHIRRequest)
  extends FhirSearchLikeRequestBuilder(onFhirClient, request)
    with IFhirBundleReturningRequestBuilder {

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
   * Send the FHIR search request and return the FHIR Bundle returned in the response parsed as FHIRSearchSetBundle if successfull, otherwise throw FhirClientException
   *
   * @param executionContext Execution context
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
   *
   * @param executionContext Execution context
   * @return
   */
  def toIterator()(implicit executionContext: ExecutionContext): Iterator[Future[FHIRSearchSetBundle]] = {
    new SearchSetIterator(this)
  }

  /**
   * Send the FHIR request and paginate over the whole result set by retrieving next page until there is no further and merge them into FHIRSearchSetBundle
   *
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
}


/**
 *
 * @param rb
 * @param executionContext
 */
class SearchSetIterator(rb: FHIRSearchSetReturningRequestBuilder)(implicit executionContext: ExecutionContext) extends Iterator[Future[FHIRSearchSetBundle]] {
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
package io.onfhir.api.client

import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.api.model.FHIRRequest

import scala.concurrent.{ExecutionContext, Future}

/**
 * For requests to load a next/previous page link in a search pagination
 * @param onFhirClient    OnFhir client
 * @param link        Partial/whole link for the search result set page (if partial the remaining part from FHIR server base-url should be given)
 *                    e.g. Test HAPI server example :
 *                        - whole: http://localhost:8080/fhir?_getpages=...&_getpageoffset=10
 *                        - partial: ?_getpages=...&_getpageoffset=10
 *                    e.g. With some specific path
 *                        - whole:  http://localhost:8080/fhir/searchset/44225?_page=3
 *                        - partial: /searchset/44225?_page=3
 */
class FhirGetSearchPageRequestBuilder(onFhirClient: IOnFhirClient, link:String)
  extends FHIRSearchSetReturningRequestBuilder(onFhirClient,
    FHIRRequest(
      interaction = FHIR_INTERACTIONS.GET_SEARCH_PAGE,
      requestUri =
        if(link.startsWith(onFhirClient.getBaseUrl()))
          link.drop(onFhirClient.getBaseUrl().length)
        else if(link.head == '/' || link.head == '?')
          link
        else
          throw new IllegalArgumentException("The link for the search page should be either whole link starting with target FHIR server base url or partial link starting as path '/' or directly with query '?'")
    )) {


  /**
   *
   * @param parsedQuery
   * @return
   */
  override def where(parsedQuery: Map[String, List[String]]): This = {
    throw new IllegalAccessError("Do not use this operation for this builder")
  }

  override def where(param: String, value: String*): This = {
    throw new IllegalAccessError("Do not use this operation for this builder")
  }
  //Do nothing
  override protected def compile():Unit = {

  }
}


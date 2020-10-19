package io.onfhir.client

import akka.http.scaladsl.model.HttpRequest

import scala.concurrent.{ExecutionContext, Future}

trait IHttpRequestInterceptor {
  /**
   * Intercept and update the http request according to interceptor's own logic e.g. adding headers
   * @param httpRequest
   * @return
   */
  def processRequest(httpRequest: HttpRequest)(implicit ex:ExecutionContext):Future[HttpRequest]
}

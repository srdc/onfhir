package io.onfhir.client.intrcp

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.headers.{Authorization, GenericHttpCredentials}
import io.onfhir.client.IHttpRequestInterceptor

import scala.concurrent.{ExecutionContext, Future}

/**
 * Interceptor to inject a fixed basic token to Http Requests
 *
 * @param token The fixed token
 */
class FixedBasicTokenInterceptor(token: String) extends IHttpRequestInterceptor with Serializable {
  /**
   * Intercept and update the http request by adding the following Authorization header: "Basic <token>"
   *
   * @param httpRequest
   * @return
   */
  override def processRequest(httpRequest: HttpRequest)(implicit ex: ExecutionContext): Future[HttpRequest] = {
    Future.apply {
      httpRequest
        .withHeaders(httpRequest.headers :+ Authorization.apply(GenericHttpCredentials.apply("Basic", token)))
    }
  }
}

package io.onfhir.client.intrcp
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, OAuth2BearerToken}
import io.onfhir.client.IHttpRequestInterceptor

import scala.concurrent.{ExecutionContext, Future}

/**
 * Basic authentication handler
 * @param username  Username
 * @param password  Password
 */
class BasicAuthenticationInterceptor(username:String, password:String) extends IHttpRequestInterceptor with Serializable {
  /**
   * Intercept and update the http request according to interceptor's own logic e.g. adding headers
   *
   * @param httpRequest
   * @return
   */
  override def processRequest(httpRequest: HttpRequest)(implicit ex: ExecutionContext): Future[HttpRequest] = {
    Future.apply {
      httpRequest
        .withHeaders(httpRequest.headers :+ Authorization.apply(BasicHttpCredentials.apply(username, password)))
    }
  }
}

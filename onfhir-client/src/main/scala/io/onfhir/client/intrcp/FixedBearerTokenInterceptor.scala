package io.onfhir.client.intrcp

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import io.onfhir.client.IHttpRequestInterceptor

import scala.concurrent.{ExecutionContext, Future}

abstract class BearerTokenInterceptor extends IHttpRequestInterceptor {
  def addHeader(httpRequest: HttpRequest, bearerToken:String):HttpRequest = {
    httpRequest
      .withHeaders(httpRequest.headers :+ Authorization.apply(OAuth2BearerToken.apply(bearerToken)))
  }
}

case class FixedBearerTokenInterceptor(bearerToken:String) extends BearerTokenInterceptor {
  /**
   * Intercept and update the http request according to interceptor's own logic e.g. adding headers
   *
   * @param httpRequest
   * @return
   */
  override def processRequest(httpRequest: HttpRequest)(implicit ex:ExecutionContext): Future[HttpRequest] = {
    Future.apply(addHeader(httpRequest, bearerToken))
  }
}

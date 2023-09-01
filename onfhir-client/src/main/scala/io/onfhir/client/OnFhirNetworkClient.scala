package io.onfhir.client

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.stream.Materializer
import io.onfhir.api.client.{BaseFhirClient, FHIRPaginatedBundle, FhirClientException, IFhirBundleReturningRequestBuilder}
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}
import io.onfhir.client.intrcp.{BasicAuthenticationInterceptor, BearerTokenInterceptorFromTokenEndpoint, FixedBasicTokenInterceptor}
import io.onfhir.client.parsers.{FHIRRequestMarshaller, FHIRResponseUnmarshaller}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}

case class OnFhirNetworkClient(serverBaseUrl:String, interceptors:Seq[IHttpRequestInterceptor] = Nil)(implicit actorSystem: ActorSystem) extends BaseFhirClient {
  val logger:Logger = LoggerFactory.getLogger(this.getClass)
  implicit val ex:ExecutionContext = actorSystem.dispatcher
  implicit val materializer: Materializer = Materializer(actorSystem)

  override def getBaseUrl(): String = serverBaseUrl

  /**
   * Client with basic authentication
   * @param username  Username
   * @param password  Password
   * @return
   */
  def withBasicAuthentication(username:String, password:String):OnFhirNetworkClient = {
    this.copy(interceptors = interceptors :+ new BasicAuthenticationInterceptor(username, password))
  }

  /**
   * Client with fixed basic token authentication
   *
   * @param token The fixed token
   * @return
   */
  def withFixedBasicTokenAuthentication(token: String): OnFhirNetworkClient = {
    this.copy(interceptors = interceptors :+ new FixedBasicTokenInterceptor(token))
  }

  /**
   * Client with OAuth2.0 Bearer Token authentication
   * @param clientId                      Client identifier
   * @param clientSecret                  Client secret
   * @param requiredScopes                Required scopes for the FHIR client to access the required data
   * @param authzServerTokenEndpoint      Authorization server token endpoint to request token
   * @param clientAuthenticationMethod    Client authentication method (client_secret_basic | client_secret_post | client_secret_jwt)
   * @return
   */
  def withOpenIdBearerTokenAuthentication(clientId:String, clientSecret:String, requiredScopes:Seq[String], authzServerTokenEndpoint:String, clientAuthenticationMethod:String = "client_secret_basic"):OnFhirNetworkClient = {
    this.copy(interceptors = interceptors :+ new BearerTokenInterceptorFromTokenEndpoint(clientId, clientSecret, requiredScopes, authzServerTokenEndpoint, clientAuthenticationMethod))
  }

  /**
   * Execute the FHIR request and return FHIR response
   * @param fhirRequest FHIR request
   *  @return
   */
  override def execute(fhirRequest: FHIRRequest): Future[FHIRResponse] = {
    FHIRRequestMarshaller
      .marshallRequest(fhirRequest, serverBaseUrl)
      .flatMap(httpRequest => executeHttpRequest(httpRequest))
      .flatMap(httpResponse => FHIRResponseUnmarshaller.unmarshallResponse(httpResponse))
      .recover {
        case t:Throwable =>
          logger.error("Problem while executing FHIR request!", t)
          throw FhirClientException("Problem while executing FHIR request!" + t.getMessage)
      }
  }

  /**
   * Execute the next page for the search
   * @param bundle
   * @tparam T
   *  @return
   */
  override def next[T <: FHIRPaginatedBundle](bundle: T): Future[T] = {
    FHIRRequestMarshaller
      .marshallRequest(bundle.request.request, serverBaseUrl)
      .flatMap(httpRequest =>
        executeHttpRequest(httpRequest.withUri(Uri.apply(bundle.getNext())))
      )
      .flatMap(httpResponse => FHIRResponseUnmarshaller.unmarshallResponse(httpResponse))
      .recover {
        case t:Throwable =>
          logger.error(s"Problem while executing FHIR request '${bundle.getNext()}'!", t)
          throw FhirClientException(s"Problem while executing FHIR request '${bundle.getNext()}'!" + t.getMessage)
      }
      .map(fhirResponse =>
        bundle.request
          .asInstanceOf[IFhirBundleReturningRequestBuilder]
          .constructBundle(fhirResponse)
          .asInstanceOf[T]
      )
  }

  /**
   * Execut the HTTP request while handling interceptors
   * @param httpRequest
   * @return
   */
  private def executeHttpRequest(httpRequest:HttpRequest):Future[HttpResponse] = {
    if(interceptors.isEmpty)
      Http().singleRequest(httpRequest)
    else
      handleInterceptors(interceptors, httpRequest)
        .flatMap(r => Http().singleRequest(r))
  }

  /**
   * Recursively handle interceptors to update the request
   * @param interceptors
   * @param httpRequest
   * @return
   */
  private def handleInterceptors(interceptors:Seq[IHttpRequestInterceptor], httpRequest: HttpRequest):Future[HttpRequest] = {
    val interceptor = interceptors.head
    val processedRequest = interceptor.processRequest(httpRequest)
    if(interceptors.tail.isEmpty)
      processedRequest
    else
      processedRequest
        .flatMap(handleInterceptors(interceptors.tail, _))

  }

}

object OnFhirNetworkClient {
  /**
   * Create an network FHIR client with the given server base url
   * @param serverBaseUrl FHIR server's base url
   * @return
   */
  def apply(serverBaseUrl:String)(implicit actorSystem: ActorSystem): OnFhirNetworkClient = new OnFhirNetworkClient(serverBaseUrl)

  /**
   * Create an network FHIR client with the given server base url and http request interceptors
   * @param serverBaseUrl
   * @param interceptors
   * @return
   */
  def apply(serverBaseUrl: String, interceptors: Seq[IHttpRequestInterceptor])(implicit actorSystem: ActorSystem): OnFhirNetworkClient = new OnFhirNetworkClient(serverBaseUrl, interceptors)

  /**
   *
   * @param serverBaseUrl
   * @param interceptor
   * @return
   */
  def apply(serverBaseUrl: String, interceptor:IHttpRequestInterceptor)(implicit actorSystem: ActorSystem):OnFhirNetworkClient = new OnFhirNetworkClient(serverBaseUrl, Seq(interceptor))
}

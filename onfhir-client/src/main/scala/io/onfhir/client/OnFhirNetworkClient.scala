package io.onfhir.client

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.stream.Materializer
import io.onfhir.api.client.{BaseFhirClient, FHIRPaginatedBundle, FhirClientException, IFhirBundleReturningRequestBuilder}
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}
import io.onfhir.client.parsers.{FHIRRequestMarshaller, FHIRResponseUnmarshaller}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}

case class OnFhirNetworkClient(serverBaseUrl:String, interceptors:Seq[IHttpRequestInterceptor] = Nil)(implicit actorSystem: ActorSystem) extends BaseFhirClient {
  val logger:Logger = LoggerFactory.getLogger(this.getClass)
  implicit val ex:ExecutionContext = actorSystem.dispatcher
  implicit val materializer: Materializer = Materializer(actorSystem)

  override def getBaseUrl(): String = serverBaseUrl

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

  override def next[T <: FHIRPaginatedBundle](bundle: T): Future[T] = {
    FHIRRequestMarshaller
      .marshallRequest(bundle.request.request, serverBaseUrl)
      .flatMap(httpRequest =>
        executeHttpRequest(httpRequest.withUri(Uri.apply(bundle.getNext())))
      )
      .flatMap(httpResponse => FHIRResponseUnmarshaller.unmarshallResponse(httpResponse))
      .recover {
        case t:Throwable =>
          logger.error("Problem while executing FHIR request!", t)
          throw FhirClientException("Problem while executing FHIR request!" + t.getMessage)
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
  implicit val system = ActorSystem.apply("OnFhirNetworkClient")
  /**
   * Create an network FHIR client with the given server base url
   * @param serverBaseUrl FHIR server's base url
   * @return
   */
  def apply(serverBaseUrl:String): OnFhirNetworkClient = new OnFhirNetworkClient(serverBaseUrl)

  /**
   * Create an network FHIR client with the given server base url and http request interceptors
   * @param serverBaseUrl
   * @param interceptors
   * @return
   */
  def apply(serverBaseUrl: String, interceptors: Seq[IHttpRequestInterceptor]): OnFhirNetworkClient = new OnFhirNetworkClient(serverBaseUrl, interceptors)

  /**
   *
   * @param serverBaseUrl
   * @param interceptor
   * @return
   */
  def apply(serverBaseUrl: String, interceptor:IHttpRequestInterceptor):OnFhirNetworkClient = new OnFhirNetworkClient(serverBaseUrl, Seq(interceptor))
}

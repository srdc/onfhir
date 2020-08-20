package io.onfhir.subscription

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.settings.ConnectionPoolSettings
import akka.stream.{OverflowStrategy, QueueOfferResult}
import akka.stream.scaladsl.{Keep, Sink, Source}
import io.onfhir.subscription.config.SubscriptionConfig

import scala.util.{Failure, Success}
import scala.concurrent.{Future, Promise}

/**
 * HTTP client for OnFhir
 * @param subscriptionConfig
 * @param actorSystem
 */
class OnFhirClient(subscriptionConfig: SubscriptionConfig)(implicit actorSystem:ActorSystem[_]) {
  implicit val executionContext = actorSystem.executionContext

  //Get onfhir hosts
  val onfhirHosts = subscriptionConfig.onFhirHosts.map(_.split(':')).map(p => p.head -> p.drop(1).lastOption.map(_.toInt).getOrElse(80)).toSeq

  //Create a connection pool for each host in the cluster
  val poolClientFlows =
    onfhirHosts.map(h =>
        Http()
          .cachedHostConnectionPool[Promise[HttpResponse]](h._1, h._2, ConnectionPoolSettings.apply(actorSystem))
    )

  //Create a HTTP Request queue for each pool
  val httpRequestQueues =
    poolClientFlows.map(pf =>
        Source.queue[(HttpRequest, Promise[HttpResponse])](20, OverflowStrategy.backpressure)
        .via(pf)
        .to(Sink.foreach({
          case ((Success(resp), p)) => p.success(resp)
          case ((Failure(e), p))    => p.failure(e)
        }))
        .run()
    )

  var queueOrder = 1

  def updateQueueOrder() = {
    synchronized(
      queueOrder = (queueOrder + 1) % httpRequestQueues.size
    )
  }

  def sendRequest(request: HttpRequest): Future[HttpResponse] = {
    tryRequest(request, 1)
  }

  private def tryRequest(request: HttpRequest, retry:Int) :Future[HttpResponse] = {
    val responsePromise = Promise[HttpResponse]()

    //TODO add authorization headers

    val queue =
      if(httpRequestQueues.size == 1)
        httpRequestQueues.head else
      {
        val q = httpRequestQueues.apply(queueOrder)
        updateQueueOrder()
        q
      }

    queue.offer(request -> responsePromise).flatMap {
      case QueueOfferResult.Enqueued    => responsePromise.future
      case QueueOfferResult.Dropped     =>
        Future.failed(new RuntimeException("onFhir HttpRequest Queue overflowed. Try again later."))
      case QueueOfferResult.Failure(ex) =>
        Future.failed(ex)
      case QueueOfferResult.QueueClosed =>
          if(retry < httpRequestQueues.size)
            tryRequest(request, retry+1)
          else
            Future.failed(new RuntimeException("onFhir HttpRequest  Queue was closed (pool shut down) while running the request. Try again later."))
    }
  }

}

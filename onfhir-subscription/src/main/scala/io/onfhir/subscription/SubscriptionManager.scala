package io.onfhir.subscription

import akka.Done
import akka.actor.Scheduler
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.{FormData, HttpMethods, HttpRequest, HttpResponse, StatusCodes}
import akka.http.scaladsl.unmarshalling.Unmarshal
import io.onfhir.subscription.cache.{AddOrUpdateSubscription, Response, UpdateSubscriptionResponse}
import io.onfhir.subscription.model.FhirSubscription
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import io.onfhir.subscription.config.SubscriptionConfig
import  io.onfhir.subscription.util.JsonSupport
import scala.concurrent.Future
import akka.pattern.retry
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.concurrent.duration._

class SubscriptionManager(onFhirClient: OnFhirClient, subscriptionConfig: SubscriptionConfig)(implicit actorSystem:ActorSystem[_]) {
  val batchSize = 500
  implicit val executionContext = actorSystem.executionContext
  implicit val scheduler: Scheduler = actorSystem.classicSystem.scheduler

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()
  /**
   * Retrieve already existing subscriptions from FHIR repository and update the cache
   * @param subscriptionCache
   * @return
   */
  def initializeSubscriptions(subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command]):Future[Boolean] = {
    initializeSubscriptionsInBatches(subscriptionCache)
  }

  /**
   * Update the status of a FHIR subscription in onFhir repository
   * @param sid       Subscription id
   * @param status    Subscription status to set
   * @param error     If status is error or of error note
   * @return
   */
  def updateSubscriptionStatus(sid:String, status:String, error:Option[String]):Future[Boolean] = {
    actorSystem.log.debug("Updating subscription status with id {} to {} ...", sid, status)

    val formDataFields = Seq("status" -> status) ++ error.map(e => "error" -> e).toSeq

    val request =
      HttpRequest(HttpMethods.POST)
        .withUri(s"/onfhir/subscriptions/$sid")
        .withEntity(FormData(formDataFields:_*).toEntity)
    onFhirClient
      .sendRequest(request)
      .map {
        case resp @ HttpResponse(StatusCodes.OK, _, _ , _ ) =>
          resp.discardEntityBytes()
          true
        case resp @ HttpResponse(code, _, _, _) =>
          actorSystem.log.error("Problem while updating status of subscription: {}", sid)
          resp.discardEntityBytes()
          false
      }

  }

  /**
   * Recursive initialization batch by batch
   * @param subscriptionCache
   * @param page
   * @return
   */
  private def initializeSubscriptionsInBatches(subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command], page:Int= 1):Future[Done] = {
    retrieveActiveSubscriptions(page)
      .flatMap(subscriptions =>
        Future.sequence(subscriptions.map(s =>
          retry(() =>  subscriptionCache
            .ask[Response](replyTo => AddOrUpdateSubscription(s, replyTo))(subscriptionConfig.processorAskTimeout, actorSystem.scheduler), attempts = 3, delay = 1.second)
        )).flatMap(cachingResults => {
          val allSuccessfull = cachingResults.forall(b => b.asInstanceOf[UpdateSubscriptionResponse].result)
          if(allSuccessfull && subscriptions.length == batchSize)
              initializeSubscriptionsInBatches(subscriptionCache, page+1)
          else {
            if(!allSuccessfull) {
              val problematicSubscriptionIndices = cachingResults.zipWithIndex.filterNot(_._1.asInstanceOf[UpdateSubscriptionResponse].result).map(_._2)
              val problematicSubscriptions = problematicSubscriptionIndices.map(i => subscriptions.apply(i))
              actorSystem.log.error("Problem while caching some subscriptions: [{}]", problematicSubscriptions.map(_.id).mkString(","))
              throw new RuntimeException(s"Problem while caching some subscriptions: [${problematicSubscriptions.map(_.id).mkString(",")}]")
            } else
              Future.apply(Done)
          }
        })
      )
  }


  /**
   *
   * @param page
   * @return
   */
  private def retrieveActiveSubscriptions(page:Int):Future[Seq[FhirSubscription]] = {
    actorSystem.log.debug("Retrieving active subscriptions, page: {} ...", page)
    val request =
      HttpRequest(HttpMethods.GET).withUri(s"/onfhir/internal/subscriptions?_page=$page&_count=$batchSize")

    onFhirClient
      .sendRequest(request)
      .flatMap {
        case resp @ HttpResponse(StatusCodes.OK, _, _ , _ ) =>
          Unmarshal(resp).to[Seq[FhirSubscription]]
        case resp @ HttpResponse(code, _, _, _) =>
          actorSystem.log.error("Retrieving active subscriptions failed for page: {}, response code: {} ", page, code)
          resp.discardEntityBytes()
          Future.apply(Nil)
      }

  }

}

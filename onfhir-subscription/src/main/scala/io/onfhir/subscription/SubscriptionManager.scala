package io.onfhir.subscription

import akka.Done
import akka.actor.Scheduler
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.{FormData, HttpMethods, HttpRequest, HttpResponse, StatusCodes}
import io.onfhir.subscription.cache.{DistributedSearchParameterConfCache, FhirSearchParameterCache}
import io.onfhir.subscription.config.SubscriptionConfig

import scala.concurrent.Future
import akka.pattern.retry
import akka.http.scaladsl.unmarshalling.Unmarshal
import io.onfhir.api.SubscriptionStatusCodes
import io.onfhir.api.model.{FhirSubscription, InternalEntity}
import io.onfhir.util.InternalJsonMarshallers._
import org.slf4j.LoggerFactory

import scala.concurrent.duration._

class SubscriptionManager(onFhirClient: OnFhirClient, fhirSearchParameterCache: ActorRef[DistributedSearchParameterConfCache.Command], subscriptionConfig: SubscriptionConfig)(implicit actorSystem:ActorSystem[_]) {
  val batchSize = 500
  lazy val log = LoggerFactory.getLogger(classOf[SubscriptionManager])
  implicit val executionContext = actorSystem.executionContext
  implicit val scheduler: Scheduler = actorSystem.classicSystem.scheduler
  /**
   * Retrieve already existing subscriptions from FHIR repository and update the cache
   * @param subscriptionCache
   * @return
   */
  def initializeSubscriptions(subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command]):Future[Done] = {
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
    log.debug("Updating subscription status with id {} to {} ...", sid:Any, status:Any)

    val formDataFields = Seq("status" -> status) ++ error.map(e => "error" -> e).toSeq

    val request =
      HttpRequest(HttpMethods.POST)
        .withUri(s"/onfhir/internal/subscriptions/$sid")
        .withEntity(FormData(formDataFields:_*).toEntity)
    onFhirClient
      .sendRequest(request)
      .map {
        case resp @ HttpResponse(StatusCodes.OK, _, _ , _ ) =>
          resp.discardEntityBytes()
          true
        case resp @ HttpResponse(code, _, _, _) =>
          log.error("Problem while updating status of subscription: {}", sid)
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
      .flatMap(subscriptions => {
        log.debug(s"Synchronizing ${subscriptions.length} subscriptions ...")
        Future.sequence(
          subscriptions.map(s =>
            retry(() =>
              FhirSubscriptionKafkaProcessor
                .handleSubscription(s, subscriptionCache, subscriptionConfig, fhirSearchParameterCache, actorSystem)
                .map(r => s -> r),
              attempts = 3, delay = 1.second
            )
          )
        ).flatMap(results => {
          val problematicSubscriptions = results.filterNot(_._2).map(_._1)
          val unrecoverable = problematicSubscriptions.filterNot(_.status == SubscriptionStatusCodes.requested)

          if (unrecoverable.nonEmpty) {
            log.error("Problem while caching some subscriptions: [{}]", unrecoverable.map(_.id).mkString(","))
            throw new RuntimeException(s"Problem while caching some subscriptions: [${unrecoverable.map(_.id).mkString(",")}]")
          } else {
            //Find all requested subscriptions
            val requested = results.filter(_._2).filter(_._1.status == SubscriptionStatusCodes.requested)
            //Update status of them based on the result of subscription handling
            val allStatusUpdateFuture =
              Future.sequence(
                requested.map(s =>
                  retry(() =>
                    updateSubscriptionStatus(s._1.id, if(s._2) SubscriptionStatusCodes.active else SubscriptionStatusCodes.off, if(s._2) None else Some("Internal problem while processing subscription! Please try again!")),
                    attempts = 3, delay = 1.second
                  )
                )
              )

            allStatusUpdateFuture.flatMap(allUpdated =>
              if (allUpdated.forall(b => b))
                if (subscriptions.length == batchSize)
                  initializeSubscriptionsInBatches(subscriptionCache, page + 1)
                else
                  Future.apply(Done)
              else {
                log.error("Problem while updating status of some subscriptions!")
                throw new RuntimeException("Problem while updating status of some subscriptions!")
              }
            )
          }
        })
      })
  }


  /**
   *
   * @param page
   * @return
   */
  private def retrieveActiveSubscriptions(page:Int):Future[Seq[FhirSubscription]] = {
    log.debug("Retrieving active subscriptions, page: {} ...", page)
    val request =
      HttpRequest(HttpMethods.GET).withUri(s"/onfhir/internal/subscriptions?_page=$page&_count=$batchSize")

    onFhirClient
      .sendRequest(request)
      .flatMap {
        case resp @ HttpResponse(StatusCodes.OK, _, _ , _ ) =>
          Unmarshal(resp.entity.httpEntity).to[Seq[InternalEntity]].map(_.map(_.asInstanceOf[FhirSubscription]))
        case resp @ HttpResponse(code, _, _, _) =>
          actorSystem.log.error("Retrieving active subscriptions failed for page: {}, response code: {} ", page, code)
          resp.discardEntityBytes()
          throw new RuntimeException(s"Retrieving active subscriptions failed for page: $page, response code: $code !")
      }

  }

}

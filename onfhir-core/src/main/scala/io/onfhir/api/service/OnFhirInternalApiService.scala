package io.onfhir.api.service

import akka.Done
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import io.onfhir.Onfhir
import io.onfhir.api._
import io.onfhir.api.model.{FhirSubscription, Parameter}
import io.onfhir.api.util.{FHIRUtil, SubscriptionUtil}
import io.onfhir.config.SearchParameterConf
import io.onfhir.db.ResourceManager
import org.json4s.JsonAST.{JObject, JString}
import io.onfhir.config.FhirConfigurationManager.fhirConfig

import scala.concurrent.{ExecutionContext, Future}

class OnFhirInternalApiService {
  implicit val executionContext:ExecutionContext = Onfhir.actorSystem.dispatcher

  val subscriptionUtil = new SubscriptionUtil(fhirConfig)

  /**
   * Retrieve subscriptions
   * @param page
   * @param count
   * @return
   */
  def retrieveSubscriptions(page:Int, count:Int):Future[Seq[FhirSubscription]] = {
    //Only retrieve the requested, active or with an error (still active)
    val queryParams = List(
      Parameter(
        FHIR_PARAMETER_CATEGORIES.NORMAL,
        FHIR_PARAMETER_TYPES.TOKEN,
        "status",
        Seq(SubscriptionStatusCodes.requested, SubscriptionStatusCodes.active, SubscriptionStatusCodes.error).map("" -> _))
    )
    ResourceManager
      .queryResources("Subscription", queryParams, count, page, sortingFields = Seq(("_lastUpdated", 1, Seq("meta.lastUpdated" -> FHIR_DATA_TYPES.INSTANT))), needTotal = false)(None)
      .map {
        case (_, subscriptions) =>
          subscriptions.map(s => subscriptionUtil.parseFhirSubscription(s))
      }
  }

  /**
   * Update FHIR Subscription status
   * @param sid     Subscription id
   * @param status  New status
   * @param error   Error description if status is error or off
   * @return
   */
  def updateSubscriptionStatus(sid:String, status:String, error:Option[String]):Future[HttpResponse] = {
      ResourceManager
        .getResource("Subscription", sid, excludeExtraFields = true)
        .flatMap {
          //No such resource, ignore it
          case None => Future.apply( HttpResponse(StatusCodes.OK))
          //Update the subscription status
          case Some(subscription) =>
            val update =
              if(error.isEmpty)
                JObject("status" -> JString(status))
              else
                JObject("status" -> JString(status), "error" -> JString(error.get))

            val updatedSubscription =
              subscription
                .removeField(f => f._1 == "status" || f._1 == "error")
                .merge(update)
                .asInstanceOf[JObject]

            val previousVersion = FHIRUtil.extractVersionFromResource(subscription)

            ResourceManager
              .updateResource("Subscription", sid, updatedSubscription, previousVersion -> subscription, silentEvent = true)
              .map(_ => HttpResponse(StatusCodes.OK))
        }
  }

  /**
   * Get search parameter configurations
   * @param rtype     Resource type
   * @param spNames   Search parameter names
   * @return
   */
  def getSearchParameterConfigurations(rtype:String, spNames:Set[String]):Future[Seq[SearchParameterConf]] = {
    Future.apply {
      spNames.flatMap(sp => fhirConfig.findSupportedSearchParameter(rtype, sp)).toSeq
    }
  }

}

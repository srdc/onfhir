package io.onfhir.subscription.util

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import io.onfhir.api.model.Parameter
import io.onfhir.config.SearchParameterConf
import io.onfhir.subscription.model.{FhirSubscription, FhirSubscriptionChannel}
import spray.json.DefaultJsonProtocol

// collect your json format instances into a support trait:
trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val searchParameterConfFormat = jsonFormat10(SearchParameterConf)
  implicit val subscriptionChannelFormat = jsonFormat4(FhirSubscriptionChannel)
  implicit val parameterFormat = jsonFormat6(Parameter)
  implicit val subscriptionFormat = jsonFormat6(FhirSubscription)
}

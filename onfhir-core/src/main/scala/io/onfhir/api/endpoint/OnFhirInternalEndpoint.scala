package io.onfhir.api.endpoint

import akka.Done
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import io.onfhir.api.service.OnFhirInternalApiService
import io.onfhir.authz.AuthManager
import io.onfhir.util.InternalJsonMarshallers._
/**
 * Internal API for onFhir to communicate with other individual modules like subscription
 */
trait OnFhirInternalEndpoint {


  val internalRoute:Route = {
      authenticateOAuth2[Done]("onfhir-internal", AuthManager.authenticateForInternalApi) {_ =>
        pathPrefix("onfhir" / "internal") {
          //Subscription handling
          pathPrefix("subscriptions") {
            //Retrieving existing subscriptions page by page
            pathEndOrSingleSlash {
              get {
                parameter("_page".as[Int], "_count".as[Int]) { (page, count) =>
                  complete(new OnFhirInternalApiService().retrieveSubscriptions(page, count))
                }
              }
            } ~ //Setting subscription status
          pathPrefix(Segment) {sid =>
              formField("status", "error".?) { (status, error) =>
                 complete(new OnFhirInternalApiService().updateSubscriptionStatus(sid, status, error))
              }
            }
          } ~ //Retrieving search parameter configurations
            pathPrefix("searchparameters" / Segment) { rtype =>
              pathEndOrSingleSlash {
                get {
                  parameter("pname") { ps =>
                    complete(new OnFhirInternalApiService().getSearchParameterConfigurations(rtype, ps.split(',').toSet))
                  }
                }
              }
            }

        }
      }
  }

}

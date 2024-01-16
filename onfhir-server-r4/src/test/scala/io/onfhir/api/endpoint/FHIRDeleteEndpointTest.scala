package io.onfhir.api.endpoint

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import akka.http.scaladsl.model.StatusCodes._
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRMarshallers._
import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRDeleteEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory: ActorSystem = system
  implicit def default(implicit system: ActorSystem): RouteTestTimeout = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val patient: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val resourceType = "Patient"
  val resourceId   = "example"
  sequential

  "FHIR Delete Endpoint" should {
    "delete (mark as deleted) an existing resource in the database" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> fhirRoute ~> check {
        status === Created
      }
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> fhirRoute ~> check {
        status === NoContent
        responseEntity.getContentLengthOption().getAsLong === 0
        checkHeaders(null, "2")
      }
    }
    "do nothing if resource is already deleted" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> fhirRoute ~> check {
        status === NoContent
        responseEntity.getContentLengthOption().getAsLong === 0
        checkHeaders(null, "2")
      }
    }
    "do nothing if resource does not exist" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + "UnknownResource") ~> fhirRoute ~> check {
        status === NoContent
        responseEntity.getContentLengthOption().getAsLong === 0
        header("ETag").isDefined must beFalse
      }
    }
    "reject delete operation for invalid id" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "+") ~> fhirRoute ~> check {
        status === NotFound
      }
    }

    "allow resources to be brought back to life" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> fhirRoute ~> check {
        status === Created
        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "3")
        checkHeaders(response, resourceType, resourceId, "3")
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> fhirRoute ~> check {
        status === OK
        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "4")
        checkHeaders(response, resourceType, resourceId, "4")
      }
    }

  }

}

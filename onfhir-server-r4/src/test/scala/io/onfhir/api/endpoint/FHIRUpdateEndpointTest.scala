package io.onfhir.api.endpoint

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.{EntityTag, RawHeader, `If-Match`}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.api.Resource
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.{FhirConfigurationManager, OnfhirConfig}
import io.onfhir.util.DateTimeUtil
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import io.onfhir.api.model.FHIRMarshallers._
import org.json4s.JsonAST.JObject

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRUpdateEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientNotParsable = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-not-parsable.json")).mkString
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString
  val patientWithInvalidId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-with-invalid-id.json")).mkString

  val practitioner = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Practitioner/practitioner.json")).mkString

  val resourceType = "Patient"
  val resourceId   = "example"

  sequential

  "FHIR Update Endpoint" should {
    //Test creating with update
   "create a new resource with a given id if not exists" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === Created
        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "1")
        checkHeaders(response, resourceType, resourceId, "1")
      }
    }
    //Testing an update
    "create a new version for given resource in database if it already exists" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === OK
        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "2")
        checkHeaders(response, resourceType, resourceId, "2")
      }

      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === OK
        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "3")
        checkHeaders(response, resourceType, resourceId, "3")
      }
    }

    "create a new version for given resource in database for correct mime type" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_format=application/fhir+json", HttpEntity(patient)) ~> routes ~> check {
        status === OK
        responseEntity.contentType.value === "application/fhir+json; charset=UTF-8"

        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "4")
        checkHeaders(response, resourceType, resourceId, "4")
      }

      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_format=application/fhir+xml", HttpEntity(patient)) ~> routes ~> check {
        status === OK
        responseEntity.contentType.value === "application/fhir+xml; charset=UTF-8"

        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "5")
        checkHeaders(response, resourceType, resourceId, "5")
      }
    }

    "reject update operation for bad json" in{
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patientNotParsable)) ~> routes ~> check {
        status === BadRequest
      }
    }
    "reject update operation for missing id" in{
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patientWithoutId)) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Missing 'id' field in given resource")
      }
    }
    "reject update operation for wrong id" in{
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/123456789", HttpEntity(patient)) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("does not match with the id field in request")
      }
    }
    "reject update operation for invalid id" in{
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "+", HttpEntity(patientWithInvalidId)) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
    }

    "manage resource contention for wrong versions" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(`If-Match`(EntityTag("3", weak = false)))) ~> routes ~> check {
        status === Conflict
      }
    }
    "manage resource contention for correct versions" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(`If-Match`(EntityTag("5", weak = false)))) ~> routes ~> check {
        status === OK

        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "6")
        checkHeaders(response, resourceType, resourceId, "6")

      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(RawHeader("If-Match", "\"6\""))) ~> routes ~> check {
        status === OK
        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "7")
        checkHeaders(response, resourceType, resourceId, "7")

      }
    }

    "reject normal update when 'versioned-update' is set for the resource type" in {
       //Create the practitioner
      Put("/" + OnfhirConfig.baseUri + "/" + "Practitioner" + "/" + "pr1", HttpEntity(practitioner)) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("only versioned updates are supported")
      }
    }

    "honor Prefer header" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(RawHeader("Prefer", "return=representation"))) ~> routes ~> check {
        status === OK
        val response = responseAs[Resource]
        checkIdAndMeta(response, resourceId, "8")
        checkHeaders(response, resourceType, resourceId, "8")
        response.obj.length === 15
      }

      //Should return empty body
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(RawHeader("Prefer", "return=minimal"))) ~> routes ~> check {
        status === OK
        checkHeaders(null, resourceType, resourceId, "9")
        responseEntity.getContentLengthOption().getAsLong === 0
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(RawHeader("Prefer", "return=OperationOutcome"))) ~> routes ~> check {
        status === OK
        checkHeaders(null, resourceType, resourceId, "10")
        val response:JObject = responseAs[Resource]
        FHIRUtil.extractValueOption[String](response,"resourceType").contains("OperationOutcome")
      }

    }

  }

}

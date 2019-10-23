package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.{EntityTag, `If-Modified-Since`, `If-None-Match`}
import akka.http.scaladsl.model.{DateTime, HttpEntity}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnfhirTest
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRReadServiceSpec extends OnfhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))
  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientNotParsable = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-not-parsable.json")).mkString
  val patientWithInvalidId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-with-invalid-id.json")).mkString
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString
  val patientWithoutType = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-type.json")).mkString

  val resourceType = "Patient"
  val resourceId   = "23132"

  sequential

  "FHIR Read Service" should {
    "return 404 Not Found for unknown resource" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === NotFound
      }
    }
    "return current content of the resource" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check{
        println(responseAs[String])
        status===Created
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === OK
        println(responseAs[String])
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
        responseAs[String] must contain("\"resourceType\"")
      }
    }
    "return version specific content of the resource" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/1") ~> routes ~> check {
        status === OK
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
        responseAs[String] must contain("\"resourceType\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/2") ~> routes ~> check {
        status === OK
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"2\""
        responseAs[String] must contain("\"resourceType\"")
      }
    }
    "return read content of resource for correct mime type" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_format=application/json+fhir") ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"2\""
        responseAs[String] must contain("\"resourceType\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/1?_format=application/json+fhir") ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"1\""
        responseAs[String] must contain("\"resourceType\"")
      }
    }
    "honor If-None-Match header" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-None-Match`(EntityTag("2", weak = true)))) ~> routes ~> check {
        status === NotModified
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-None-Match`(EntityTag("2", weak = false)))) ~> routes ~> check {
        status === NotModified
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
          .withHeaders(List(`If-None-Match`(EntityTag("3", weak = true)))) ~> routes ~> check {
        status === OK
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"2\""
        responseAs[String] must contain("\"resourceType\"")
      }
    }
    "honor If-Modified-Since header" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-Modified-Since`(DateTime.now + 1000 * 60 /*1 minute later*/))) ~> routes ~> check {
        status === NotModified
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-Modified-Since`(DateTime.now - 1000 * 60 /*1 minute before*/))) ~> routes ~> check {
        status === OK
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"2\""
        responseAs[String] must contain("\"resourceType\"")
      }
    }
    "return 410 Gone for deleted resource" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === Gone
        header("ETag").get.value === "W/\"3\""
        responseAs[String] must contain("\"resourceType\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/3") ~> routes ~> check {
        status === Gone
        header("ETag").get.value === "W/\"3\""
        responseAs[String] must contain("\"resourceType\"")
      }
    }
    "reject read operation for invalid id" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "+") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/3+") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
    }
    "return read content of resource wrt _summary parameter" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=true") ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"4\""
        responseAs[String] must contain("\"SUBSETTED\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=data") ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"4\""
        responseAs[String] must contain("\"SUBSETTED\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=text") ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"4\""
        responseAs[String] must contain("\"SUBSETTED\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=false") ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"4\""
      }
    }
    "throw invalidParameterException when _summary parameter is misused" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=invalid") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("invalid for _summary parameter")
      }
    }
  }
  /*
  step {
    MongoDB.getDatabase.drop().head()
  }*/
}

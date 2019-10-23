package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnfhirTest
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRDeleteServiceSpec extends OnfhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString
  val observationWithoutId =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutId.json")).mkString
  val observation =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-example.json")).mkString
  val practitioner = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Practitioner/practitioner.json")).mkString

  val resourceType = "Patient"
  val resourceId   = "23132"
  val resourceType2 = "Observation"
  val resourceId2  = "2222"

  sequential



  "FHIR Delete Service" should {
    "delete (mark as deleted) an existing resource in the database" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === Created
      }
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === NoContent
        header("ETag").get.value === "W/\"2\""
      }
    }
    "do nothing if resource is already deleted" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === NoContent
        header("ETag").get.value === "W/\"2\""
      }
    }
    "do nothing if resource does not exist" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + "UnknownResource") ~> routes ~> check {
        status === NoContent
        header("ETag").isDefined must beFalse
      }
    }
    "reject delete operation for invalid id" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "+") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
    }
    "allow resources to be brought back to life" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === Created
        header("ETag").get.value === "W/\"3\""
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"4\""
      }
    }
    "return a 412 Precondition Failed error when there are multiple matches with given parameters and multiple conditional delete is not supported"  in {
      Post("/" + OnfhirConfig.baseUri + "/" + "Practitioner" , HttpEntity(practitioner)) ~> routes ~> check{status===Created}
      Post("/" + OnfhirConfig.baseUri + "/" + "Practitioner" , HttpEntity(practitioner)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + "Practitioner" , HttpEntity(practitioner)) ~> routes ~> check(status===Created)
      //For practitioner the single conditional delete is supported
      Delete("/" + OnfhirConfig.baseUri + "/" + "Practitioner" + "?identifier=23") ~> routes ~> check {
        status === PreconditionFailed
        responseAs[String] must contain("Multiple matches exist with given parameters and multiple conditional delete is not supported")
      }
    }
    "successfully delete multiple matches with given parameters when multiple conditional delete is  supported"  in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId)) ~> routes ~> check{status===Created}
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId)) ~> routes ~> check(status===Created)
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "?gender=male") ~> routes ~> check {
        status === NoContent
      }
    }
    "delete (mark as deleted) an existing resource in the database when there is exact match with given parameters" in {
      //Post("/" + Config.baseUri + "/" + resourceType2 , HttpEntity(observation)) ~> routes ~> check{status===Created}
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType2 , HttpEntity(observationWithoutId)) ~> routes ~> check{status===Created}
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?subject:Patient=example") ~> routes ~> check {
        status === NoContent
        header("ETag").get.value === "W/\"2\""
      }
    }
    "return 200 OK with warning when there is no such document with given parameters" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?subject=Patient/exampless") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("There is no such resource")
      }
    }
  }

}

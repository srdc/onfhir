package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.{EntityTag, RawHeader, `If-Match`}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnfhirTest
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRUpdateServiceSpec extends OnfhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientNotParsable = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-not-parsable.json")).mkString
  val patientWithInvalidId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-with-invalid-id.json")).mkString
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString
  val patientWithoutType = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-type.json")).mkString

  val observationWithoutId =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutId.json")).mkString
  val observationWithoutSystem =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutSystemField.json")).mkString
  val observation =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-example.json")).mkString

  val carePlan = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/CarePlan/careplan-with-id.json")).mkString
  val carePlanId = "1235"

  val resourceType = "Patient"
  val resourceId   = "23132"
  val resourceType2 = "Observation"
  val resourceId2  = "2222"

  sequential

  "FHIR Update Service" should {
    "create a new resource in database if not exists" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === Created
        header("Location").isDefined === true
        header("Location").get.value === FHIRUtil.resourceLocationWithVersion(resourceType, resourceId, 1L)
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
      }
    }
    "create a new version for given resource in database if it already exists" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === OK
        header("Location").isDefined === true
        header("Location").get.value === FHIRUtil.resourceLocationWithVersion(resourceType, resourceId, 2L)
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"2\""
      }

      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === OK
        header("Location").isDefined === true
        header("Location").get.value === FHIRUtil.resourceLocationWithVersion(resourceType, resourceId, 3L)
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"3\""
      }
    }
    "create a new version for given resource in database for correct mime type" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_format=application/json+fhir", HttpEntity(patient)) ~> routes ~> check {
        status === OK
        header("Location").isDefined === true
        header("Location").get.value === FHIRUtil.resourceLocationWithVersion(resourceType, resourceId, 4L)
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"4\""
      }
    }
    "reject update operation for bad json" in{
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patientNotParsable)) ~> routes ~> check {
        status === BadRequest
        //responseAs[String] must contain("Invalid JSON")
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
    "reject update operation for missing type" in{
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patientWithoutType)) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Missing resourceType field for given resource")
      }
    }
    "reject update operation for wrong type" in{
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient2" + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === NotFound
        responseAs[String] must contain("is not supported")
      }
    }
    "manage resource contention for wrong versions" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(`If-Match`(EntityTag("3", weak = false)))) ~> routes ~> check {
        status === Conflict
        responseAs[String] must contain("Conflicting Version" )
      }
    }
    "manage resource contention for correct versions" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(`If-Match`(EntityTag("4", weak = false)))) ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"5\""
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(RawHeader("If-Match", "\"5\""))) ~> routes ~> check {
        status === OK
        header("ETag").get.value === "W/\"6\""
      }
    }
    "honor Prefer header" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient))
        .withHeaders(List(RawHeader("Prefer", "return=representation"))) ~> routes ~> check {

        status === OK
        header("ETag").get.value === "W/\"7\""
        responseAs[String] must contain("\"resourceType\"")
      }
    }
    "create a new resource in database if there is no such resource with given parameters" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?code=Example", HttpEntity(observation)) ~> routes ~> check {
        println(responseAs[String])
        status === Created
        header("Location").isDefined === true
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
      }
    }
    "create a new version for given resource in database if there is only one match with given parameters" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?_tag=RP", HttpEntity(observation)) ~> routes ~> check {
        println(responseAs[String])
        status === OK
        header("Location").isDefined === true
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"2\""
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?_tag=RP", HttpEntity(observationWithoutId)) ~> routes ~> check {
        println(responseAs[String])
        status === OK
        header("Location").isDefined === true
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"3\""
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?_tag=RP", HttpEntity(observationWithoutId)) ~> routes ~> check {
        println(responseAs[String])
        status === OK
        header("Location").isDefined === true
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"4\""
      }
    }
    "return 412 Precondition Failed Error when there are multiple matches with given parameters" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?_tag=RPPP", HttpEntity(observationWithoutId)) ~> routes ~> check {
        println(responseAs[String])
        status === Created
        header("Location").isDefined === true
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType2 + "?_tag=RP", HttpEntity(observation)) ~> routes ~> check {
        status === PreconditionFailed
        responseAs[String]  must contain  ("Multiple matches exist")
      }
    }
    "return 405 Method not allowed if it is not allowed to create resource with update" in {
      Put("/" + OnfhirConfig.baseUri + "/CarePlan/"+carePlanId, HttpEntity(carePlan)) ~> routes ~> check {
        status === MethodNotAllowed
      }
    }
  }
/*
  step {
    MongoDB.getDatabase.drop().head()
  }*/
}

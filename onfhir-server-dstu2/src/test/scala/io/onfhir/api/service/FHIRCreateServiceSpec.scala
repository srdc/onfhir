package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnfhirTest
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRCreateServiceSpec extends OnfhirTest  with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientNotParsable = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-not-parsable.json")).mkString
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString
  val patientWithoutType = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-type.json")).mkString
  val patientWithoutIdAndType = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id-and-type.json")).mkString

  val resourceType = "Patient"
  val resourceId   = "23132"

  sequential

  "FHIR Create Service" should {
    /*"sss" in {
      import org.json4s.JsonDSL._
      val jobj = ("test" -> "x") ~ ("t2" -> ("t3"-> Seq(("t5"-> 7), ("t6" ->8))) ~ ("t4" -> "2"))

      (jobj \ "test").isInstanceOf[JValue] mustEqual true
      (jobj \ "t2" \ "t3").isInstanceOf[JArray] mustEqual true
      (jobj \ "t2" \ "t9") mustEqual JNothing

      Extraction.extract[Int](jobj \ "test") mustEqual 3
      1 === 1
    }*/

    "create a new resource in database" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patientWithoutId)) ~> routes ~> check {
        eventually(status === Created)
        header("Location").isDefined === true
        header("Location").get.value must contain("_history")
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
      }
    }
    "create a new resource in database for correct mime type" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_format=application/json+fhir", HttpEntity(patientWithoutId)) ~> routes ~> check {
        status === Created
        header("Location").isDefined === true
        header("Location").get.value must contain("_history")
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
      }
    }
    //We found that this is not an error
    "accept create operation for existing id in resource" in{
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patient)) ~> routes ~> check {
        status === Created
        //responseAs[String] must contain("'id' field of resources should not be provided")
      }
    }
    "reject create operation for bad json" in{
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patientNotParsable)) ~> routes ~> check {
        status === BadRequest
        //responseAs[String] must contain("Invalid JSON")
      }
    }
    "reject create operation for missing type" in{
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patientWithoutIdAndType)) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Missing resourceType field for given resource")
      }
    }
    "reject create operation for wrong type" in{
      Post("/" + OnfhirConfig.baseUri + "/" + "Patient2", HttpEntity(patientWithoutId)) ~> routes ~> check {
        status === NotFound
        responseAs[String] must contain("is not supported")
      }
    }
    "honor Prefer header" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId))
        .withHeaders(List(RawHeader("Prefer", "return=representation"))) ~> routes ~> check {
        status === Created
        responseAs[String] must contain("\"resourceType\":\"Patient\"")
      }
    }
  }

}

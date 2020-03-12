package io.onfhir.api.endpoint

import java.util.concurrent.TimeUnit
import akka.http.scaladsl.model.StatusCodes._
import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRCreateEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  //Test resources
  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientNotParsable = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-not-parsable.json")).mkString
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString

  val resourceType = "Patient"
  val resourceId   = "example"

  sequential

  "FHIR Create Service" should {
    "create a new resource" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patientWithoutId)) ~> routes ~> check {
        eventually(status === Created)
        header("Location").isDefined === true
        header("Location").get.value must contain("_history")
        header("Last-Modified").isDefined === true
        header("ETag").get.value === "W/\"1\""
      }
    }
  }
}

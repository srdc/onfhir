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

/**
  * Created by abdulkadir on 9.3.2016.
  */

@RunWith(classOf[JUnitRunner])
class FHIRPatientSearchServiceSpec extends OnfhirTest with FHIREndpoint{
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString
  val patientBoris =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-Boris.json")).mkString

  val resourceType = "Patient"

  sequential

  "FHIR Patient Search Service" should {


    "return a bundle for the resources wrt given active parameter"  in {  // token type like Boolean
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId)) ~> routes ~> check{
        println(responseAs[String])
        status===Created
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientBoris)) ~> routes ~> check{
        println(responseAs[String])
        status===Created
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?active=true" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }
/*
    "return a bundle for the resources wrt given family parameter" in { // string type
      Get("/" + Config.baseUri + "/" + resourceType + "?family=Betterhalf" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + Config.baseUri + "/" + resourceType + "?family:contains=er" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given gender parameter" in { // token type like [code]
      Get("/" + Config.baseUri + "/" + resourceType + "?gender=male" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + Config.baseUri + "/" + resourceType + "?gender:not=other" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given identifier parameter" in { // token type like [system]|[value]
      Get("/" + Config.baseUri + "/" + resourceType + "?identifier=http://hl7.org/fhir/sid/us-ssn|444888888" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given organization parameter" in { // reference type
      Get("/" + Config.baseUri + "/" + resourceType + "?organization=Organization/1" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + Config.baseUri + "/" + resourceType + "?organization:Organization=1" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }
*/
    "return a bundle for the resources wrt given phone parameter" in { // contactPoint token type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?phone=555-555-2008" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given language parameter" in { // token type [system]|[code]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?language=urn:ietf:bcp:47|nl" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?language:text=Nederlands" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

  }

  /*
  step {
    MongoDB.getDatabase.drop().head()
  }*/
}

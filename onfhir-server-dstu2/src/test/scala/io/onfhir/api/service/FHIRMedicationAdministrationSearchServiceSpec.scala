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
class FHIRMedicationAdministrationSearchServiceSpec extends OnfhirTest  with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))
  val medicationAdministration =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/MedicationAdministration/MedicationAdministration.json")).mkString
  val medicationAdministration2 =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/MedicationAdministration/MedicationAdministration2.json")).mkString

  val resourceType = "MedicationAdministration"

  sequential

  "FHIR MedicationAdministration Search Service" should {

    "return a bundle for the resources wrt given code parameter"  in {  // token type like [system]|[code]
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(medicationAdministration)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(medicationAdministration)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(medicationAdministration)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(medicationAdministration)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(medicationAdministration2)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(medicationAdministration2)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(medicationAdministration2)) ~> routes ~> check(())

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?code=http://snomed.info/sct|66493003" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?code=66493003" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?code:text=Theophylline+200mg" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":7")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given effectivetime parameter"  in {  // date type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?effectivetime=2016-03" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?effectivetime=2015-05-31T22:01:00+04:00" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?effectivetime=2016-04" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }
    }

    "return a bundle for the resources wrt given prescription parameter"  in {  // reference type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?prescription=MedicationOrder/medrx008" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?prescription:MedicationOrder=medrx007" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?prescription="+OnfhirConfig.fhirRootUrl+"/MedicationOrder/medrx007" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given identifier parameter"  in {  // token type like [system]|[value]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?identifier=rjx0oSLgpLXJUq62E2eK7w" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":7")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?identifier=|rjx0oSLgpLXJUq62E2eK7w" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?identifier=||rjx0oSLgpLXJUq62E2eK7w" ) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid usage of parameter")
      }
    }

    "return a bundle for the resources wrt given notgiven parameter"  in {  // token type like [Boolean]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?notgiven=false" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?notgiven=False" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given status parameter"  in {  // token type like [code]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?status=on-hold" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?status:not=on-hold" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

  }

  /*
  step {
    MongoDB.getDatabase.drop().head()
  }*/
}

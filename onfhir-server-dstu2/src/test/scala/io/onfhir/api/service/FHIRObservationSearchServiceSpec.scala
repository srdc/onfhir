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
class FHIRObservationSearchServiceSpec extends OnfhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))
  val observation =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-example.json")).mkString
  val observationWithoutSystemField=Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutSystemField.json")).mkString
  val observationWithoutSystemAndId=Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutSystemAndId.json")).mkString
  val observationWithoutId =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutId.json")).mkString

  val resourceType = "Observation"
  val resourceId   = "2222"
  val resourceId2 = "1111"

  sequential

  "FHIR Observation Search Service" should {


    "return a bundle for the resources wrt given category(token-CC) parameter"  in {  // token type like [system]|[code]
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(observation)) ~> routes ~> check{
        println(responseAs[String])
        status===Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(observation)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId2, HttpEntity(observationWithoutSystemField)) ~> routes ~> check{
        println(responseAs[String])
        status===Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId2, HttpEntity(observationWithoutSystemField)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutSystemAndId)) ~> routes ~> check{
        println(responseAs[String])
        status===Created
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutSystemAndId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutId)) ~> routes ~> check{
        println(responseAs[String])
        status===Created
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutId)) ~> routes ~> check(())

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category=http://hl7.org/fhir/observation-category|vital-signs" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category=http://hl7.org/fhir/observation-category|VITAL-signs|" ) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid usage of parameter")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category=|http://hl7.org/fhir/observation-category|VITAL-signs" ) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid usage of parameter")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category:text=Vital" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category:not=vital-signs" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category:not=|vital-signs" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given status(token-c) parameter"  in { // token type like [code]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?status=final" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?status:not=final" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }
    }

    "return a bundle for the resources wrt given code-value-[x](composite) parameter"  in {  // composite type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?code-value-quantity=3141-9$185||lbs" ) ~> routes ~> check { //  token$quantity
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":2")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given value-quantity(quantity) parameter"  in {  // quantity type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=185|http://unitsofmeasure.org|[lb_av]" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=ap185|http://unitsofmeasure.org|[lb_av]" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=lt185||[lb_av]" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=gt180||lbs" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=ap185|http://unitsofmeasureses.org|[lb_av]" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=ap185|http://unitsofmeasure.org|[lv]" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=gt180||lbs|" ) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid usage of parameter")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=nonnumeric||lbs" ) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid usage of parameter")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?value-quantity=nonnumeric" ) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid usage of parameter")
      }
    }


    "return a bundle for the resources wrt given subject(reference) parameter"  in {  // reference type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?subject=Patient/example") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?subject:Device=example") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":2")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?subject="+OnfhirConfig.fhirRootUrl+"/Patient/example") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      /*Get("/" + Config.baseUri + "/" + resourceType + "?subject=XYZ/fhir/Patient/example") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid usage of parameter")
      }*/
    }

    "return a bundle for the resources wrt provided mixed parameters"  in { // type=value&type=value1,value2...
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category:not=|vital-signs&status=final" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category=|vital-signs&status=final" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category=vital-signs&status=final,finale" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }
  }
  /*
  step {
    MongoDB.getDatabase.drop().head()
  }*/
}

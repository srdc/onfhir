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
  * Created by abdulkadir on 11.3.2016.
  */

@RunWith(classOf[JUnitRunner])
class FHIRCarePlanSearchServiceSpec extends OnfhirTest with FHIREndpoint{
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val carePlan = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/CarePlan/CarePlan.json")).mkString
  val carePlan2 =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/CarePlan/CarePlan2.json")).mkString
  val carePlan3 =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/CarePlan/CarePlan3.json")).mkString

  val resourceType = "CarePlan"
  sequential

  "FHIR CarePlan Search Service" should {

    "return a bundle for the resources wrt given activitydate parameter"  in {  // date Type
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan2)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan2)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan2)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan3)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan3)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan3)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan3)) ~> routes ~> check(status===Created)
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(carePlan3)) ~> routes ~> check(status===Created)

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?activitydate:missing=true" ) ~> routes ~> check {
        print(responseAs[String])
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?activitydate:missing=false" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?activitydate=le2011-06-27T09:30:10+01:00" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?activitydate=ge2012-09-10" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given activitycode parameter"  in { // token type as a [code]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?activitycode:text=Operation+on+heart" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given date parameter"  in { // date type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?date=ge2011&date=le2015" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given subject parameter"  in { // reference type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?subject:Patient=id1452867835109" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given related parameter"  in { // composite type [code]$[reference]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?related=includes$CarePlan/id1452871352243" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }
  }

}

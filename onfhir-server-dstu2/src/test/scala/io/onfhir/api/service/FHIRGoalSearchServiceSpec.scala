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
class FHIRGoalSearchServiceSpec extends OnfhirTest  with FHIREndpoint{
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))
  val goal = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Goal/goal.json")).mkString
  val goal2 =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Goal/goal2.json")).mkString

  val resourceType = "Goal"

  sequential

  "FHIR Goal Search Service" should {


    "return a bundle for the resources wrt given category parameter"  in {  // token type [System]|[code]
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(goal)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(goal)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(goal)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(goal2)) ~> routes ~> check(())

     Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category=http://snomed.info/sct|289169006" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?category:text=weight+loss" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given identifier parameter" in { // token type like [system]|[value]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?identifier=201509230840" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given subject parameter"  in { // reference type
      // reference type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?subject=Patient/example") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given status parameter"  in { // token type like [code]
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?status=accepted" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?status:not=final" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources wrt given targetdate parameter"  in { // date Type
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?targetdate=2016-01-26") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

  }

}

package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes._
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
class FHIRHistoryServiceSpec extends OnfhirTest  with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))
  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientWithoutId = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString
  val observation = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-example.json")).mkString
  val resourceType = "Patient"
  val resourceId   = "23132"
  val newresourceType = "Observation"
  val newresourceId   = "2222"
  var now=DateTime.now.toIsoDateTimeString

  sequential

  "FHIR History Service" should {
    "return a resource which is created by post operation" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType  , HttpEntity(patientWithoutId)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/_history") ~> routes ~> check {
        println(responseAs[String])
        status === OK
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"method\":\"POST\"")
      }
    }

    "return all versions of the resource with id"  in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"method\":\"PUT\"")
      }
    }

    "return all versions of all resources with given type" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType  , HttpEntity(patientWithoutId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType  , HttpEntity(patientWithoutId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType  , HttpEntity(patientWithoutId)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType   + "/_history") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":7")
      }
    }

    "return last 2 versions of the resource with given id" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history?_count=2") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"versionId\""+":"+"\"3\"")
        responseAs[String] must contain("\"versionId\""+":"+"\"2\"")
        responseAs[String] must not contain("\"versionId\""+":"+"\"1\"")

      }
    }

    "return versions of the resource when updated after some time" in {
      Thread.sleep(1000)
      now=DateTime.now.toIsoDateTimeString
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history?_since="+now) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"versionId\""+":"+"\"4\"")

      }
    }

    "return all the versions of the resource which is deleted" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"method\":\"DELETE\"")

      }
    }
    /*
    "return all the resources on the database" in {
      Put("/" + Config.baseUri + "/" + newresourceType + "/" + newresourceId, HttpEntity(observation)) ~> routes ~> check()
      Put("/" + Config.baseUri + "/" + newresourceType + "/" + newresourceId, HttpEntity(observation)) ~> routes ~> check()
      Put("/" + Config.baseUri + "/" + newresourceType + "/" + newresourceId, HttpEntity(observation)) ~> routes ~> check()
      Get("/" + Config.baseUri + "/_history") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":14")
      }
    }
    */
/*
    "return desired # of resources on the database" in {
      Get("/" + Config.baseUri + "/_history?_count=2") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":14")
        StringUtil.numberOfOccurences(responseAs[String], "fullUrl") === 2
      }
      Get("/" + Config.baseUri + "/_history?_count=4") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":14")
        StringUtil.numberOfOccurences(responseAs[String], "fullUrl") === 4
        responseAs[String] must contain("\"url\":\""+Config.fhirRootUrl+"/_history?_count=4&_page=4\"")
      }
      Get("/" + Config.baseUri + "/_history?_count=17") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":14")
        StringUtil.numberOfOccurences(responseAs[String], "fullUrl") === 14
        responseAs[String] must contain("\"url\":\""+Config.fhirRootUrl+"/_history?_count=17\"") //no page=x as there is only 1 page
      }
    }
*/
    "return desired resources on the database as pages wrt count parameter" in{
      val i=0
      for(i <-  0 to 76){
        Post("/" + OnfhirConfig.baseUri + "/" + resourceType  , HttpEntity(patientWithoutId)) ~> routes ~> check(())
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType   + "/_history") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":86")
        responseAs[String] must contain("\"relation\":\"last\",\"url\":\""+ OnfhirConfig.fhirRootUrl +"/Patient/_history?_page=5\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType   + "/_history?_count=19") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":86")
        responseAs[String] must contain("\"relation\":\"self\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=1\"")
        responseAs[String] must contain("\"relation\":\"first\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=1\"")
        responseAs[String] must contain("\"relation\":\"next\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=2\"")
        responseAs[String] must contain("\"relation\":\"last\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=5\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType   + "/_history?_count=19&_page=5") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":86")
        responseAs[String] must contain("\"relation\":\"self\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=5\"")
        responseAs[String] must contain("\"relation\":\"first\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=1\"")
        responseAs[String] must not contain "\"relation\":\"next\""
        responseAs[String] must contain("\"relation\":\"last\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=5\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType   + "/_history?_count=19&_page=12") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"total\":86")
        responseAs[String] must contain("\"relation\":\"self\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=12\"")
        responseAs[String] must contain("\"relation\":\"first\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=1\"")
        responseAs[String] must not contain "\"relation\":\"next\""
        responseAs[String] must contain("\"relation\":\"last\",\"url\":\""+OnfhirConfig.fhirRootUrl+"/Patient/_history?_count=19&_page=5\"")
        responseAs[String] must contain("\"entry\":[]")
      }
    }

    "return 404 Not Found when there is no resource with given id" in{
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + "11111" + "/_history") ~> routes ~> check {
        status === NotFound
        responseAs[String] must contain("No such resource with type")
      }
    }

    "return 404 Not Found when there is no resource with given type" in{
      Get("/" + OnfhirConfig.baseUri + "/" + "Pat" + "/_history") ~> routes ~> check {
        status === NotFound
        responseAs[String] must contain("is not supported")
      }
    }

    "reject history service for invalid id" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "+" +"/_history") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/3+") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
    }
  }

}

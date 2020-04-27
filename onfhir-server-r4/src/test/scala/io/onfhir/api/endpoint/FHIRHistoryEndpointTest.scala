package io.onfhir.api.endpoint

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{DateTime, HttpEntity}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.`Last-Modified`
import io.onfhir.api.Resource
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JObject}

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRHistoryEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  //Test resources
  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString

  val resourceType = "Patient"
  val resourceId   = "example"

  sequential

  "FHIR History Service" should {
    "return history of a resource for create, update, and delete interactions" in {
      var rid = ""
      var patientResource = ""
      var lastModifiedCreate = ""
      var lastModifiedUpdate = ""
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patient)) ~> routes ~> check {
        status === Created
        val resource = responseAs[Resource]
        rid = FHIRUtil.extractIdFromResource(resource)
        lastModifiedCreate = FHIRUtil.extractValueOptionByPath[String](resource, "meta.lastUpdated").get
        patientResource = resource.toJson
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + rid, HttpEntity(patientResource)) ~> routes ~> check {
        status === OK
        val resource = responseAs[Resource]
        lastModifiedUpdate = FHIRUtil.extractValueOptionByPath[String](resource, "meta.lastUpdated").get
      }
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + rid) ~> routes ~> check {
        status === NoContent
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + rid + "/_history") ~> routes ~> check {
        status === OK
        val bundle = responseAs[Resource]
        (bundle \ "type").extractOpt[String] must beSome("history")
        (bundle \ "total").extractOpt[Int] must beSome(3)

        val createEntry = ((bundle \ "entry") (2)).asInstanceOf[JObject]
        (createEntry \ "request" \ "method").extractOpt[String] must beSome("POST")
        (createEntry \ "request" \ "url").extractOpt[String] must beSome("Patient")
        (createEntry \ "response" \ "lastModified").extractOpt[String] must beSome(lastModifiedCreate)
        (createEntry \ "response" \ "status").extractOpt[String] must beSome("201")
        (createEntry \ "response" \ "etag").extractOpt[String] must beSome("1")

        val updateEntry = ((bundle \ "entry") (1)).asInstanceOf[JObject]
        (updateEntry \ "request" \ "method").extractOpt[String] must beSome("PUT")
        (updateEntry \ "request" \ "url").extractOpt[String] must beSome("Patient/" + rid)
        (updateEntry \ "response" \ "lastModified").extractOpt[String] must beSome(lastModifiedUpdate)
        (updateEntry \ "response" \ "status").extractOpt[String] must beSome("200")
        (updateEntry \ "response" \ "etag").extractOpt[String] must beSome("2")

        val deleteEntry = ((bundle \ "entry") (0)).asInstanceOf[JObject]
        (deleteEntry \ "request" \ "method").extractOpt[String] must beSome("DELETE")
        (deleteEntry \ "request" \ "url").extractOpt[String] must beSome("Patient/" + rid)
        (deleteEntry \ "response" \ "lastModified").extractOpt[String] must beSome
        (deleteEntry \ "response" \ "status").extractOpt[String] must beSome("204")
        (deleteEntry \ "response" \ "etag").extractOpt[String] must beSome("3")
      }
      //Handle paging
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + rid + "/_history?_page=2&_count=2") ~> routes ~> check {
        status === OK
        val bundle = responseAs[Resource]
        (bundle \ "type").extractOpt[String] must beSome("history")
        (bundle \ "total").extractOpt[Int] must beSome(3)

        (bundle \ "entry").asInstanceOf[JArray].arr.length === 1

        (bundle \ "entry" \ "request" \ "method").extract[Seq[String]] must contain("POST")
      }
    }


    "return history of resource type" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status === Created
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/_history") ~> routes ~> check {
        status === OK
        val bundle = responseAs[Resource]
        (bundle \ "type").extractOpt[String] must beSome("history")
        (bundle \ "total").extractOpt[Int] must beSome(4)

        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(resourceId)
      }
    }

    "honor since parameter" in {
      Thread.sleep(1000)
      val now=DateTime.now.toIsoDateTimeString
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history?_since="+now) ~> routes ~> check {
        status === OK
        val bundle = responseAs[Resource]
        (bundle \ "type").extractOpt[String] must beSome("history")
        (bundle \ "total").extractOpt[Int] must beSome(1)

        (bundle \ "entry" \ "resource" \ "meta" \ "versionId").extract[Seq[String]]  === Seq("2")
      }
    }

    "honor at parameter" in {
      Thread.sleep(2000)
      val now=DateTime.now.toIsoDateTimeString
      Thread.sleep(1000)
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check {
        status == OK
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType  + "/_history?_at="+now) ~> routes ~> check {
        status === OK
        val bundle = responseAs[Resource]
        (bundle \ "type").extractOpt[String] must beSome("history")
        (bundle \ "total").extractOpt[Int] must beSome(2)

        val entryPatient = (bundle \ "entry").asInstanceOf[JArray].arr.find(e => (e \ "fullUrl").extract[String].split('/').last == resourceId)
        entryPatient.map(e => (e \  "response" \ "etag").extract[String]) must beSome("2")

        val otherPatient = (bundle \ "entry").asInstanceOf[JArray].arr.find(e => (e \ "fullUrl").extract[String].split('/').last != resourceId)
        otherPatient.map(e => (e \  "response" \ "etag").extract[String]) must beSome("3")
      }
    }


    "return 404 Not Found when there is no resource with given id" in{
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + "11111" + "/_history") ~> routes ~> check {
        status === NotFound
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

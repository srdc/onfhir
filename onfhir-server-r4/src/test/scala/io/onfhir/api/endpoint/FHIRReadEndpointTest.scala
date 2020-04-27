package io.onfhir.api.endpoint

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{DateTime, HttpEntity}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.config.{FhirConfigurationManager, OnfhirConfig}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.{EntityTag, `If-Modified-Since`, `If-None-Match`}
import io.onfhir.api.Resource
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api._

import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JNothing, JNull, JObject, JString}

/***
 * Tests for FHIR read and vread endpoints
 */
@RunWith(classOf[JUnitRunner])
class FHIRReadEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString

  val resourceType = "Patient"
  val resourceId   = "example"

  sequential

  "FHIR Read Endpoint" should {
    "return 404 Not Found for unknown resource type" in {
      Get("/" + OnfhirConfig.baseUri + "/" + "Ali" + "/" + resourceId) ~> routes ~> check {
        status === NotFound
      }
    }
    "return 404 Not Found for unknown resource" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === NotFound
      }
    }

    "return current content of the resource" in {
      var lastModified = ""
      var resource:Resource = null
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check{
        status===Created
        resource = responseAs[Resource]
        lastModified = FHIRUtil.extractValueOptionByPath[String](resource, "meta.lastUpdated").get
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === OK
        checkHeaders(lastModified, "1")
        val resourceRead = responseAs[Resource]
        resource === resourceRead
      }
      //Reading with head
      Head("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === OK
        checkHeaders(lastModified, "1")
        val resourceRead = responseAs[Resource]
        resource === resourceRead
      }
    }

    "return version specific content of the resource" in {
      val updatedPatient = (patient.parseJson.mapField {
        case ("family", _ ) => "family" -> JString("Chalmerson")
        case oth => oth
      }).asInstanceOf[JObject].toJson
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(updatedPatient)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/1") ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        checkIdAndMeta(resource, resourceId, "1")
        checkHeaders(resource, resourceType, resourceId, "1")
        (responseAs[Resource] \ "name" \ "family").extract[Seq[String]].contains("Chalmers")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/2") ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        checkIdAndMeta(resource, resourceId, "2")
        checkHeaders(resource, resourceType, resourceId, "2")
        (responseAs[Resource] \ "name" \ "family").extract[Seq[String]].contains("Chalmerson")
      }
      Head("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/1") ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        checkIdAndMeta(resource, resourceId, "1")
        checkHeaders(resource, resourceType, resourceId, "1")
        (responseAs[Resource] \ "name" \ "family").extract[Seq[String]].contains("Chalmers")
      }
    }

    "return read content of resource for correct mime type" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_format=application/fhir+json") ~> routes ~> check {
        status === OK
        responseEntity.contentType.value === "application/fhir+json; charset=UTF-8"
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/1?_format=application/fhir+xml") ~> routes ~> check {
        status === OK
        responseEntity.contentType.value === "application/fhir+xml; charset=UTF-8"
      }
    }

    "honor If-None-Match header" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-None-Match`(EntityTag("2", weak = true)))) ~> routes ~> check {
        status === NotModified
        responseEntity.getContentLengthOption().getAsLong === 0
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-None-Match`(EntityTag("2", weak = false)))) ~> routes ~> check {
        status === NotModified
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-None-Match`(EntityTag("1", weak = true)))) ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        resource.obj.length === 15
        checkIdAndMeta(resource, resourceId, "2")
        checkHeaders(resource, resourceType, resourceId, "2")
      }
    }
    "honor If-Modified-Since header" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-Modified-Since`(DateTime.now + 1000 * 60 /*1 minute later*/))) ~> routes ~> check {
        status === NotModified
        responseEntity.getContentLengthOption().getAsLong === 0
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId)
        .withHeaders(List(`If-Modified-Since`(DateTime.now - 1000 * 60 /*1 minute before*/))) ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        resource.obj.length === 15
        checkIdAndMeta(resource, resourceId, "2")
        checkHeaders(resource, resourceType, resourceId, "2")
      }
    }

    "return 410 Gone for deleted resource" in {
      Delete("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status == Gone
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId) ~> routes ~> check {
        status === Gone
        header("ETag").get.value === "W/\"3\""
        (responseAs[Resource] \ "resourceType").extractOpt[String] must beSome("OperationOutcome")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/3") ~> routes ~> check {
        status === Gone
        header("ETag").get.value === "W/\"3\""
        (responseAs[Resource] \ "resourceType").extractOpt[String] must beSome("OperationOutcome")
      }
    }

    "reject read operation for invalid id" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "+") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "/_history/3+") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("Invalid identifier")
      }
    }

    "return read content of resource wrt _summary parameter" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(patient)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=true") ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        checkIdAndMeta(resource, resourceId, "4")
        checkHeaders(resource, resourceType, resourceId, "4")
        //Check the SUBSETTED tag
        (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.TAG \ FHIR_COMMON_FIELDS.CODE).extract[Seq[String]] must contain("SUBSETTED")
        (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.TAG \ FHIR_COMMON_FIELDS.SYSTEM).extract[Seq[String]] must contain(FhirConfigurationManager.fhirConfig.FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM)

        //This is not a summary parameter
        (resource \ "contact")  === JNothing
        //This is a summary parameter
        (resource \ "identifier") !== JNothing

        resource.obj.length === 11
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=data") ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        checkIdAndMeta(resource, resourceId, "4")
        checkHeaders(resource, resourceType, resourceId, "4")
        //Check the SUBSETTED tag
        (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.TAG \ FHIR_COMMON_FIELDS.CODE).extract[Seq[String]] must contain("SUBSETTED")
        (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.TAG \ FHIR_COMMON_FIELDS.SYSTEM).extract[Seq[String]] must contain(FhirConfigurationManager.fhirConfig.FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM)

        //Only remove text element
        resource.obj.length === 14
        (resource \ "text")  === JNothing
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=text") ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        checkIdAndMeta(resource, resourceId, "4")
        checkHeaders(resource, resourceType, resourceId, "4")
        //Check the SUBSETTED tag
        (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.TAG \ FHIR_COMMON_FIELDS.CODE).extract[Seq[String]] must contain("SUBSETTED")
        (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.TAG \ FHIR_COMMON_FIELDS.SYSTEM).extract[Seq[String]] must contain(FhirConfigurationManager.fhirConfig.FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM)

        //Only text element
        resource.obj.length === 4
        (resource \ "text")  !== JNothing
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=false") ~> routes ~> check {
        status === OK
        val resource  = responseAs[Resource]
        checkIdAndMeta(resource, resourceId, "4")
        checkHeaders(resource, resourceType, resourceId, "4")

        (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.TAG \ FHIR_COMMON_FIELDS.CODE).extract[Seq[String]] must(not(contain("SUBSETTED")))
        resource.obj.length === 15
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId + "?_summary=invalid") ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("invalid for _summary parameter")
      }
    }
  }

}

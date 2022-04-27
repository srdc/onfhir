package io.onfhir.api.endpoint

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Accept, RawHeader}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.OnfhirConfig
import org.json4s.JsonAST.JObject
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import java.nio.charset.StandardCharsets
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, Future}
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
  val patientWithMeta = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-with-meta.json")).mkString

  val patientXml =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.xml")).mkString
  val patientInvalidXml =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-invalid.xml")).mkString
  val patientNotParsableXml =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-not-parsable.xml")).mkString


  val resourceType = "Patient"
  val resourceId   = "example"

  sequential

  "FHIR Create Endpoint" should {
    "wait a bit" in {
      Await.result(Future.apply(Thread.sleep(5000)), FiniteDuration.apply(6, "seconds"))
      1 === 1
    }
    //Create a new resource given in JSON and XML formats
    "create a new resource" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patientWithoutId)) ~> fhirRoute ~> check {
        eventually(status === Created)

        val response = responseAs[Resource]
        checkIdAndMeta(response, null, "1")
        checkHeaders(response, resourceType, FHIRUtil.extractValue[String](response, "id"), "1")
      }

      Post("/" + OnfhirConfig.baseUri + "/" + resourceType,
          HttpEntity.apply(ContentType.apply(MediaTypes.`application/xml`, () => HttpCharsets.`UTF-8`), patientXml.getBytes(StandardCharsets.UTF_8)))
        .addHeader(Accept.apply(MediaRange.apply(MediaTypes.`application/xml`))) ~> fhirRoute ~> check {
        eventually(status === Created)

        val response = responseAs[Resource]
        checkIdAndMeta(response, null, "1")
        checkHeaders(response, resourceType, FHIRUtil.extractValue[String](response, "id"), "1")
      }
    }

    //Create with a given id, skip it
    "accept create operation for existing id in resource" in{
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patient)) ~> fhirRoute ~> check {
        status === Created
      }
    }
    "reject create operation for non parsable content" in{
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patientNotParsable)) ~> fhirRoute ~> check {
        status === BadRequest
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType,
        HttpEntity.apply(ContentType.apply(MediaTypes.`application/xml`, () => HttpCharsets.`UTF-8`), patientNotParsableXml.getBytes(StandardCharsets.UTF_8)))
        .addHeader(Accept.apply(MediaRange.apply(MediaTypes.`application/xml`))) ~> fhirRoute ~> check {
        status === BadRequest
      }
    }

    "reject create operation for non matched resource type" in{
      Post("/" + OnfhirConfig.baseUri + "/Observation"  , HttpEntity(patient)) ~> fhirRoute ~> check {
        status === BadRequest
      }
    }

    "reject create operation for invalid resource type" in{
      Post("/" + OnfhirConfig.baseUri + "/" + "Patient2", HttpEntity(patientWithoutId)) ~> fhirRoute ~> check {
        status === NotFound
      }
    }

    "handle existing metadata" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType, HttpEntity(patientWithMeta)) ~> fhirRoute ~> check {
        eventually(status === Created)
        val response = responseAs[Resource]

        checkIdAndMeta(response, null, "1")
        checkHeaders(response, resourceType, FHIRUtil.extractValue[String](response, "id"), "1")

        //And does not touch the other metadata
        FHIRUtil.extractValueOptionByPath[String](response, "meta.source").contains("https://example.com") mustEqual true
        FHIRUtil.extractValueOptionByPath[Seq[String]](response, "meta.tag.code").getOrElse(Nil).contains("t1") mustEqual true
        FHIRUtil.extractValueOptionByPath[Seq[String]](response, "meta.tag.system").getOrElse(Nil).contains("http://tag.com") mustEqual true
      }
    }

    "honor Prefer header" in {
      //Should return whole resource
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId))
        .withHeaders(List(RawHeader("Prefer", "return=representation"))) ~> fhirRoute ~> check {
        status === Created
        val response:JObject = responseAs[Resource]
        //All elements
        response.obj.length === 15
      }
      //Should return empty body
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId))
        .withHeaders(List(RawHeader("Prefer", "return=minimal"))) ~> fhirRoute ~> check {
        status === Created
        responseEntity.getContentLengthOption().getAsLong === 0
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(patientWithoutId))
        .withHeaders(List(RawHeader("Prefer", "return=OperationOutcome"))) ~> fhirRoute ~> check {
        status === Created
        val response:JObject = responseAs[Resource]
        FHIRUtil.extractValueOption[String](response,"resourceType").contains("OperationOutcome")
      }
    }
  }
}

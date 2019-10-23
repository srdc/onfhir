package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpEntity, StatusCodes}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnfhirTest
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

/**
  * Created by tuncay on 2/23/2017.
  */
@RunWith(classOf[JUnitRunner])
class FHIRValidationServiceSpec extends OnfhirTest  with FHIREndpoint{
  def actorRefFactory = system

  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(5, TimeUnit.SECONDS))

  final val resourceType = "Patient"
  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patientNotParsable = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-not-parsable.json")).mkString
  val patientInvalid = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-invalid-two-gender.json")).mkString
  val patientWithoutType = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-type.json")).mkString

  sequential

  "FHIR Validation Service" should {
    "should make a validation by direct request (resource in body)" in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "/$validate" , HttpEntity(patient)) ~> routes ~> check {
        status === StatusCodes.OK
        responseAs[String] must contain("All OK")
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "/$validate" , HttpEntity(patientNotParsable)) ~> routes ~> check {
        status === StatusCodes.BadRequest
      }
      /*Post("/" + Config.baseUri + "/" + resourceType + "/$validate" , HttpEntity(patientInvalid)) ~> routes ~> check {
        status === StatusCodes.OK
        responseAs[String] must not contain("All OK")
      }*/
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "/$validate" , HttpEntity(patientWithoutType)) ~> routes ~> check {
        status === StatusCodes.BadRequest
      }
    }

    "should make a validation by direct request (resource in body) with profile" in {
      //Profile not supported
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "/$validate?profile=http://hl7.org/fhir/StructureDefinition/daf-patient" , HttpEntity(patient)) ~> routes ~> check {
        status === StatusCodes.MethodNotAllowed
      }
      //Supported profile
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "/$validate?profile=http://hl7.org/fhir/StructureDefinition/Patient" , HttpEntity(patient)) ~> routes ~> check {
        status === StatusCodes.OK
        responseAs[String] must contain("All OK")
      }
    }

    "should make a validation within Parameters as body" in {
      def wrapWithParameters(resource:String):String = {
        "{" +
          "   \"resourceType\": \"Parameters\"," +
          "   \"parameter\": [{" +
          "        \"name\":\"resource\"," +
          "        \"resource\": " + resource +
          "    }]" +
          "}"
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "/$validate" , HttpEntity(wrapWithParameters(patient))) ~> routes ~> check {
        status === StatusCodes.OK
        responseAs[String] must contain("All OK")
      }
    }
  }

}

package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.RawHeader
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
class FHIRSearchServiceSpec extends OnfhirTest  with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))
  val observation = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-example.json")).mkString
  val observationWithoutSystemField=Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutSystemField.json")).mkString
  val observationWithoutSystemAndId=Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutSystemAndId.json")).mkString
  val observationWithoutId =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observationWithoutId.json")).mkString

  val patient = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-for-include.json")).mkString
  val patient2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-for-include2.json")).mkString
  val practitioner = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Practitioner/Practitioner.json")).mkString
  val relatedPerson = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RelatedPerson/RelatedPerson.json")).mkString

  val allergyIntolerance = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/AllergyIntolerance/AllergyIntolerance1.json")).mkString
  val allergyIntolerance1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/AllergyIntolerance/AllergyIntolerance2.json")).mkString
  val allergyIntolerance2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/AllergyIntolerance/AllergyIntolerance3.json")).mkString

  val resourceType = "Observation"
  val resourceId   = "2222"
  val resourceId2 = "1111"

  val patientId = "1000"
  val patientId2 = "1003"
  val relatedPersonId = "1001"
  val practitionerId = "1002"

  val allergyIntoleranceId = "2000"
  val allergyIntoleranceId1 = "2001"
  val allergyIntoleranceId2 = "2002"

  sequential

  "FHIR Search Service" should {

    "return a 501 not implemented error when a parameter which server doesn't support passed with strict header" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_error=error").withHeaders(RawHeader("Prefer", "handling=strict")) ~> routes ~> check {
        status === NotImplemented
      }
    }

    "ignore erroneous search parameter and execute search when the lenient header is used" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_error=error").withHeaders(RawHeader("Prefer", "handling=lenient")) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must not contain("_error=error")
        responseAs[String] must contain("\"total\":0")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_error=error") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must not contain("_error=error")
        responseAs[String] must contain("\"total\":0")
      }
    }

    "return a bundle with 0 entry for the resource which is not found on the server" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_id=" + resourceId) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("Observation?_id=2222")
        responseAs[String] must contain("\"total\":0")
      }
    }

    "return a bundle with an entry for the resource with given ID"  in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(observation)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId, HttpEntity(observation)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_id=" + resourceId) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources according to given _lastUpdated parameter"  in {
      Thread.sleep(1000)
      val now=DateTime.now.toIsoDateTimeString
      Thread.sleep(3000)
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId2, HttpEntity(observationWithoutSystemField)) ~> routes ~> check{
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + resourceId2, HttpEntity(observationWithoutSystemField)) ~> routes ~> check{
        status === OK
      }
      /*
      Post("/" + Config.baseUri + "/" + resourceType+ "/_search", FormData(Map( "_lastUpdated" ->  now))) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }*/
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_lastUpdated=<" + now) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_lastUpdated=>" + now) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_lastUpdated=le" + now) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_lastUpdated=ge" + now) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources according to given _tag parameter"  in {
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutSystemAndId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutSystemAndId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutId)) ~> routes ~> check(())
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType , HttpEntity(observationWithoutId)) ~> routes ~> check(())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_tag=urn:oid:2.16.840.1.113883.5.100|RP") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":2")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_tag=RP") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_tag=|RP") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Post("/" + OnfhirConfig.baseUri + "/" + resourceType + "/_search", FormData.apply(Map("_tag" -> HttpEntity("RP"))).toEntity())  ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_tag:text=Production") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_tag:not=|RP") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources according to given _security parameter"  in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security=http://hl7.org/fhir/v3/ActCode|DELAU") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security=DELAU") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security=|DELAU") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":2")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security:text=delete+after+use") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":5")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security:not=|DELAU") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a bundle for the resources according to given _profile parameter"  in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_profile=http://ExampleProfile") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_profile=http://ExampleProfile2") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }

      /* _profile is token in FHIR but uri in Power2dm
      Post("/" + Config.baseUri + "/" + resourceType + "/_search" ,  FormData(Map("_profile:not" -> "rp")) ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      */
    }

    "return a bundle for the resources according to the some mixtures of parameters" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_profile=http://ExampleProfile&_security:not=|DELAU") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_profile=http://ExampleProfile&_security:missing=true") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security:not=|DELAU&_lastUpdated=le" + DateTime.now.toIsoDateTimeString) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security:not=|DELAU&_lastUpdated=le" + DateTime.now.toIsoDateTimeString + "&_id=2222") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security:not=|DELAU&_lastUpdated=le" + DateTime.now.toIsoDateTimeString + "&code=3141-9" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":2")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_security:not=|DELAU&_lastUpdated=le" + DateTime.now.toIsoDateTimeString + "&code=3141-9,body-weight" ) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":4")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

    }

    "return a bundle for the resources which order wrt _sort parameter" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_sort=_id") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String].indexOf("\"id\":\"2222\"") must greaterThan(responseAs[String].indexOf("\"id\":\"1111\""))
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_sort:asc=_id") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String].indexOf("\"id\":\"2222\"") must greaterThan(responseAs[String].indexOf("\"id\":\"1111\""))
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_sort:desc=_id&_count=10") ~> routes ~> check {
        print(responseAs[String])
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String].indexOf("\"id\":\"1111\"") must greaterThan(responseAs[String].indexOf("\"id\":\"2222\""))
      }
    }

    "return a bundle contains resources which is arranged according to _summary parameter" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_summary=true") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"code\":\"SUBSETTED\"")
        responseAs[String] must not contain "\"category\":"
        responseAs[String] must contain("\"valueQuantity\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_summary=false") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must not contain "\"code\":\"SUBSETTED\""
        responseAs[String] must contain("\"category\":")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_summary=count") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must not contain "\"entry\""
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_summary=data") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must not contain "\"text\""
        responseAs[String] must contain("\"code\":\"SUBSETTED\"")
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_summary=text") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"code\":\"SUBSETTED\"")
        responseAs[String] must not contain "\"category\":"
      }
    }

    "return a 404 bad request error when wrong _summary parameter passed" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_summary=error") ~> routes ~> check {
        status === BadRequest
      }
    }


    "return a bundle with resources which contains only mandatory fields and fields which are implied by _elements parameter" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_elements=category,text") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":6")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain ("\"category\":")
        responseAs[String] must not contain "\"valueQuantity\""
      }
    }

    "return a bundle with the resources that are queried with _include and _revinclude parameters" in {
      Put("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "/" + allergyIntoleranceId, HttpEntity(allergyIntolerance)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "/" + allergyIntoleranceId1, HttpEntity(allergyIntolerance1)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "/" + allergyIntoleranceId2, HttpEntity(allergyIntolerance2)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + patientId, HttpEntity(patient)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + patientId2, HttpEntity(patient2)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "Practitioner" + "/" + practitionerId, HttpEntity(practitioner)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "RelatedPerson" + "/" + relatedPersonId, HttpEntity(relatedPerson)) ~> routes ~> check(())

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=AllergyIntolerance:patient") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"Patient\"")
        responseAs[String] must not contain("\"resourceType\":\"Practitioner\"")
        responseAs[String] must not contain("\"resourceType\":\"RelatedPerson\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=AllergyIntolerance:reporter&_id=" + allergyIntoleranceId) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"Patient\"")
        responseAs[String] must not contain("\"resourceType\":\"Practitioner\"")
        responseAs[String] must not contain("\"resourceType\":\"RelatedPerson\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=AllergyIntolerance:reporter&_id=" + allergyIntoleranceId1) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"Practitioner\"")
        responseAs[String] must not contain("\"resourceType\":\"Patient\"")
        responseAs[String] must not contain("\"resourceType\":\"RelatedPerson\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=*&_id=" + allergyIntoleranceId1) ~> routes ~> check {
        print(responseAs[String])
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"Practitioner\"")
        responseAs[String] must contain("\"resourceType\":\"Patient\"")
        responseAs[String] must not contain("\"resourceType\":\"RelatedPerson\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=AllergyIntolerance:reporter&_id=" + allergyIntoleranceId2) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"RelatedPerson\"")
        responseAs[String] must not contain("\"resourceType\":\"Patient\"")
        responseAs[String] must not contain("\"resourceType\":\"Practitioner\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=AllergyIntolerance:reporter") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"RelatedPerson\"")
        responseAs[String] must contain("\"resourceType\":\"Patient\"")
        responseAs[String] must contain("\"resourceType\":\"Practitioner\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=AllergyIntolerance:reporter:Patient") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must not contain("\"resourceType\":\"RelatedPerson\"")
        responseAs[String] must contain("\"resourceType\":\"Patient\"")
        responseAs[String] must not contain("\"resourceType\":\"Practitioner\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?_include=AllergyIntolerance:reporter:RelatedPerson") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"RelatedPerson\"")
        responseAs[String] must not contain("\"resourceType\":\"Patient\"")
        responseAs[String] must not contain("\"resourceType\":\"Practitioner\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + "?_revinclude=AllergyIntolerance:reporter&_id=" + patientId) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"AllergyIntolerance\"")
        responseAs[String] must contain ("\"id\":" + "\"" + allergyIntoleranceId + "\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "RelatedPerson" + "?_revinclude=AllergyIntolerance:reporter&_id=" + relatedPersonId) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"AllergyIntolerance\"")
        responseAs[String] must contain ("\"id\":" + "\"" + allergyIntoleranceId2 + "\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Practitioner" + "?_revinclude=AllergyIntolerance:reporter&_id=" + practitionerId) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"AllergyIntolerance\"")
        responseAs[String] must contain ("\"id\":" + "\"" + allergyIntoleranceId1 + "\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + "?_revinclude=AllergyIntolerance:reporter&_include=Patient:link&_id=" + patientId) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
        responseAs[String] must contain("\"mode\":\"include\"")
        responseAs[String] must contain("\"resourceType\":\"AllergyIntolerance\"")
        responseAs[String] must contain ("\"id\":" + "\"" + allergyIntoleranceId + "\"")
        responseAs[String] must contain ("\"id\":" + "\"" + patientId2 + "\"")
      }
    }

    "return a bundle with the resources that are queried using reference chaining" in {
      Put("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "/" + allergyIntoleranceId, HttpEntity(allergyIntolerance)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "/" + allergyIntoleranceId1, HttpEntity(allergyIntolerance1)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "/" + allergyIntoleranceId2, HttpEntity(allergyIntolerance2)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + patientId, HttpEntity(patient)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + patientId2, HttpEntity(patient2)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "Practitioner" + "/" + practitionerId, HttpEntity(practitioner)) ~> routes ~> check(())
      Put("/" + OnfhirConfig.baseUri + "/" + "RelatedPerson" + "/" + relatedPersonId, HttpEntity(relatedPerson)) ~> routes ~> check(())

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?patient.name=Chalmers") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":3")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?patient.name=Chalmers&reporter:Practitioner.identifier=23") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "AllergyIntolerance" + "?reporter.identifier=23") ~> routes ~> check {
        status === BadRequest
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "RelatedPerson" + "?patient.name=Chalmers") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + "?_has:AllergyIntolerance:patient:reporter=Practitioner/1002") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + "?_has:AllergyIntolerance:invalid:reporter=Practitioner/1002") ~> routes ~> check {
        status === BadRequest
      }
    }
  }
    /*
    step {
      MongoDB.getDatabase.drop().head()
    }*/
  }

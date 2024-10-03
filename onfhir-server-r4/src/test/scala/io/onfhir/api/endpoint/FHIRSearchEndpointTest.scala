package io.onfhir.api.endpoint

import java.time.{Instant, LocalDate}
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.IfMatch
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.headers.{EntityTag, EntityTagRange, RawHeader}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import akka.http.scaladsl.model.StatusCodes._
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.util.DateTimeUtil
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.JArray

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRSearchEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory: ActorSystem = system

  implicit def default(implicit system: ActorSystem): RouteTestTimeout = RouteTestTimeout(new FiniteDuration(600, TimeUnit.SECONDS))

  val obsGlucose: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-glucose.json")).mkString
  val obsHemoglobin: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-hemoglobin.json")).mkString
  val obsHemoglobin2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-hemoglobin2.json")).mkString
  val obsCholesterol: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-cholesterol.json")).mkString
  val obsBP: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-bp.json")).mkString
  val obsBP2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-bp2.json")).mkString
  val obsEkg: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-ekg.json")).mkString

  val patient: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString
  val patient2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-with-meta.json")).mkString
  val patient3: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString

  val actdef: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/ActivityDefinition/activity-def.json")).mkString
  val actdef2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/ActivityDefinition/activity-def2.json")).mkString

  val geneticRisk: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RiskAssessment/genetic.json")).mkString
  val cardiacRisk: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RiskAssessment/cardiac.json")).mkString
  val cardiacRisk2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RiskAssessment/cardiac2.json")).mkString
  val cardiacRisk3: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RiskAssessment/cardiac3.json")).mkString
  val cardiacRisk4: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RiskAssessment/cardiac4.json")).mkString
  val breastCancerRisk: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RiskAssessment/breastcancer.json")).mkString
  val breastCancerRisk2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/RiskAssessment/breastcancer2.json")).mkString

  val molseq: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/MolecularSequence/molecular-sequence.json")).mkString

  val questionnaire: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Questionnaire/q.json")).mkString
  val questionnaireResponse: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/QuestionnaireResponse/qr.json")).mkString
  val questionnaireResponse2: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/QuestionnaireResponse/qr2.json")).mkString

  val patientLinked1: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-linked1.json")).mkString
  val patientLinked2: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-linked2.json")).mkString
  val patientLinked3: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-linked3.json")).mkString

  val practitioner: String =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Practitioner/practitioner.json")).mkString


  val resourceType = "Observation"
  val obsGlucoseId = "obsglucose"
  val obsHemoglobinId = "obshemoglobin"
  val obsHemoglobin2Id = "obshemoglobin2"
  val obsCholesterolId = "obscholesterol"
  val obsBpId = "obsbp"
  val obsBp2Id = "obsbp2"
  val obsEkgId = "obsekg"
  val patientId = "example"
  val patient2Id = "example2"
  val actdefId = "actdef"
  val actdef2Id = "actdef2"
  val geneticRiskId = "genetic"
  val cardiacRiskId = "cardiac"
  val cardiacRisk2Id = "cardiac2"
  val cardiacRisk3Id = "cardiac3"
  val cardiacRisk4Id = "cardiac4"
  val breastCancerRiskId = "breastcancer"
  val breastCancerRisk2Id = "breastcancer2"
  val molseqId = "molseq"

  sequential
  "FHIR Search endpoint" should {

    "reject when a parameter which server doesn't support passed with strict header" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_error=error")
        .withHeaders(RawHeader("Prefer", "handling=strict")) ~> fhirRoute ~> check {
        status === NotImplemented
      }
      //Default is also strict
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_error=error") ~> fhirRoute ~> check {
        status === NotImplemented
      }
    }

    "ignore erroneous search parameter and execute search when the lenient header is used" in {
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "?_error=error").withHeaders(RawHeader("Prefer", "handling=lenient")) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, None)
      }
    }

    "honor search on resource id (_id)" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsGlucoseId, HttpEntity(obsGlucose)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsHemoglobinId, HttpEntity(obsHemoglobin)) ~> fhirRoute ~> check {
        status === Created
      }
      var query = "?_id=" + obsGlucoseId
      //Return the resource if exist
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
      }
      //Not return the resource if not exist
      query = "?_id=" + obsGlucoseId + "212"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
    }

    "honor search on resource last update time (_lastUpdated)" in {
      var query = "?_lastUpdated=ge" + LocalDate.now().minusDays(1).toString
      //Return the resource if exist
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
      }
      query = "?_lastUpdated=ge" + DateTimeUtil.serializeInstant(Instant.now())
      //Update one of the resource again, It should not be returned now
      Thread.sleep(1000)
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsHemoglobinId, HttpEntity(obsHemoglobin)) ~> fhirRoute ~> check {
        status === OK
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsHemoglobinId)
      }
      query = "?_lastUpdated=ge" + DateTimeUtil.serializeInstant(Instant.now())
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
    }

    "handle token type search parameters on CodeableConcept" in {
      //Query on CodeableConcept with whole system and code
      var query = "?code=http://loinc.org|15074-8"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }
      //No result
      query = "?code=http://loinc.org|000"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //Query on CodeableConcept with only code
      query = "?code=15074-8"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }
      //Query on CodeableConcept with only code and no system
      query = "?code=|hmgb"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsHemoglobinId)
      }
      //Nor result because there is a system
      query = "?code=|15074-8"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //Query on CodeableConcept with system
      query = "?code=http://loinc.org|"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
      }
      //No result
      query = "?code=http://loinc2.org|"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
    }

    "handle token type search parameter on Identifier" in {
      //Query on identifier with system and value
      var query = "?identifier=http://www.bmc.nl/zorgportal/identifiers/observations|6323"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }
      //No result
      query = "?identifier=http://www.bmc.nl|6323"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //Query on identifier with value only
      query = "?identifier=6323"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }
      //No result
      query = "?identifier=6324"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //Query on identifier with value only and no system
      query = "?identifier=|81912"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }
      //No result
      query = "?identifier=|6323"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      query = "?identifier=http://www.bmc.nl/zorgportal/identifiers/observations|"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
      }
      //No result
      query = "?identifier=http://www.bmc.nl|"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
    }

    "handle token type search parameter on code" in {
      var query = "?status=final"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
      }
      //No result
      query = "?status=preliminary"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
    }

    "handle token type search parameter on boolean" in {
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + patientId, HttpEntity(patient)) ~> fhirRoute ~> check {
        status === Created
      }
      var query = "?active=true"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 1, Some(query))
      }
      //No result
      query = "?active=false"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 0, Some(query))
      }
    }

    "handle token type search parameter on ContactPoint" in {
      var query = "?phone=%2803%29%205555%206473"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 1, Some(query))
      }
      //Negative result
      query = "?phone=%2893%29%205555%206473"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 0, Some(query))
      }
    }

    "handle modifier 'text' for token type search parameters" in {
      //Test text modifier with complete text
      var query = "?code:text=Glucose%20%20in%20blood"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }
      //Test text modifier with partial text (start and case insensitivity)
      query = "?code:text=gluco"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }
      query = "?code:text=hypertension"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //TODO Ignorance of accents or other diacritical marks, punctuation and non-significant whitespace is not supported yet
      /*query = "?code:text=glucose%20in"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType+ query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsGlucoseId)
      }*/
    }

    "handle modifier 'not' for token type" in {
      //Test modifier not
      var query = "?code:not=http://loinc.org|15074-8"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsHemoglobinId)
      }
      query = "?code:not=15074-8"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsHemoglobinId)
      }
      //Also return resources that does not have the element
      query = "?identifier:not=6324"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 2, Some(query))
      }
      //multiple not
      query = "?code:not=http://loinc.org|15074-8,http://loinc.org|718-7"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      query = "?code:not=http://loinc.org|15074-8,http://loinc.org|748-7"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsHemoglobinId)
      }
    }

    "handle modifier 'in' and 'not-in' for token type" in {
      //Test modifier in
      var query = "?code:in=http://hl7.org/fhir/ValueSet/example-extensional"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsCholesterolId, HttpEntity(obsCholesterol)) ~> fhirRoute ~> check {
        status === Created
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(obsCholesterolId)
      }
      //not-in
      query = "?code:not-in=http://hl7.org/fhir/ValueSet/example-extensional"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must not(contain(obsCholesterolId))
      }

      query = "?code:in=http://unknown.com"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === NotImplemented
      }
    }

    "handle modifier 'of-type' for token type" in {
      var query = "?identifier:of-type=http://terminology.hl7.org/CodeSystem/v2-0203|MR|12345"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(patientId)
      }
      //negative result as value does not match
      query = "?identifier:of-type=http://terminology.hl7.org/CodeSystem/v2-0203|MR|12344"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 0, Some(query))
      }
      //negative result as code is not matching
      query = "?identifier:of-type=http://terminology.hl7.org/CodeSystem/v2-0203|MZ|12345"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 0, Some(query))
      }
      //negative result as system is not matching
      query = "?identifier:of-type=http://terminology.hl7.org/CodeSystem/dsfsdf|MR|12345"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 0, Some(query))
      }
      //Only used for Identifiers
      query = "?code:of-type=http://terminology.hl7.org/CodeSystem/dsfsdf|MR|12345"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === BadRequest
      }
    }

    "handle date type search parameters" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsBpId, HttpEntity(obsBP)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsBp2Id, HttpEntity(obsBP2)) ~> fhirRoute ~> check {
        status === Created
      }
      //Date precision query on Period, instant and dateTime and Timing
      var query = "?date=2013-04-02"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId)

      }
      query = "?date=2014-04-02"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))

      }
      //Month precision query on Period, instant and dateTime
      query = "?date=2013-04"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 5, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId, obsHemoglobinId, obsBpId, obsBp2Id)
      }
      query = "?date=2013-03"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))

      }
      //Year precision query on Period, instant and dateTime
      query = "?date=2013"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 5, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId, obsHemoglobinId, obsBpId, obsBp2Id)
      }
      query = "?date=2014"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //Instant precision
      query = "?date=2013-04-02T09:30:05.123+01:00"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId)
      }
      //DateTime precision
      query = "?date=2013-04-02T09:30:05+01:00"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId)
      }
      //DateTime precision with different time zone
      query = "?date=2013-04-02T08:30:05Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId)
      }
      //Create other patients
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + patient2Id, HttpEntity(patient2)) ~> fhirRoute ~> check {
        status === Created
      }
      Post("/" + OnfhirConfig.baseUri + "/" + "Patient", HttpEntity(patient3)) ~> fhirRoute ~> check {
        status === Created
      }

      query = "?birthdate=1974-12-25"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(patientId)
      }

      query = "?birthdate=1974-12"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(patientId)
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must not(contain(patient2Id))
      }
      query = "?birthdate=1974"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(patientId)
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(patient2Id)
      }
    }

    "handle prefixes for date type search parameters" in {
      //Testing ne
      var query = "?birthdate=ne1974"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      query = "?birthdate=ne1974-12"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]] must contain(patient2Id)
      }
      query = "?date=ne2013-04-02T08:30:05Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId, obsBpId, obsBp2Id)
      }
      query = "?date=ne2013-04-02"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId, obsBpId, obsBp2Id)
      }
      //Testing gt
      query = "?date=gt2013-04-02T08:30:11Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId, obsBpId, obsBp2Id)
      }
      // the range above the search value intersects (i.e. overlaps) with the range of the target value
      // 8:30:00 - 8:30:59.999 above this is 8:31 which does not intersect with 8:30:10
      query = "?date=gt2013-04-02T08:30Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId, obsBpId, obsBp2Id)
      }
      //Testing lt - the range below the search value intersects (i.e. overlaps) with the range of the target value
      query = "?date=lt2013-04-02T08:30:11Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId, obsBpId)
      }
      query = "?date=lt2013-04-02T08:30Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsBpId)
      }

      //Testing ge and le
      query = "?date=ge2013-04-02T08:30Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 4, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId, obsBpId, obsHemoglobinId)
      }
      query = "?date=le2013-04-02T08:30Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId, obsBpId)
      }
      //Testing sa
      query = "?date=sa2013-04-03"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId)
      }
      query = "?date=sa2013-04-01"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 5, Some(query))
      }
      //Testing eb
      query = "?date=eb2013-04-03"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId)
      }
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsHemoglobin2Id, HttpEntity(obsHemoglobin2)) ~> fhirRoute ~> check {
        status === Created
      }
      query = "?date=eb2013-04-07"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 5, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId, obsCholesterolId, obsBpId, obsBp2Id, obsHemoglobinId)
      }
      //Testing ap (%10 approximation)
      query = "?date=ap2013-04-02T08:29Z"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsCholesterolId, obsGlucoseId)
      }
    }

    "handle quantity type search parameters" in {
      var query = "?value-quantity=72"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsCholesterolId)
      }
      //Test minus
      query = "?value-quantity=-6.3"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsGlucoseId)
      }

      //Query with unit and system
      query = "?value-quantity=72|http://unitsofmeasure.org|mg/dL"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsCholesterolId)
      }
      query = "?value-quantity=72|http://unitsofmeasure.org|mg"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      query = "?value-quantity=72||mg/dL"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsCholesterolId)
      }
      //Test precision
      query = "?value-quantity=7.5||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobin2Id)
      }
      query = "?value-quantity=7.49||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobin2Id)
      }
      query = "?value-quantity=749.4e-2||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobin2Id)
      }
      query = "?value-quantity=7.493||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
    }

    "handle prefixes for quantity type search parameters" in {
      //Test ge, gt
      var query = "?value-quantity=ge7.49||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobin2Id)
      }
      query = "?value-quantity=gt7.496||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //Test le, lt
      query = "?value-quantity=le7.4945||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId, obsHemoglobin2Id)
      }
      query = "?value-quantity=lt7.494||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId)
      }
      //Test ap
      query = "?value-quantity=ap8||g/dl"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId, obsHemoglobin2Id)
      }
      //Test query on SampledData
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + obsEkgId, HttpEntity(obsEkg)) ~> fhirRoute ~> check {
        status === Created
      }
      query = "?value-quantity=gt-1000||U"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsEkgId)
      }
      query = "?value-quantity=sa-4000||U"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsEkgId)
      }
      query = "?value-quantity=eb2500||U"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      //Test query on Range
      Put("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + "/" + actdefId, HttpEntity(actdef)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + "/" + actdef2Id, HttpEntity(actdef2)) ~> fhirRoute ~> check {
        status === Created
      }
      query = "?context-quantity=gt10||a"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdefId, actdef2Id)
      }
      query = "?context-quantity=sa10||a"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdefId, actdef2Id)
      }
      query = "?context-quantity=eb19||a"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdef2Id)
      }
    }

    "handle reference type search parameters" in {
      var query = "?patient=Patient/example"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 4, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsBpId, obsBp2Id, obsCholesterolId, obsGlucoseId)
      }
      //Test with type given as type modifier
      query = "?patient:Patient=example"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 4, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsBpId, obsBp2Id, obsCholesterolId, obsGlucoseId)
      }
      //Test direct id query
      query = "?encounter=e001"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsEkgId)
      }
      //Not allowed as the search parameter has two target types Patient,Group
      query = "?patient=example"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === BadRequest
      }
      query = "?encounter=Encounter/e001"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsEkgId)
      }
      //Test query with full url
      query = s"?encounter=${OnfhirConfig.fhirRootUrl}/Encounter/e001"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsEkgId)
      }
      query = s"?encounter=http://example.com/Encounter/e001"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 0, Some(query))
      }
      query = s"?organization=http://example.com/fhir/Organization/1"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Patient", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(patientId)
      }
      //Test query on versioned references
      query = s"?encounter=Encounter/e002"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobinId)
      }
      //Test query on canonical without version
      query = s"?depends-on=Library/zika-virus-intervention-logic"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdefId, actdef2Id)
      }
      //Test query on canonical with version
      query = s"?depends-on=Library/zika-virus-intervention-logic|1.2"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdef2Id)
      }
      //Test query on canonical with version with below
      query = s"?depends-on:below=Library/zika-virus-intervention-logic|1"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdefId, actdef2Id)
      }

      //Test 'identifier' modifier
      query = s"?based-on:identifier=https://acme.org/identifiers|1234"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsBpId, obsBp2Id)
      }
    }

    "handle number type search parameters on decimals" in {
      Put("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + "/" + geneticRiskId, HttpEntity(geneticRisk)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + "/" + cardiacRiskId, HttpEntity(cardiacRisk)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + "/" + breastCancerRiskId, HttpEntity(breastCancerRisk)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + "/" + breastCancerRisk2Id, HttpEntity(breastCancerRisk2)) ~> fhirRoute ~> check {
        status === Created
      }

     var query = s"?probability=2e-2"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(cardiacRiskId)
      }
      //Test precision query e.g. 99.5 - 100.5
      query = s"?probability=100"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(breastCancerRiskId)
      }
      //Test precision query e.g. 99.95 - 100.05
      query = s"?probability=100.0"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 0, Some(query))
      }
      //Test precision query e.g. 95 - 105
      query = s"?probability=1e2"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(breastCancerRiskId, breastCancerRisk2Id)
      }
      //Test precision query e.g. 95 - 105
      query = s"?probability=0.1e3"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(breastCancerRiskId, breastCancerRisk2Id)
      }
      query = s"?probability=99"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 0, Some(query))
      }
      //Test precision query e.g. [0.001325, 0.001335)
      query = s"?probability=0.00133"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(geneticRiskId)
      }
      //Test precision query e.g. [0.001325, 0.001335)
      query = s"?probability=1.33e-3"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(geneticRiskId)
      }
      //Test precision query e.g. [0.001325, 0.001335)
      query = s"?probability=0.001330"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 0, Some(query))
      }
    }

    "handle prefixes for number type search parameters" in {
      //Test ne with precision not in 99.5 - 100.5
      var query = s"?probability=ne100"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(breastCancerRisk2Id, cardiacRiskId, geneticRiskId)
      }
      query = s"?probability=ne100.0"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 4, Some(query))
      }
      //Test comparisons
      query = s"?probability=ge99.7"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(breastCancerRiskId)
      }
      query = s"?probability=gt99.7"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 0, Some(query))
      }
      query = s"?probability=sa99.7"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 0, Some(query))
      }
      query = s"?probability=lt99.69"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 3, Some(query))
      }
      query = s"?probability=eb99.69"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 3, Some(query))
      }
      query = s"?probability=le99.7"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 4, Some(query))
      }
      //Test ap
      query = s"?probability=ap90"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(breastCancerRisk2Id)
      }
    }

    "handle number type search parameters on Range and integer" in{
      Put("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + "/" + cardiacRisk2Id, HttpEntity(cardiacRisk2)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + "/" + cardiacRisk3Id, HttpEntity(cardiacRisk3)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + "/" + cardiacRisk4Id, HttpEntity(cardiacRisk4)) ~> fhirRoute ~> check {
        status === Created
      }

      //Test on Range range should contain the target range
      var query =  s"?probability=26"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(cardiacRisk2Id)
      }
      query =  s"?probability=ne26"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 6, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet should not(contain(cardiacRisk2Id))
      }
      query =  s"?probability=gt25"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 5, Some(query))
      }
      query =  s"?probability=gt45"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet should not(contain(cardiacRisk4Id))
      }
      query =  s"?probability=le20"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 4, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet should contain(cardiacRisk3Id, cardiacRisk4Id)
      }
      query =  s"?probability=sa25.5"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(breastCancerRiskId, breastCancerRisk2Id)
      }
      query =  s"?probability=sa25"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(cardiacRisk2Id, breastCancerRiskId, breastCancerRisk2Id)
      }
      query =  s"?probability=eb45"
      Get("/" + OnfhirConfig.baseUri + "/" + "RiskAssessment" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 3, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(cardiacRiskId, cardiacRisk2Id, geneticRiskId)
      }

      Put("/" + OnfhirConfig.baseUri + "/" + "MolecularSequence" + "/" + molseqId, HttpEntity(molseq)) ~> fhirRoute ~> check {
        status === Created
      }
      //Test exact match
      query =  s"?variant-start=22125503"
      Get("/" + OnfhirConfig.baseUri + "/" + "MolecularSequence" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(molseqId)
      }
      //If number is given directly, it should not match
      query =  s"?variant-start=22125500"
      Get("/" + OnfhirConfig.baseUri + "/" + "MolecularSequence" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 0, Some(query))
      }
      //If number is given in exponential, it should  match 22125450 - 22125550
      query =  s"?variant-start=221255e2"
      Get("/" + OnfhirConfig.baseUri + "/" + "MolecularSequence" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "RiskAssessment", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(molseqId)
      }
    }

    "handle uri type search parameters" in {
      //Test exact url match
      var query = "?url=http://example.org/ActivityDefinition/administer-zika-virus-exposure-assessment"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdefId)
      }
      //Should be case sensitive
      query = "?url=http://example.org/ActivityDefinition/Administer-zika-virus-exposure-assessment"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 0, Some(query))
      }
      //Test below
      query = "?url:below=http://example.org"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 2, Some(query))
      }
      query = "?url:below=http://example.org/Activity"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 0, Some(query))
      }
      //Test above
      query = "?url:above=http://example.org/ActivityDefinition/administer-zika-virus-exposure-assessment/_history/5"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdefId, actdef2Id)
      }
      query = "?url=http://example.org/ActivityDefinition/administer-zika-virus-exposure-assessment|1"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(actdefId)
      }
    }

    "handle composite type search parameters" in {
      //No common path between parameters
      var query = "?code-value-quantity=http://loinc.org|718-7$gt7.3|http://unitsofmeasure.org|g/dL"
      Get("/" + OnfhirConfig.baseUri + "/" + "Observation" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Observation", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsHemoglobin2Id)
      }
      //multiple paths
      query = "?combo-code-value-quantity=http://loinc.org|8480-6$gt108"
      Get("/" + OnfhirConfig.baseUri + "/" + "Observation" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Observation", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsBp2Id)
      }
      //common paths
      query = "?component-code-value-quantity=http://loinc.org|8480-6$gt108|http://unitsofmeasure.org|mm[Hg]"
      Get("/" + OnfhirConfig.baseUri + "/" + "Observation" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Observation", 1, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsBp2Id)
      }
    }

    "handle a parameter which has restrictions on path" in {
      //depends-on also checks type of relation
      var query = s"?depends-on=https://www.cdc.gov/zika/hc-providers/pregnant-woman.html"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 0, Some(query))
      }

      query = s"?depends-on=Questionnaire/zika-virus-exposure-assessment"
      Get("/" + OnfhirConfig.baseUri + "/" + "ActivityDefinition" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "ActivityDefinition", 2, Some(query))
      }
    }

    "handle include parameter (no iteration) on reference" in {
      //Query on CodeableConcept with only code and include
      val query = "?code=15074-8&_include=Observation:patient"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq(obsGlucoseId))

        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq(patientId))
      }
    }

    "handle include parameter (no iteration) on canonical" in {
      Put("/" + OnfhirConfig.baseUri + "/" + "Questionnaire" + "/" + "gcs", HttpEntity(questionnaire)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "QuestionnaireResponse" + "/" + "gcsr", HttpEntity(questionnaireResponse)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "QuestionnaireResponse" + "/" + "gcsr2", HttpEntity(questionnaireResponse2)) ~> fhirRoute ~> check {
        status === Created
      }
      //Query on CodeableConcept with only code and include
      var query = "?_id=gcsr&_include=QuestionnaireResponse:questionnaire"
      Get("/" + OnfhirConfig.baseUri + "/" + "QuestionnaireResponse" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq("gcsr"))

        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq("gcs"))
      }

      query = "?_id=gcsr2&_include=QuestionnaireResponse:questionnaire"
      Get("/" + OnfhirConfig.baseUri + "/" + "QuestionnaireResponse" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq("gcsr2"))

        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq("gcs"))
      }

      query = "?_include=QuestionnaireResponse:questionnaire"
      Get("/" + OnfhirConfig.baseUri + "/" + "QuestionnaireResponse" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 2, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]).toSet must be_==(Set("gcsr","gcsr2"))

        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq("gcs"))
      }
  }

    "handle include parameter (with iteration) on reference" in {
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + "link1", HttpEntity(patientLinked1)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + "link2", HttpEntity(patientLinked2)) ~> fhirRoute ~> check {
        status === Created
      }
      Put("/" + OnfhirConfig.baseUri + "/" + "Patient" + "/" + "link3", HttpEntity(patientLinked3)) ~> fhirRoute ~> check {
        status === Created
      }
      //Query on CodeableConcept with only code and include
      val query = "?code=15074-8&_include=Observation:patient&_include:iterate=Patient:link"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq(obsGlucoseId))

        (bundle \ "entry").asInstanceOf[JArray].arr
          .count(e => (e \ "search" \ "mode").extract[String] == "include") === 4

        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]).toSet must be_==(Set(patientId, "link1", "link2", "link3"))
      }
    }

    "handle include parameter on reference with *" in {
      Put("/" + OnfhirConfig.baseUri + "/" + "Practitioner" + "/" + "pr1", HttpEntity(practitioner)).withHeaders(IfMatch.create(EntityTagRange.apply(EntityTag.apply("0", true)))) ~> fhirRoute ~> check {
        status === Created
      }
      val query = "?code=15074-8&_include=Observation:*"
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq(obsGlucoseId))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]).toSet must be_==(Set(patientId, "pr1"))
      }
    }

    "handle _revinclude parameter (no iteration) on reference" in {
      val query = "?_id=example&_revinclude=Observation:patient"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq(patientId))

        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]).toSet must be_==(Set(obsGlucoseId, obsCholesterolId, obsBpId, obsBp2Id))
      }
    }

    "handle _revinclude parameter (no iteration) on canonical" in {
      val query = "?_revinclude=QuestionnaireResponse:questionnaire"
      Get("/" + OnfhirConfig.baseUri + "/" + "Questionnaire" + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, resourceType, 1, Some(query))
        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "match" )
          .map(e => (e \ "resource" \ "id").extract[String]) must be_==(Seq("gcs"))

        (bundle \ "entry").asInstanceOf[JArray].arr
          .filter(e => (e \ "search" \ "mode").extract[String] == "include" )
          .map(e => (e \ "resource" \ "id").extract[String]).toSet must be_==(Set("gcsr"))
      }
    }

  }

  "FHIR Compartment Search endpoint" should {
    "handle compartment search" in {
      val query = "?code=85354-9"
      Get("/" + OnfhirConfig.baseUri + "/" + "Patient/example/Observation" +  query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Observation", 2, Some(query))
        (bundle \ "entry" \ "resource" \ "id").extract[Seq[String]].toSet === Set(obsBpId, obsBp2Id)
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Patient/example22/Observation" +  query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "Observation", 0, Some(query))
      }
    }

//    "handle compartment search for all resources in the compartment" in {
//      val query = "?_sort=-_lastUpdated"
//      Get("/" + OnfhirConfig.baseUri + "/" + "Patient/example/*" +  query) ~> fhirRoute ~> check {
//        status === OK
//        val bundle = responseAs[Resource]
//        checkSearchResult(bundle, "", 7, Some(query))
//        val rtypes = (bundle \ "entry" \ "resource" \ "resourceType").extract[Seq[String]]
//        rtypes.count(_ == "Observation") mustEqual 4
//        rtypes.count(_ == "RiskAssessment") mustEqual 2
//        rtypes.count(_ == "MolecularSequence") mustEqual 1
//      }
//    }
//
//    "handle compartment search for some resource types in the compartment" in {
//      val query = "?_type=RiskAssessment,MolecularSequence&_sort=-_lastUpdated"
//      Get("/" + OnfhirConfig.baseUri + "/" + "Patient/example/*" +  query) ~> fhirRoute ~> check {
//        status === OK
//        val bundle = responseAs[Resource]
//        checkSearchResult(bundle, "", 3, Some(query))
//        val rtypes = (bundle \ "entry" \ "resource" \ "resourceType").extract[Seq[String]]
//        rtypes.count(_ == "RiskAssessment") mustEqual 2
//        rtypes.count(_ == "MolecularSequence") mustEqual 1
//      }
//    }
  }
/* THIS PART IS COMMENTED OUT AS system level search requires Mongo 4.4 and embedded mongo does not have it yet
  "FHIR System Level Search endpoint" should {
    "handle search on multiple resource types without any query" in {
      val query = "?_type=Observation,RiskAssessment"
      Get("/" + OnfhirConfig.baseUri + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "", 14, Some(query))
        val rtypes = (bundle \ "entry" \ "resource" \ "resourceType").extract[Seq[String]]
        rtypes.count(_ == "Observation") mustEqual 7
        rtypes.count(_ == "RiskAssessment") mustEqual 7
      }
    }
    "handle search on multiple resource types with a common parameter" in {
      val query="?_type=Observation,RiskAssessment&date=ge2015"
      Get("/" + OnfhirConfig.baseUri + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "", 4, Some(query))
        val rtypes = (bundle \ "entry" \ "resource" \ "resourceType").extract[Seq[String]]
        rtypes.count(_ == "Observation") mustEqual 2
        rtypes.count(_ == "RiskAssessment") mustEqual 2
      }
    }
    "handle search on multiple resource types with sorting on a common parameter" in {
      var query="?_type=Observation,RiskAssessment&date=ge2015&_sort=-_lastUpdated"
      Get("/" + OnfhirConfig.baseUri + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "", 4, Some(query))
        val rtypes = (bundle \ "entry" \ "resource" \ "resourceType").extract[Seq[String]]
        rtypes.count(_ == "Observation") mustEqual 2
        rtypes.count(_ == "RiskAssessment") mustEqual 2

        ((bundle \ "entry")(0) \ "resource" \ "resourceType").extract[String] mustEqual("RiskAssessment")
        ((bundle \ "entry")(1) \ "resource" \ "resourceType").extract[String] mustEqual("RiskAssessment")
      }

      query="?_type=Observation,RiskAssessment&date=ge2015&_sort=date"
      Get("/" + OnfhirConfig.baseUri + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "", 4, Some(query))
        val rtypes = (bundle \ "entry" \ "resource" \ "resourceType").extract[Seq[String]]
        rtypes.count(_ == "Observation") mustEqual 2
        rtypes.count(_ == "RiskAssessment") mustEqual 2

        ((bundle \ "entry")(0) \ "resource" \ "resourceType").extract[String] mustEqual("Observation")
        ((bundle \ "entry")(1) \ "resource" \ "resourceType").extract[String] mustEqual("Observation")
      }
    }

    "handle search on multiple resource types with paging" in {
      val query = "?_type=Observation,RiskAssessment&date=ge2015&_sort=-_lastUpdated&_count=2&_page=2"
      Get("/" + OnfhirConfig.baseUri + query) ~> fhirRoute ~> check {
        status === OK
        val bundle = responseAs[Resource]
        checkSearchResult(bundle, "", 4, Some(query))
        val rtypes = (bundle \ "entry" \ "resource" \ "resourceType").extract[Seq[String]]
        rtypes.count(_ == "Observation") mustEqual 2
        rtypes.count(_ == "RiskAssessment") mustEqual 0
      }
    }

    "reject search on multiple types if query parameter is not common" in {
      val query="?_type=Observation,RiskAssessment&device=Device/123"
      Get("/" + OnfhirConfig.baseUri + query) ~> fhirRoute ~> check {
        status === BadRequest
      }
    }
    "reject search on multiple types if sorting parameter is not common" in {
      val query="?_type=Observation,RiskAssessment&_sort=device"
      Get("/" + OnfhirConfig.baseUri + query) ~> fhirRoute ~> check {
        status === BadRequest
      }
    }
  }*/


}

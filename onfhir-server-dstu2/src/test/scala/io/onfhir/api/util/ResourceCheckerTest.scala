package io.onfhir.api.util

import io.onfhir.OnfhirTest
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import io.onfhir.util.JsonFormatter._

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class ResourceCheckerTest extends OnfhirTest {

  val observation = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-example.json")).mkString.parseJson

  sequential
  "ResourceChecker" should {
   "handle compartment query for positive results" in {
      var query = FHIRSearchParameterValueParser.constructCompartmentSearchParameter("Patient", "example", "Observation")
      ResourceChecker.checkIfResourceSatisfies("Observation", List(query), observation)
    }


    "handle reference query for positive results" in {
      var query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("subject" -> List("Patient/example")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation)

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("subject" -> List("http://127.0.0.1:8080/fhir/Patient/example")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation)
    }

    "handle reference query for negative results" in {
      var query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("subject" -> List("Patient/example2")))
      !ResourceChecker.checkIfResourceSatisfies("Observation", query, observation)

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("subject" -> List("Device/example")))
      !ResourceChecker.checkIfResourceSatisfies("Observation", query, observation)
    }

    "handle token query for positive results" in {
      var query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("27113001")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual true

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("http://snomed.info/sct|27113001")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual true

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("|deneme")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual true

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("http://snomed.info/sct|")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual true

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code:text" -> List("Body Weight")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual true

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code:not" -> List("27113002")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual true

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code:not" -> List("http://snomed.info/sct|27113002")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual true
    }

    "handle token query for negative results" in {
      var query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("27113002")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual false

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("http://snomed.info/sc|27113001")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual false

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("|deneme2")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual false

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code" -> List("http://snomed.info/sc|")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual false

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code:text" -> List("Body Weight22")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual false

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code:not" -> List("27113001")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual false

      query = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("code:not" -> List("http://snomed.info/sct|27113001")))
      ResourceChecker.checkIfResourceSatisfies("Observation", query, observation) mustEqual false

    }


  }
}

package io.onfhir.mapping
import java.time.{Instant, LocalDate, Year, ZonedDateTime}
import java.time.temporal.{ChronoField, ChronoUnit, TemporalAdjuster, TemporalAdjusters, TemporalUnit}

import io.onfhir.mapping.engine.{MappingEngine, MappingUtilityWithSuppliedMaps}
import io.onfhir.mapping.model.StructureMappingException
import io.onfhir.mapping.parsers.BaseStructureMapParser
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JString}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class MappingEngineTest  extends Specification {

  val simpleMapSource = Source.fromInputStream(getClass.getResourceAsStream("/sources/activity-definition-1.json")).mkString.parseJson
  val simpleMap= Source.fromInputStream(getClass.getResourceAsStream("/structuremaps/mapping-to-medication-request.json")).mkString.parseJson
  val simpleMapUrl = "http://onfhir.io/fhir/StructureMap/medicationrequest-transform"

  val simpleFailedMap1= Source.fromInputStream(getClass.getResourceAsStream("/structuremaps/mapping-to-medication-request-failed1.json")).mkString.parseJson
  val simpleFailedMap1Url = "http://onfhir.io/fhir/StructureMap/medicationrequest-transform-failed1"
  val simpleFailedMap2= Source.fromInputStream(getClass.getResourceAsStream("/structuremaps/mapping-to-medication-request-failed2.json")).mkString.parseJson
  val simpleFailedMap2Url = "http://onfhir.io/fhir/StructureMap/medicationrequest-transform-failed2"
  val simpleFailedMap3= Source.fromInputStream(getClass.getResourceAsStream("/structuremaps/mapping-to-medication-request-failed3.json")).mkString.parseJson
  val simpleFailedMap3Url = "http://onfhir.io/fhir/StructureMap/medicationrequest-transform-failed3"

  val dependentMap= Source.fromInputStream(getClass.getResourceAsStream("/structuremaps/mapping-to-medication-request-dependent.json")).mkString.parseJson
  val dependentMapUrl = "http://onfhir.io/fhir/StructureMap/medicationrequest-transform-dependent"

  val dependentImportMap = Source.fromInputStream(getClass.getResourceAsStream("/structuremaps/mapping-to-medication-request-dependent-import.json")).mkString.parseJson
  val dependentImportMapUrl = "http://onfhir.io/fhir/StructureMap/medicationrequest-transform-dependent-import"

  val importedMap = Source.fromInputStream(getClass.getResourceAsStream("/structuremaps/dose-import.json")).mkString.parseJson
  val importedMapUrl = "http://onfhir.io/fhir/StructureMap/dose-import"


  val mappingUtility = new MappingUtilityWithSuppliedMaps(
    Map(
      simpleMapUrl ->simpleMap,
      simpleFailedMap1Url -> simpleFailedMap1,
      simpleFailedMap2Url -> simpleFailedMap2,
      simpleFailedMap3Url -> simpleFailedMap3,
      dependentMapUrl -> dependentMap,
      dependentImportMapUrl-> dependentImportMap,
      importedMapUrl -> importedMap
    ), new BaseStructureMapParser)
  val mappingEngine = new MappingEngine(mappingUtility)

  sequential

  "Mapping Engine" should {

    "handle a simple one to one mapping" in {
      val medicationRequest = mappingEngine.mapStructure(simpleMapUrl, simpleMapSource)
      //Testing resource type creation
      FhirPathEvaluator().evaluateString("resourceType", medicationRequest).headOption must beSome("MedicationRequest")
      //Testing fixed FHIR path evaluations
      FhirPathEvaluator().evaluateString("status", medicationRequest).headOption must beSome("draft")
      FhirPathEvaluator().evaluateString("category", medicationRequest).headOption must beSome("proposal")
      // Testing copy operation but not exist in source
      FhirPathEvaluator().evaluateString("priority", medicationRequest).headOption must beEmpty
      // Testing copy operation
      FhirPathEvaluator().evaluateString("medicationReference.reference", medicationRequest).headOption must beSome("Medication/citalopramMedication")
      // Testing copy single to array
      (medicationRequest \ "instantiatesCanonical") mustEqual JArray(List(JString("http://motivemi.com/artifacts/ActivityDefinition/citalopramPrescription")))
      // Testing hierarchy of rules and variables (only copying route under ActivityDefinition.dosage)
      FhirPathEvaluator().evaluateString("dosageInstruction.route.coding.code", medicationRequest) mustEqual Seq("26643006")
      FhirPathEvaluator().evaluateString("dosageInstruction.text",medicationRequest) mustEqual Nil
      //Testing FHIR path evaluation with time function
      FhirPathEvaluator().evaluateDateTime("authoredOn",medicationRequest).until(ZonedDateTime.now(), ChronoUnit.SECONDS) < 60
      //Testing FHIR path evaluation on source content
      FhirPathEvaluator().evaluateString("performerType.text",medicationRequest) mustEqual Seq("patient")
      //Testing source constraints min, max, condition, check for positive
      FhirPathEvaluator().evaluateString("reasonCode.coding.code",medicationRequest) mustEqual Seq("87512008", "40379007", "225444004", "306206005")
    }

    "handle a simple one to one mapping for failed mappings due to source constraints" in {
      //Due to min constraint
      mappingEngine.mapStructure(simpleFailedMap1Url, simpleMapSource) must throwA[StructureMappingException]()
      //Due to max constraint
      mappingEngine.mapStructure(simpleFailedMap2Url, simpleMapSource) must throwA[StructureMappingException]()
      //Due to check constraint
      mappingEngine.mapStructure(simpleFailedMap3Url, simpleMapSource) must throwA[StructureMappingException]()
    }

    "handle dependent groups in mappings" in {
      //Due to min constraint
      val medicationRequest = mappingEngine.mapStructure(dependentMapUrl, simpleMapSource)
      FhirPathEvaluator().evaluateString("dosageInstruction.route.coding.code", medicationRequest) mustEqual Seq("26643006")
      FhirPathEvaluator().evaluateString("dosageInstruction.text",medicationRequest) mustEqual Nil
    }

    "handle dependent groups in mappings within imports" in {
      //Due to min constraint
      val medicationRequest = mappingEngine.mapStructure(dependentImportMapUrl, simpleMapSource)
      FhirPathEvaluator().evaluateString("dosageInstruction.route.coding.code", medicationRequest) mustEqual Seq("26643006")
      FhirPathEvaluator().evaluateString("dosageInstruction.text",medicationRequest) mustEqual Nil
    }

  }
}

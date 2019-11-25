package io.onfhir.validation

import io.onfhir.api.util.IOUtil
import io.onfhir.config.FhirConfigurationManager
import io.onfhir.r4.config.R4Configurator
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class ProfileValidationTest extends Specification {
  //Initialize the environment
  val resourceProfiles = IOUtil.readStandardBundleFile("profiles-resources.json", Set("StructureDefinition")).flatMap(StructureDefinitionParser.parseProfile)
  val dataTypeProfiles = IOUtil.readStandardBundleFile("profiles-types.json", Set("StructureDefinition")).flatMap(StructureDefinitionParser.parseProfile)

  FhirConfigurationManager.initialize(new R4Configurator)
  FhirConfigurationManager.fhirConfig.profileRestrictions = (dataTypeProfiles ++ resourceProfiles).map(p => p.url -> p).toMap



  sequential
  "ProfileValidation" should {
    "validate a valid FHIR resource against base definitions" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/r4/valid/observation-glucose.json")).mkString).asInstanceOf[JObject]
      var issues = FhirContentValidator.validateComplexContentAgainstProfile("http://hl7.org/fhir/StructureDefinition/Observation", observation, None, Nil)
      issues.isEmpty mustEqual(true)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/r4/valid/observation-erythrocyte.json")).mkString).asInstanceOf[JObject]
      issues = FhirContentValidator.validateComplexContentAgainstProfile("http://hl7.org/fhir/StructureDefinition/Observation", observation, None, Nil)
      issues.isEmpty mustEqual(true)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/r4/valid/observation-bp.json")).mkString).asInstanceOf[JObject]
      issues = FhirContentValidator.validateComplexContentAgainstProfile("http://hl7.org/fhir/StructureDefinition/Observation", observation, None, Nil)
      issues.isEmpty mustEqual(true)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/r4/valid/observation-group.json")).mkString).asInstanceOf[JObject]
      issues = FhirContentValidator.validateComplexContentAgainstProfile("http://hl7.org/fhir/StructureDefinition/Observation", observation, None, Nil)
      issues.isEmpty mustEqual(true)


      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/r4/valid/observation-reject.json")).mkString).asInstanceOf[JObject]
      issues = FhirContentValidator.validateComplexContentAgainstProfile("http://hl7.org/fhir/StructureDefinition/Observation", observation, None, Nil)
      issues.isEmpty mustEqual(true)
    }

    "not validate an invalid FHIR resource against base definitions" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/r4/invalid/observation-invalid-cardinality.json")).mkString).asInstanceOf[JObject]
      var issues = FhirContentValidator.validateComplexContentAgainstProfile("http://hl7.org/fhir/StructureDefinition/Observation", observation, None, Nil)
      issues.isEmpty mustEqual(false)
      issues.exists(i => i.location.head == "identifier") mustEqual(true)  // Should be an array but given as object
      issues.exists(_.location.head == "code.coding") mustEqual(true) //Should an array but given as object
      issues.exists(_.location.head == "subject") mustEqual(true) //Should an array but given as object
      issues.exists(i => i.location.head == "status") mustEqual(true) //Is required but not given

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/r4/invalid/observation-invalid-basics.json")).mkString).asInstanceOf[JObject]
      issues = FhirContentValidator.validateComplexContentAgainstProfile("http://hl7.org/fhir/StructureDefinition/Observation", observation, None, Nil)
      issues.isEmpty mustEqual(false)
      issues.exists(i => i.location.head == "status") mustEqual(true) //Invalid primitive empty code
      issues.exists(i => i.location.head == "extraElement") mustEqual(true) //Invalid extra element
      issues.exists(i => i.location.head == "category[0].coding[0].display") mustEqual(true) //Invalid primitive empty string
      issues.exists(i => i.location.head == "component[0].valueQuantity.value") mustEqual(true) //Invalid decimal (decimal as string)
      issues.exists(i => i.location.head == "component[0].interpretation[0].coding[0].extraElement") mustEqual(true) //Invalid extra element in inner parts
      issues.exists(i => i.location.head == "extension[0].url") mustEqual(true) //Required but not exist
      issues.exists(i => i.location.head == "extension[0]")  mustEqual(true) //Constraint failure both extension has value and extension
    }

  }
}

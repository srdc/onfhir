package io.onfhir.validation

import io.onfhir.api.DEFAULT_RESOURCE_PATHS
import io.onfhir.api.util.{FHIRUtil, IOUtil}
import io.onfhir.config.OnfhirConfig
import io.onfhir.r4.config.FhirR4Configurator
import io.onfhir.r4.parsers.StructureDefinitionParser
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StructureDefinitionParserTest extends Specification {

  val resourceProfiles = IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS,"profiles-resources.json", Set("StructureDefinition"))
  val dataTypeProfiles = IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS,"profiles-types.json", Set("StructureDefinition"))
  val otherProfiles = IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS,"profiles-others.json", Set("StructureDefinition"))
  val furtherProfiles =
    Seq(
      IOUtil.readModuleResource("/fhir/validation/profiles/MyObservation.StructureDefinition.json"),
      IOUtil.readModuleResource("/fhir/validation/profiles/MySampledData.StructureDefinition.json"),
    )
  var r4Configurator = new FhirR4Configurator()
  val allDataTypes = dataTypeProfiles.flatMap(r4Configurator.getTypeFromStructureDefinition)
  var FHIR_COMPLEX_TYPES = allDataTypes.filter(_.head.isUpper).toSet
  var FHIR_PRIMITIVE_TYPES = allDataTypes.filter(_.head.isLower).toSet

  sequential
  "ProfileParser" should {
    /*"parse the base FHIR resource profiles" in {
      val profiles = resourceProfiles.flatMap(StructureDefinitionParser.parseProfile)
      profiles.length mustEqual resourceProfiles.length
    }

    "parse the base FHIR data type profiles" in {
      val profiles = dataTypeProfiles.flatMap(StructureDefinitionParser.parseProfile)
      profiles.length < dataTypeProfiles.length
    }*/

    "parse the other profiles" in {
      var lipidProfileSd = otherProfiles.find(r => FHIRUtil.extractValue[String](r, "url") == "http://hl7.org/fhir/StructureDefinition/lipidprofile").get
      val lipidProfile = new StructureDefinitionParser(FHIR_COMPLEX_TYPES, FHIR_PRIMITIVE_TYPES).parseProfile(lipidProfileSd, includeElementMetadata = true)
      lipidProfile mustNotEqual(null)
      //val profiles = otherProfiles.flatMap(StructureDefinitionParser.parseProfile)
      //profiles.length mustEqual furtherProfiles.length
    }

    /*"parse the defined profiles" in {
      val profiles = furtherProfiles.flatMap(StructureDefinitionParser.parseProfile)
      profiles.length mustEqual furtherProfiles.length
    }*/

  }
}

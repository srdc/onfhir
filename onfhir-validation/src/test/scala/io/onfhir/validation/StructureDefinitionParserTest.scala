package io.onfhir.validation

import io.onfhir.api.util.{FHIRUtil, IOUtil}
import io.onfhir.r4.parsers.StructureDefinitionParser
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StructureDefinitionParserTest extends Specification {

  val resourceProfiles = IOUtil.readStandardBundleFile("profiles-resources.json", Set("StructureDefinition"))
  val dataTypeProfiles = IOUtil.readStandardBundleFile("profiles-types.json", Set("StructureDefinition"))
  val otherProfiles = IOUtil.readStandardBundleFile("profiles-others.json", Set("StructureDefinition"))
  val furtherProfiles =
    Seq(
      IOUtil.readInnerResource("/fhir/r4/profiles/MyObservation.StructureDefinition.json"),
      IOUtil.readInnerResource("/fhir/r4/profiles/MySampledData.StructureDefinition.json"),
    )
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
      val lipidProfile = new StructureDefinitionParser().parseProfile(lipidProfileSd)
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

package io.onfhir.validation

import io.onfhir.api.util.IOUtil
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StructureDefinitionParserTest extends Specification {

  val resourceProfiles = IOUtil.readStandardBundleFile("profiles-resources.json", Set("StructureDefinition"))
  val dataTypeProfiles = IOUtil.readStandardBundleFile("profiles-types.json", Set("StructureDefinition"))


  sequential
  "ProfileParser" should {
    "parse the base FHIR resource profiles" in {
      val profiles = resourceProfiles.flatMap(StructureDefinitionParser.parseProfile)
      profiles.length mustEqual resourceProfiles.length
    }

    "parse the base FHIR data type profiles" in {
      val profiles = dataTypeProfiles.flatMap(StructureDefinitionParser.parseProfile)
      profiles.length < dataTypeProfiles.length
    }
  }
}

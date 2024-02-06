package io.onfhir.validation

import io.onfhir.api.DEFAULT_RESOURCE_PATHS
import io.onfhir.api.util.IOUtil
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TerminologyParserTest extends Specification {
  val valueSetsOrCodeSystems =
    IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4,"valuesets.json", Set("ValueSet", "CodeSystem")) ++
      IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4,"v3-codesystems.json", Set("ValueSet", "CodeSystem"))

  sequential
  "TerminologyParserTest" should {
    "Parse the base valueset definitions" in {
      val results = new TerminologyParser().parseValueSetBundle(valueSetsOrCodeSystems)

      results.nonEmpty
    }
  }
}

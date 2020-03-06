package io.onfhir.validation

import io.onfhir.api.util.IOUtil
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TerminologyParserTest extends Specification {
  val valueSetsOrCodeSystems =
    IOUtil.readStandardBundleFile("valuesets.json", Set("ValueSet", "CodeSystem")) ++
      IOUtil.readStandardBundleFile("v3-codesystems.json", Set("ValueSet", "CodeSystem"))

  sequential
  "TerminologyParserTest" should {
    "Parse the base valueset definitions" in {
      val results = new TerminologyParser().parseValueSetBundle(valueSetsOrCodeSystems)

      results.nonEmpty
    }
  }
}

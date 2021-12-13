package io.onfhir.api.util

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FHIRUtilTest extends Specification {
  "FHIRUtil" should {
    "parse resource urls" in {
      FHIRUtil.parseRestfullFhirResourceUrl("https://example.com/fhir") must beEmpty
      FHIRUtil.parseRestfullFhirResourceUrl("x.com") must beEmpty
      FHIRUtil.parseRestfullFhirResourceUrl("urn:uuid:658464646") must beEmpty
      FHIRUtil.parseRestfullFhirResourceUrl("https://example.com/fhir/Patient/123") must beSome((Some("https://example.com/fhir"), "Patient", "123", None))
      FHIRUtil.parseRestfullFhirResourceUrl("Patient/123") must beSome((None, "Patient", "123", None))
      FHIRUtil.parseRestfullFhirResourceUrl("Patient/123/_history/1") must beSome((None, "Patient", "123", Some("1")))
      FHIRUtil.parseRestfullFhirResourceUrl("https://example.com/fhir/Patient/123/_history/1") must beSome((Some("https://example.com/fhir"), "Patient", "123", Some("1")))
    }
  }
}

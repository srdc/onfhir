package io.onfhir.api.util

import io.onfhir.util.JsonFormatter.{formats, parseFromJson}
import org.json4s.JsonAST.{JBool, JString}
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

    "get parameter value" in {
      val parametersResourceJson = """{
                   |    "resourceType": "Parameters",
                   |    "parameter": [
                   |        {
                   |            "name": "result",
                   |            "valueBoolean": true
                   |        },
                   |        {
                   |            "name": "match",
                   |            "part": [
                   |                {
                   |                    "name": "relationship",
                   |                    "valueCode": "equivalent"
                   |                },
                   |                {
                   |                    "name": "concept",
                   |                    "valueCoding": {
                   |                        "system": "http://snomed.info/sct",
                   |                        "code": "309068002",
                   |                        "display": "Skin lesion specimen"
                   |                    }
                   |                },
                   |                {
                   |                    "name": "originalMap",
                   |                    "valueString": "http://hl7.org/fhir/ConceptMap/102"
                   |                }
                   |            ]
                   |        },
                   |        {
                   |            "name": "match",
                   |            "part": [
                   |                {
                   |                    "name": "relationship",
                   |                    "valueCode": "equivalent"
                   |                },
                   |                {
                   |                    "name": "concept",
                   |                    "valueCoding": {
                   |                        "system": "http://snomed.info/sct",
                   |                        "code": "309068001",
                   |                        "display": "Skin lesion specimen 2"
                   |                    }
                   |                },
                   |                {
                   |                    "name": "other",
                   |                    "part": [
                   |                      {
                   |                        "name": "x",
                   |                        "valueCode": "y"
                   |                      }
                   |                    ]
                   |                }
                   |            ]
                   |        }
                   |    ]
                   |}""".stripMargin

      val parameters = parametersResourceJson.parseJson
      FHIRUtil.getParameterValueByPath(parameters, "result") shouldEqual Seq(JBool(true))
      FHIRUtil.getParameterValueByPath(parameters, "match.concept").map(c => (c \ "code").extract[String]).toSet shouldEqual Set("309068002", "309068001")
      FHIRUtil.getParameterValueByPath(parameters, "match.other.x") shouldEqual Seq(JString("y"))
    }
  }
}

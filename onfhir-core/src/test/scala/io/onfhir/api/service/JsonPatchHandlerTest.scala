package io.onfhir.api.service

import io.onfhir.exception.BadRequestException
import io.onfhir.path.FhirPathEvaluator
import org.specs2.mutable.Specification
import io.onfhir.util.JsonFormatter._
import org.json4s.Diff
import org.json4s.JsonAST.{JArray, JNothing, JObject, JString}

import scala.io.Source

class JsonPatchHandlerTest extends Specification {
  sequential

  val observation =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/observation.json")).mkString.parseJson

  val addPatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add1.json")).mkString.parseJson
  val addPatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add2.json")).mkString.parseJson
  val addPatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add3.json")).mkString.parseJson
  val addPatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add4.json")).mkString.parseJson
  val addPatch5 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add5.json")).mkString.parseJson
  val addPatch6 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add6.json")).mkString.parseJson
  val addPatch7 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add7.json")).mkString.parseJson
  val addPatch8 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add8.json")).mkString.parseJson
  val addPatch9 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add9.json")).mkString.parseJson
  val addPatch10 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-add10.json")).mkString.parseJson

  val removePatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-remove1.json")).mkString.parseJson
  val removePatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-remove2.json")).mkString.parseJson
  val removePatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-remove3.json")).mkString.parseJson
  val removePatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-remove4.json")).mkString.parseJson

  val replacePatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-replace1.json")).mkString.parseJson
  val replacePatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-replace2.json")).mkString.parseJson
  val replacePatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-replace3.json")).mkString.parseJson
  val replacePatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-replace4.json")).mkString.parseJson

  val movePatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-move1.json")).mkString.parseJson
  val movePatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-move2.json")).mkString.parseJson
  val movePatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-move3.json")).mkString.parseJson
  val movePatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-move4.json")).mkString.parseJson

  val copyPatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-copy1.json")).mkString.parseJson
  val copyPatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-copy2.json")).mkString.parseJson
  val copyPatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-copy3.json")).mkString.parseJson

  val multiPatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-json-patch-multi1.json")).mkString.parseJson

  "FhirPathPatchHandler" should {
    "handle add operation for non-repetitive elements that does not exist" in {
      val result = JsonPatchHandler.applyPatch(addPatch1, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JNothing
      diff.deleted mustEqual JNothing
      diff.added mustEqual JObject("encounter" -> JObject("reference" -> JString("Encounter/123")))
    }
    "handle add operation for non-repetitive elements that exist (replace)" in {
      val result = JsonPatchHandler.applyPatch(addPatch2, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JObject("status" -> JString("preliminary"))
      diff.deleted mustEqual JNothing
      diff.added mustEqual JNothing
    }
    "handle add operation for repetitive elements that exist (putting at given index)" in {
      val result = JsonPatchHandler.applyPatch(addPatch3, "Observation", observation)
      //It should put it to the second component and as final coding
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].system", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("271649006")
    }
    "handle add operation for repetitive elements that exist (putting at end)" in {
      val result = JsonPatchHandler.applyPatch(addPatch4, "Observation", observation)
      //It should put it to the second component and as final coding
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].system", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("271649006")
    }
    "handle add operation for repetitive elements that does not exist (putting complete array)" in {
      val result = JsonPatchHandler.applyPatch(addPatch5, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JNothing
      diff.deleted mustEqual JNothing
      diff.added mustEqual JObject("identifier" -> JArray(List(JObject("system" -> JString("test"), "value" -> JString("test")))))
    }
    "handle add operation for repetitive elements that does not exist (putting at beginning)" in {
      val result = JsonPatchHandler.applyPatch(addPatch6, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JNothing
      diff.deleted mustEqual JNothing
      diff.added mustEqual JObject("identifier" -> JArray(List(JObject("system" -> JString("test"), "value" -> JString("test")))))
    }

    "reject add operation if parent path is invalid" in {
     JsonPatchHandler.applyPatch(addPatch7, "Observation", observation) must throwA[BadRequestException]
    }

    "reject add operation if index is greater than array size in the last element" in {
      JsonPatchHandler.applyPatch(addPatch8, "Observation", observation) must throwA[BadRequestException]
    }

    "reject add operation if the last element is not an array but an index is given at the end" in {
      JsonPatchHandler.applyPatch(addPatch9, "Observation", observation) must throwA[BadRequestException]
    }

    "reject add operation if replacing an array with non-array value" in {
      JsonPatchHandler.applyPatch(addPatch10, "Observation", observation) must throwA[BadRequestException]
    }

    "handle remove operation for non-repetitive element" in {
      val result = JsonPatchHandler.applyPatch(removePatch1, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JNothing
      diff.added mustEqual JNothing
      diff.deleted mustEqual JObject("subject" -> JObject("reference" -> JString("Patient/example")))
    }

    "handle remove operation for non-repetitive element" in {
      val result = JsonPatchHandler.applyPatch(removePatch2, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("bp-s")
    }

    "reject remove operation when index is greater than array size" in {
     JsonPatchHandler.applyPatch(removePatch3, "Observation", observation) must throwA[BadRequestException]
    }

    "reject remove operation when index is greater than array size" in {
      JsonPatchHandler.applyPatch(removePatch4, "Observation", observation) must throwA[BadRequestException]
    }

    "handle replace operation for non-repetitive element" in {
      val result = JsonPatchHandler.applyPatch(replacePatch1, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JObject("status" -> JString("preliminary"))
      diff.added mustEqual JNothing
      diff.deleted mustEqual JNothing
    }

    "handle replace operation for repetitive element (replacing the element at given index)" in {
      val result = JsonPatchHandler.applyPatch(replacePatch2, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("bp-s")
    }

    "reject replace operation if path does not match any element" in {
      JsonPatchHandler.applyPatch(replacePatch3, "Observation", observation) must throwA[BadRequestException]
    }

    "reject replace operation if type of replaced element does not match with the value" in {
      JsonPatchHandler.applyPatch(replacePatch4, "Observation", observation) must throwA[BadRequestException]
    }

    "handle move operation for repetitive element (same array change index)" in {
      val result = JsonPatchHandler.applyPatch(movePatch1, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("271649006")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("bp-s")
    }

    "handle move operation for repetitive element (move to another array)" in {
      val result = JsonPatchHandler.applyPatch(movePatch2, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("bp-s")
      FhirPathEvaluator().evaluateNumerical("component[0].code.coding.count()", result) mustEqual 2

      FhirPathEvaluator().evaluateString("component[1].code.coding[0].code", result) mustEqual Seq("271649006")
      FhirPathEvaluator().evaluateString("component[1].code.coding[1].code", result) mustEqual Seq("8462-4")
    }

    "handle move operation for non-repetitive element" in {
      val result = JsonPatchHandler.applyPatch(movePatch3, "Observation", observation)
      FhirPathEvaluator().evaluateString("status", result) mustEqual Nil
      FhirPathEvaluator().evaluateString("subject.reference", result) mustEqual Seq("final")
    }

    "reject move operation if given from path does not match any element" in {
      JsonPatchHandler.applyPatch(movePatch4, "Observation", observation) must throwA[BadRequestException]
    }

    "handle copy operation for repetitive element" in {
      val result = JsonPatchHandler.applyPatch(copyPatch1, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("271649006")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("bp-s")

      FhirPathEvaluator().evaluateString("component[1].code.coding[0].code", result) mustEqual Seq("8462-4")
      FhirPathEvaluator().evaluateString("component[1].code.coding[1].code", result) mustEqual Seq("8480-6")
    }

    "handle copy operation for non-repetitive element" in {
      val result = JsonPatchHandler.applyPatch(copyPatch2, "Observation", observation)
      FhirPathEvaluator().evaluateString("status", result) mustEqual Seq("final")
      FhirPathEvaluator().evaluateString("subject.reference", result) mustEqual Seq("final")
    }

    "reject copy operation if given from path does not match any element" in {
      JsonPatchHandler.applyPatch(copyPatch3, "Observation", observation) must throwA[BadRequestException]
    }

    "handle multi patch operation in order" in {
      val result = JsonPatchHandler.applyPatch(multiPatch1, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("8480-6")

    }
  }
}

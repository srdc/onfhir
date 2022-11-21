package io.onfhir.api.service

import io.onfhir.config.FhirConfigurationManager
import io.onfhir.exception.BadRequestException
import io.onfhir.path.FhirPathEvaluator
import org.slf4j.{Logger, LoggerFactory}
import org.specs2.mutable.Specification
import io.onfhir.util.JsonFormatter._
import io.onfhir.validation.BaseFhirProfileHandler
import org.json4s.Diff
import org.json4s.JsonAST.{JArray, JNothing, JObject, JString}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FhirPathPatchHandlerTest extends Specification {
  sequential
  //Mock profile handler to check cardinality of paths for add operation
  val profileHandler = new BaseFhirProfileHandler(null) {
    override protected val logger: Logger = LoggerFactory.getLogger(classOf[FhirPathPatchHandlerTest])

    override def findPathCardinality(path:String, rtype:String):Boolean = {
      (rtype, path) match {
        case ("Observation", "identifier") => true
        case _ => false
      }
    }
  }

  val fhirPatchHandler = new FhirPathPatchHandler(FhirConfigurationManager, profileHandler)

  val observation =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/observation.json")).mkString.parseJson

  val addPatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add1.json")).mkString.parseJson
  val addPatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add2.json")).mkString.parseJson
  val addPatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add3.json")).mkString.parseJson
  val addPatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add4.json")).mkString.parseJson
  val addPatch5 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add5.json")).mkString.parseJson
  val addPatch6 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add6.json")).mkString.parseJson
  val addPatch7 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add7.json")).mkString.parseJson
  val addPatch8 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-add8.json")).mkString.parseJson

  val insertPatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-insert1.json")).mkString.parseJson
  val insertPatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-insert2.json")).mkString.parseJson
  val insertPatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-insert3.json")).mkString.parseJson
  val insertPatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-insert4.json")).mkString.parseJson
  val insertPatch5 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-insert5.json")).mkString.parseJson
  val insertPatch6 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-insert6.json")).mkString.parseJson
  val insertPatch7 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-insert7.json")).mkString.parseJson

  val deletePatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-delete1.json")).mkString.parseJson
  val deletePatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-delete2.json")).mkString.parseJson
  val deletePatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-delete3.json")).mkString.parseJson

  val replacePatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-replace1.json")).mkString.parseJson
  val replacePatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-replace2.json")).mkString.parseJson
  val replacePatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-replace3.json")).mkString.parseJson
  val replacePatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-replace4.json")).mkString.parseJson
  val replacePatch5 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-replace5.json")).mkString.parseJson
  val replacePatch6 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-replace6.json")).mkString.parseJson


  val movePatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move1.json")).mkString.parseJson
  val movePatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move2.json")).mkString.parseJson
  val movePatch3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move3.json")).mkString.parseJson
  val movePatch4 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move4.json")).mkString.parseJson
  val movePatch5 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move5.json")).mkString.parseJson
  val movePatch6 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move6.json")).mkString.parseJson
  val movePatch7 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move7.json")).mkString.parseJson
  val movePatch8 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-move8.json")).mkString.parseJson

  val multiPatch1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/patch/fhir-path-patch-multi1.json")).mkString.parseJson

  "FhirPathPatchHandler" should {
    "handle add operation for non-repetitive elements if it does not exist" in {
      val result = fhirPatchHandler.applyPatch(addPatch1, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JNothing
      diff.deleted mustEqual JNothing
      diff.added mustEqual JObject("encounter" -> JObject("reference" -> JString("Encounter/123")))
    }

    "handle add operation for non-repetitive elements by replacing if it exist" in {
      val result = fhirPatchHandler.applyPatch(addPatch2, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.added mustEqual JNothing
      diff.deleted mustEqual JNothing
      diff.changed mustEqual JObject("status" -> JString("preliminary"))
    }

    "handle add operation for repetitive elements by appending to end if it exist" in {
      val result = fhirPatchHandler.applyPatch(addPatch3, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.deleted mustEqual JNothing
      diff.changed mustEqual JNothing
      diff.added mustNotEqual JNothing
      //It should put it to the second component and as final coding
      FhirPathEvaluator().evaluateString("component[1].code.coding[1].system", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[1].code.coding[1].code", result) mustEqual Seq("test")
    }

    "handle add operation for repetitive elements by initializing the array if not exist" in {
      val result = fhirPatchHandler.applyPatch(addPatch4, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.deleted mustEqual JNothing
      diff.changed mustEqual JNothing
      diff.added mustEqual JObject("identifier" -> JArray(List(JObject(List("system" -> JString("test"), "value" -> JString("test"))))))
    }

    "reject if path does not match any element" in {
      fhirPatchHandler.applyPatch(addPatch5, "Observation", observation) must throwA[BadRequestException]
    }

    "reject add operation if path does match more than one element" in {
      fhirPatchHandler.applyPatch(addPatch6, "Observation", observation) must throwA[BadRequestException]
    }
    "reject add operation if operation definition is invalid (missing items)" in {
      fhirPatchHandler.applyPatch(addPatch7, "Observation", observation) must throwA[BadRequestException]
    }

    "handle add operation for backbone elements constructed from param tree" in {
      val result = fhirPatchHandler.applyPatch(addPatch8, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.deleted mustEqual JNothing
      diff.changed mustEqual JNothing
      FhirPathEvaluator().evaluateString("component[2].code.text", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateNumerical("component[2].valueQuantity.value", result).head mustEqual 10
    }

    "handle insert operation - inserting to a middle of an array" in {
      val result = fhirPatchHandler.applyPatch(insertPatch1, "Observation", observation)
      //We have inserted to index 1
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].system", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("271649006")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
    }

    "handle insert operation - inserting to the start of array" in {
      val result = fhirPatchHandler.applyPatch(insertPatch2, "Observation", observation)
      //We have inserted to index 1
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].system", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("8480-6")
    }

    "handle insert operation - inserting to the end of array" in {
      val result = fhirPatchHandler.applyPatch(insertPatch3, "Observation", observation)
      //We have inserted to index 1
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].system", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("bp-s")
    }

    "reject insert operation for invalid index" in {
      fhirPatchHandler.applyPatch(insertPatch4, "Observation", observation) must throwA[BadRequestException]
    }

    "reject insert operation when path match a non-repetitive element" in {
      fhirPatchHandler.applyPatch(insertPatch5, "Observation", observation) must throwA[BadRequestException]
    }

    "reject insert operation when required parameters are missing" in {
      fhirPatchHandler.applyPatch(insertPatch6, "Observation", observation) must throwA[BadRequestException]
    }

    "reject insert operation when path does not match any element" in {
      fhirPatchHandler.applyPatch(insertPatch7, "Observation", observation) must throwA[BadRequestException]
    }

    "handle delete operation when path matches an element" in {
      val result = fhirPatchHandler.applyPatch(deletePatch1, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.deleted mustEqual JObject("basedOn" -> JArray(List( JObject("identifier"-> JObject(List("system" -> JString("https://acme.org/identifiers"), "value" -> JString("1234")))))))
      diff.changed mustEqual JNothing
      diff.added mustEqual JNothing
    }

    "handle delete operation when path does not match an element (not return error)" in {
      val result = fhirPatchHandler.applyPatch(deletePatch2, "Observation", observation)
      result mustEqual observation
    }

    "handle delete operation when path matchs an array element" in {
      val result = fhirPatchHandler.applyPatch(deletePatch3, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("bp-s")
    }

    "handle replace operation when path does match non-repetitive element" in {
      val result = fhirPatchHandler.applyPatch(replacePatch1, "Observation", observation)
      val diff:Diff = observation.diff(result)
      diff.changed mustEqual JObject("status" -> JString("preliminary"))
      diff.added mustEqual JNothing
      diff.deleted mustEqual JNothing
    }

    "handle replace operation when path does match an element in the middle of an array" in {
      val result = fhirPatchHandler.applyPatch(replacePatch2, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("bp-s")
    }
    "handle replace operation when path does match an element at the beginning of an array" in {
      val result = fhirPatchHandler.applyPatch(replacePatch3, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("271649006")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("bp-s")
    }
    "handle replace operation when path does match an element at the end of an array" in {
      val result = fhirPatchHandler.applyPatch(replacePatch4, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("271649006")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
    }
    "handle replace operation when path does match a repetitive element (replacing the whole array)" in {
      val result = fhirPatchHandler.applyPatch(replacePatch5, "Observation", observation)
      FhirPathEvaluator().evaluateNumerical("component[0].code.coding.count()", result).head mustEqual 1
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
    }

    "reject replace operation when path does not match any element" in {
      fhirPatchHandler.applyPatch(replacePatch6, "Observation", observation) must throwA[BadRequestException]
    }

    "handle move operation - moving an element to later position" in {
      //Moving second to third
      var result = fhirPatchHandler.applyPatch(movePatch1, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("bp-s")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("271649006")

      result = fhirPatchHandler.applyPatch(movePatch2, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("bp-s")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("271649006")

      result = fhirPatchHandler.applyPatch(movePatch3, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("bp-s")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("271649006")
    }
    "handle move operation - moving an element to earlier position" in {
      //Moving second to third
      var result = fhirPatchHandler.applyPatch(movePatch4, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("bp-s")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("271649006")

      result = fhirPatchHandler.applyPatch(movePatch5, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("bp-s")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("271649006")

      result = fhirPatchHandler.applyPatch(movePatch6, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("8480-6")
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("bp-s")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("271649006")
    }

    "reject move operation when path does match a non-repetitive element" in {
      fhirPatchHandler.applyPatch(movePatch7, "Observation", observation) must throwA[BadRequestException]
    }

    "reject move operation when indexes are invalid" in {
      fhirPatchHandler.applyPatch(movePatch8, "Observation", observation) must throwA[BadRequestException]
    }

    "handle a multi operation patch in correct order" in {
      var result = fhirPatchHandler.applyPatch(multiPatch1, "Observation", observation)
      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("bp-s")
    }

  }

}

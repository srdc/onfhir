package io.onfhir.api.endpoint

import java.time.{Instant, LocalDate}
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentType, HttpCharsets, HttpEntity, MediaType, StatusCodes}
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.api.Resource
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.OnfhirConfig
import io.onfhir.path.FhirPathEvaluator
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.util.DateTimeUtil

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRPatchEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory = system
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val obsBP = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-bp.json")).mkString

  val jsonPatch = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-json-patch-multi1.json")).mkString
  val jsonPatchProblem1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-json-patch-with-problem1.json")).mkString
  val jsonPatchProblem2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-json-patch-with-problem2.json")).mkString
  val jsonPatchProblem3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-json-patch-with-problem3.json")).mkString

  val fhirPathPatch = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-path-patch-multi1.json")).mkString
  val fhirPathPatchProblem1 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-path-patch-with-problem1.json")).mkString
  val fhirPathPatchProblem2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-path-patch-with-problem2.json")).mkString

  val jsonPatchContentType = ContentType.apply(MediaType.applicationWithOpenCharset("json-patch+json"), HttpCharsets.`UTF-8`)
  val jsonFhirContentType =ContentType.apply(MediaType.applicationWithOpenCharset("fhir+json"), HttpCharsets.`UTF-8`)

  val bodyWeightGoal = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Goal/goal-weight.json")).mkString
  val goalStatusPatch = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-path-patch-with-expression-value.json")).mkString
  val goalStatusPatch2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-path-patch-with-expression-value-2.json")).mkString

  val resourceType = "Observation"
  val observationId = "obsbp"

  sequential

  "FHIR Patch Endpoint" should {
    "handle JSON patch for a resource" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity(obsBP)) ~> routes ~> check {
        eventually(status === StatusCodes.Created)

        val response = responseAs[Resource]
        checkIdAndMeta(response, null, "1")
        checkHeaders(response, resourceType, FHIRUtil.extractValue[String](response, "id"), "1")
      }

      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity.apply(jsonPatchContentType, jsonPatch)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
        FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
        FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
        FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("8480-6")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId)~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
        FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
        FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
        FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("8480-6")
      }
    }

    "handle FHIR Path patch for a resource" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity(obsBP)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
      }

      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity.apply(jsonFhirContentType, fhirPathPatch)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
        FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
        FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
        FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("bp-s")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId)~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
        FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
        FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
        FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("bp-s")
      }
    }

    "reject JSON patch for a resource if patch is not valid" in {
      //Path with a test that failed
      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity.apply(jsonPatchContentType, jsonPatchProblem1)) ~> routes ~> check {
        eventually(status === StatusCodes.PreconditionFailed)
      }
      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity.apply(jsonPatchContentType, jsonPatchProblem2)) ~> routes ~> check {
        eventually(status === StatusCodes.BadRequest)
      }
      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity.apply(jsonPatchContentType, jsonPatchProblem3)) ~> routes ~> check {
        eventually(status === StatusCodes.BadRequest)
      }
    }

    "reject FHIR Path patch for a resource if patch is not valid" in {
      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity.apply(jsonFhirContentType, fhirPathPatchProblem1)) ~> routes ~> check {
        eventually(status === StatusCodes.BadRequest)
      }
      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity.apply(jsonFhirContentType, fhirPathPatchProblem2)) ~> routes ~> check {
        eventually(status === StatusCodes.BadRequest)
      }
    }

    "handle conditional JSON patch for a resource" in {
      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity(obsBP)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
      }
      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType + "?code=85354-9", HttpEntity.apply(jsonPatchContentType, jsonPatch)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
        FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
        FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
        FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("8480-6")
      }

      Put("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId, HttpEntity(obsBP)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
      }
      Patch("/" + OnfhirConfig.baseUri + "/" + resourceType  + "?code=85354-0", HttpEntity.apply(jsonPatchContentType, jsonPatch)) ~> routes ~> check {
        eventually(status === StatusCodes.NotFound)
      }
      Get("/" + OnfhirConfig.baseUri + "/" + resourceType + "/" + observationId)~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustNotEqual Seq("test")
        FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustNotEqual Seq("test2")
        FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustNotEqual Seq("test3")
        FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustNotEqual Seq("8480-6")
      }
    }

    "handle fhir patch with expression - Goal status updating example" in {
      Post("/" + OnfhirConfig.baseUri + "/" + "Goal", HttpEntity(bodyWeightGoal)) ~> routes ~> check {
        eventually(status === StatusCodes.Created)
      }

      val now = DateTimeUtil.serializeInstant(Instant.now())
      //We should also add a search param for target-measure but not defined in main standard
      val conditionalQuery = "?patient=Patient/example&lifecycle-status=active,accepted&achievement-status:not=not-achieved&target-date=ge"+now

      Patch("/" + OnfhirConfig.baseUri + "/" + "Goal"  + conditionalQuery, HttpEntity.apply(jsonFhirContentType, goalStatusPatch)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("achievementStatus.coding.code", result) mustEqual Seq("achieved")
        FhirPathEvaluator().evaluateString("outcomeReference.reference", result) mustEqual Seq("Observation/1321313")
        FhirPathEvaluator().evaluateDateTime("statusDate", result) mustEqual LocalDate.now()
      }

      Patch("/" + OnfhirConfig.baseUri + "/" + "Goal"  + conditionalQuery, HttpEntity.apply(jsonFhirContentType, goalStatusPatch2)) ~> routes ~> check {
        eventually(status === StatusCodes.OK)
        val result = responseAs[Resource]
        FhirPathEvaluator().evaluateString("achievementStatus.coding.code", result) mustEqual Seq("in-progress")
        FhirPathEvaluator().evaluateString("outcomeReference.reference", result) mustEqual Seq("Observation/1321313", "Observation/1321314")
        FhirPathEvaluator().evaluateDateTime("statusDate", result) mustEqual LocalDate.now()
      }
    }

  }

}

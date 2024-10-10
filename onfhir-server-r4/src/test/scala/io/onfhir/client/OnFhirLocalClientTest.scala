package io.onfhir.client

import java.time.Instant
import akka.http.javadsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCode
import io.onfhir.OnFhirTest
import io.onfhir.api.Resource
import io.onfhir.api.client.{FHIRHistoryBundle, FHIRSearchSetBundle, FHIRTransactionBatchBundle, OnFhirLocalClient}
import io.onfhir.api.model.FHIRResponse
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.BadRequestException
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JObject, JString}
import org.json4s.jackson.JsonMethods
import org.junit.runner.RunWith

import scala.io.Source
import org.specs2.concurrent.ExecutionEnv
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class OnFhirLocalClientTest extends OnFhirTest {
  val patient =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient.json")).mkString.parseJson
  val patientWithoutId =  Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Patient/patient-without-id.json")).mkString.parseJson
  val obsGlucose = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-glucose.json")).mkString.parseJson
  val obsHemoglobin = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-hemoglobin.json")).mkString.parseJson
  val obsBP = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-bp.json")).mkString.parseJson

  val jsonPatch = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-json-patch-multi1.json")).mkString
  val fhirPathPatch = Source.fromInputStream(getClass.getResourceAsStream("/fhir/patch/fhir-path-patch-multi1.json")).mkString

  sequential

  implicit val ee: ExecutionEnv = ExecutionEnv.fromGlobalExecutionContext

  "OnFhirLocalClient" should {
    //Create a new resource given in JSON and XML formats
   "should help creating a new resource - explicitly call execute and return FHIRResponse"  in {
      val response =
        OnFhirLocalClient
          .create(patientWithoutId)
          .execute()

      response.map(_.httpStatus) must be_==(StatusCodes.CREATED).await
    }

    "should help creating a new resource with conditional create - return OK as resource already exist" in {
      val response =
        OnFhirLocalClient
        .create(patientWithoutId)
        .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345")
        .execute()

      response.map(_.httpStatus) must be_==(StatusCodes.OK).await
    }

    "should help creating a new resource -  execute called implicitly and return FHIRResponse"  in {
      val response:Future[FHIRResponse] = OnFhirLocalClient.create(patientWithoutId)
      response.map(_.httpStatus) must be_==(StatusCodes.CREATED).await
    }

    "should help creating a new resource -  execute called implicitly and implicit conversion to resource"  in {
      val createdResource:Future[Resource] = OnFhirLocalClient.create(patientWithoutId)
      createdResource.map(FHIRUtil.extractValue[String](_, "resourceType")) must not be empty.await
    }

    "should help creating a new resource - handling error" in {
      OnFhirLocalClient.create(JObject()).execute() must throwA[BadRequestException]
    }

    "should help creating a new resource with conditional create - return precondition failed as there are multiple" in {
       val response=
         OnFhirLocalClient
          .create(patientWithoutId)
          .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345")
          .execute()

      response.map(_.httpStatus) must be_==(StatusCodes.PRECONDITION_FAILED).await
    }

    "should help creating a new resource with preferences" in {
      val response:Future[FHIRResponse] = OnFhirLocalClient
        .create(patientWithoutId)
        .returnMinimal()

      response.map(_.httpStatus) must be_==(StatusCodes.CREATED).await
      response.map(_.responseBody) must beNone.await
    }

    "should help updating a resource"  in {
      val createdResource:Future[Resource]  = OnFhirLocalClient.create(patientWithoutId)
      val response = createdResource.flatMap(r => OnFhirLocalClient.update(r.mapField(f => f._1 -> (if(f._1 == "gender") JString("female") else f._2)).asInstanceOf[JObject]).execute())
      response.map(_.httpStatus) must be_==(StatusCodes.OK).await
      response.map(r => FHIRUtil.extractValue[String](r.responseBody.getOrElse(JObject()), "gender")) must be_==("female").await
    }

    "should help updating a resource with conditional update" in {
      val response = OnFhirLocalClient
          .update(patientWithoutId)
          .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345")
          .execute()
      response.map(_.httpStatus) must be_==(StatusCodes.PRECONDITION_FAILED).await
    }

    "should help updating a resource with version aware update" in {
      val createdResource:Future[Resource]  = OnFhirLocalClient.create(patientWithoutId)
      val response = createdResource.flatMap(r =>
        OnFhirLocalClient
          .update(r.mapField(f => f._1 -> (if(f._1 == "versionId") JString("2") else f._2)).asInstanceOf[JObject])
          .execute()
      )

      response.map(_.httpStatus) must be_==(StatusCodes.PRECONDITION_FAILED).await
      //As we don't force version controlled update, it accepts
      val resource:Future[Resource] = createdResource.flatMap(r =>
        OnFhirLocalClient
          .update(r.mapField(f => f._1 -> (if(f._1 == "versionId") JString("2") else f._2)).asInstanceOf[JObject], forceVersionControl = false)
      )

      resource.map(r => FHIRUtil.extractVersionFromResource(r)) must beEqualTo(2).await
    }

    "should help deleting a resource" in {
      val createdResource:Future[Resource]  = OnFhirLocalClient.create(patientWithoutId)

      val response:Future[FHIRResponse]  =
        createdResource.flatMap(r =>
          OnFhirLocalClient.delete("Patient", FHIRUtil.extractValue[String](r, "id"))
        )

      response.map(_.httpStatus) must be_==(StatusCodes.NO_CONTENT).await
    }

    "should help deleting a resource with conditional delete" in {
      val response:Future[FHIRResponse] =
        OnFhirLocalClient
        .delete("Patient")
        .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345")

      response.map(_.httpStatus) must be_==(StatusCodes.NO_CONTENT).await
    }

    "should help reading a resource" in {
      val createdResource:Resource = Await.result(OnFhirLocalClient.create(patientWithoutId), 5 seconds)
      val rid =  FHIRUtil.extractValue[String](createdResource, "id")
      val resource:Future[Resource] = OnFhirLocalClient.read("Patient",rid)

      resource.map(FHIRUtil.extractValue[String](_, "gender")) must be_==("male").await
    }

    "should help reading a resource with conditional read" in {
      val createdResource:Resource = Await.result(OnFhirLocalClient.create(patientWithoutId), 5 seconds)
      val rid =  FHIRUtil.extractValue[String](createdResource, "id")
      val lastUpdated = FHIRUtil.extractLastUpdatedFromResource(createdResource)

      var response:Future[FHIRResponse] =
        OnFhirLocalClient
          .read("Patient",rid)
          .ifModifiedSince(lastUpdated)
          .execute()

      response.map(_.httpStatus) must be_==(StatusCodes.NOT_MODIFIED).await

      response =
        OnFhirLocalClient
          .read("Patient",rid)
          .ifNoneMatch(1)
          .execute()

      response.map(_.httpStatus) must be_==(StatusCodes.NOT_MODIFIED).await
    }

    "should help reading a resource with element preferences" in {
      val createdResource:Resource = Await.result(OnFhirLocalClient.create(patientWithoutId), 5 seconds)
      val rid =  FHIRUtil.extractValue[String](createdResource, "id")

      var resource:Future[Resource] =
        OnFhirLocalClient
          .read("Patient",rid)
          .summary("text")

      resource.map(r => FHIRUtil.extractValueOption[String](r, "gender")) must beNone.await


      resource=
        OnFhirLocalClient
          .read("Patient",rid)
          .elements("gender", "birthDate")

      resource.map(r => FHIRUtil.extractValueOption[String](r, "gender")) must beSome("male").await
      resource.map(r => FHIRUtil.extractValueOption[String](r, "birthDate")) must beSome("1974-12").await
      resource.map(r => FHIRUtil.extractValueOption[String](r, "identifier")) must beNone.await
    }

    "should help searching resources - returning bundle" in {
      //Create patient; updateCreate
      Await.result(OnFhirLocalClient.update(patient).execute(), 5 seconds)
      //Create observations
      Await.result(OnFhirLocalClient.update(obsGlucose).execute(), 5 seconds)
      Await.result(OnFhirLocalClient.update(obsHemoglobin).execute(), 5 seconds)

      var bundle:Future[Resource] =
        OnFhirLocalClient
          .search("Observation")

      bundle.map(r => FHIRUtil.extractValueOption[Int](r, "total")) must beSome(2).await

      bundle =
        OnFhirLocalClient
          .search("Observation")
          .where("patient", "Patient/example")
          .where("code", "15074-8")

      bundle.map(r => FHIRUtil.extractValueOption[Int](r, "total")) must beSome(1).await

      bundle =
        OnFhirLocalClient
          .search("Observation")
          .where("patient", "Patient/example")
          .where("code", "15074-6")

      bundle.map(r => FHIRUtil.extractValueOption[Int](r, "total")) must beSome(0).await
    }

    "should help searching resources - retrieving next pages" in {
      var bundle:FHIRSearchSetBundle =
      Await.result(
        OnFhirLocalClient
          .search("Observation", count = 1)
          .where("_sort", "-_lastUpdated"), 5 seconds)


      FHIRUtil.extractValue[String](bundle.searchResults.head, "id") must be_==("obshemoglobin")

      while(bundle.hasNext()){
        bundle = Await.result(OnFhirLocalClient.next(bundle), 5 seconds)
        FHIRUtil.extractValue[String](bundle.searchResults.head, "id") must be_==("obsglucose")
      }

      //Try with iterator
      val itr =
        OnFhirLocalClient
        .search("Observation", count = 1)
        .where("_sort", "-_lastUpdated")
        .toIterator()

      itr.next().map(b => FHIRUtil.extractValue[String](b.searchResults.head, "id")) must be_==("obshemoglobin").awaitFor(2 seconds)
      itr.next().map(b => FHIRUtil.extractValue[String](b.searchResults.head, "id")) must be_==("obsglucose").awaitFor(2 seconds)
    }

    "should help searching resources - compartment search" in {
      val bundle:Future[FHIRSearchSetBundle] =
        OnFhirLocalClient
        .search("Observation")
        .forCompartment("Patient","example")
        .where("code", "15074-8")
      bundle.map(r => r.total) must beSome(1L).await
    }

    "should help patching a resource" in {
      var resource:Resource =
        Await.result(
        OnFhirLocalClient
        .patch("Observation","obsglucose")
        .jsonPatch()
        .patchReplace("status", JString("preliminary"))
          , 5 seconds)

      FHIRUtil.extractValue[String](resource, "status") mustEqual("preliminary")

      resource =
        Await.result(
          OnFhirLocalClient
            .patch("Observation","obsglucose")
            .where("status", "preliminary")
            .fhirPathPatch()
            .patchReplace("Observation.status", "code" ->  JString("final"))
          , 5 seconds)

      FHIRUtil.extractValue[String](resource, "status") mustEqual("final")
    }

    "should help retrieving history of a resource" in {
      val bundle:FHIRHistoryBundle =  Await.result(
        OnFhirLocalClient
        .history("Observation","obsglucose"), 5 seconds)

      bundle.getHistory().length mustEqual(3)
      bundle.getHistory().head._1 === 3
      bundle.getHistory().apply(1)._1 === 2
      bundle.getHistory().apply(2)._1 === 1
    }
    "should help retrieving history of a resource with params" in {
      val bundle:FHIRHistoryBundle =  Await.result(
        OnFhirLocalClient
          .history("Observation")
          .since(Instant.now())
        , 5 seconds)
      bundle.total must beSome(0)
    }

    "should help retrieving history of resource type" in {
      val bundle:FHIRHistoryBundle =  Await.result(
        OnFhirLocalClient
          .history("Observation")
        , 5 seconds)

      bundle.getHistory("obsglucose").length mustEqual(3)
      bundle.getHistory("obshemoglobin").length mustEqual(1)
    }

    "should help retrieving history of resource type - retrieving next page" in {
      var bundle:FHIRHistoryBundle =  Await.result(
        OnFhirLocalClient
          .history("Observation", count = 2)
        , 5 seconds)

      bundle.getHistories.flatMap(_._2).size mustEqual(2)
      bundle.hasNext() mustEqual true
      bundle = Await.result(OnFhirLocalClient.next(bundle) , 5 seconds)
      bundle.getHistories.flatMap(_._2).size mustEqual(2)
      bundle.hasNext() mustEqual false
    }
    "should help reading a specific version" in {
      val resource:Resource = Await.result(
        OnFhirLocalClient
          .vread("Observation", "obsglucose", "2")
        , 5 seconds)

      FHIRUtil.extractValue[String](resource, "status") mustEqual("preliminary")
    }


    "should help sending batch requests" in {
      val bundle:FHIRTransactionBatchBundle = Await.result(
        OnFhirLocalClient
          .batch()
          .entry(_.create(patientWithoutId))
          .entry(_.update(patientWithoutId).where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345"))
          .entry(_.delete(patient)),
        5 seconds)

      bundle.responses.length mustEqual 3
      atLeastOnce(bundle.responses.map(_._2.httpStatus)) ((_:StatusCode) must be_==(StatusCodes.NO_CONTENT))
      atLeastOnce(bundle.responses.map(_._2.httpStatus)) ((_:StatusCode) must be_==(StatusCodes.PRECONDITION_FAILED))
      atLeastOnce(bundle.responses.map(_._2.httpStatus)) ((_:StatusCode) must be_==(StatusCodes.CREATED))
    }

    "should help sending transaction requests" in {
      val bundle:FHIRTransactionBatchBundle = Await.result(
        OnFhirLocalClient
          .transaction()
          .entry("urn:uuid:74891afc-ed52-42a2-bcd7-f13d9b60f096", _.create(patientWithoutId))
          .entry("urn:uuid:88f151c0-a954-468a-88bd-5ae15c08e059", _.update(patient)),
        5 seconds)

      bundle.responses.length mustEqual 2
      bundle.getResponse("urn:uuid:74891afc-ed52-42a2-bcd7-f13d9b60f096").httpStatus mustEqual(StatusCodes.CREATED)
      bundle.getResponse("urn:uuid:88f151c0-a954-468a-88bd-5ae15c08e059").httpStatus mustEqual(StatusCodes.CREATED)
    }

    "should help sending operation requests" in {
      val opResponse = Await.result(OnFhirLocalClient
        .operation("validate")
        .on("Observation")
        .addSimpleParam("mode", "general")
        .addResourceParam("resource", obsGlucose),

        5 seconds
      )

      opResponse.httpStatus mustEqual(StatusCodes.OK)
      opResponse.getOutputParam("return") must not beEmpty
    }

    "should help patching a resource with directly supplying patch content" in {
      Await.result(OnFhirLocalClient.update(obsBP).execute(), 5 seconds)
      var result: Resource =
        Await.result(
          OnFhirLocalClient
            .patch("Observation", "obsbp")
            .patchContent(JsonMethods.parse(jsonPatch))
          , 5 seconds)

      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("8480-6")

      Await.result(OnFhirLocalClient.update(obsBP).execute(), 5 seconds)
      result =
      Await.result(
        OnFhirLocalClient
          .patch("Observation", "obsbp")
          .patchContent(JsonMethods.parse(fhirPathPatch))
        , 5 seconds)

      FhirPathEvaluator().evaluateString("component[0].code.coding[0].code", result) mustEqual Seq("test")
      FhirPathEvaluator().evaluateString("component[0].code.coding[1].code", result) mustEqual Seq("test2")
      FhirPathEvaluator().evaluateString("component[0].code.coding[2].code", result) mustEqual Seq("test3")
      FhirPathEvaluator().evaluateString("component[0].code.coding[3].code", result) mustEqual Seq("bp-s")
    }
  }
}

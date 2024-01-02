package io.onfhir.client

import akka.actor.ActorSystem
import akka.http.javadsl.model.StatusCodes
import io.onfhir.api.Resource
import io.onfhir.api.client.{FHIRHistoryBundle, FHIRSearchSetBundle, FHIRTransactionBatchBundle}
import io.onfhir.api.model.FHIRResponse
import io.onfhir.api.util.FHIRUtil

import scala.concurrent.duration.DurationInt
import scala.io.Source
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JObject, JString}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

object OnFhirNetworkClientTest extends Specification {
  val baseUrl = "http://localhost:8080/fhir"
  val patientWithoutId: Resource =  Source.fromInputStream(getClass.getResourceAsStream("/patient-without-id.json")).mkString.parseJson
  val obsGlucose: Resource = Source.fromInputStream(getClass.getResourceAsStream("/observation-glucose.json")).mkString.parseJson
  implicit val actorSystem: ActorSystem = ActorSystem("OnFhirClientTest")
  implicit val ec:ExecutionContext = ExecutionContext.global
  val onFhirClient: OnFhirNetworkClient = OnFhirNetworkClient.apply(baseUrl)
  implicit val ee: ExecutionEnv = ExecutionEnv.fromGlobalExecutionContext
  var createdResource:Resource = _
  sequential

  "OnFhirNetworkClient" should {
    "help deleting resources with conditional delete" in {
      val response:FHIRResponse = Await.result(
        onFhirClient
          .delete("Patient")
          .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345"),
        5 seconds
      )

      response.httpStatus.isSuccess() mustEqual(true)
    }

    "help creating a new resource" in {
      createdResource  = Await.result(onFhirClient.create(patientWithoutId), 5 seconds)
      FHIRUtil.extractValueOption[String](createdResource, "id") must beSome
    }

    "help creating a new resource with conditional create - return OK as resource already exist" in {
      val response =
        onFhirClient
          .create(patientWithoutId)
          .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345")
          .execute()

      response.map(_.httpStatus) must be_==(StatusCodes.OK).await
    }

    "should help updating a resource"  in {
      createdResource = Await.result(onFhirClient
        .update(
          createdResource
            .mapField(f => f._1 -> (if(f._1 == "gender") JString("female") else f._2)).asInstanceOf[JObject]
        ), 5 seconds)

      FHIRUtil.extractValue[String](createdResource, "gender") must be_==("female")
    }

    "should help updating a resource with version aware update" in {
      val response =
        onFhirClient
          .update(createdResource.mapField(f => f._1 -> (if(f._1 == "versionId") JString("1") else f._2)).asInstanceOf[JObject])
          .execute()
      response.map(_.httpStatus) must be_==(StatusCodes.PRECONDITION_FAILED).await
    }

    "should help reading a resource" in {
      val resource:Resource =
        Await.result(onFhirClient
        .read("Patient", FHIRUtil.extractIdFromResource(createdResource)), 5 seconds)

     resource mustEqual createdResource
    }

    "should help reading a resource with conditional read" in {
      val lastUpdated = FHIRUtil.extractLastUpdatedFromResource(createdResource)
      var response:Future[FHIRResponse] =
        onFhirClient
          .read("Patient", FHIRUtil.extractIdFromResource(createdResource))
          .ifModifiedSince(lastUpdated)

      response.map(_.httpStatus) must be_==(StatusCodes.NOT_MODIFIED).await

      response =
        onFhirClient
          .read("Patient",FHIRUtil.extractIdFromResource(createdResource))
          .ifNoneMatch(2)

      response.map(_.httpStatus) must be_==(StatusCodes.NOT_MODIFIED).await
    }

    "should help reading a resource with element preferences" in {
      var resource:Future[Resource] =
        onFhirClient
        .read("Patient",FHIRUtil.extractIdFromResource(createdResource))
        .summary("text")
      resource.map(r => FHIRUtil.extractValueOption[String](r, "gender")) must beNone.await

      resource = onFhirClient
        .read("Patient",FHIRUtil.extractIdFromResource(createdResource))
        .elements("gender", "birthDate")
      resource.map(r => FHIRUtil.extractValueOption[String](r, "gender")) must beSome("female").await
      resource.map(r => FHIRUtil.extractValueOption[String](r, "birthDate")) must beSome("1974-12").await
      resource.map(r => FHIRUtil.extractValueOption[String](r, "identifier")) must beNone.await
    }

    "should help searching resources - returning bundle" in {
      var bundle:Future[FHIRSearchSetBundle] =
        onFhirClient
        .search("Patient")
        .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345")

      bundle.map(_.total) must beSome(1L).await

      bundle =
        onFhirClient
          .search("Patient")
          .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12345")
          .where("gender", "female")
          .byHttpPost()

      bundle.map(_.total) must beSome(1L).await
    }

    "should help searching resources - retrieving next pages" in {
      onFhirClient.create(patientWithoutId).execute().map(_.httpStatus) must be_==(StatusCodes.CREATED).await
      onFhirClient.create(patientWithoutId).execute().map(_.httpStatus) must be_==(StatusCodes.CREATED).await
      onFhirClient.create(patientWithoutId).execute().map(_.httpStatus) must be_==(StatusCodes.CREATED).await

      var bundle:FHIRSearchSetBundle = Await.result(
        onFhirClient
        .search("Patient", count = 2)
        .where("gender", "male"), 5 seconds)

      bundle.total must beSome(3L)
      bundle.searchResults.length mustEqual(2)

      bundle = Await.result(onFhirClient.next(bundle), 5 seconds)
      bundle.searchResults.length mustEqual(1)
      bundle.hasNext() mustEqual(false)

      val itr = onFhirClient
        .search("Patient", count = 2)
        .where("gender", "male")
        .toIterator()

      itr.next().map(_.searchResults.length) must be_==(2).await
      itr.next().map(_.searchResults.length) must be_==(1).await
    }

    "should help searching resources- compartment search" in {
      onFhirClient
        .update(obsGlucose).execute().map(_.httpStatus.isSuccess()) must be_==(true).await

      val bundle:Future[FHIRSearchSetBundle] =
        onFhirClient
        .search("Observation")
        .forCompartment("Patient", "example")
        .where("code", "15074-8")

      bundle.map(_.total) must beSome(1L).await
    }

    "should help searching resources - getting merged results" in {
      var bundle:FHIRSearchSetBundle =
        Await.result(
          onFhirClient
            .search("Patient", count = 1)
            .where("gender", "male")
            .executeAndMergeBundle(), 3 seconds)

      bundle.total must beSome(3L)
      bundle.searchResults.length mustEqual(3)
    }

    "should help patching a resource" in {
      var resource:Future[Resource] =
        onFhirClient
        .patch("Observation","obsglucose")
        .jsonPatch()
        .patchReplace("status", JString("preliminary"))

      resource.map(r => FHIRUtil.extractValue[String](r, "status")) must be_==("preliminary").await

      resource =
          onFhirClient
            .patch("Observation","obsglucose")
            .where("status", "preliminary")
            .fhirPathPatch()
            .patchReplace("Observation.status", "code" ->  JString("final"))

      resource.map(r =>FHIRUtil.extractValue[String](r, "status")) must be_==("final").await
    }



    "should help retrieving history of a resource" in {
      val bundle:FHIRHistoryBundle =  Await.result(
        onFhirClient
          .history("Observation","obsglucose"), 5 seconds)

      bundle.getHistory().length must be_>=(3)
    }

    "should help retrieving history of resource type - retrieving next page" in {
      var bundle:FHIRHistoryBundle =  Await.result(
        onFhirClient
          .history("Observation", "obsglucose", count = 2)
        , 5 seconds)

      bundle.getHistories.flatMap(_._2).size mustEqual(2)
      bundle.hasNext() mustEqual true
      bundle = Await.result(onFhirClient.next(bundle) , 5 seconds)
      bundle.getHistories.flatMap(_._2).size must be_>=(1)
    }

    "should help reading a specific version" in {
      val resource:Future[Resource] =
        onFhirClient
        .vread("Observation", "obsglucose", "1")

      resource.map(r =>FHIRUtil.extractValue[String](r, "status")) must be_==("final").await
    }

    "should help sending batch requests" in {
      val bundle:Future[FHIRTransactionBatchBundle] =
      onFhirClient.
        batch()
        .entry(_.create(patientWithoutId))
        .entry( _.create(patientWithoutId))
        .entry(
          _
            .delete("Patient")
            .where("identifier", "urn:oid:1.2.36.146.595.217.0.1|12346")
        )

      bundle.map(_.responses.length) must be_==(3).await
      bundle.map(_.responses.map(_._2.httpStatus.isSuccess()).forall(g => g)) must be_==(true).await
    }

    "should help sending operation requests" in {
      val opResponse = Await.result(
        onFhirClient
        .operation("validate")
        .on("Observation")
        .addSimpleParam("mode", "general")
        .addResourceParam("resource", obsGlucose),

        5 seconds
      )

      opResponse.httpStatus mustEqual(StatusCodes.OK)
      opResponse.getOutputParam("return") must not beEmpty
    }

  }

}

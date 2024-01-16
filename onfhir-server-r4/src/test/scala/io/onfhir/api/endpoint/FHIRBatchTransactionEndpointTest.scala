package io.onfhir.api.endpoint

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnFhirTest
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRMarshallers._
import io.onfhir.config.OnfhirConfig
import io.onfhir.path.FhirPathEvaluator
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class FHIRBatchTransactionEndpointTest extends OnFhirTest with FHIREndpoint {
  def actorRefFactory: ActorSystem = system
  implicit def default(implicit system: ActorSystem): RouteTestTimeout = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  val batch1: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/BatchTransaction/batch1.json")).mkString
  val batch2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/BatchTransaction/batch2.json")).mkString

  val transaction1: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/BatchTransaction/transaction1.json")).mkString
  val transaction2: String = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/BatchTransaction/transaction2.json")).mkString


  sequential

 "FHIR Batch Endpoint" should {
    "handle batch request" in {
      Post("/" + OnfhirConfig.baseUri , HttpEntity(batch1)) ~> fhirRoute ~> check {
        eventually(status === OK)
        val response = responseAs[Resource]
        FhirPathEvaluator().evaluateString("type", response).head mustEqual "batch-response"
        //Invalid Observation creation
        //First patient creation in batch
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '400 Bad Request' " +
            ").exists()", response)
        //First patient creation in batch
        FhirPathEvaluator().satisfies(
          "entry.where(" +
              "$this.response.status = '201 Created' and " +
              "$this.response.etag = 'W/\"1\"' and " +
              "$this.response.lastModified.exists() and " +
              "$this.resource.resourceType = 'Patient' and " +
              "$this.resource.name.family='Chalmers'" +
            ").exists()", response)
        //Observation update create
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '201 Created' and " +
            "$this.response.etag = 'W/\"1\"' and " +
            "$this.response.lastModified.exists() and " +
            "$this.resource.resourceType = 'Observation' and " +
            "$this.resource.id='obsbp'" +
            ").exists()", response)
        //Patient conditional create
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '201 Created' and " +
            "$this.response.etag = 'W/\"1\"' and " +
            "$this.response.lastModified.exists() and " +
            "$this.resource.resourceType = 'Patient' and " +
            "$this.resource.identifier.value='12345'" +
            ").exists()", response)
        //Patient conditional delete
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '200 OK' and " +
            "$this.response.etag.empty() and " +
            "$this.response.lastModified.empty() and " +
            "$this.resource.empty() and " +
            "$this.response.outcome.resourceType == 'OperationOutcome' " +
            ").exists()", response)
      }

      Post("/" + OnfhirConfig.baseUri , HttpEntity(batch2)) ~> fhirRoute ~> check {
        eventually(status === OK)
        val response = responseAs[Resource]
        //Check patient deletion
        FhirPathEvaluator().satisfies(
         "entry.where(" +
                  "$this.response.status = '204 No Content' and " +
                  "$this.response.etag = 'W/\"2\"' and " +
                  "$this.resource.empty() and " +
                  "$this.response.outcome.empty()"+
                ").exists()", response)
        //Check patch
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '200 OK' and " +
            "$this.response.etag = 'W/\"2\"' and " +
            "$this.resource.id = 'obsbp' and " +
            "$this.resource.code.coding[0].code = '89354-9' " +
            ").exists()", response)
        //Check search and reads and operation ordering
        //Searching deleted Patient
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '200 OK' and " +
            "$this.resource.total = 0 and " +
            "$this.resource.link.where(relation='self').url.contains('Patient?family=Chalmers') " +
            ").exists()", response)
        //Check Observation read
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '200 OK' and " +
            "$this.response.etag = 'W/\"2\"' and " +
            "$this.response.location.endsWith('/Observation/obsbp/_history/2') and " +
            "$this.resource.resourceType = 'Observation' and " +
            "$this.resource.id = 'obsbp' and " +
            "$this.resource.code.coding[0].code = '89354-9' " +
            ").exists()", response)
        //Check Observation version aware read
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '304 Not Modified'" +
            ").exists()", response)
        //Check Observation version read
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '200 OK' and " +
            "$this.response.etag = 'W/\"1\"' and " +
            "$this.response.location.endsWith('/Observation/obsbp/_history/1') and " +
            "$this.resource.resourceType = 'Observation' and " +
            "$this.resource.id = 'obsbp' and " +
            "$this.resource.code.coding[0].code = '85354-9' " +
            ").exists()", response)
        //Check history instance
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '200 OK' and " +
            "$this.resource.total = 2 and " +
            "$this.resource.entry[0].resource.meta.versionId = '2' and" +
            "$this.resource.entry[1].resource.meta.versionId = '1' " +
            ").exists()", response)
        //Check search
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '200 OK' and " +
            "$this.resource.total = 1 and " +
            "$this.resource.link.where(relation='self').url.contains('Patient?identifier') " +
            ").exists()", response)
      }
    }
  }
/*
  "FHIR Transaction Endpoint" should {
    "handle transaction with inter dependencies" in {
      Post("/" + OnfhirConfig.baseUri , HttpEntity(transaction1)) ~> fhirRoute ~> check {
        eventually(status === OK)
        val response = responseAs[Resource]
        FhirPathEvaluator().evaluateString("type", response).head mustEqual "transaction-response"
        //Check patient creation
        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '201 Created' and " +
            "$this.response.etag = 'W/\"1\"' and " +
            "$this.response.lastModified.exists() and " +
            "$this.resource.resourceType = 'Patient' and " +
            "$this.resource.name.family='Chalmers'" +
            ").exists()", response)

        val pid = FhirPathEvaluator().evaluateString("entry.resource.where(resourceType='Patient').id", response).head

        FhirPathEvaluator().satisfies(
          "entry.where(" +
            "$this.response.status = '201 Created' and " +
            "$this.response.etag = 'W/\"1\"' and " +
            "$this.response.lastModified.exists() and " +
            "$this.resource.resourceType = 'Observation' and " +
            "$this.resource.id = 'obscholesterol' and " +
            "$this.resource.subject.reference='Patient/"+pid+"'" +
            ").exists()", response)
      }
    }

    "reject transaction if there is error" in {
      Post("/" + OnfhirConfig.baseUri , HttpEntity(transaction2)) ~> fhirRoute ~> check {
        eventually(status === BadRequest)
      }
    }

    "handle prefer header in batch/transaction" in {
      Post("/" + OnfhirConfig.baseUri , HttpEntity(batch1)).withHeaders(List((RawHeader("Prefer", "return=minimal")))) ~> fhirRoute ~> check {
        eventually(status === OK)
        val response = responseAs[Resource]
        FhirPathEvaluator().satisfies("entry.resource.empty()", response)
      }
    }
  }*/

}

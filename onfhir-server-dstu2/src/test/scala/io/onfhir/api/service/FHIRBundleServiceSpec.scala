package io.onfhir.api.service

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.RouteTestTimeout
import io.onfhir.OnfhirTest
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.config.OnfhirConfig
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

/**
  * Created by ozan on 7.03.2017.
  */
@RunWith(classOf[JUnitRunner])
class FHIRBundleServiceSpec extends OnfhirTest with FHIREndpoint {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(new FiniteDuration(60, TimeUnit.SECONDS))

  def actorRefFactory = system

  val batchRequest= Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Bundle/Batch/batch-request.json")).mkString
  val batchRequestWInterdependency = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Bundle/Batch/batch-request-w-interdependency.json")).mkString
  val successfulTransactionRequest = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Bundle/Transaction/transaction-request.json")).mkString
  val localReferenceTransaction = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Bundle/Transaction/transaction-request-locref.json")).mkString
  val unsuccessfulTransactionRequest = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Bundle/Transaction/transaction-request-unsuccessful.json")).mkString

  sequential

  "FHIR Bundle Service" should {

    "return a status OK with bundle that contains batch responses" in {
      Post("/" + OnfhirConfig.baseUri, HttpEntity(batchRequest)) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("204 No Content")
        responseAs[String] must contain("There is no such resource matching with given parameters!!!")
        responseAs[String] must contain("200 OK")
        responseAs[String] must contain("\"birthDate\":\"1974-12-25\"")
        responseAs[String] must contain("201 Created")
        responseAs[String] must contain("Conflicting Version")
        responseAs[String] must contain("409 Conflict")
        responseAs[String] must contain("\"total\":0")
        responseAs[String] must contain("404 Not Found")
        responseAs[String] must contain("Resource with type (Patient), id (12334) not found...")
      }

      Post("/" + OnfhirConfig.baseUri, HttpEntity(batchRequestWInterdependency)) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("204 No Content")
        responseAs[String] must contain("There is no such resource matching with given parameters!!!")
        responseAs[String] must contain("200 OK")
        responseAs[String] must contain("\"birthDate\":\"1974-12-25\"")
        responseAs[String] must contain("201 Created")
        responseAs[String] must contain("Conflicting Version")
        responseAs[String] must contain("409 Conflict")
        responseAs[String] must contain("\"total\":0")
        responseAs[String] must contain("404 Not Found")
        responseAs[String] must contain("Resource with type (Patient), id (12334) not found...")
        responseAs[String] must contain("Interdependency to batch entry with url urn:uuid:61ebe359-bfdc-4613-8bf2-c5e300945f0a")
        responseAs[String] must contain("412 Precondition Failed")
      }
    }

   "return a status OK with bundle that contains transaction responses" in {
      Post("/" + OnfhirConfig.baseUri, HttpEntity(successfulTransactionRequest)) ~> routes ~> check {
        val x = responseAs[String]
        println(x)
        status === OK
        responseAs[String] must contain("204 No Content")
        responseAs[String] must contain("There is no such resource matching with given parameters!!!")
        responseAs[String] must contain("200 OK")
        responseAs[String] must contain("\"birthDate\":\"1974-12-25\"")
        responseAs[String] must contain("201 Created")
        responseAs[String] must contain("\"total\":7")
        //responseAs[String] must contain("404 Not Found")
        //responseAs[String] must contain("Resource with type (Patient), id (12334) not found...")
      }
    }


    "convert the local references within transactions to logical references" in {
      Post("/" + OnfhirConfig.baseUri, HttpEntity(localReferenceTransaction)) ~> routes ~> check {
        status === OK
        responseAs[String] must contain("201 Created")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + "?link:Patient.name=ChalmersXXX") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":1")
        responseAs[String] must contain("\"mode\":\"match\"")
      }
    }

    "return a operation outcome and not perform the create operation" in {
      Post("/" + OnfhirConfig.baseUri, HttpEntity(unsuccessfulTransactionRequest)) ~> routes ~> check {
        status === BadRequest
        responseAs[String] must contain("\"resourceType\":\"OperationOutcome\"")
        responseAs[String] must contain("\"severity\":\"error\"")
      }

      Get("/" + OnfhirConfig.baseUri + "/" + "Patient" + "?link:Patient.name=notExist") ~> routes ~> check {
        status === OK
        responseAs[String] must contain("\"type\":\"searchset\"")
        responseAs[String] must contain("\"total\":0")
      }
    }

  }

  /*
  step {
    MongoDB.getDatabase.drop().head()
  }*/
}

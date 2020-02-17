package io.onfhir.authz

import java.util.concurrent.TimeUnit

import akka.http.scaladsl.model.{HttpEntity, StatusCodes}
import io.onfhir.{OnfhirSetup, OnfhirTest}
import io.onfhir.api._
import io.onfhir.api.endpoint.FHIREndpoint
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.{FHIRBatchTransactionService, FHIRCreateService, FHIRDeleteService, FHIRSearchService, FHIRUpdateService}
import io.onfhir.config.OnfhirConfig
import io.onfhir.util.JsonFormatter._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class AuthzManagerTest extends OnfhirTest {

  override def beforeAll() = {
    super.beforeAll()
    AuthzConfigurationManager.authorizationHandler = new SmartAuthorizer()
    AuthzConfigurationManager.tokenResolver = new JWTResolver(OnfhirConfig.authzConfig)
  }


  val observation = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-stu3.json")).mkString.parseJson
  val observation2 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-stu3-2.json")).mkString.parseJson
  val observation3 = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Observation/observation-stu3-3.json")).mkString.parseJson

  val batch = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Bundle/Batch/batch-request-stu3.json")).mkString.parseJson
  val transaction = Source.fromInputStream(getClass.getResourceAsStream("/fhir/samples/Bundle/Transaction/transaction-request-stu3.json")).mkString.parseJson

  val authzContextWriteForPatient  = Some(AuthzContext(true, Some("test-client"), Seq("patient/Observation.write"), None, aud = Seq(OnfhirConfig.fhirRootUrl), sub = Some("f005"), furtherParams = Map("patient" -> "f001")))
  val authzContextWriteForPatient2  = Some(AuthzContext(true, Some("test-client"), Seq("patient/Observation.write"), None, aud = Seq(OnfhirConfig.fhirRootUrl), sub = Some("f006"), furtherParams = Map("patient" -> "f002")))
  val authzContextWriteForPatient3  = Some(AuthzContext(true, Some("test-client"), Seq("patient/Observation.write"), None, aud = Seq(OnfhirConfig.fhirRootUrl), sub = Some("f006"), furtherParams = Map("patient" -> "f003")))

  sequential
  "AuthzManager" should {
    "correctly authorize FHIR create interaction" in {
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.CREATE, requestUri = "/Observation")
      request.initializeCreateRequest("Observation", None, None)
      request.resource = Some(observation)

      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
    }

    "correctly not authorize FHIR create interaction" in {
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.CREATE, requestUri = "/Observation")
      request.initializeCreateRequest("Observation", None, None)
      request.resource = Some(observation)

      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient2, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
    }

    "correctly authorize FHIR update interaction" in {
      //Create the observation in db
      val createdObservationId = createObservation(observation)
      //Check if we can update it
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.UPDATE, requestUri = "/Observation/"+createdObservationId)
      request.initializeUpdateRequest("Observation", Some(createdObservationId), None, None)
      request.resource = Some(observation)
      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
    }

    "correctly not authorize FHIR update interaction due to unauthorized content" in {
      //Create the observation in db
      val createdObservationId = createObservation(observation)
      //Try to change the pid of observation although we don't have right
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.UPDATE, requestUri = "/Observation/"+createdObservationId)
      request.initializeUpdateRequest("Observation", Some(createdObservationId), None, None)
      request.resource = Some(observation2)
      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
    }

    "correctly not authorize FHIR update interaction on unauthorized resource" in {
      //Create the observation2 in db which the first user does not have any right
      val createdObservationId = createObservation(observation2)

      //User is sending authorized content but not authorize for the resource
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.UPDATE, requestUri = "/Observation/"+createdObservationId)
      request.initializeUpdateRequest("Observation", Some(createdObservationId), None, None)
      request.resource = Some(observation)
      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
    }

    "correctly authorize FHIR delete interaction" in {
      //Create the observation in db which the first user does not have any right
      val createdObservationId = createObservation(observation)

      //User is sending authorized content but not authorize for the resource
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.DELETE, requestUri = "/Observation/"+createdObservationId)
      request.initializeDeleteRequest("Observation", Some(createdObservationId), None)

      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
    }

    "correctly not authorize FHIR delete interaction for unauthorized resource" in {
      //Create the observation in db which the first user does not have any right
      val createdObservationId = createObservation(observation2)

      //User is sending authorized content but not authorize for the resource
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.DELETE, requestUri = "/Observation/"+createdObservationId)
      request.initializeDeleteRequest("Observation", Some(createdObservationId), None)

      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
    }

    "correctly authorize FHIR read/vread interaction" in {
      //Create the observation in db which the first user does not have any right
      val createdObservationId = createObservation(observation)

      //User is sending authorized content but not authorize for the resource
      var request = FHIRRequest(interaction = FHIR_INTERACTIONS.READ, requestUri = "/Observation/"+createdObservationId)
      request.initializeReadRequest("Observation", createdObservationId, None, None, None)

      var authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true

      authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true

      request = FHIRRequest(interaction = FHIR_INTERACTIONS.VREAD, requestUri = "/Observation/"+createdObservationId)
      request.initializeVReadRequest("Observation", createdObservationId, "1")

      authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
    }

    "correctly not authorize FHIR read/vread interaction for unauthorized resource" in {
      val createdObservationId = createObservation(observation2)

      //User is sending authorized content but not authorize for the resource
      var request = FHIRRequest(interaction = FHIR_INTERACTIONS.READ, requestUri = "/Observation/"+createdObservationId)
      request.initializeReadRequest("Observation", createdObservationId, None, None, None)

      var authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false

      request = FHIRRequest(interaction = FHIR_INTERACTIONS.VREAD, requestUri = "/Observation/"+createdObservationId)
      request.initializeVReadRequest("Observation", createdObservationId, "1")

      authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
    }

    "correctly filter unauthorized resources for FHIR Search interaction" in {
      createObservation(observation3)

      //User is sending authorized content but not authorize for the resource
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = "/Observation")
      request.initializeSearchRequest("Observation", None)

      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient3, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
      authzResult.resourceRestrictions.nonEmpty mustEqual true
      authzResult.resourceRestrictions.find(r => r.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT && r.valuePrefixList.head._1 == "Patient")
      request.queryParams.nonEmpty mustEqual true

      val response = Await.result(new FHIRSearchService().executeInteraction(request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      (response.responseBody.get \ "total").extract[Int] mustEqual 1
    }

    "correctly not authorize FHIR Compartment Search interaction" in {
      createObservation(observation3)

      //User is sending authorized content but not authorize for the resource
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = "/Observation")
      request.initializeCompartmentSearchRequest("Patient", "f001", "Observation", None)

      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient3, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
    }

    "correctly filter unauthorized resources for FHIR Conditional update/delete" in {
      //Conditional delete on Observations that is performed by Practitioner f006 (observation2, and observation3)
      var request = FHIRRequest(interaction = FHIR_INTERACTIONS.DELETE, requestUri = "/Observation")
      request.initializeDeleteRequest("Observation", None, None)
      request.queryParams = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("performer" -> List("Practitioner/f006")))

      var authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient3, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
      authzResult.resourceRestrictions.nonEmpty mustEqual true
      authzResult.resourceRestrictions.find(r => r.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT && r.valuePrefixList.head._1 == "Patient")
      request.queryParams.size mustEqual 2
      //It should delete all  observation3 resources
      var response = Await.result(new FHIRDeleteService().executeInteraction(request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      response.httpStatus mustEqual StatusCodes.NoContent

      request = FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = "/Observation")
      request.initializeCompartmentSearchRequest("Patient", "f003", "Observation", None)
      response = Await.result(new FHIRSearchService().executeInteraction(request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      (response.responseBody.get \ "total").extract[Int] mustEqual 0
      //But not delete other ones
      request = FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = "/Observation")
      request.initializeSearchRequest("Observation", None)
      request.queryParams = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("performer" -> List("Practitioner/f006")))
      response = Await.result(new FHIRSearchService().executeInteraction(request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      (response.responseBody.get \ "total").extract[Int] > 0 mustEqual true

      //Again create one observation3
      createObservation(observation3)
      //Run a conditional update
      request = FHIRRequest(interaction = FHIR_INTERACTIONS.UPDATE, requestUri = "/Observation")
      request.initializeUpdateRequest("Observation", None, None, None)
      request.resource = Some(observation2)
      request.queryParams = FHIRSearchParameterValueParser.parseSearchParameters("Observation", Map("performer" -> List("Practitioner/f006")))
      //First try with wrong content
      authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient3, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
      //Then try with correct one
      request.resource = Some(observation3)
      authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient3, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
      authzResult.resourceRestrictions.nonEmpty mustEqual true
      authzResult.resourceRestrictions.find(r => r.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT && r.valuePrefixList.head._1 == "Patient")
      request.queryParams.size mustEqual 2

      response = Await.result(new FHIRUpdateService().executeInteraction(request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      response.httpStatus mustEqual StatusCodes.OK
    }

    "correctly authorize batch request" in {
      //Create an observation by specifying its id
      var request = FHIRRequest(interaction = FHIR_INTERACTIONS.UPDATE, requestUri = "/Observation")
      request.initializeUpdateRequest("Observation", Some("1234"), None, None)
      request.resource = Some(observation2)

      var response = Await.result(new FHIRUpdateService().executeInteraction(request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      response.httpStatus mustEqual StatusCodes.Created

      request = FHIRRequest(interaction = FHIR_INTERACTIONS.BATCH, requestUri = "/")
      request.initializeTransactionOrBatchRequest(batch)
      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual true
      //request.childRequests.head.isUnauthorized mustEqual false
      //request.childRequests.apply(1).isUnauthorized mustEqual true

      response = Await.result(new FHIRBatchTransactionService().executeInteraction(request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      val results = (response.responseBody.get \ "entry" \ "response" \ "status").extract[Seq[String]]
      results.contains("201 Created")
      results.contains("401 Unauthorized")
    }

    "correctly not authorize transaction request" in {
      val request = FHIRRequest(interaction = FHIR_INTERACTIONS.TRANSACTION, requestUri = "/")
      request.initializeTransactionOrBatchRequest(transaction)
      val authzResult = Await.result(AuthzManager.forceAuthorization(authzContextWriteForPatient, request), FiniteDuration.apply(30, TimeUnit.SECONDS))
      authzResult.isAuthorized mustEqual false
    }

  }

  def createObservation(resource:Resource):String = {
    val request = FHIRRequest(interaction = FHIR_INTERACTIONS.CREATE, requestUri = "/Observation")
    request.initializeCreateRequest("Observation", None, None)
    request.resource = Some(resource)
    (Await.result(new FHIRCreateService().completeInteraction(request),FiniteDuration.apply(30, TimeUnit.SECONDS)).responseBody.get \ "id").extract[String]
  }

}

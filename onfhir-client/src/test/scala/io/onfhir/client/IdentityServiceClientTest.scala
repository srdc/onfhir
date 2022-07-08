package io.onfhir.client

import akka.actor.ActorSystem
import io.onfhir.api.service.IFhirIdentityCache
import io.onfhir.api.util.FHIRUtil
import io.onfhir.util.JsonFormatter.parseFromJson
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterAll

import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.language.postfixOps

class IdentityServiceClientTest extends Specification with BeforeAfterAll {
  val baseUrl = "http://localhost:8080/fhir"
  val patientWithoutId =  Source.fromInputStream(getClass.getResourceAsStream("/patient-without-id.json")).mkString.parseJson
  val patientWithoutLink1 =  Source.fromInputStream(getClass.getResourceAsStream("/patient-with-link.json")).mkString.parseJson
  val patientWithoutLink2 =  Source.fromInputStream(getClass.getResourceAsStream("/patient-with-link2.json")).mkString.parseJson

  implicit val actorSystem = ActorSystem("OnFhirClientTest")
  implicit val ec:ExecutionContext = ExecutionContext.global
  val onFhirClient = OnFhirNetworkClient.apply(baseUrl)

  var pid1:String = _

  val identityService = new IdentityServiceClient(onFhirClient)
  //Mock identity cache
  val identityCache:IFhirIdentityCache = new IFhirIdentityCache {
    val localCache:mutable.HashMap[String, String] = new mutable.HashMap[String, String]()
    override def storeIdentity(resourceType: String, identifier: String, system: Option[String], correspondingId: String): Future[Unit] = {
      Future.apply(
        localCache.put(s"$resourceType:${system.getOrElse("_")}:$identifier", correspondingId)
      )
    }

    override def findMatching(resourceType: String, identifier: String, system: Option[String]): Future[Option[String]] = {
      Future.apply(
        localCache.get(s"$resourceType:${system.getOrElse("_")}:$identifier")
      )
    }
  }

  override def beforeAll():Unit = {
    val createdPatient1 =
      Await.result( onFhirClient
        .create(patientWithoutId)
        .executeAndReturnResource(), 5 seconds)
    pid1 = FHIRUtil.extractIdFromResource(createdPatient1)

    Await.result(onFhirClient.update(patientWithoutLink1).execute(), 5 seconds)
    Await.result(onFhirClient.update(patientWithoutLink2).execute(), 5 seconds)
  }

  override def afterAll():Unit = {
    Await.result( onFhirClient.delete("Patient", pid1).execute(),  5 seconds)
    Await.result( onFhirClient.delete("Patient", "pat1").execute(),  5 seconds)
    Await.result( onFhirClient.delete("Patient", "pat2").execute(),  5 seconds)
  }

  "IdentityServiceClient" should {
    "find single matching patient with identifier system and value" in {
      Await.result(identityService.findMatching("Patient", "12345", Some("urn:oid:1.2.36.146.595.217.0.1")), 5 seconds) shouldEqual Some(pid1)
    }
    "find single matching patient with identifier value" in {
      Await.result(identityService.findMatching("Patient", "12345", None), 5 seconds) shouldEqual Some(pid1)
    }
    "find  matching patient with identifier system and value among multiple matches" in {
      Await.result(identityService.findMatching("Patient", "654321", Some("urn:oid:0.1.2.3.4.5.6.7")), 5 seconds) shouldEqual Some("pat2")
    }
    "find  matching patient with identifier system and value among multiple matches from cache" in {
      val identityService = new IdentityServiceClient(onFhirClient, Some(identityCache))
      Await.result(identityService.findMatching("Patient", "654321", Some("urn:oid:0.1.2.3.4.5.6.7")), 5 seconds) shouldEqual Some("pat2")
      Await.result(identityService.findMatching("Patient", "654321", Some("urn:oid:0.1.2.3.4.5.6.7")), 5 seconds) shouldEqual Some("pat2")
    }
  }
}

package io.onfhir.client

import akka.actor.ActorSystem
import io.onfhir.api.util.FHIRUtil
import io.onfhir.client.intrcp.BasicAuthenticationInterceptor
import org.json4s.JsonAST.{JBool, JObject, JString}
import org.specs2.mutable.Specification

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

class TerminologyServiceClientTest extends Specification {
  //Test with LOINC's terminology server
  val baseUrl = "https://fhir.loinc.org/"
  val username = "<!!!!FILL HERE BEFORE TESTING>"
  val password = "<!!!!FILL HERE BEFORE TESTING>"

  implicit val actorSystem: ActorSystem = ActorSystem("TerminologyServiceClientTest")
  implicit val ec:ExecutionContext = ExecutionContext.global

  val terminologyServiceClient = new TerminologyServiceClient(OnFhirNetworkClient.apply(baseUrl, new BasicAuthenticationInterceptor(username, password)))

  sequential

  "TerminologyServiceClientTest" should {
    "handle lookup operation" in {
      val result = Await.result(terminologyServiceClient.lookup("1963-8", "http://loinc.org"), Duration.Inf)
      result.isEmpty shouldEqual false
      FHIRUtil.getParameterValueByName(result.get, "display") shouldEqual Some(JString("Bicarbonate [Moles/volume] in Serum or Plasma"))
    }

    "handle translate operation with given concept map on code and system" in {
      val result = Await.result(terminologyServiceClient.translate("LP129054-5", "http://loinc.org", "https://www.ncbi.nlm.nih.gov/clinvar"), Duration.Inf)
      FHIRUtil.getParameterValueByName(result, "result") shouldEqual Some(JBool(true))
    }

    "handle translate operation with given concept map on coding" in {
      val coding = JObject(
        "code" -> JString("LP129054-5"),
        "system" -> JString("http://loinc.org")
      )
      val result = Await.result(terminologyServiceClient.translate(coding, "https://www.ncbi.nlm.nih.gov/clinvar"), Duration.Inf)
      FHIRUtil.getParameterValueByName(result, "result") shouldEqual Some(JBool(true))
    }
  }

}

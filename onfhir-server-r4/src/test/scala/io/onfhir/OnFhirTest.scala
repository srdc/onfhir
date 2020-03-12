package io.onfhir

import java.util.concurrent.TimeUnit

import akka.http.scaladsl.testkit.Specs2RouteTest
import io.onfhir.db.MongoDB
import io.onfhir.r4.config.FhirR4Configurator
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterAll

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object OnfhirSetup  {
  lazy val environment:Onfhir = {
     Onfhir.apply(new FhirR4Configurator)
  }
}

trait OnFhirTest extends Specification with Specs2RouteTest with BeforeAfterAll {

  override def beforeAll() = OnfhirSetup.environment

  override def afterAll() = Await.result(MongoDB.getDatabase.drop().head(), Duration.apply(5, TimeUnit.SECONDS))
}

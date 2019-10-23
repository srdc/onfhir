package io.onfhir

import java.util.concurrent.TimeUnit

import akka.http.scaladsl.testkit.Specs2RouteTest
import io.onfhir.db.MongoDB
import io.onfhir.dstu2.config.DSTU2Configurator
import org.junit.rules.Timeout
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterAll

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object OnfhirSetup  {
  lazy val environment:Onfhir = {
    api.CONFORMANCE_FILE_SUFFIX = ".json"
    Onfhir.apply(new DSTU2Configurator())
  }
}

// use the createDB lazy val to create the database once for every specification inheriting from
// the DatabaseSpec trait
trait OnfhirTest extends Specification with Specs2RouteTest with BeforeAfterAll {
  //lazy val dbSetup = OnfhirSetup
  //override def map(fs: =>Fragments) =   Step(dbSetup.createDb)  ^ fs

  //override def map(fragments:Fragments) =

  override def beforeAll() = OnfhirSetup.environment

  override def afterAll() = Await.result(MongoDB.getDatabase.drop().head(), Duration.apply(2, TimeUnit.SECONDS))
}

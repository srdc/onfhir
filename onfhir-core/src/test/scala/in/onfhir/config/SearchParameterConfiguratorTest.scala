package in.onfhir.config

import io.onfhir.config.SearchParameterConfigurator
import org.specs2.mutable.Specification

class SearchParameterConfiguratorTest extends Specification {
  sequential

  "SearchParameterConfigurator" should {
    "parse search parameter simple path expressions" in {
      val expr = "ActivityDefinition.useContext.code"

      val (path, restriction) = SearchParameterConfigurator.parsePathExpression(expr)
      path mustEqual "useContext.code"
      restriction must beEmpty
    }

    "parse search parameter path expressions with as" in {
      var expr = "(ActivityDefinition.useContext.value as CodeableConcept)"

      val (path, restriction) = SearchParameterConfigurator.parsePathExpression(expr)
      path mustEqual "useContext.valueCodeableConcept"
      restriction must beEmpty

      expr = "(Observation.value as CodeableConcept).text"
      val (path2, restriction2) = SearchParameterConfigurator.parsePathExpression(expr)
      path2 mustEqual "valueCodeableConcept.text"
      restriction2 must beEmpty
    }

    "parse search parameter path expressions with as within expression" in {
      var expr = "Condition.abatement.as(Age)"
      val (path, restriction) = SearchParameterConfigurator.parsePathExpression(expr)
      path mustEqual "abatementAge"
      restriction must beEmpty

      expr = "Condition.x.as(dateTime).text"
      val (path2, restriction2) = SearchParameterConfigurator.parsePathExpression(expr)
      path2 mustEqual "xDateTime.text"
      restriction2 must beEmpty
    }

    "parse search parameter path expressions with restriction" in {
      var expr = "ActivityDefinition.relatedArtifact.where(type='composed-of').resource"
      val (path, restriction) = SearchParameterConfigurator.parsePathExpression(expr)
      path mustEqual "relatedArtifact.resource"
      restriction mustEqual Seq("@.type" -> "composed-of")

      expr= "Library.relatedArtifact.where(type=&#39;composed-of&#39;).resource"
      val (path2, restriction2) = SearchParameterConfigurator.parsePathExpression(expr)
      path2 mustEqual "relatedArtifact.resource"
      restriction2 mustEqual Seq("@.type" -> "composed-of")

      expr= "Library.relatedArtifact.where(type=&#39;composed-of&#39;)"
      val (path3, restriction3) = SearchParameterConfigurator.parsePathExpression(expr)
      path3 mustEqual "relatedArtifact"
      restriction3 mustEqual Seq("type" -> "composed-of")
    }

    "parse search parameter path expressions with index" in {
      val expr = "Bundle.entry[0].resource"
      val (path, restriction) = SearchParameterConfigurator.parsePathExpression(expr)
      path mustEqual "entry[0].resource"
      restriction must beEmpty
    }
  }
}

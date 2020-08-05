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

    "parse search parameter xpath expressions" in {
      var pr  = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathElementName, "f:Communication")
      pr.successful mustEqual true
      pr.get mustEqual "Communication"

      pr  = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathElementName, "f:valueCode")
      pr.successful mustEqual true
      pr.get mustEqual "valueCode"

      val pr2  = SearchParameterConfigurator.parse[(String, Seq[(String, String)])](SearchParameterConfigurator.xpathExtensionItem, "f:extension[@url='http://www.c3-cloud.eu/fhir/StructureDefinition/senderStatus']")
      pr2.successful mustEqual true
      pr2.get._1 mustEqual "extension[i]"
      pr2.get._2 mustEqual Seq("url" -> "http://www.c3-cloud.eu/fhir/StructureDefinition/senderStatus")

      val pr31 =  SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathConditionItem, "f:type/@value='composed-of'")
      pr31.successful mustEqual true
      pr31.get._1 mustEqual "type"
      pr31.get._2 mustEqual "composed-of"


      var pr3 = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathItem, "f:Communication")
      pr3.successful mustEqual true
      pr3 = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathItem, "f:valueCode")
      pr3.successful mustEqual true
      pr3 = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathItem, "f:extension[@url='http://www.c3-cloud.eu/fhir/StructureDefinition/senderStatus']")
      pr3.successful mustEqual true
      pr3 = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathItem, "f:relatedArtifact[f:type/@value='composed-of']")
      pr3.successful mustEqual true
      pr3.get._2 mustEqual Seq("type" -> "composed-of")

      var pr4 = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathPath, "f:Communication/f:extension[@url='http://www.c3-cloud.eu/fhir/StructureDefinition/senderStatus']/f:valueCode")
      pr4.successful mustEqual true
      pr4 = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathPath, "f:ActivityDefinition/f:relatedArtifact[f:type/@value='composed-of']/f:resource")
      pr4.successful mustEqual true
      pr4.get.apply(1)._1 mustEqual "relatedArtifact"
      pr4.get.apply(1)._2 mustEqual Seq("type" -> "composed-of")


      var pr5 = SearchParameterConfigurator.parse(SearchParameterConfigurator.xpathMultiplePath, "f:Communication/f:extension[@url='http://www.c3-cloud.eu/fhir/StructureDefinition/senderStatus']/f:valueCode")
      pr5.successful mustEqual true
      pr5.get.length mustEqual 1
      pr5.get.head.length mustEqual 3
    }

    "parse search parameter xpath expression" in {
      val xpath = "f:Communication/f:component/f:code/f:text"
      val paths = SearchParameterConfigurator.parseXpath("Communication", xpath)
      paths.length mustEqual 1
      paths.head._1 mustEqual "component.code.text"
      paths.head._2 must beEmpty
    }

    "parse search parameter xpath expression with extension" in {
      val xpath = "f:Communication/f:extension[@url='http://www.c3-cloud.eu/fhir/StructureDefinition/senderStatus']/f:valueCode"
      val paths = SearchParameterConfigurator.parseXpath("Communication", xpath)
      paths.length mustEqual 1
      paths.head._1 mustEqual "extension[i].valueCode"
      paths.head._2 mustEqual Seq("@.url" -> "http://www.c3-cloud.eu/fhir/StructureDefinition/senderStatus")
    }

    "parse search parameter xpath expression with restriction" in {
      var xpath = "f:ActivityDefinition/f:relatedArtifact[f:type/@value='composed-of']/f:resource"
      var paths = SearchParameterConfigurator.parseXpath("ActivityDefinition", xpath)
      paths.length mustEqual 1
      paths.head._1 mustEqual "relatedArtifact.resource"
      paths.head._2 mustEqual Seq("@.type" -> "composed-of")

      xpath = "f:ActivityDefinition/f:relatedArtifact[f:type/@value=&#39;composed-of&#39;]/f:resource"
      paths = SearchParameterConfigurator.parseXpath("ActivityDefinition", xpath)
      paths.length mustEqual 1


      xpath = "f:ActivityDefinition/f:relatedArtifact[f:type/@value='composed-of' and f:code/@value='ali']/f:resource"
      paths = SearchParameterConfigurator.parseXpath("ActivityDefinition", xpath)
      paths.length mustEqual 1
      paths.head._1 mustEqual "relatedArtifact.resource"
      paths.head._2 mustEqual Seq("@.type" -> "composed-of", "@.code" -> "ali")
    }

    "parse search parameter xpath expression with multiple paths" in {
      val xpath = "f:Consent/f:dateTime | f:SupplyRequest/f:authoredOn | f:RiskAssessment/f:occurrenceDateTime | f:CareTeam/f:period | f:FamilyMemberHistory/f:date | f:Encounter/f:period | f:AllergyIntolerance/f:assertedDate | f:CarePlan/f:period | f:EpisodeOfCare/f:period | f:Procedure/f:performedDateTime | f:Procedure/f:performedPeriod | f:List/f:date | f:Immunization/f:date | f:Flag/f:period | f:Observation/f:effectiveDateTime | f:Observation/f:effectivePeriod | f:DiagnosticReport/f:effectiveDateTime | f:DiagnosticReport/f:effectivePeriod | f:Composition/f:date | f:DetectedIssue/f:date | f:ClinicalImpression/f:date"
      val paths = SearchParameterConfigurator.parseXpath("Observation", xpath)
      paths.length mustEqual 2
      paths.head._1 mustEqual "effectiveDateTime"
      paths.head._2 must beEmpty
      paths.last._1 mustEqual "effectivePeriod"
      paths.last._2 must beEmpty
    }

  }
}

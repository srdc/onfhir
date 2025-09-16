package io.onfhir.authz

import io.onfhir.api.{FHIR_INTERACTIONS, FHIR_PARAMETER_CATEGORIES, FHIR_PARAMETER_TYPES}
import io.onfhir.api.model.Parameter
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.config.{FSConfigReader, FhirConfigurationManager}
import io.onfhir.r4.config.FhirR4Configurator
import org.specs2.mutable.Specification
import io.onfhir.authz.AuthzResult
import scala.language.postfixOps

class SmartAuthorizerTest extends Specification {
  sequential
  val fsConfigReader = new FSConfigReader(fhirVersion = "R4")
  var r4Configurator = new FhirR4Configurator()

  val serverConfig = r4Configurator
    .initializeServerPlatform(
      configReader = fsConfigReader,
      Set.empty
    )
  FhirConfigurationManager.fhirConfig = serverConfig
  val fhirSearchParameterValueParser = new FHIRSearchParameterValueParser(serverConfig)
  FhirConfigurationManager.fhirSearchParameterValueParser = fhirSearchParameterValueParser
  val smartAuthorizer = new SmartAuthorizer(FhirConfigurationManager)

  /**
   * Tests on companion object common methods
   */
  "SmartAuthorizer - companion" should {
    "parse Smart-on-Fhir v1 scopes" in {
      SmartAuthorizer.parse("patient/Observation.read") must beSome(WGHeartScope("patient", "Observation", permissions = Set("r", "s")))
      SmartAuthorizer.parse("patient/Observation.write") must beSome(WGHeartScope("patient", "Observation", permissions = Set("c", "u", "d")))
      SmartAuthorizer.parse("patient/Observation.*") must beSome(WGHeartScope("patient", "Observation", permissions = Set("*")))
      SmartAuthorizer.parse("conf/N.read") must beSome(WGHeartScope("conf", "N", permissions = Set("r", "s")))
    }

    "parse simple Smart-on-Fhir v2 scopes" in {
      SmartAuthorizer.parse("patient/Observation.read") must beSome(WGHeartScope("patient", "Observation", permissions = Set("r", "s")))
      SmartAuthorizer.parse("patient/Observation.write") must beSome(WGHeartScope("patient", "Observation", permissions = Set("c", "u", "d")))
      SmartAuthorizer.parse("patient/Observation.cruds") must beSome(WGHeartScope("patient", "Observation", permissions = Set("c", "r", "u", "d", "s")))
      SmartAuthorizer.parse("patient/Observation.cu") must beSome(WGHeartScope("patient", "Observation", permissions = Set("c", "u")))
      SmartAuthorizer.parse("patient/Observation.$stats") must beSome(WGHeartScope("patient", "Observation", permissions = Set("$stats")))
    }

    "parse Smart-on-Fhir v2 scopes" in {
      SmartAuthorizer.parse("patient/Observation.rs?category=laboratory&status=final", fhirSearchParameterValueParser) must beSome(WGHeartScope("patient", "Observation", permissions = Set("r", "s"),
        query = Some(List(
          Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory")),
          Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "status", valuePrefixList = Seq("" -> "final")),
        ))))
    }

    "combine mergeable scopes - distinct result sets" in {
      val scope1 = SmartAuthorizer.parse("patient/Observation.rs?category=laboratory", fhirSearchParameterValueParser).get
      val scope2 = SmartAuthorizer.parse("patient/Observation.rs?category=vital-sign", fhirSearchParameterValueParser).get
      SmartAuthorizer.tryCombiningScopes(scope1, scope2) must beSome(
        WGHeartScope(
          "patient",
          "Observation",
          Set("r","s"),
          query = Some(List(
            Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign"))
          ))
        )
      )
    }

    "combine mergeable scopes - distinct result sets with extra params" in {
      val scope1 = SmartAuthorizer.parse("patient/Observation.rs?category=laboratory&status=final", fhirSearchParameterValueParser).get
      val scope2 = SmartAuthorizer.parse("patient/Observation.rs?category=vital-sign&status=final", fhirSearchParameterValueParser).get
      SmartAuthorizer.tryCombiningScopes(scope1, scope2) must beSome(
        WGHeartScope(
          "patient",
          "Observation",
          Set("r", "s"),
          query = Some(List(
            Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "status", valuePrefixList = Seq("" -> "final")),
            Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign"))
          ))
        )
      )
    }

    "combine mergeable scopes - one scope covers other - case 1" in {
      val scope1 = SmartAuthorizer.parse("patient/Observation.rs", fhirSearchParameterValueParser).get
      val scope2 = SmartAuthorizer.parse("patient/Observation.rs?category=vital-sign", fhirSearchParameterValueParser).get
      SmartAuthorizer.tryCombiningScopes(scope1, scope2) must beSome(
        WGHeartScope(
          "patient",
          "Observation",
          Set("r", "s"),
          query = None
        )
      )
    }

    "combine mergeable scopes - one scope covers other - case 2" in {
      val scope1 = SmartAuthorizer.parse("patient/Observation.rs?category=laboratory,vital-sign", fhirSearchParameterValueParser).get
      val scope2 = SmartAuthorizer.parse("patient/Observation.rs?category=vital-sign&status=final", fhirSearchParameterValueParser).get
      SmartAuthorizer.tryCombiningScopes(scope1, scope2) must beSome(
        WGHeartScope(
          "patient",
          "Observation",
          Set("r", "s"),
          query = Some(
            List(
              Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign"))
            )
          )
        )
      )
    }

    "keep non-mergeable scopes - totally distinct" in {
      val scope1 = SmartAuthorizer.parse("patient/Observation.rs?category=laboratory", fhirSearchParameterValueParser).get
      val scope2 = SmartAuthorizer.parse("patient/Observation.rs?code=XXX", fhirSearchParameterValueParser).get
      SmartAuthorizer.tryCombiningScopes(scope1, scope2) must be empty
    }

    "keep non-mergeable scopes - have some intersection but distinct" in {
      val scope1 = SmartAuthorizer.parse("patient/Observation.rs?category=laboratory", fhirSearchParameterValueParser).get
      val scope2 = SmartAuthorizer.parse("patient/Observation.rs?category=vital-sign&code=XXX", fhirSearchParameterValueParser).get
      SmartAuthorizer.tryCombiningScopes(scope1, scope2) must be empty
    }
  }

  "SmartAuthorizer" should {
    "authorize or reject for system level scopes" in {
        val authzContext = AuthzContext(isActive = true,
          scopes =
            Seq(
              "system/Patient.cud",
              "system/Observation.*?category=laboratory", "system/Observation.rs?category=vital-sign", "system/Observation.s?category=symptom", //mergeable scopes
              "system/Condition.c?category=problem",
              "system/RiskAssessment.read", "system/RiskAssessment.rs?risk=cv", //Overriding scope
              "system/Goal.rs?category=behavioral&lifecycle-status=active", "system/Goal.rs?category=treatment", //Non mergeable scopes
              "ignored" //Unknown scopes are ignored
            )
        )
        smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.CREATE, Some("Patient"), None) === AuthzResult.success()
        smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.UPDATE, Some("Patient"), None) === AuthzResult.success()
        smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.SEARCH, Some("Patient"), None).isAuthorized === false

        //Check permission specific scope merging
        smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.SEARCH, Some("Observation"), None) ===
          AuthzResult.filtering(
            AuthzConstraints(
              filters = Seq(
                List(Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign", ""-> "symptom")))
              )
            )
          )
        smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.READ, Some("Observation"), None) ===
          AuthzResult.filtering(
            AuthzConstraints(
              filters = Seq(
                List(Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign")))
              )
            )
          )
        smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.UPDATE, Some("Observation"), None) ===
          AuthzResult.filtering(
            AuthzConstraints(
              filters = Seq(
                List(
                  Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory")))
              )
            )
          )

      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.SEARCH, Some("RiskAssessment"), None) === AuthzResult.success()
      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.CREATE, Some("RiskAssessment"), None).isAuthorized === false

      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.SEARCH, Some("Goal"), None) ===
        AuthzResult.filtering(
          AuthzConstraints(
            filters = Seq(
              List(
                Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "treatment")),
              ),
              List(
                Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "behavioral")),
                Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "lifecycle-status", valuePrefixList = Seq("" -> "active")),
              )
            )
          )
        )
    }

    "authorize or reject for patient level scopes" in {
      val authzContext =
        AuthzContext(isActive = true,
          scopes =
            Seq(
              "patient/Observation.*?category=laboratory,vital-sign", "system/Observation.rs?category=vital-sign&status=final", //mergeable scopes
              "patient/Condition.cud",
              "patient/RiskAssessment.read?invalid=XXX", //Invalid FHIR query part
              "patient/Goal.xxx", //Invalid permissions
              "ignored" //Unknown scopes are ignored
            ),
          furtherParams = Map("patient" -> "123")
        )

      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.CREATE, Some("Condition"), None) ===
        AuthzResult.filtering(
          AuthzConstraints.apply(
            filters = Seq(List(
              Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.COMPARTMENT, paramType = FHIR_PARAMETER_TYPES.REFERENCE, name = "", valuePrefixList = Seq("Patient" -> "123"), chain = Seq("" -> "patient", ""-> "asserter")),
            ))
          )
        )
      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.SEARCH, Some("Condition"), None).isAuthorized === false
      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.SEARCH, Some("RiskAssessment"), None).isAuthorized === false
      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.SEARCH, Some("Goal"), None).isAuthorized === false

      smartAuthorizer.authorize(authzContext, FHIR_INTERACTIONS.CREATE, Some("Observation"), None) ===
        AuthzResult.filtering(
          AuthzConstraints.apply(
            filters = Seq(List(
              Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.COMPARTMENT, paramType = FHIR_PARAMETER_TYPES.REFERENCE, name = "", valuePrefixList = Seq("Patient" -> "123"), chain = Seq("" -> "subject", ""-> "performer")),
              Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign"))
            ))
          )
        )
    }
  }
}

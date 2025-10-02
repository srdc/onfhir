package io.onfhir.authz

import com.typesafe.config.ConfigFactory
import io.onfhir.api.{FHIR_INTERACTIONS, FHIR_PARAMETER_CATEGORIES, FHIR_PARAMETER_TYPES}
import io.onfhir.api.model.{FHIRRequest, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.config.{FSConfigReader, FhirConfigurationManager}
import io.onfhir.r4.config.FhirR4Configurator
import org.specs2.mutable.Specification
import io.onfhir.authz.AuthzResult
import org.json4s.JsonAST.JString

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
  val smartAuthorizer = new SmartAuthorizer(None, FhirConfigurationManager)

  val smartAuthorizationConf = ConfigFactory.load("smart-authorization.conf")
  val smartAuthorizer2 = new SmartAuthorizer(Some(smartAuthorizationConf), FhirConfigurationManager)

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
        smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.CREATE, resourceType = Some("Patient"))) === AuthzResult.success()
        smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.UPDATE, resourceType = Some("Patient"))) === AuthzResult.success()
        smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("Patient"))).isAuthorized === false

        //Check permission specific scope merging
        smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("Observation"))) ===
          AuthzResult.filtering(
            AuthzConstraints(
              filters = Seq(
                List(Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign", ""-> "symptom")))
              )
            )
          )
        smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.READ, resourceType = Some("Observation"))) ===
          AuthzResult.filtering(
            AuthzConstraints(
              filters = Seq(
                List(Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign")))
              )
            )
          )
        smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.UPDATE, resourceType = Some("Observation"))) ===
          AuthzResult.filtering(
            AuthzConstraints(
              filters = Seq(
                List(
                  Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory")))
              )
            )
          )

      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("RiskAssessment"))) === AuthzResult.success()
      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.CREATE, resourceType = Some("RiskAssessment"))).isAuthorized === false

      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("Goal"))) ===
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
          furtherParams = Map("patient" -> JString("123"))
        )

      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.CREATE, resourceType = Some("Condition"))) ===
        AuthzResult.filtering(
          AuthzConstraints.apply(
            filters = Seq(List(
              Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.COMPARTMENT, paramType = FHIR_PARAMETER_TYPES.REFERENCE, name = "", valuePrefixList = Seq("Patient" -> "123"), chain = Seq("" -> "patient", ""-> "asserter")),
            ))
          )
        )
      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("Condition"))).isAuthorized === false
      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("RiskAssessment"))).isAuthorized === false
      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("Goal"))).isAuthorized === false

      smartAuthorizer.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.CREATE, resourceType = Some("Observation"))) ===
        AuthzResult.filtering(
          AuthzConstraints.apply(
            filters = Seq(List(
              Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.COMPARTMENT, paramType = FHIR_PARAMETER_TYPES.REFERENCE, name = "", valuePrefixList = Seq("Patient" -> "123"), chain = Seq("" -> "subject", ""-> "performer")),
              Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory", "" -> "vital-sign"))
            ))
          )
        )
    }

    "authorize for default scopes" in {
      val authzContext =
        AuthzContext(isActive = true,
          scopes =
            Seq(
              "patient/Observation.cud?category=laboratory"
            ),
          furtherParams = Map("patient" -> JString("123"))
        )
      smartAuthorizer2.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("ValueSet"))) === AuthzResult.success()
      smartAuthorizer2.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.SEARCH, resourceType = Some("CodeSystem"))).isAuthorized === false
    }

    "authorize with further restrictions" in {
      val authzContext =
        AuthzContext(isActive = true,
          scopes =
            Seq(
              "patient/Observation.cud?category=laboratory"
            ),
          furtherParams = Map("patient" -> JString("123"), "fhirUser" -> JString("Practitioner/135131"))
        )
      smartAuthorizer2.authorize(authzContext, FHIRRequest(requestUri = "", interaction = FHIR_INTERACTIONS.CREATE, resourceType = Some("Observation"))) ===
        AuthzResult.filtering(AuthzConstraints(
          filters = Seq(List(
            Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.COMPARTMENT, paramType = FHIR_PARAMETER_TYPES.REFERENCE, name = "", valuePrefixList = Seq("Patient" -> "123"), chain = Seq("" -> "subject", "" -> "performer")),
            Parameter(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, paramType = FHIR_PARAMETER_TYPES.TOKEN, name = "category", valuePrefixList = Seq("" -> "laboratory"))
          )),
          contentConstraints = Seq(
            "performer.reference.exists($this = %claims.fhirUser)"
          )
        ))
    }
  }
}

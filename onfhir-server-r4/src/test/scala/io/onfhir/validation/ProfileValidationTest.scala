package io.onfhir.validation

import io.onfhir.api.{DEFAULT_IMPLEMENTED_FHIR_OPERATIONS, DEFAULT_RESOURCE_PATHS, Resource}
import io.onfhir.api.model.{FhirLiteralReference, FhirReference}
import io.onfhir.api.util.IOUtil
import io.onfhir.api.validation.IReferenceResolver
import io.onfhir.config.{FhirConfigurationManager, FhirServerConfig, OnfhirConfig, ResourceConf}
import io.onfhir.r4.config.FhirR4Configurator
import io.onfhir.r4.parsers.StructureDefinitionParser
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class ProfileValidationTest extends Specification {
  var dataTypeProfileResources = IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4, "profiles-types.json", Set("StructureDefinition"))
  var resourceProfileResources = IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4, "profiles-resources.json", Set("StructureDefinition"))

  var r4Configurator = new FhirR4Configurator()

  var fhirConfig = new FhirServerConfig("R4")
  fhirConfig.FHIR_RESOURCE_TYPES = resourceProfileResources.flatMap(r4Configurator.getTypeFromStructureDefinition).toSet
  val allDataTypes = dataTypeProfileResources.flatMap(r4Configurator.getTypeFromStructureDefinition)
  fhirConfig.FHIR_COMPLEX_TYPES = allDataTypes.filter(_.head.isUpper).toSet
  fhirConfig.FHIR_PRIMITIVE_TYPES = allDataTypes.filter(_.head.isLower).toSet

  val sdParser = new StructureDefinitionParser(fhirConfig.FHIR_COMPLEX_TYPES, fhirConfig.FHIR_PRIMITIVE_TYPES)

  //Initialize the environment
  val resourceProfiles = resourceProfileResources.map(sdParser.parseProfile)
  val dataTypeProfiles = dataTypeProfileResources.map(sdParser.parseProfile)
  val otherProfiles = IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4, "profiles-others.json", Set("StructureDefinition")).map(sdParser.parseProfile)
  val extensions = IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4, "extension-definitions.json", Set("StructureDefinition")).map(sdParser.parseProfile)

  val valueSetsOrCodeSystems =
    IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4,"valuesets.json", Set("ValueSet", "CodeSystem")) ++
      IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4,"v3-codesystems.json", Set("ValueSet", "CodeSystem")) ++
      IOUtil.readStandardBundleFile(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS_R4,"v2-tables.json", Set("ValueSet", "CodeSystem"))

  val extraProfiles = Seq(
    IOUtil.readModuleResource("/fhir/validation/profiles/MyObservation.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MySampledData.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyMyObservation.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyList.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyList2.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyExtension.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyExtension2.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyFamilyMemberHistory.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyFamilyMemberHistoryRelationshipExtension1.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyFamilyMemberHistoryRelationshipExtension2.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/MyFamilyMemberHistoryRelationshipExtension3.StructureDefinition.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-observation-clinical-result.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-observation-lab.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-birthsex.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-ethnicity.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-genderIdentity.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-patient.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-race.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-sex.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-specimen.json"),
    IOUtil.readModuleResource("/fhir/validation/profiles/StructureDefinition-us-core-tribal-affiliation.json")
  ).map(sdParser.parseProfile)


  fhirConfig.profileRestrictions = (resourceProfiles ++ dataTypeProfiles ++ otherProfiles ++ extensions ++ extraProfiles).map(p => p.url -> Map("latest" -> p)).toMap
  fhirConfig.valueSetRestrictions = new TerminologyParser().parseValueSetBundle(valueSetsOrCodeSystems)

  //Make reference validation policy as enforced for DiagnosticReport
  fhirConfig.resourceConfigurations = Map(
    "DiagnosticReport" -> ResourceConf(
      resource = "DiagnosticReport",
      profile = None,
      supportedProfiles = Set.empty[String],
      interactions = Set.empty[String],
      searchParams = Set.empty,
      versioning = "no-version",
      readHistory = true,
      updateCreate = true,
      conditionalCreate = true,
      conditionalRead = "",
      conditionalUpdate = true,
      conditionalDelete = "",
      searchInclude = Set.empty[String],
      searchRevInclude = Set.empty[String],
      referencePolicies = Set("literal","enforced")
    )
  )

  //Reference resolver for tests
  var referenceResolverForLipidProfileSample = new IReferenceResolver {
    override def resolveReference(reference: FhirReference): Future[Option[Resource]] = {
      Future.apply(
        reference match {
          case FhirLiteralReference(_, "Observation", rid, _) =>
            rid match {
              case "cholesterol" =>
                Some(IOUtil.readModuleResource("/fhir/validation/dreport/cholesterol.json"))
              case "triglyceride" =>
                Some(IOUtil.readModuleResource("/fhir/validation/dreport/tryglyceride.json"))
              case "hdlcholesterol" =>
                Some(IOUtil.readModuleResource("/fhir/validation/dreport/hdlcholesterol.json"))
              case "ldlcholesterol" =>
                Some(IOUtil.readModuleResource("/fhir/validation/dreport/ldlcholesterol.json"))
              case "extra" =>
                Some(IOUtil.readModuleResource("/fhir/validation/valid/observation-bp.json"))
              case _ => None
            }
          case _ => None
        }
      )
    }

    /**
     * Check if a referenced resource exist
     *
     * @param reference FHIR  reference
     * @param profiles  Profiles that resource is expected to conform
     */
    override def isReferencedResourceExist(reference: FhirReference, profiles: Set[String]): Future[Boolean] = {
      Future.apply(
        reference match {
          case FhirLiteralReference(_, "Observation", rid, _) =>
            rid match {
              case "cholesterol" => profiles.isEmpty || profiles.contains("http://hl7.org/fhir/StructureDefinition/cholesterol")
              case "triglyceride" => profiles.isEmpty || profiles.contains("http://hl7.org/fhir/StructureDefinition/triglyceride")
              case "hdlcholesterol" => profiles.isEmpty || profiles.contains("http://hl7.org/fhir/StructureDefinition/hdlcholesterol")
              case "ldlcholesterol" => profiles.isEmpty || profiles.contains("http://hl7.org/fhir/StructureDefinition/ldlcholesterol")
              case "extra" =>  profiles.isEmpty
              case _ => false
            }
          case FhirLiteralReference(_, "Patient", "pat2", _) => true
          case FhirLiteralReference(_, "Organization", "1832473e-2fe0-452d-abe9-3cdb9879522f", _) => true
          case _ => false
        }
      )
    }

    /**
     * Current resource validated
     */
    override val resource: Resource = JObject()
    /**
     * If resource is a part of a bundle (for inter bundle references), the root part of fullUrl of resource validated and the whole Bundle
     */
    override val bundle:  Option[(Option[String], Resource)] = None
  }



  sequential
  "ProfileValidation" should {
   "validate a valid FHIR resource against base definitions" in {
      val fhirContentValidator = FhirContentValidator.apply(fhirConfig, "http://hl7.org/fhir/StructureDefinition/Observation")
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/observation-glucose.json")).mkString).asInstanceOf[JObject]
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(true)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/observation-erythrocyte.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(true)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/observation-bp.json")).mkString).asInstanceOf[JObject]
      issues =  Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(true)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/observation-group.json")).mkString).asInstanceOf[JObject]
      issues =  Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(true)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/observation-reject.json")).mkString).asInstanceOf[JObject]
      issues =  Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }

    "not validate an invalid FHIR resource against base definitions" in {
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/StructureDefinition/Observation")
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/observation-invalid-cardinality.json")).mkString).asInstanceOf[JObject]
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(false)
      issues.exists(i => i.expression.head == "identifier") mustEqual(true)  // Should be an array but given as object
      issues.exists(_.expression.head == "code.coding") mustEqual(true) //Should an array but given as object
      issues.exists(_.expression.head == "subject") mustEqual(true) //Should an array but given as object
      issues.exists(i => i.expression.head == "status") mustEqual(true) //Is required but not given

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/observation-invalid-basics.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(false)
      issues.exists(i => i.expression.head == "status") mustEqual(true) //Invalid primitive empty code
      issues.exists(i => i.expression.head == "extraElement") mustEqual(true) //Invalid extra element
      issues.exists(i => i.expression.head == "category[0].coding[0].display") mustEqual(true) //Invalid primitive empty string
      issues.exists(i => i.expression.head == "component[0].valueQuantity.value") mustEqual(true) //Invalid decimal (decimal as string)
      issues.exists(i => i.expression.head == "component[0].interpretation[0].coding[0].extraElement") mustEqual(true) //Invalid extra element in inner parts
      issues.exists(i => i.expression.head == "extension[0].url") mustEqual(true) //Required but not exist
      issues.exists(i => i.expression.head == "extension[0]")  mustEqual(true) //Constraint failure both extension has value and extension

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/observation-invalid-complex.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(false)
      issues.exists(i => i.expression.head == "status" && i.severity == "error") mustEqual true
      issues.exists(i => i.expression.head == "category[0]" && i.severity == "warning") mustEqual true
      issues.exists(i => i.expression.head == "interpretation[0]" && i.severity == "warning") mustEqual true
      issues.exists(i => i.expression.head == "component[0].interpretation[0]" && i.severity == "warning") mustEqual true
    }

    "validate a valid FHIR resource with primitive extensions against base definitions" in {
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/StructureDefinition/Patient")
      val patient = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/patient.json")).mkString).asInstanceOf[JObject]
      val issues = Await.result(fhirContentValidator.validateComplexContent(patient), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }
    "not validate an invalid FHIR resource with primitive extensions against base definitions" in {
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/StructureDefinition/Patient")
      val patient = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/patient.json")).mkString).asInstanceOf[JObject]
      val issues = Await.result(fhirContentValidator.validateComplexContent(patient), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "birthDate.as" && i.severity == "error") mustEqual true
      issues.exists(i => i.expression.head == "contact[0].name.family" && i.severity == "error") mustEqual true
      issues.exists(i => i.expression.head == "contact[0].name.given" && i.severity == "error") mustEqual true
    }

    "validate a valid FHIR resource against profile" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/observation-my.json")).mkString).asInstanceOf[JObject]
      var fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyObservation")
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(true)

      //Genetics example for fhir-observation-genetics profiles
      fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/StructureDefinition/observation-genetics")

      observation = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/genetics/observation-example-diplotype1.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)

      observation = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/genetics/observation-example-haplotype1.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)

      observation = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/genetics/observation-example-TPMT-haplotype-one.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)

      observation = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/genetics/observation-example-genetics-brcapat.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }

    "not validate an invalid FHIR resource against profile" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/observation-my-invalid.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyObservation")
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(false)
      issues.exists(i => i.expression.head == "identifier[0].system" && i.severity == "error") mustEqual true //Slice indicates system as required
      issues.exists(i => i.expression.head == "status" && i.severity == "error") mustEqual true //Fixed value error
      issues.exists(i => i.expression.head == "category" && i.severity == "error") mustEqual true //Not array error
      issues.exists(i => i.expression.head == "code" && i.severity == "error") mustEqual true //Shpuld not be array error
      issues.exists(i => i.expression.head == "subject" && i.severity == "error" && i.diagnostics.exists(d => d.contains("expected target types"))) mustEqual true //Reference type does not match
      issues.exists(i => i.expression.head == "subject" && i.severity == "error" && i.diagnostics.exists(d => d.contains("version independent"))) mustEqual true // Reference has version although it is stated as independent
      issues.exists(i => i.expression.head == "encounter" && i.severity == "error" && i.diagnostics.exists(d => d.contains("version specific"))) //Reference should be version specific
      issues.exists(i => i.expression.head == "encounter" && i.severity == "error" && i.diagnostics.exists(d => d.contains("expected target types"))) //Reference should be version specific
      issues.exists(i => i.expression.head == "effectiveDateTime" && i.severity == "error") mustEqual true //Invalid date time format
      issues.exists(i => i.expression.head == "issued" && i.severity == "error") mustEqual true //Invalid instant format
      issues.exists(i => i.expression.head == "note[0].authorString" && i.severity == "error") mustEqual true //Exceed max length
      issues.exists(i => i.expression.head == "note[0].text" && i.severity == "error") //Required field is missing
      issues.exists(i => i.expression.head == "component[0].valueSampledData.factor" && i.severity == "error") mustEqual true  //Fixed value error
      issues.exists(i => i.expression.head == "component[0].valueSampledData.upperLimit" && i.severity == "error") mustEqual true //Invalid decimal (given as string)
      issues.exists(i => i.expression.head == "component[0].valueSampledData.lowerLimit" && i.severity == "error") mustEqual true //DataType profile, Missing element
      issues.exists(i => i.expression.head == "component[0].valueSampledData.origin" && i.severity == "error") mustEqual true //DataType profile, missing element
      issues.exists(i => i.expression.head == "component[6].valueString" && i.severity == "error") mustEqual true  //Slice, wrong data type
      issues.exists(i => i.expression.head == "component[7].valueInteger" && i.severity == "error") mustEqual true  //Slice, wrong data type
      issues.exists(i => i.expression.head == "component" && i.severity == "error") mustEqual true  //Slice cardinality
      issues.exists(i => i.expression.head == "component[1].interpretation" && i.severity == "error") mustEqual true //Slicing match, required field is missing
      issues.exists(i => i.expression.head == "component[1].referenceRange" && i.severity == "error") mustEqual true //Slicing match, required field is missing
      issues.exists(i => i.expression.head == "basedOn" && i.severity == "error") mustEqual true//Missing element
    }

    "validate a valid FHIR resource against derived profile" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/observation-mymy.json")).mkString).asInstanceOf[JObject]
      var fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyMyObservation")
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(true)
    }


    "not validate an invalid FHIR resource against derived profile" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/observation-mymy-invalid.json")).mkString).asInstanceOf[JObject]
      var fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyMyObservation")
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.isEmpty mustEqual(false)

      issues.exists(i => i.expression.head == "method" && i.severity == "error") mustEqual true //method should not be used
      issues.exists(i => i.expression.head == "component[4].valueInteger" && i.severity == "error") mustEqual true //Error according to the base profile
      issues.exists(i => i.expression.head == "interpretation" && i.severity == "error") mustEqual true //required element according to this new derived profile
      issues.exists(i => i.expression.head == "component" && i.severity == "error") mustEqual true
    }

    "validate a valid resource with slicing that has resolve() expression in discriminator" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/StructureDefinition/lipidprofile", referenceResolverForLipidProfileSample)
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids2.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }

    "not validate an invalid FHIR resource with slicing that has resolve() expression in discriminator" in {
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/StructureDefinition/lipidprofile", referenceResolverForLipidProfileSample)

      //Missing reference to a cholesterol profile
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids-invalid-missing-cholesterol.json")).mkString).asInstanceOf[JObject]
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "result" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "Cholesterol"))) mustEqual true

      //Missing reference to a tryglyceride profile
      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids-invalid-missing-tryglyceride.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "result" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "Triglyceride"))) mustEqual true

      //Extra reference to a tryglyceride profile (max cardinality 1, cardinality 2)
      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids-invalid-extra-tryglyceride.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "result" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "Triglyceride"))) mustEqual true

      //Extra reference used in a closed slicing
      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids-invalid-extra-reference-for-closed-slicing.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "result" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "closed"))) mustEqual true

      //Unordered references in a ordered slicing
      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids-invalid-unordered.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "result" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "Triglyceride") && d.contains("index '2' should be in index '1'"))) mustEqual true
      issues.exists(i => i.expression.head == "result" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "HDLCholesterol") && d.contains("index '1' should be in index '3'"))) mustEqual true

      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/dreport/diagnosticreport-example-lipids-invalid-missing-reference.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.severity == "error" && i.diagnostics.exists(d => d.contains( "Referenced resource Patient/pat3 does not exist"))) mustEqual true
    }

    "validate a valid resource with slicing with discriminator type: 'type' and 'exists'" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist-valid.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyList")
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }

    "not validate a invalid resource with slicing with discriminator type: 'type' and 'exists'" in {
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyList")

      //Missing element for withAuthor slice although the min cardinality is one
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist-invalid-missing-withAuthor.json")).mkString).asInstanceOf[JObject]
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "note" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "WithAuthor"))) mustEqual true

      //Invalid slice matching (invalid target Reference type and missing time element)
      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist-invalid-withAuthor-uncompliant-slice.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "note[0].authorReference" && i.severity == "error") mustEqual true
      issues.exists(i => i.expression.head == "note[0].time" && i.severity == "error") mustEqual true

      //Invalid slice matching (extra time element)
      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist-invalid-withoutAuthor-extra-uncompliant-slice.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "note[2].time" && i.severity == "error") mustEqual true

      //Missing element for Patients slice although the min cardinality is one
      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist-invalid-missing-Patients-slice.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "entry" && i.severity == "error" && i.diagnostics.exists(d => d.contains( "Patients"))) mustEqual true


      observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist-invalid-Patients-uncompliant-slice.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "entry[1].date" && i.severity == "error") mustEqual true
    }


    "validate a valid resource with slicing with extension in discriminator path" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist2-valid.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyList2")
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }

    "not validate a invalid resource with slicing with extension in discriminator path" in {
      var observation =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/list/mylist2-invalid.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://example.org/fhir/StructureDefinition/MyList2")
      var issues = Await.result(fhirContentValidator.validateComplexContent(observation), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(true)
      issues.exists(i => i.expression.head == "entry[1].item" && i.severity == "error") mustEqual true //Referenced type does not match
      issues.exists(i => i.expression.head == "entry[2].item.extension[1].valueString" && i.severity == "error") mustEqual true //legnth of string exceeds max length
      issues.exists(i => i.expression.head == "entry" && i.severity == "error" && i.diagnostics.exists(d => d.contains("e2"))) mustEqual true //slice e2 has minimum cardinality 1
      issues.exists(i => i.expression.head == "entry" && i.severity == "error" && i.diagnostics.exists(d => d.contains("openAtEnd"))) mustEqual true //extra entry is given in beginning
    }

    "validate base conformance statement" in  {
      var cptStatement = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/foundation/conformance-statement.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/StructureDefinition/CapabilityStatement")
      var issues = Await.result(fhirContentValidator.validateComplexContent(cptStatement), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }

    "validate discriminator $this with child restrictions" in {
      var fh =  JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/familymemberhistory1.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "https://www.medizininformatik-initiative.de/fhir/ext/modul-molgen/StructureDefinition/familienanamnese")
      var issues = Await.result(fhirContentValidator.validateComplexContent(fh), 1 minutes)
      issues.exists(_.severity == "error") mustEqual(false)
    }

    "validate extended slices" in {
      val valid = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/valid/us-core-lab.json")).mkString).asInstanceOf[JObject]
      val fhirContentValidator = FhirContentValidator(fhirConfig, "http://hl7.org/fhir/us/core/StructureDefinition/us-core-observation-lab")
      var issues = Await.result(fhirContentValidator.validateComplexContent(valid), 1 minutes)
      issues.exists(_.severity == "error") mustEqual (false)

      val invalid = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/us-core-lab-invalid.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(invalid), 1 minutes)
      issues.exists(_.severity == "error") mustEqual (true)

      val invalid2 = JsonMethods.parse(Source.fromInputStream(getClass.getResourceAsStream("/fhir/validation/invalid/us-core-lab-invalid2.json")).mkString).asInstanceOf[JObject]
      issues = Await.result(fhirContentValidator.validateComplexContent(invalid2), 1 minutes)
      issues.exists(_.severity == "error") mustEqual (true)
    }


  }
}

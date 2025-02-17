package io.onfhir.config

import io.onfhir.api.FHIR_FOUNDATION_RESOURCES.{FHIR_CODE_SYSTEM, FHIR_STRUCTURE_DEFINITION, FHIR_VALUE_SET}
import io.onfhir.api.model.OutcomeIssue
import io.onfhir.api.parsers.IFhirFoundationResourceParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintKeys, ElementRestrictions, IReferenceResolver, ProfileRestrictions, SimpleReferenceResolver}
import io.onfhir.api.{FHIR_DATA_TYPES, FHIR_ROOT_URL_FOR_DEFINITIONS, Resource}
import io.onfhir.exception.InitializationException
import io.onfhir.util.JsonFormatter.formats
import io.onfhir.validation.{FhirContentValidator, ReferenceRestrictions, TypeRestriction}
import org.json4s.Extraction
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

/**
 * Base FHIR configurator that configure the core onFhir.io FHIR specific configurations (profiles and value sets)
 * supported for the underlying application
 */
abstract class BaseFhirConfigurator extends IFhirVersionConfigurator {
  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Parse the base FHIR standard bundle and given profiles, value sets and code systems,
   * and provide a configuration for the server
   *
   * @param configReader Reader for configuration files
   * @return
   */
  override def initializePlatform(configReader: IFhirConfigReader): BaseFhirConfig = {
    logger.info("Reading base FHIR foundation resources (base standard) to start configuration of onFhir server ...")
    //Read base resource profiles defined in the standard
    val baseResourceProfileResources = configReader.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read the base data type profiles defined in the standard
    val baseDataTypeProfileResources = configReader.readStandardBundleFile(PROFILES_TYPES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read other profiles and extensions given in zip file
    val baseOtherProfileResources = configReader.readStandardBundleFile(PROFILES_OTHERS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    val baseExtensionProfileResources = configReader.readStandardBundleFile(PROFILES_EXTENSIONS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read the base ValueSet and CodeSystem definitions defined in the standard
    val baseValueSetsAndCodeSystems = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => configReader.readStandardBundleFile(file, Set(FHIR_VALUE_SET, FHIR_CODE_SYSTEM)))

    //Initialize Base FHIR configuration
    var fhirConfig = new BaseFhirConfig(fhirVersion)
    //Get the complex and primitive types defined in this version
    val allTypes = baseDataTypeProfileResources.flatMap(getTypeFromStructureDefinition)
    //Initialize base types and resource types
    fhirConfig.FHIR_COMPLEX_TYPES = allTypes.filter(_.head.isUpper).toSet
    fhirConfig.FHIR_PRIMITIVE_TYPES = allTypes.filter(_.head.isLower).toSet
    fhirConfig.FHIR_RESOURCE_TYPES = baseResourceProfileResources.flatMap(getTypeFromStructureDefinition).toSet
    //Get the parser for parsing FHIR foundation resources
    val foundationResourceParser = getFoundationResourceParser(fhirConfig.FHIR_COMPLEX_TYPES, fhirConfig.FHIR_PRIMITIVE_TYPES)

    logger.info("Reading FHIR foundation resources to start configuration of onFHIR server ...")
    //Read the StructureDefinitions for all supported profiles
    val profileResources = configReader.getInfrastructureResources(FHIR_STRUCTURE_DEFINITION)
    //Read the ValueSet definitions to be used in this server (within the profiles)
    val valueSetResources = configReader.getInfrastructureResources(FHIR_VALUE_SET)
    //Read the CodeSystem definitions to be used in this server (within the profiles)
    val codeSystemResources = configReader.getInfrastructureResources(FHIR_CODE_SYSTEM)
    logger.info("Configuring the platform accordingly ...")

    logger.info("Parsing base FHIR foundation resources (base standard) ...")
    //Parsing base definitions
    val baseResourceProfiles = parseStructureDefinitionsConvertToMap(foundationResourceParser, baseResourceProfileResources)

    val baseDataTypeProfiles = parseStructureDefinitionsConvertToMap(foundationResourceParser, baseDataTypeProfileResources)

    val baseProfiles =
      baseResourceProfiles ++
        baseDataTypeProfiles.filter(_._1.split('/').last.head.isUpper) ++ //Get only complex types
        parseStructureDefinitionsConvertToMap(foundationResourceParser, baseOtherProfileResources) ++
        parseStructureDefinitionsConvertToMap(foundationResourceParser, baseExtensionProfileResources)

    //Initialize fhir config with base profiles and value sets to prepare for validation
    fhirConfig.profileRestrictions = baseProfiles
    fhirConfig.valueSetRestrictions = foundationResourceParser.parseValueSetAndCodeSystems(baseValueSetsAndCodeSystems)
    logger.info("Validating given FHIR foundation resources for base specification conformance ...")
    validateGivenInfrastructureResources(fhirConfig, FHIR_STRUCTURE_DEFINITION, profileResources)
    validateGivenInfrastructureResources(fhirConfig, FHIR_VALUE_SET, valueSetResources)
    validateGivenInfrastructureResources(fhirConfig, FHIR_CODE_SYSTEM, codeSystemResources)
    logger.info("Parsing given FHIR foundation resources ...")
    //Parsing the profiles and value sets into our compact form
    val profiles =  parseStructureDefinitionsConvertToMap(foundationResourceParser, profileResources)

    //Parse all as bundle
    val valueSets = foundationResourceParser.parseValueSetAndCodeSystems(valueSetResources ++ codeSystemResources ++ baseValueSetsAndCodeSystems)

    logger.info("Configuring supported FHIR profiles ...")
    fhirConfig = validateAndConfigureProfiles(fhirConfig, profiles, baseProfiles)

    //TODO also check if all mentioned valuesets are provided
    logger.info("Configuring supported FHIR ValueSets ...")
    fhirConfig.valueSetRestrictions = valueSets

    fhirConfig
  }

  /**
   * Reset the summary parameters for base resource profiles
   * @param fhirConfig
   * @param baseResourceProfiles
   * @param dataTypeProfiles
   */
  protected def configureSummaryParametersForBaseResources(fhirConfig:BaseFhirConfig, baseResourceProfiles:Seq[ProfileRestrictions], dataTypeProfiles:Map[String, ProfileRestrictions]):Unit = {
    baseResourceProfiles
      .foreach(pr => {
        val elemsAndTypes =
          pr
            .summaryElements
            .flatMap(se => getElementsAndDataType(se, pr.elementRestrictions))

        val finalSummaryElems:Set[String] =
          elemsAndTypes
            .flatMap {
              case (el, FHIR_DATA_TYPES.BACKBONE) if elemsAndTypes.exists(_._1.startsWith(s"$el.")) => Set.empty
              case (el, special) if fhirConfig.FHIR_SPECIAL_TYPES_FOR_SUMMARY.contains(special) =>
                val subSummaryElems =
                  dataTypeProfiles(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$special")
                    .summaryElements

                subSummaryElems.map(se => s"$el.$se")
              case (oth, _) => Set(oth)
            }

        pr.summaryElements = finalSummaryElems
      })
    }

  /**
   * Find elements and their data types for path (only for base standard profiles)
   * @param path                Path to the element e.g. status, value[x]
   * @param elemRestrictions    All element restrictions for that base profile
   * @return
   */
  private def getElementsAndDataType(path:String, elemRestrictions:Seq[(String, ElementRestrictions)]):Seq[(String, String)] = {
    path match {
      case choice if choice.endsWith("[x]") =>
        elemRestrictions
          .find(_._1 == path)
          .map(_._2)
          .flatMap(
            _
              .restrictions
              .get(ConstraintKeys.DATATYPE)
              .map(_.asInstanceOf[TypeRestriction])
              .map(
                _
                  .dataTypesAndProfiles
                  .map(_._1)
                  .map(dt =>
                    s"${path.dropRight(3)}${dt.capitalize}" -> dt
                  )
              )
          )
          .getOrElse(Nil)

      case oth =>
        elemRestrictions
          .find(_._1 == path)
          .map(_._2)
          .flatMap(
            _
              .restrictions
              .get(ConstraintKeys.DATATYPE)
              .map(_.asInstanceOf[TypeRestriction])
              .flatMap(_.dataTypesAndProfiles.headOption.map(_._1))
          )
          .map(dt => Seq(oth -> dt))
          .getOrElse(Seq(oth -> "*")) //For recursive definitions
    }
  }

  /**
   * Parse StructureDefinition resources and make them Map of Map (url and version)
   * @param foundationResourceParser  Resource parser
   * @param resources                 StructureDefinition resources
   * @return
   */
  protected def parseStructureDefinitionsConvertToMap(foundationResourceParser:IFhirFoundationResourceParser, resources:Seq[Resource], includeElementMetadata:Boolean = true):Map[String, Map[String, ProfileRestrictions]] = {
    resources
      .map(foundationResourceParser.parseStructureDefinition(_, includeElementMetadata = includeElementMetadata))
      .map(s => (s.url, s.version, s))
      .groupBy(_._1) // group by the URL
      .map { case (url, defs) =>
        val withoutVersion = defs.filter(_._2.isEmpty) // Find all StructureDefinitions without a version

        // If there are multiple StructureDefinitions without a version, log a warning
        if (withoutVersion.size > 1) {
          logger.warn(s"Multiple StructureDefinitions without a version for URL: $url. Only the last one will be used.")
        }

        // Convert to the desired map structure: url -> (version -> StructureDefinition)
        url -> defs.map(s => s._2.getOrElse("latest") -> s._3).toMap
      }
  }

  /**
   * Validate and handle profile configurations
   *
   * @param fhirConfig   FhirConfig obj until now
   * @param profiles     Profiles given by configuration
   * @param baseProfiles Base profiles given in standard bundle
   * @return
   */
  private def validateAndConfigureProfiles(fhirConfig: BaseFhirConfig,
                                           profiles: Map[String, Map[String, ProfileRestrictions]],
                                           baseProfiles: Map[String, Map[String, ProfileRestrictions]]): BaseFhirConfig = {
    //Check if all mentioned profiles within the given profiles also exist in profile set (Profile set is closed)
    val allProfilesAndExtensionsMentionedInSomewhere = findMentionedProfiles(fhirConfig, profiles.values.flatMap(_.values).toSeq)
    //Find those ones that the definitions are not given
    val profileDefinitionsNotGiven =
      allProfilesAndExtensionsMentionedInSomewhere
        .filter(p => FHIRUtil.getMentionedProfile(p, profiles ++ baseProfiles).isEmpty)
        .toSeq
    if (profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in profile configurations for the referred profiles (${profileDefinitionsNotGiven.mkString(",")}) within the given profiles (e.g. as base profile 'StructureDefinition.baseDefinition', target profile for an element StructureDefinition.differential.element.type.profile or reference StructureDefinition.differential.element.type.targetProfile) ! All mentioned profiles should be given for configuration of the application!")

    //We need all profile configurations
    fhirConfig.profileRestrictions = profiles ++ baseProfiles

    fhirConfig
  }

  /**
   * Find URLs of mentioned profiles in this set (except the profiles of simple types coming from the FHIR ROOT URL (http://hl7.org/fhir)
   *
   * @param profiles All profile restrictions
   * @return
   */
  protected def findMentionedProfiles(fhirConfig: BaseFhirConfig, profiles: Seq[ProfileRestrictions]): Set[(String, Option[String])] = {
    profiles
      .flatMap(p => {
        val isBaseProfile = p.url.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS)
        p.elementRestrictions.map(_._2)
          .flatMap(e =>
            e.restrictions.get(ConstraintKeys.DATATYPE).toSeq.map(_.asInstanceOf[TypeRestriction])
              .flatMap(_.dataTypesAndProfiles.flatMap(dtp => dtp._2 match {
                case Nil =>
                  if (isBaseProfile || fhirConfig.FHIR_COMPLEX_TYPES.contains(dtp._1))
                    Seq(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/${dtp._1}")
                  else
                    Nil
                case oth => oth
              }).toSet) ++
              e.restrictions.get(ConstraintKeys.REFERENCE_TARGET).toSeq.map(_.asInstanceOf[ReferenceRestrictions]).flatMap(_.targetProfiles).toSet
          ).map(FHIRUtil.parseCanonicalValue) ++
            p.baseUrl.toSeq
      })
      .filterNot(p => {
        val ns = s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/"
        p._1.startsWith(ns) && Character.isLowerCase(p._1.drop(ns.length).head)
      })
      .toSet
  }

  /**
   * Get Resource type or Data type from a StructureDefinition resource if it is not abstract
   *
   * @param structureDefinition FHIR StructureDefinition resource
   * @return
   */
  def getTypeFromStructureDefinition(structureDefinition: Resource): Option[String] = {
    val rtype = FHIRUtil.extractValueOption[String](structureDefinition, "type").get
    val isAbstract = FHIRUtil.extractValueOption[Boolean](structureDefinition, "abstract").get
    if (isAbstract)
      None
    else
      Some(rtype)
  }

  /**
   * Validate the given infrastructure resources and throw exception if any invalid
   *
   * @param baseFhirConfig Base FHIR configurations
   * @param rtype          FHIR Resource type
   * @param resources      Resources to validate
   * @throws InitializationException If there is a problem in given profile or value set definitions
   */
  override def validateGivenInfrastructureResources(baseFhirConfig: BaseFhirConfig, rtype: String, resources: Seq[Resource]): Unit = {
    val issuesForEachResource =
      resources
        .map(resource => validatedGivenInfrastructureResource(baseFhirConfig, rtype, resource))
    val resourcesWithProblems =
      issuesForEachResource
        .filter(rIssues => rIssues._2.exists(i => i.isError))

    if (resourcesWithProblems.nonEmpty) {
      val errors = resourcesWithProblems.map(rp => s"${rp._1} :: ${rp._2.map(i => Extraction.decompose(i)).mkString(",")}").mkString("\n")
      throw new InitializationException(s"Some of the given infrastructure resources (${resourcesWithProblems.map(_._1).mkString(",")}) of type $rtype does not conform to base FHIR specification! $errors")
    }
  }

  /**
   * Validate the given infrastructure resource and return outcome issues
   *
   * @param baseFhirConfig Base FHIR configurations
   * @param rtype          FHIR Resource type
   * @param resource       Resoruce to validate
   * @return
   */
  protected def validatedGivenInfrastructureResource(baseFhirConfig: BaseFhirConfig, rtype: String, resource: Resource): (String, Seq[OutcomeIssue]) = {
    (resource \ "url")
      .extractOpt[String] match {
      case None =>
        throw new InitializationException(s"All foundation resources used for onFhir configuration should have a 'url'!")
      case Some(url) =>
        val referenceResolver: IReferenceResolver = new SimpleReferenceResolver(resource)
        val fhirContentValidator = FhirContentValidator.apply(baseFhirConfig, s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype", referenceResolver)
        url -> Await.result(fhirContentValidator.validateComplexContent(resource), 1 minutes)
    }
  }
}

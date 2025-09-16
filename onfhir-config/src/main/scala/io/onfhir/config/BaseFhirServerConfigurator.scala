package io.onfhir.config

import io.onfhir.api.FHIR_FOUNDATION_RESOURCES._
import io.onfhir.api._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation._
import io.onfhir.db.BaseDBInitializer
import io.onfhir.exception.InitializationException

import scala.language.postfixOps

/**
 * Configuration of FHIR related capabilites based on foundation resources provided (CapabilityStatement, StructureDefinition, etc)
 */
abstract class BaseFhirServerConfigurator extends BaseFhirConfigurator with IFhirServerConfigurator {

  // Cached resources read from the configurations
  var conformanceResource:Resource = _
  var profileResources:Seq[Resource] = _
  var searchParameterResources:Seq[Resource] = _
  var operationDefResources:Seq[Resource] = _
  var compartmentDefResources:Seq[Resource] = _
  var valueSetResources:Seq[Resource] = _
  var codeSystemResources:Seq[Resource] = _
  var indexConfigurations:Map[String, ResourceIndexConfiguration] = _

  var FHIR_COMPLEX_TYPES:Set[String] = _
  var FHIR_PRIMITIVE_TYPES:Set[String] = _

  /**
   * Initialize the platform by preparing a FHIR configuration from the given FHIR Foundation resources and base standard
   * @param configReader          Reader for configuration files
   * @param fhirOperationsImplemented   URLs of FHIR Operation implementations that an implementation is provided
   * @return
   */
  override def initializeServerPlatform(configReader: IFhirConfigReader, fhirOperationsImplemented:Set[String]):FhirServerConfig = {
    logger.info("Reading base FHIR foundation resources (base standard) to start configuration of onFhir server ...")
    //Read base resource profiles defined in the standard
    val baseResourceProfileResources = configReader.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read the base data type profiles defined in the standard
    val baseDataTypeProfileResources = configReader.readStandardBundleFile(PROFILES_TYPES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read other profiles and extensions given in zip file
    val baseOtherProfileResources = configReader.readStandardBundleFile(PROFILES_OTHERS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    val baseExtensionProfileResources = configReader.readStandardBundleFile(PROFILES_EXTENSIONS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read the base search parameters defined in the standard
    val baseSearchParameterResources = configReader.readStandardBundleFile(SEARCH_PARAMETERS_BUNDLE_FILE_NAME, Set(FHIR_SEARCH_PARAMETER))
    //Read the base ValueSet definitions defined in the standard
    val baseValueSetsAndCodeSystems = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => configReader.readStandardBundleFile(file, Set(FHIR_VALUE_SET, FHIR_CODE_SYSTEM)))
    //Read the base operation definitions
    val baseOperationDefinitionResources = configReader.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_OPERATION_DEFINITION))
    //Read the base compartment definitions
    val baseCompartmentDefinitionResources = configReader.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_COMPARTMENT_DEFINITION))

    //Initalize fhir configuration
    var fhirConfig = new FhirServerConfig(fhirVersion)

    val allTypes = baseDataTypeProfileResources.flatMap(getTypeFromStructureDefinition)
    FHIR_COMPLEX_TYPES = allTypes.filter(_.head.isUpper).toSet
    FHIR_PRIMITIVE_TYPES = allTypes.filter(_.head.isLower).toSet

    val foundationResourceParser = getFoundationResourceParser(FHIR_COMPLEX_TYPES, FHIR_PRIMITIVE_TYPES)

    //Initialize base types and resource types
    fhirConfig.FHIR_COMPLEX_TYPES = FHIR_COMPLEX_TYPES
    fhirConfig.FHIR_PRIMITIVE_TYPES = FHIR_PRIMITIVE_TYPES
    fhirConfig.FHIR_RESOURCE_TYPES = baseResourceProfileResources.flatMap(getTypeFromStructureDefinition).toSet

    logger.info("Reading FHIR foundation resources to start configuration of onFhir server ...")
    //Read the FHIR Conformance statement specified for this server
    conformanceResource = configReader.readCapabilityStatement()
    //Read the StructureDefinitions for all supported profiles
    profileResources = configReader.getInfrastructureResources(FHIR_STRUCTURE_DEFINITION)
    //Read the defined search parameters in addition to base search parameters defined in the standard
    searchParameterResources = configReader.getInfrastructureResources(FHIR_SEARCH_PARAMETER)
    //Read defined operation definitions in addition to base operation definitions defined in the standard
    operationDefResources = configReader.getInfrastructureResources(FHIR_OPERATION_DEFINITION)
    //Read compartment definitions supported by this server
    compartmentDefResources = configReader.getInfrastructureResources(FHIR_COMPARTMENT_DEFINITION)
    //Read the ValueSet definitions to be used in this server (within the profiles)
    valueSetResources = configReader.getInfrastructureResources(FHIR_VALUE_SET)
    //Read the CodeSystem definitions to be used in this server (within the profiles)
    codeSystemResources = configReader.getInfrastructureResources(FHIR_CODE_SYSTEM)

    logger.info("Configuring the platform accordingly ...")

    logger.info("Parsing base FHIR foundation resources (base standard) ...")
    //Parsing base definitions
    val baseResourceProfiles = parseStructureDefinitionsConvertToMap(foundationResourceParser, baseResourceProfileResources, includeElementMetadata = false)
    val baseDataTypeProfiles = parseStructureDefinitionsConvertToMap(foundationResourceParser, baseDataTypeProfileResources, includeElementMetadata = false)

    val baseProfiles =
      baseResourceProfiles ++
        baseDataTypeProfiles.filter(_._1.split('/').last.head.isUpper) ++
        parseStructureDefinitionsConvertToMap(foundationResourceParser, baseOtherProfileResources, includeElementMetadata = false) ++
        parseStructureDefinitionsConvertToMap(foundationResourceParser, baseExtensionProfileResources, includeElementMetadata = false)

    val baseSearchParameters = baseSearchParameterResources.map(foundationResourceParser.parseSearchParameter).map(s => s.url -> s).toMap
    val baseOperationDefinitions = baseOperationDefinitionResources.map(foundationResourceParser.parseOperationDefinition).map(p => p.url -> p).toMap
    val baseCompartmentDefinitions = baseCompartmentDefinitionResources.map(foundationResourceParser.parseCompartmentDefinition).map(c => c.url -> c).toMap

    //Initialize fhir config with base profiles and value sets to prepare for validation
    fhirConfig.profileRestrictions = baseProfiles
    fhirConfig.valueSetRestrictions = foundationResourceParser.parseValueSetAndCodeSystems(baseValueSetsAndCodeSystems)

    logger.info("Validating given FHIR foundation resources for base specification conformance ...")
    //Validations
    validateGivenInfrastructureResources(fhirConfig, FHIR_CONFORMANCE, Seq(conformanceResource))
    validateGivenInfrastructureResources(fhirConfig, FHIR_STRUCTURE_DEFINITION, profileResources)
    validateGivenInfrastructureResources(fhirConfig, FHIR_SEARCH_PARAMETER, searchParameterResources)
    validateGivenInfrastructureResources(fhirConfig, FHIR_OPERATION_DEFINITION, operationDefResources)
    validateGivenInfrastructureResources(fhirConfig, FHIR_COMPARTMENT_DEFINITION, compartmentDefResources)
    validateGivenInfrastructureResources(fhirConfig, FHIR_VALUE_SET, valueSetResources)
    validateGivenInfrastructureResources(fhirConfig, FHIR_CODE_SYSTEM, codeSystemResources)

    logger.info("Parsing given FHIR foundation resources ...")
    //Parsing the Conformance statement into our compact form
    val conformance = foundationResourceParser.parseCapabilityStatement(conformanceResource)
    val profiles =  parseStructureDefinitionsConvertToMap(foundationResourceParser, profileResources, includeElementMetadata = false)
    val searchParameters = searchParameterResources.map(foundationResourceParser.parseSearchParameter).map(s => s.url -> s).toMap
    val operationDefs = operationDefResources.map(opDef => foundationResourceParser.parseOperationDefinition(opDef)).map(o => o.url -> o).toMap
    val compartments = compartmentDefResources.map(foundationResourceParser.parseCompartmentDefinition).map(c => c.url -> c).toMap
    //Parse all as bundle
    val valueSets = foundationResourceParser.parseValueSetAndCodeSystems(valueSetResources ++ codeSystemResources ++ baseValueSetsAndCodeSystems)

    fhirConfig.fhirVersion = conformance.fhirVersion
    logger.info("Configuring supported FHIR resources and profiles ...")
    fhirConfig = validateAndConfigureProfiles(fhirConfig, conformance, profiles, baseProfiles)

    //Determine the actual summary parameters for each supported resource type
    configureSummaryParametersForBaseResources(
      fhirConfig,
      baseResourceProfiles
        .values.map(_.head._2).toSeq
        .filter(rr => fhirConfig.resourceConfigurations.contains(rr.resourceType)), //We only need to configure the ones that are supported
      baseDataTypeProfiles.map(bp => bp._1 -> bp._2.head._2)
    )

    logger.info("Configuring supported FHIR search parameters for supported resources ...")
    fhirConfig = validateAndConfigureSearchParameters(fhirConfig, conformance, searchParameters, baseSearchParameters)

    logger.info("Configuring supported FHIR operations ...")
    fhirConfig = validateAndConfigureOperations(fhirConfig, conformance, operationDefs, baseOperationDefinitions, fhirOperationsImplemented)

    logger.info("Configuring supported FHIR compartments ...")
    fhirConfig = validateAndConfigureCompartments(fhirConfig, conformance, compartments, baseCompartmentDefinitions)

    logger.info("Configuring supported FHIR system level interaction ...")
    //Set system level interactions
    fhirConfig.supportedInteractions = conformance.systemLevelInteractions

    logger.info("Configuring supported FHIR ValueSets ...")
    fhirConfig.valueSetRestrictions = valueSets

    logger.info("Configuring required database indexes and shard keys ...")
    fhirConfig = readIndexConfigurationsAndConfigureShardKeys(fhirConfig)

    logger.info("Configuring other FHIR version specific parameters (mime types, etc)...")
    fhirConfig.FHIR_RESULT_PARAMETERS = FHIR_RESULT_PARAMETERS
    fhirConfig.FHIR_SPECIAL_PARAMETERS = FHIR_SPECIAL_PARAMETERS

    val unsupportedFormats = conformance.formats.diff(FHIR_FORMATS.JSON ++ FHIR_FORMATS.XML)
    //onFHIR.io is not supporting turtle format yet
    if(unsupportedFormats.nonEmpty) {
      logger.error(s"Formats $unsupportedFormats is/are not supported by onFHIR.io yet, please correct your CapabilityStatement.format part!")
      throw new InitializationException(s"Formats $unsupportedFormats is/are not supported by onFHIR.io yet, please correct your CapabilityStatement.format part!")
    }
    val majorFhirVersion = fhirConfig.fhirVersion.split('.').take(2).mkString(".")
    //If supported json format
    fhirConfig.FHIR_JSON_MEDIA_TYPES = if(conformance.formats.intersect(FHIR_FORMATS.JSON).nonEmpty) FHIR_JSON_MEDIA_TYPES(majorFhirVersion) else Nil
    //If supported xml format
    fhirConfig.FHIR_XML_MEDIA_TYPES = if(conformance.formats.intersect(FHIR_FORMATS.XML).nonEmpty) FHIR_XML_MEDIA_TYPES(majorFhirVersion) else Nil

    fhirConfig.FHIR_FORMAT_MIME_TYPE_MAP = FHIR_FORMAT_MIME_TYPE_MAP

    //Check patch formats
    val unsupportedPatchFormats = conformance.patchFormats.diff(FHIR_FORMATS.JSON_PATCH ++ FHIR_FORMATS.JSON ++ FHIR_FORMATS.XML)
    if(unsupportedPatchFormats.nonEmpty){
      logger.error(s"Patch formats $unsupportedPatchFormats  is/are not supported by onFHIR.io yet, please correct your CapabilityStatement.patchFormat part!")
      throw new InitializationException(s"Patch formats $unsupportedPatchFormats  is/are not supported by onFHIR.io yet, please correct your CapabilityStatement.patchFormat part!")
    }
    fhirConfig.FHIR_PATCH_MEDIA_TYPES = if(conformance.patchFormats.isEmpty) FHIR_PATCH_MEDIA_TYPES else if(conformance.patchFormats.intersect(FHIR_FORMATS.JSON_PATCH).nonEmpty) FHIR_PATCH_MEDIA_TYPES else Nil

    fhirConfig.FHIR_DEFAULT_MEDIA_TYPE = FHIR_DEFAULT_MEDIA_TYPE

    fhirConfig
  }

  /**
   * Setup the platfrom by initializating/updating the database
   * @param fhirConfig
   */
  override def setupPlatform(configReader: IFhirConfigReader,
                             baseDBInitializer: BaseDBInitializer,
                             fhirConfig: FhirServerConfig):Unit = {
    logger.info("Setting up (or updating) the platform as requested ...")
    //Basic preparation for database
    baseDBInitializer.prepareDatabase()

    //Get supported resources and versioning mechanism
    var supportedResourcesAndVersions =
      fhirConfig
        .resourceConfigurations
        .map(rconf =>
          rconf._1 ->
            rconf._2.versioning
        )

    //Add Foundation resources as we will save them
    supportedResourcesAndVersions = Map(
      FHIR_CONFORMANCE -> "no-version",
      FHIR_STRUCTURE_DEFINITION -> "no-version",
      FHIR_SEARCH_PARAMETER -> "no-version",
      FHIR_OPERATION_DEFINITION -> "no-version",
      FHIR_COMPARTMENT_DEFINITION -> "no-version",
      FHIR_VALUE_SET -> "no-version",
      FHIR_CODE_SYSTEM -> "no-version",
      FHIR_AUDIT_EVENT -> "no-version"
    ) ++ supportedResourcesAndVersions

    //Create collections in database for each supported resource
    baseDBInitializer.createCollections(supportedResourcesAndVersions)
    //Create the indexes in DB
    baseDBInitializer.createIndexes(supportedResourcesAndVersions, fhirConfig.resourceQueryParameters, fhirConfig.commonQueryParameters, indexConfigurations)
    //Store the infrastructure resources into DB
    logger.info("Storing infrastructure resources to database ...")
    //Store the Conformance statement (Set a fixed id for the conformance)
    baseDBInitializer.storeInfrastructureResources(FHIR_CONFORMANCE, Seq(FHIRUtil.setId(conformanceResource, SERVER_CONFORMANCE_STATEMENT_ID)))
    //Store the Profiles (Structure Definitions)
    baseDBInitializer.storeInfrastructureResources(FHIR_STRUCTURE_DEFINITION, profileResources)
    //Store the Search Parameters
    baseDBInitializer.storeInfrastructureResources(FHIR_SEARCH_PARAMETER, searchParameterResources)
    //Store the CompartmentDefinitions
    baseDBInitializer.storeInfrastructureResources(FHIR_COMPARTMENT_DEFINITION,  compartmentDefResources)
    //Store the ValueSets
    baseDBInitializer.storeInfrastructureResources(FHIR_VALUE_SET, valueSetResources)
    //Store the OperationDefinitions
    baseDBInitializer.storeInfrastructureResources(FHIR_OPERATION_DEFINITION,  operationDefResources)
    //Store the Code systems
    baseDBInitializer.storeInfrastructureResources(FHIR_CODE_SYSTEM,  codeSystemResources)

    //If we need to persist the base FHIR standard definitions
    OnfhirConfig.fhirPersistBaseDefinitions.foreach {
      case FHIR_STRUCTURE_DEFINITION =>
        val baseStructureDefinitions =
          //Base resource definitions
          configReader.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION)) ++
          //Read the base data type profiles defined in the standard
            configReader.readStandardBundleFile(PROFILES_TYPES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION)) ++
          //Read other profiles and extensions given in zip file
            configReader.readStandardBundleFile(PROFILES_OTHERS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION)) ++
            configReader.readStandardBundleFile(PROFILES_EXTENSIONS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
        //Store the definitions
        baseDBInitializer.storeInfrastructureResources(FHIR_STRUCTURE_DEFINITION, baseStructureDefinitions)

      case FHIR_SEARCH_PARAMETER =>
        //Read the base search parameters defined in the standard
        val baseSearchParameterResources = configReader.readStandardBundleFile(SEARCH_PARAMETERS_BUNDLE_FILE_NAME, Set(FHIR_SEARCH_PARAMETER))
        //Store the search parameters
        baseDBInitializer.storeInfrastructureResources(FHIR_SEARCH_PARAMETER, baseSearchParameterResources)

      case FHIR_OPERATION_DEFINITION =>
        val baseOperationDefinitionResources = configReader.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_OPERATION_DEFINITION))
        //Store the operation definitions
        baseDBInitializer.storeInfrastructureResources(FHIR_OPERATION_DEFINITION, baseOperationDefinitionResources)

      case FHIR_VALUE_SET =>
        //Read the base ValueSet definitions defined in the standard
        val baseValueSets = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => configReader.readStandardBundleFile(file, Set(FHIR_VALUE_SET)))
        //Store the value sets
        baseDBInitializer.storeInfrastructureResources(FHIR_VALUE_SET, baseValueSets)

      case FHIR_CODE_SYSTEM =>
        //Read the base ValueSet definitions defined in the standard
        val baseCodeSystems = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => configReader.readStandardBundleFile(file, Set(FHIR_CODE_SYSTEM)))
        //Store the value sets
        baseDBInitializer.storeInfrastructureResources(FHIR_CODE_SYSTEM, baseCodeSystems)

      case FHIR_COMPARTMENT_DEFINITION =>
        //Read the base compartment definitions
        val baseCompartmentDefinitionResources = configReader.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_COMPARTMENT_DEFINITION))
        //Store the compartment definitions
        baseDBInitializer.storeInfrastructureResources(FHIR_COMPARTMENT_DEFINITION, baseCompartmentDefinitionResources)
    }
  }

  /**
   * Validate and handle profile configurations
   * @param fhirConfig            FhirConfig obj until now
   * @param conformance           Conformance configurations
   * @param profiles              Profiles given by configuration
   * @param baseProfiles          Base profiles given in standard bundle
   * @return
   */
  protected def validateAndConfigureProfiles(fhirConfig: FhirServerConfig, conformance:FHIRCapabilityStatement, profiles:Map[String, Map[String, ProfileRestrictions]], baseProfiles:Map[String, Map[String, ProfileRestrictions]]):FhirServerConfig = {
    //Check if all base profiles mentioned in Conformance are given in profile configurations
    var profileDefinitionsNotGiven =
      conformance.restResourceConf.flatMap(_.profile).map(FHIRUtil.parseCanonicalValue).filter(p => FHIRUtil.getMentionedProfile(p, profiles ++ baseProfiles).isEmpty)

    if(profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in profile configurations for some of the base profiles (${profileDefinitionsNotGiven.map(p => s"${p._1}${p._2.map(v => s"|$v").getOrElse("")}").mkString(",")}) declared in CapabilityStatement (CapabilityStatement.rest.resource.profile)! ")
    //Check if all supported profiles mentioned in Conformance are given in profile configurations
    profileDefinitionsNotGiven = conformance.restResourceConf.flatMap(_.supportedProfiles.toSeq).map(FHIRUtil.parseCanonicalValue).filter(p => FHIRUtil.getMentionedProfile(p, profiles ++ baseProfiles).isEmpty)
    if(profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in profile configurations for some of the supported profiles (${profileDefinitionsNotGiven.map(p => s"${p._1}${p._2.map(v => s"|$v").getOrElse("")}").mkString(",")}) declared in CapabilityStatement (CapabilityStatement.rest.resource.supportedProfile)! ")

    //Get the URLs of all used base profiles
    val baseProfilesUrlsUsed =
      (conformance.restResourceConf.flatMap(_.profile).map(FHIRUtil.parseCanonicalValue).filter(p => FHIRUtil.getMentionedProfile(p, baseProfiles).nonEmpty) ++
       conformance.restResourceConf.flatMap(_.supportedProfiles.toSeq).map(FHIRUtil.parseCanonicalValue).filter(p => FHIRUtil.getMentionedProfile(p, baseProfiles).nonEmpty)).toSet


    //Check if all mentioned profiles within the given profiles also exist in profile set (Profile set is closed)
    var allProfilesAndExtensionsMentionedInSomewhere = findMentionedProfiles(fhirConfig, profiles.values.flatMap(_.values).toSeq)
    profileDefinitionsNotGiven = allProfilesAndExtensionsMentionedInSomewhere.filter(p => FHIRUtil.getMentionedProfile(p, profiles ++ baseProfiles).isEmpty).toSeq
    if(profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in o profile configurations for the referred profiles (${profileDefinitionsNotGiven.map(p => s"${p._1}${p._2.map(v => s"|$v").getOrElse("")}").mkString(",")}) within the given profiles (e.g. as base profile 'StructureDefinition.baseDefinition', target profile for an element StructureDefinition.differential.element.type.profile or reference StructureDefinition.differential.element.type.targetProfile) ! All mentioned profiles should be given for validation!")

    allProfilesAndExtensionsMentionedInSomewhere =
      allProfilesAndExtensionsMentionedInSomewhere.filter(p => FHIRUtil.getMentionedProfile(p, profiles).isEmpty) ++ //Mentioned profiles that are not given in configured profile set
        baseProfilesUrlsUsed ++ //Base FHIR standart definitions
        //Base profiles used in FHIR interactions
        Set(
          s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/OperationOutcome" -> None,
          s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/Bundle" -> None,
          s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/Parameters" -> None,
        )

    fhirConfig.supportedProfiles = conformance.restResourceConf.map(restConf => restConf.resource -> restConf.supportedProfiles.map(FHIRUtil.parseCanonicalValue).groupBy(_._1).map(g => g._1 -> g._2.flatMap(_._2))).toMap
    fhirConfig.resourceConfigurations = conformance.restResourceConf.map(restConf => restConf.resource -> restConf).toMap

    fhirConfig.profileRestrictions =
      profiles ++
        findClosureForBaseProfiles(fhirConfig, allProfilesAndExtensionsMentionedInSomewhere, baseProfiles) //Only add base profiles that are mentioned
    fhirConfig
  }

  /**
   * Return the base profiles that is only necessary
   * @param fhirConfig                FHIR configurations for onFHIR server
   * @param mentionedBaseProfileUrls  All mentioned profile urls
   * @param baseProfiles              All base profiles provided in the standard
   * @return
   */
  protected def findClosureForBaseProfiles(fhirConfig: FhirServerConfig, mentionedBaseProfileUrls:Set[(String, Option[String])], baseProfiles:Map[String, Map[String, ProfileRestrictions]]):Map[String, Map[String, ProfileRestrictions]] = {
    val mentionedBaseProfiles = mentionedBaseProfileUrls.map(p => p._1 -> Map(p._2.getOrElse("latest") -> FHIRUtil.getMentionedProfile(p, baseProfiles).get)).toMap
    val deepMentionedBaseProfiles = findMentionedProfiles(fhirConfig, mentionedBaseProfiles.values.flatMap(_.values).toSeq)
    val newUrls = deepMentionedBaseProfiles.diff(mentionedBaseProfileUrls)
    if(newUrls.nonEmpty)
      findClosureForBaseProfiles(fhirConfig, mentionedBaseProfileUrls ++ newUrls, baseProfiles)
    else
      mentionedBaseProfiles
  }

  /**
   * Validate and handle search parameter configurations
   * @param fhirConfig            FhirConfig obj until now
   * @param conformance           Conformance configurations
   * @param searchParameters      All search parameters given
   * @param baseSearchParameters  All base search parameters given in standard
   * @param allProfiles           All profiles both base and given
   * @return
   */
  protected def validateAndConfigureSearchParameters(fhirConfig: FhirServerConfig, conformance:FHIRCapabilityStatement, searchParameters:Map[String, FHIRSearchParameter], baseSearchParameters:Map[String, FHIRSearchParameter]) :FhirServerConfig = {
    //Check if for all search parameters mentioned in the Conformance, a SearchParameter definition exist in base standard or given configuration
    val resourcesWithMissingSearchParameterDefs =
      conformance.restResourceConf
        .map(r => r.resource -> r.searchParams.filter(sp => !(searchParameters ++ baseSearchParameters).contains(sp._2) || (searchParameters ++ baseSearchParameters).get(sp._2).exists(spd => !spd.base.contains("Resource") && !spd.base.contains(r.resource))))
        .filter(_._2.nonEmpty)

    if(resourcesWithMissingSearchParameterDefs.nonEmpty)
      throw new InitializationException(s"Missing SearchParameter definitions (${resourcesWithMissingSearchParameterDefs.map(r => s"${r._1}: [${r._2.mkString(",")}]" ).mkString(",")}) in search parameter configurations!")

    fhirConfig.resourceQueryParameters =
      fhirConfig.resourceConfigurations
        .map(r => {
          //Construct search parameter configurator by giving required information
          val searchParamsMap = r._2.searchParams.map(sp => sp._2 -> searchParameters.getOrElse(sp._2, baseSearchParameters(sp._2)).name).toMap
          val spConfigurator = new SearchParameterConfigurator(r._1, r._2.profile, fhirConfig, searchParamsMap)
          r._1 ->
            (
              r._2.searchParams.map(sp => {
                val searchParamDef:FHIRSearchParameter = searchParameters.getOrElse(sp._2, baseSearchParameters(sp._2))
                spConfigurator.createSearchParameterConf(searchParamDef)
              }).filter(_.isDefined).map(_.get)
                .map(spc => spc.pname -> spc).toMap
              )
        })

    fhirConfig.commonQueryParameters =
      conformance.searchParamDefUrls
        .flatMap(spUrl => {
          baseSearchParameters.get(spUrl).map(searchParamDef => {
              val spConfigurator =
                if(spUrl.split('/').last.startsWith("DomainResource"))
                  new SearchParameterConfigurator("DomainResource", None, fhirConfig, Map.empty)
                else
                  new SearchParameterConfigurator("Resource", None, fhirConfig, Map.empty)

              spConfigurator.createSearchParameterConf(searchParamDef)
          })
        }).filter(_.isDefined).map(_.get)
        .map(spc => spc.pname -> spc).toMap

    fhirConfig
  }

  /**
   * Validate and handle operation configurations
   * @param fhirConfig                FhirConfig obj until now
   * @param conformance               Conformance configurations
   * @param operationDefs             OperationDefinitions given in configuration
   * @param baseOperationDefinitions  Base OperationDefinitions given in standard bundle
   * @param fhirOperationsImplemented URLs of FHIR Operations that implementation is provided
   * @return
   */
  protected def validateAndConfigureOperations(fhirConfig: FhirServerConfig,
                                               conformance:FHIRCapabilityStatement,
                                               operationDefs:Map[String, OperationConf],
                                               baseOperationDefinitions:Map[String, OperationConf],
                                               fhirOperationsImplemented:Set[String]): FhirServerConfig = {
    //Check if all operations mentioned in Conformance are given in operation definition configurations
    val operationDefinitionsNotGiven = conformance.operationDefUrls.diff(operationDefs.keySet ++ baseOperationDefinitions.keySet)
    if(operationDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing OperationDefinition in operation definition configurations for the operation definitions (${operationDefinitionsNotGiven.mkString(",")}) declared in CapabilityStatement (CapabilityStatement.rest.resource.operation | CapabilityStatement.rest.operation)! ")

    val operationImplementationsNotExist = conformance.operationDefUrls.diff(fhirOperationsImplemented ++ baseOperationDefinitions.keySet)
    if(operationImplementationsNotExist.nonEmpty)
      throw new InitializationException(s"Missing implementations for FHIR operations ${operationImplementationsNotExist.mkString(",")}! Please provide class path of the implementation for each FHIR operation declared in CapabilityStatement.")

    fhirConfig.supportedOperations =
      conformance
        .operationDefUrls
        .map(opDefUrl =>
          operationDefs.getOrElse(opDefUrl, baseOperationDefinitions.apply(opDefUrl))
        )
        .toSeq
    fhirConfig
  }

  /**
   * Validate and handle compartment configurations
   * @param fhirConfig                  FhirConfig obj until now
   * @param conformance                 Conformance configurations
   * @param compartments                Compartments given in configuration
   * @param baseCompartmentDefinitions  Base compartments given in standard bundle
   * @return
   */
  protected def validateAndConfigureCompartments(fhirConfig: FhirServerConfig, conformance:FHIRCapabilityStatement, compartments:Map[String, FHIRCompartmentDefinition], baseCompartmentDefinitions:Map[String, FHIRCompartmentDefinition]):FhirServerConfig = {
    //Check if all compartments mentioned in Conformance are given in compartment configurations
    val compartmentsNotGiven = conformance.compartments.diff(baseCompartmentDefinitions.keySet ++ compartments.keySet)
    if(compartmentsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing CompartmentDefinition (${compartmentsNotGiven.mkString(",")})  in compartment definition configurations declared in CapabilityStatement (CapabilityStatement.rest.compartment)! ")

    fhirConfig.compartmentRelations = conformance.compartments
      .map(compUrl => {
        val compartmentDefinition = compartments.getOrElse(compUrl, baseCompartmentDefinitions(compUrl))
        compartmentDefinition.code -> compartmentDefinition.relations
      }).toMap

    fhirConfig
  }

  /**
   * Configure shard keys if sharding is enabled
   * @param fhirConfig
   * @return
   */
  private def readIndexConfigurationsAndConfigureShardKeys(fhirConfig: FhirServerConfig):FhirServerConfig = {
    //fhirConfig.fhirVersion
    val dbIndexFileDefaultPath = fhirVersion match {
      case "R4" => DEFAULT_RESOURCE_PATHS.INDEX_CONF_PATH_R4
      case "R5" => DEFAULT_RESOURCE_PATHS.INDEX_CONF_PATH_R5
      case oth =>
        FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER,s"db-index-conf-${oth.toLowerCase}.json")
    }
    //Parse Index configurations, and set configured shard keys
    indexConfigurations = IndexConfigurator.parseIndexConfigurationFile(OnfhirConfig.dbIndexConfigurationPath, dbIndexFileDefaultPath, fhirConfig.compartmentRelations)
    if(OnfhirConfig.mongoShardingEnabled)
      fhirConfig.shardKeys = indexConfigurations.map(c => c._1 -> c._2.shardKey.getOrElse(Nil).toSet)
    fhirConfig
  }
}

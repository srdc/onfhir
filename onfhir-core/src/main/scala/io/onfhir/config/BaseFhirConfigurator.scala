package io.onfhir.config

import io.onfhir.api.service.{FHIROperationHandler, FHIROperationHandlerService}
import io.onfhir.api.{DEFAULT_RESOURCE_PATHS, FHIR_ROOT_URL_FOR_DEFINITIONS, Resource, SERVER_CONFORMANCE_STATEMENT_ID}
import io.onfhir.db.DBInitializer
import org.slf4j.{Logger, LoggerFactory}
import io.onfhir.api.util.{FHIRUtil, IOUtil}
import io.onfhir.api.validation.{ConstraintKeys, FHIRResourceValidator, IFhirResourceValidator, IFhirTerminologyValidator, ProfileRestrictions, ReferenceResolver}
import io.onfhir.exception.InitializationException
import io.onfhir.validation.{FhirContentValidator, FhirTerminologyValidator, ReferenceRestrictions, TypeRestriction}
import org.json4s.Extraction
import io.onfhir.api._

/**
 * Configuration of FHIR related capabilites based on foundation resources provided (CapabilityStatement, StructureDefinition, etc)
 */
abstract class BaseFhirConfigurator extends IFhirVersionConfigurator {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  //Name of the files that includes the Bundle for search parameter definitions in base FHIR specification
  protected  val SEARCH_PARAMETERS_BUNDLE_FILE_NAME = s"search-parameters$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for Resources in base FHIR specification
  protected  val PROFILES_RESOURCES_BUNDLE_FILE_NAME = s"profiles-resources$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for Types in base FHIR specification
  protected  val PROFILES_TYPES_BUNDLE_FILE_NAME = s"profiles-types$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for other FHIR profiles given in base
  protected  val PROFILES_OTHERS_BUNDLE_FILE_NAME = s"profiles-others$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the file that includes the Bundle for Structure Definitions for extensions given in base
  protected  val PROFILES_EXTENSIONS_BUNDLE_FILE_NAME = s"extension-definitions$FOUNDATION_RESOURCES_FILE_SUFFIX"
  //Name of the files that includes the Bundle for ValueSets and CodeSystems in base FHIR specification
  protected val VALUESET_AND_CODESYSTEM_BUNDLE_FILES = Seq(s"valuesets$FOUNDATION_RESOURCES_FILE_SUFFIX", s"v3-codesystems$FOUNDATION_RESOURCES_FILE_SUFFIX", s"v2-tables$FOUNDATION_RESOURCES_FILE_SUFFIX")

  // Configuration resources to configure FHIR Capabilities of the server
  var conformanceResource:Resource = _
  var profileResources:Seq[Resource] = _
  var searchParameterResources:Seq[Resource] = _
  var operationDefResources:Seq[Resource] = _
  var compartmentDefResources:Seq[Resource] = _
  var valueSetResources:Seq[Resource] = _
  var codeSystemResources:Seq[Resource] = _
  var indexConfigurations:Map[String, IndexConfigurator.ResourceIndexConfiguration] = _

  var FHIR_COMPLEX_TYPES:Set[String] = _
  var FHIR_PRIMITIVE_TYPES:Set[String] = _

  /**
   * Initialize the platform by preparing a FHIR configuration from the given FHIR Foundation resources and base standard
   * @param fromConfig            If initialized from configuration files or from database (previously stored configs)
   * @param fhirOperationImplms   FHIR Operation implementations (URL -> Classpath)
   * @return
   */
  override def initializePlatform(fromConfig:Boolean = false, fhirOperationImplms:Map[String, String]):FhirConfig = {
    logger.info("Reading base FHIR foundation resources (base standard) to start configuration of onFhir server ...")
    //Read base resource profiles defined in the standard
    val baseResourceProfileResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read the base data type profiles defined in the standard
    val baseDataTypeProfileResources = IOUtil.readStandardBundleFile(PROFILES_TYPES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read other profiles and extensions given in zip file
    val baseOtherProfileResources = IOUtil.readStandardBundleFile(PROFILES_OTHERS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    val baseExtensionProfileResources = IOUtil.readStandardBundleFile(PROFILES_EXTENSIONS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
    //Read the base search parameters defined in the standard
    val baseSearchParameterResources = IOUtil.readStandardBundleFile(SEARCH_PARAMETERS_BUNDLE_FILE_NAME, Set(FHIR_SEARCH_PARAMETER))
    //Read the base ValueSet definitions defined in the standard
    val baseValueSetsAndCodeSystems = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => IOUtil.readStandardBundleFile(file, Set(FHIR_VALUE_SET, FHIR_CODE_SYSTEM)))
    //Read the base operation definitions
    val baseOperationDefinitionResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_OPERATION_DEFINITION))
    //Read the base compartment definitions
    val baseCompartmentDefinitionResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_COMPARTMENT_DEFINITION))

    //Initalize fhir configuration
    var fhirConfig = new FhirConfig(fhirVersion)

    val allTypes = baseDataTypeProfileResources.flatMap(getTypeFromStructureDefinition)
    FHIR_COMPLEX_TYPES = allTypes.filter(_.head.isUpper).toSet
    FHIR_PRIMITIVE_TYPES = allTypes.filter(_.head.isLower).toSet
    //Initialize base types and resource types
    fhirConfig.FHIR_COMPLEX_TYPES = FHIR_COMPLEX_TYPES
    fhirConfig.FHIR_PRIMITIVE_TYPES = FHIR_PRIMITIVE_TYPES
    fhirConfig.FHIR_RESOURCE_TYPES = baseResourceProfileResources.flatMap(getTypeFromStructureDefinition).toSet

    logger.info("Reading FHIR foundation resources to start configuration of onFhir server ...")
    //Read the FHIR Conformance statement specified for this server
    conformanceResource = getConformance(fromConfig)
    //Read the StructureDefinitions for all supported profiles
    profileResources = getInfrastructureResources(fromConfig, FHIR_STRUCTURE_DEFINITION)
    //Read the defined search parameters in addition to base search parameters defined in the standard
    searchParameterResources = getInfrastructureResources(fromConfig, FHIR_SEARCH_PARAMETER)
    //Read defined operation definitions in addition to base operation definitions defined in the standard
    operationDefResources = getInfrastructureResources(fromConfig, FHIR_OPERATION_DEFINITION)
    //Read compartment definitions supported by this server
    compartmentDefResources = getInfrastructureResources(fromConfig, FHIR_COMPARTMENT_DEFINITION)
    //Read the ValueSet definitions to be used in this server (within the profiles)
    valueSetResources = getInfrastructureResources(fromConfig, FHIR_VALUE_SET)
    //Read the CodeSystem definitions to be used in this server (within the profiles)
    codeSystemResources = getInfrastructureResources(fromConfig, FHIR_CODE_SYSTEM)

    logger.info("Configuring the platform accordingly ...")

    logger.info("Parsing base FHIR foundation resources (base standard) ...")
    //Parsing base definitions
    val baseResourceProfiles = baseResourceProfileResources.map(parseStructureDefinition).map(p => p.url -> p).toMap
    val baseDataTypeProfiles = baseDataTypeProfileResources.map(parseStructureDefinition).map(p => p.url -> p).toMap
    val baseProfiles =
      baseResourceProfiles ++
        baseDataTypeProfiles.filter(_._1.split('/').last.head.isUpper) ++
          baseOtherProfileResources.map(parseStructureDefinition).map(p => p.url -> p).toMap ++
            baseExtensionProfileResources.map(parseStructureDefinition).map(p => p.url -> p).toMap
    val baseSearchParameters = baseSearchParameterResources.map(parseSearchParameter).map(s => s.url -> s).toMap
    val baseOperationDefinitions = baseOperationDefinitionResources.map(parseOperationDefinition).map(p => p.url -> p).toMap
    val baseCompartmentDefinitions = baseCompartmentDefinitionResources.map(parseCompartmentDefinition).map(c => c.url -> c).toMap

    //Initialize fhir config with base profiles and value sets to prepare for validation
    //fhirConfig.FHIR_RESOURCE_TYPES = baseResourceProfiles.filterNot(_._2.isAbstract).map(_._1.split('/').last).toSet
    //fhirConfig.FHIR_COMPLEX_TYPES = baseDataTypeProfiles.filterNot(_._2.isAbstract).filter(_._1.split('/').last.head.isUpper).map(_._1.split('/').last).toSet
    //fhirConfig.FHIR_PRIMITIVE_TYPES = baseDataTypeProfiles.filterNot(_._2.isAbstract).filter(_._1.split('/').last.head.isLower).map(_._1.split('/').last).toSet
    fhirConfig.profileRestrictions = baseProfiles
    fhirConfig.valueSetRestrictions = parseValueSetAndCodeSystems(baseValueSetsAndCodeSystems)

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
    val conformance = parseCapabilityStatement(conformanceResource)
    val profiles = profileResources.map(parseStructureDefinition).map(p => p.url -> p).toMap
    val searchParameters = searchParameterResources.map(parseSearchParameter).map(s => s.url -> s).toMap
    val operationDefs = operationDefResources.map(opDef => parseOperationDefinition(opDef)).map(o => o.url -> o).toMap
    val compartments = compartmentDefResources.map(parseCompartmentDefinition).map(c => c.url -> c).toMap
    //Parse all as bundle
    val valueSets = parseValueSetAndCodeSystems(valueSetResources ++ codeSystemResources ++ baseValueSetsAndCodeSystems)

    logger.info("Configuring supported FHIR resources and profiles ...")
    fhirConfig = validateAndConfigureProfiles(fhirConfig, conformance, profiles, baseProfiles)

    logger.info("Configuring supported FHIR search parameters for supported resources ...")
    fhirConfig = validateAndConfigureSearchParameters(fhirConfig, conformance, searchParameters, baseSearchParameters, baseProfiles ++ profiles)

    logger.info("Configuring supported FHIR operations ...")
    fhirConfig = validateAndConfigureOperations(fhirConfig, conformance, operationDefs, baseOperationDefinitions, fhirOperationImplms)

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
    fhirConfig.FHIR_JSON_MEDIA_TYPES = FHIR_JSON_MEDIA_TYPES
    fhirConfig.FHIR_XML_MEDIA_TYPES = FHIR_XML_MEDIA_TYPES
    fhirConfig.FHIR_FORMAT_MIME_TYPE_MAP = FHIR_FORMAT_MIME_TYPE_MAP
    fhirConfig.FHIR_JSON_PATCH_MEDIA_TYPE = FHIR_JSON_PATCH_MEDIA_TYPE
    fhirConfig.FHIR_DEFAULT_MEDIA_TYPE = FHIR_DEFAULT_MEDIA_TYPE

    fhirConfig
  }

  /**
   * Retrieve type from a structure definition
   * @param structureDefinition
   * @return
   */
  def getTypeFromStructureDefinition(structureDefinition:Resource):Option[String] = {
    val rtype =  FHIRUtil.extractValueOption[String](structureDefinition, "type").get
    val isAbstract = FHIRUtil.extractValueOption[Boolean](structureDefinition, "abstract").get
    if(isAbstract)
      None
    else
      Some(rtype)
  }

  /**
   * Setup the platfrom by initializating/updating the database
   * @param fhirConfig
   */
  override def setupPlatform(fhirConfig: FhirConfig):Unit = {
    logger.info("Setting up (or updating) the platform as requested ...")
    //Basic preparation for database
    DBInitializer.prepareDatabase()

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
    DBInitializer.createCollections(supportedResourcesAndVersions)
    //Create the indexes in DB
    DBInitializer.createIndexes(supportedResourcesAndVersions, fhirConfig.resourceQueryParameters, fhirConfig.commonQueryParameters, indexConfigurations)
    //Store the infrastructure resources into DB
    logger.info("Storing infrastructure resources to database ...")
    //Store the Conformance statement (Set a fixed id for the conformance)
    DBInitializer.storeInfrastructureResources(FHIR_CONFORMANCE, Seq(FHIRUtil.setId(conformanceResource, SERVER_CONFORMANCE_STATEMENT_ID)))
    //Store the Profiles (Structure Definitions)
    DBInitializer.storeInfrastructureResources(FHIR_STRUCTURE_DEFINITION, profileResources)
    //Store the Search Parameters
    DBInitializer.storeInfrastructureResources(FHIR_SEARCH_PARAMETER, searchParameterResources)
    //Store the CompartmentDefinitions
    DBInitializer.storeInfrastructureResources(FHIR_COMPARTMENT_DEFINITION,  compartmentDefResources)
    //Store the ValueSets
    DBInitializer.storeInfrastructureResources(FHIR_VALUE_SET, valueSetResources)
    //Store the OperationDefinitions
    DBInitializer.storeInfrastructureResources(FHIR_OPERATION_DEFINITION,  operationDefResources)
    //Store the Code systems
    DBInitializer.storeInfrastructureResources(FHIR_CODE_SYSTEM,  codeSystemResources)

    //If we need to persist the base FHIR standard definitions
    OnfhirConfig.fhirPersistBaseDefinitions.foreach {
      case FHIR_STRUCTURE_DEFINITION =>
        val baseStructureDefinitions =
          //Base resource definitions
          IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION)) ++
          //Read the base data type profiles defined in the standard
          IOUtil.readStandardBundleFile(PROFILES_TYPES_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION)) ++
          //Read other profiles and extensions given in zip file
          IOUtil.readStandardBundleFile(PROFILES_OTHERS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION)) ++
          IOUtil.readStandardBundleFile(PROFILES_EXTENSIONS_BUNDLE_FILE_NAME, Set(FHIR_STRUCTURE_DEFINITION))
        //Store the definitions
        DBInitializer.storeInfrastructureResources(FHIR_STRUCTURE_DEFINITION, baseStructureDefinitions)

      case FHIR_SEARCH_PARAMETER =>
        //Read the base search parameters defined in the standard
        val baseSearchParameterResources = IOUtil.readStandardBundleFile(SEARCH_PARAMETERS_BUNDLE_FILE_NAME, Set(FHIR_SEARCH_PARAMETER))
        //Store the search parameters
        DBInitializer.storeInfrastructureResources(FHIR_SEARCH_PARAMETER, baseSearchParameterResources)

      case FHIR_OPERATION_DEFINITION =>
        val baseOperationDefinitionResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_OPERATION_DEFINITION))
        //Store the operation definitions
        DBInitializer.storeInfrastructureResources(FHIR_OPERATION_DEFINITION, baseOperationDefinitionResources)

      case FHIR_VALUE_SET =>
        //Read the base ValueSet definitions defined in the standard
        val baseValueSets = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => IOUtil.readStandardBundleFile(file, Set(FHIR_VALUE_SET)))
        //Store the value sets
        DBInitializer.storeInfrastructureResources(FHIR_VALUE_SET, baseValueSets)

      case FHIR_CODE_SYSTEM =>
        //Read the base ValueSet definitions defined in the standard
        val baseCodeSystems = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => IOUtil.readStandardBundleFile(file, Set(FHIR_CODE_SYSTEM)))
        //Store the value sets
        DBInitializer.storeInfrastructureResources(FHIR_CODE_SYSTEM, baseCodeSystems)

      case FHIR_COMPARTMENT_DEFINITION =>
        //Read the base compartment definitions
        val baseCompartmentDefinitionResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(FHIR_COMPARTMENT_DEFINITION))
        //Store the compartment definitions
        DBInitializer.storeInfrastructureResources(FHIR_COMPARTMENT_DEFINITION, baseCompartmentDefinitionResources)
    }
  }

  /**
   * Return our generic resource validator
   * @param fhirConfig
   * @return
   */
  override def getResourceValidator(fhirConfig: FhirConfig):IFhirResourceValidator = {
    new FHIRResourceValidator(fhirConfig)
  }

  /**
   * Return our generic terminology validator
   * @param fhirConfig
   * @return
   */
  override def getTerminologyValidator(fhirConfig: FhirConfig):IFhirTerminologyValidator = {
    new FhirTerminologyValidator(fhirConfig)
  }

  /**
   * Validate and handle profile configurations
   * @param fhirConfig            FhirConfig obj until now
   * @param conformance           Conformance configurations
   * @param profiles              Profiles given by configuration
   * @param baseProfiles          Base profiles given in standard bundle
   * @return
   */
  private def validateAndConfigureProfiles(fhirConfig: FhirConfig, conformance:FHIRCapabilityStatement, profiles:Map[String, ProfileRestrictions], baseProfiles:Map[String, ProfileRestrictions]):FhirConfig = {
    //Check if all base profiles mentioned in Conformance are given in profile configurations
    var profileDefinitionsNotGiven = conformance.restResourceConf.flatMap(_.profile).filter(p => !profiles.contains(p) && !baseProfiles.contains(p))
    if(profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in profile configurations for some of the base profiles (${profileDefinitionsNotGiven.mkString(",")}) declared in CapabilityStatement (CapabilityStatement.rest.resource.profile)! ")
    //Check if all supported profiles mentioned in Conformance are given in profile configurations
    profileDefinitionsNotGiven = conformance.restResourceConf.flatMap(_.supportedProfiles.toSeq).filter(p => !profiles.contains(p) && !baseProfiles.contains(p))
    if(profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in profile configurations for some of the supported profiles (${profileDefinitionsNotGiven.mkString(",")}) declared in CapabilityStatement (CapabilityStatement.rest.resource.supportedProfile)! ")

    //Get the URLs of all used base profiles
    val baseProfilesUrlsUsed =
      (conformance.restResourceConf.flatMap(_.profile).filter(baseProfiles.contains) ++
       conformance.restResourceConf.flatMap(_.supportedProfiles.toSeq).filter(baseProfiles.contains)).toSet


    //Check if all mentioned profiles within the given profiles also exist in profile set (Profile set is closed)
    var allProfilesAndExtensionsMentionedInSomewhere = findMentionedProfiles(fhirConfig, profiles.values.toSeq)
    profileDefinitionsNotGiven = allProfilesAndExtensionsMentionedInSomewhere.filter(p => !profiles.contains(p) && !baseProfiles.contains(p)).toSeq
    if(profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in o profile configurations for the referred profiles (${profileDefinitionsNotGiven.mkString(",")}) within the given profiles (e.g. as base profile 'StructureDefinition.baseDefinition', target profile for an element StructureDefinition.differential.element.type.profile or reference StructureDefinition.differential.element.type.targetProfile) ! All mentioned profiles should be given for validation!")

    allProfilesAndExtensionsMentionedInSomewhere =
      allProfilesAndExtensionsMentionedInSomewhere.diff(profiles.keySet) ++
        baseProfilesUrlsUsed ++
        //Base profiles used in FHIR interactions
        Set(
          s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/OperationOutcome",
          s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/Bundle",
          s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/Parameters",
        )

    fhirConfig.supportedProfiles = conformance.restResourceConf.map(restConf => restConf.resource -> restConf.supportedProfiles).toMap
    fhirConfig.resourceConfigurations = conformance.restResourceConf.map(restConf => restConf.resource -> restConf).toMap

    fhirConfig.profileRestrictions =
      profiles ++
        findClosureForBaseProfiles(fhirConfig, allProfilesAndExtensionsMentionedInSomewhere, baseProfiles) //Only add base profiles that are mentioned
    fhirConfig
  }

  private def findClosureForBaseProfiles(fhirConfig: FhirConfig, mentionedBaseProfileUrls:Set[String], baseProfiles:Map[String, ProfileRestrictions]):Map[String, ProfileRestrictions] = {
    val mentionedBaseProfiles = mentionedBaseProfileUrls.map(url => url -> baseProfiles.apply(url)).toMap
    val deepMentionedBaseProfiles = findMentionedProfiles(fhirConfig, mentionedBaseProfiles.values.toSeq)
    val newUrls = deepMentionedBaseProfiles.diff(mentionedBaseProfileUrls)
    if(newUrls.nonEmpty)
      findClosureForBaseProfiles(fhirConfig, mentionedBaseProfileUrls ++ newUrls, baseProfiles)
    else
      mentionedBaseProfiles
  }

  /**
   * Find URLs of mentioned profiles in this set
   * @param profiles
   * @return
   */
  private def findMentionedProfiles(fhirConfig: FhirConfig, profiles:Seq[ProfileRestrictions]):Set[String] = {
    profiles.flatMap(p => {
      p.elementRestrictions.map(_._2)
        .flatMap(e =>
          e.restrictions.get(ConstraintKeys.DATATYPE).toSeq.map(_.asInstanceOf[TypeRestriction])
            .flatMap(_.dataTypesAndProfiles.flatMap(dtp => dtp._2 match {
              case Nil =>
                if (fhirConfig.FHIR_COMPLEX_TYPES.contains(dtp._1))
                  Seq(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/${dtp._1}")
                else
                  Nil
              case oth => oth
            }).toSet) ++
            e.restrictions.get(ConstraintKeys.REFERENCE_TARGET).toSeq.map(_.asInstanceOf[ReferenceRestrictions]).flatMap(_.targetProfiles).toSet) ++
        p.baseUrl.toSeq
    }).toSet
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
  private def validateAndConfigureSearchParameters(fhirConfig: FhirConfig, conformance:FHIRCapabilityStatement, searchParameters:Map[String, FHIRSearchParameter], baseSearchParameters:Map[String, FHIRSearchParameter], allProfiles:Map[String, ProfileRestrictions]) :FhirConfig = {
    //Check if for all search parameters mentioned in the Conformance, a SearchParameter definition exist in base standard or given configuration
    val resourcesWithMissingSearchParameterDefs =
      conformance.restResourceConf
        .map(r => r.resource -> r.searchParams.filter(sp => !(searchParameters ++ baseSearchParameters).contains(sp) || (searchParameters ++ baseSearchParameters).get(sp).exists(spd => !spd.base.contains("Resource") && !spd.base.contains(r.resource))))
        .filter(_._2.nonEmpty)

    if(resourcesWithMissingSearchParameterDefs.nonEmpty)
      throw new InitializationException(s"Missing SearchParameter definitions (${resourcesWithMissingSearchParameterDefs.map(r => s"${r._1}: [${r._2.mkString(",")}]" ).mkString(",")}) in search parameter configurations!")

    fhirConfig.resourceQueryParameters =
      fhirConfig.resourceConfigurations
        .map(r => {
          val spConfigurator = new SearchParameterConfigurator(r._1, r._2.profile, fhirConfig, r._2.searchParams)
          r._1 ->
            (
              r._2.searchParams.map(sp => {
                val searchParamDef:FHIRSearchParameter = searchParameters.getOrElse(sp, baseSearchParameters(sp))
                spConfigurator.createSearchParameterConf(searchParamDef)
              }).filter(_.isDefined).map(_.get)
                .map(spc => spc.pname -> spc).toMap
              )
        })
    val spConfigurator = new SearchParameterConfigurator("DomainResource", None, fhirConfig, Set.empty[String])
    fhirConfig.commonQueryParameters =
      conformance.searchParamDefUrls
        .flatMap(spUrl => {
          baseSearchParameters.get(spUrl).map(searchParamDef => {
              val spConfigurator =
                if(spUrl.split('/').last.startsWith("DomainResource"))
                  new SearchParameterConfigurator("DomainResource", None, fhirConfig, Set.empty[String])
                else
                  new SearchParameterConfigurator("Resource", None, fhirConfig, Set.empty[String])

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
   * @return
   */
  private def validateAndConfigureOperations(fhirConfig: FhirConfig, conformance:FHIRCapabilityStatement, operationDefs:Map[String, OperationConf], baseOperationDefinitions:Map[String, OperationConf], fhirOperationImplms:Map[String, String]): FhirConfig = {
    //Check if all operations mentioned in Conformance are given in operation definition configurations
    val operationDefinitionsNotGiven = conformance.operationDefUrls.diff(operationDefs.keySet ++ baseOperationDefinitions.keySet)
    if(operationDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing OperationDefinition in operation definition configurations for the operation definitions (${operationDefinitionsNotGiven.mkString(",")}) declared in CapabilityStatement (CapabilityStatement.rest.resource.operation | CapabilityStatement.rest.operation)! ")

    val operationImplementationsNotExist = conformance.operationDefUrls.diff(fhirOperationImplms.keySet)
    if(operationImplementationsNotExist.nonEmpty)
      throw new InitializationException(s"Missing implementations for FHIR operations ${operationImplementationsNotExist.mkString(",")}! Please provide class path of the implementation for each FHIR operation declared in CapabilityStatement.")

    val opHandler = new FHIROperationHandler()
    val operationsWithInvalidClassPaths =
      fhirOperationImplms
        .filterKeys(conformance.operationDefUrls.contains)
        .filter(opImpl => opHandler.loadOperationClass(opImpl._2).isEmpty)

     if(operationsWithInvalidClassPaths.nonEmpty)
       throw new InitializationException(s"Invalid class paths ${operationsWithInvalidClassPaths.map(op => op._2).mkString(",")} for FHIR operation implementations!")

    fhirConfig.supportedOperations =
      conformance.operationDefUrls.map(opDefUrl => {
        val opDef = operationDefs.getOrElse(opDefUrl, baseOperationDefinitions.apply(opDefUrl))
        opDef.classPath = fhirOperationImplms(opDefUrl)
        opDef
      }).toSeq

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
  private def validateAndConfigureCompartments(fhirConfig: FhirConfig,  conformance:FHIRCapabilityStatement, compartments:Map[String, FHIRCompartmentDefinition], baseCompartmentDefinitions:Map[String, FHIRCompartmentDefinition]):FhirConfig = {
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
  private def readIndexConfigurationsAndConfigureShardKeys(fhirConfig: FhirConfig):FhirConfig = {
    //Parse Index configurations, and set configured shard keys
    indexConfigurations = IndexConfigurator.parseIndexConfigurationFile(OnfhirConfig.dbIndexConfigurationPath, DEFAULT_RESOURCE_PATHS.INDEX_CONF_PATH, fhirConfig.compartmentRelations)
    if(OnfhirConfig.mongoShardingEnabled)
      fhirConfig.shardKeys = indexConfigurations.mapValues(iconf => iconf.shardKey.getOrElse(Nil).toSet)
    fhirConfig
  }

  /**
   * Validate the given infrasturtcure resources and throw exception if any invalid
   * @param baseFhirConfig
   * @param rtype
   * @param resources
   */
  private def validateGivenInfrastructureResources(baseFhirConfig:FhirConfig, rtype:String, resources:Seq[Resource]) = {
    import io.onfhir.util.JsonFormatter._

    val issuesForEachResource = resources.map(resource =>
      (resource \ "url").extractOpt[String] match {
        case None => throw new InitializationException(s"All infrastructure resources used for onFhir configuration shoud have a url!")
        case Some(url) =>
          val fhirContentValidator = FhirContentValidator.apply(baseFhirConfig, s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype", new ReferenceResolver(baseFhirConfig, resource))
          url ->  fhirContentValidator.validateComplexContent(resource)
      }
    )
    val resourcesWithProblems = issuesForEachResource.filter(rIssues => rIssues._2.exists(i => i.isError))
    if(resourcesWithProblems.nonEmpty) {
      val errors = resourcesWithProblems.map(rp => s"${rp._1} :: ${rp._2.map(i => Extraction.decompose(i)).mkString(",")}").mkString("\n")
      throw new InitializationException(s"Some of the given infrastructure resources (${resourcesWithProblems.map(_._1).mkString(",")}) of type $rtype does not conform to base FHIR specification! $errors")
    }
  }

  /**
   * Read and parse Conformance statement (either from the Config or Default conformance path or from database)
   * @return
   */
  protected def getConformance(fromConfig:Boolean = false): Resource = {
    fromConfig match {
      //If true, read from configuration folders/files
      case true =>
        IOUtil.readResource(OnfhirConfig.conformancePath, DEFAULT_RESOURCE_PATHS.CONFORMANCE_PATH, FHIR_CONFORMANCE)
      //If not, read from database
      case false =>
        DBInitializer.getConformance(FHIR_CONFORMANCE)
    }
  }

  /**
   * Read FHIR infrastructure resources of a specific type from database or configuration folders/zips
   * @param fromConfig
   * @param rtype
   * @return
   */
  def getInfrastructureResources(fromConfig:Boolean = false, rtype:String):Seq[Resource] = {
    val pathsToSearch =
      rtype match {
        case FHIR_SEARCH_PARAMETER =>
          OnfhirConfig.searchParametersPath -> DEFAULT_RESOURCE_PATHS.SEARCH_PARAMETER
        case FHIR_STRUCTURE_DEFINITION =>
          OnfhirConfig.profilesPath -> DEFAULT_RESOURCE_PATHS.PROFILES_FOLDER
        case  FHIR_OPERATION_DEFINITION =>
          OnfhirConfig.operationDefinitionsPath -> DEFAULT_RESOURCE_PATHS.OPDEFS_PATH
        case FHIR_COMPARTMENT_DEFINITION =>
          OnfhirConfig.compartmentDefinitionsPath -> DEFAULT_RESOURCE_PATHS.COMPARTMENTS_PATH
        case FHIR_VALUE_SET =>
          OnfhirConfig.valueSetsPath -> DEFAULT_RESOURCE_PATHS.VALUESETS_PATH
        case FHIR_CODE_SYSTEM =>
          OnfhirConfig.codeSystemsPath -> DEFAULT_RESOURCE_PATHS.CODESYSTEMS_PATH
      }

    fromConfig match {
      //If true, read from configuration files
      case true=>
        IOUtil.readResourcesInFolderOrZip(pathsToSearch._1, pathsToSearch._2)
      //Otherwise read from db
      case false=>
        DBInitializer.getInrastructureResources(rtype)
    }
  }

}

package io.onfhir.config

import io.onfhir.api.service.{FHIROperationHandler, FHIROperationHandlerService}
import io.onfhir.api.{DEFAULT_RESOURCE_PATHS, FHIR_ROOT_URL_FOR_DEFINITIONS, Resource, SERVER_CONFORMANCE_STATEMENT_ID}
import io.onfhir.db.DBInitializer
import org.slf4j.{Logger, LoggerFactory}
import io.onfhir.api.util.{FHIRUtil, IOUtil}
import io.onfhir.api.validation.{ConstraintKeys, ProfileRestrictions}
import io.onfhir.exception.InitializationException
import io.onfhir.validation.{FhirContentValidator, ReferenceRestrictions, TypeRestriction}

import org.json4s.Extraction


/**
 * Configuration of FHIR related capabilites based on foundation resources provided (CapabilityStatement, StructureDefinition, etc)
 * @param fhirVersionConfigurator   Version specific configurator
 */
class FhirConfigurator(fhirVersionConfigurator: IFhirVersionConfigurator) {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  //Name of the files that includes the Bundle for search parameter definitions in base FHIR specification
  final val SEARCH_PARAMETERS_BUNDLE_FILE_NAME = "search-parameters.json"
  //Name of the file that includes the Bundle for Structure Definitions for Resources in base FHIR specification
  final val PROFILES_RESOURCES_BUNDLE_FILE_NAME = "profiles-resources.json"
  //Name of the file that includes the Bundle for Structure Definitions for Types in base FHIR specification
  final val PROFILES_TYPES_BUNDLE_FILE_NAME = "profiles-types.json"
  //Name of the files that includes the Bundle for ValueSets and CodeSystems in base FHIR specification
  final val VALUESET_AND_CODESYSTEM_BUNDLE_FILES = Seq("valuesets.json", "v3-codesystems.json")

  // Configuration resources to configure FHIR Capabilities of the server
  var conformanceResource:Resource = _
  var profileResources:Seq[Resource] = _
  var searchParameterResources:Seq[Resource] = _
  var operationDefResources:Seq[Resource] = _
  var compartmentDefResources:Seq[Resource] = _
  var valueSetResources:Seq[Resource] = _
  var codeSystemResources:Seq[Resource] = _
  var indexConfigurations:Map[String, IndexConfigurator.ResourceIndexConfiguration] = _
  /**
   *
   * @param fromConfig
   * @return
   */
  def initializePlatform(fromConfig:Boolean = false):FhirConfig = {
    logger.info("Reading base FHIR foundation resources (base standard) to start configuration of onFhir server ...")
    //Read base resource profiles defined in the standard
    val baseResourceProfileResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(fhirVersionConfigurator.FHIR_STRUCTURE_DEFINITION))
    //Read the base data type profiles defined in the standard
    val baseDataTypeProfileResources = IOUtil.readStandardBundleFile(PROFILES_TYPES_BUNDLE_FILE_NAME, Set(fhirVersionConfigurator.FHIR_STRUCTURE_DEFINITION))
    //Read the base search parameters defined in the standard
    val baseSearchParameterResources = IOUtil.readStandardBundleFile(SEARCH_PARAMETERS_BUNDLE_FILE_NAME, Set(fhirVersionConfigurator.FHIR_SEARCH_PARAMETER))
    //Read the base ValueSet definitions defined in the standard
    val baseValueSetsAndCodeSystems = VALUESET_AND_CODESYSTEM_BUNDLE_FILES.flatMap(file => IOUtil.readStandardBundleFile(file, Set(fhirVersionConfigurator.FHIR_VALUE_SET, fhirVersionConfigurator.FHIR_CODE_SYSTEM)))
    //Read the base operation definitions
    val baseOperationDefinitionResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(fhirVersionConfigurator.FHIR_OPERATION_DEFINITION))
    //Read the base compartment definitions
    val baseCompartmentDefinitionResources = IOUtil.readStandardBundleFile(PROFILES_RESOURCES_BUNDLE_FILE_NAME, Set(fhirVersionConfigurator.FHIR_COMPARTMENT_DEFINITION))

    logger.info("Reading FHIR foundation resources to start configuration of onFhir server ...")
    //Read the FHIR Conformance statement specified for this server
    conformanceResource = getConformance(fromConfig)
    //Read the StructureDefinitions for all supported profiles
    profileResources = getInfrastructureResources(fromConfig, fhirVersionConfigurator.FHIR_STRUCTURE_DEFINITION)
    //Read the defined search parameters in addition to base search parameters defined in the standard
    searchParameterResources = getInfrastructureResources(fromConfig, fhirVersionConfigurator.FHIR_SEARCH_PARAMETER)
    //Read defined operation definitions in addition to base operation definitions defined in the standard
    operationDefResources = getInfrastructureResources(fromConfig, fhirVersionConfigurator.FHIR_OPERATION_DEFINITION)
    //Read compartment definitions supported by this server
    compartmentDefResources = getInfrastructureResources(fromConfig, fhirVersionConfigurator.FHIR_COMPARTMENT_DEFINITION)
    //Read the ValueSet definitions to be used in this server (within the profiles)
    valueSetResources = getInfrastructureResources(fromConfig, fhirVersionConfigurator.FHIR_VALUE_SET)
    //Read the CodeSystem definitions to be used in this server (within the profiles)
    codeSystemResources = getInfrastructureResources(fromConfig, fhirVersionConfigurator.FHIR_CODE_SYSTEM)

    logger.info("Configuring the platform accordingly ...")
    //Initalize fhir configuration
    var fhirConfig = new FhirConfig(fhirVersionConfigurator.fhirVersion)
    logger.info("Parsing base FHIR foundation resources (base standard) ...")
    //Parsing base definitions
    val baseProfiles =
      baseResourceProfileResources.flatMap(fhirVersionConfigurator.parseStructureDefinition).map(p => p.url -> p).toMap ++
        baseDataTypeProfileResources.flatMap(fhirVersionConfigurator.parseStructureDefinition).map(p => p.url -> p).toMap
    val baseSearchParameters = baseSearchParameterResources.map(fhirVersionConfigurator.parseSearchParameter).map(s => s.url -> s).toMap
    val baseOperationDefinitions = baseOperationDefinitionResources.map(fhirVersionConfigurator.parseOperationDefinition).map(p => p.url -> p).toMap
    val baseCompartmentDefinitions = baseCompartmentDefinitionResources.map(fhirVersionConfigurator.parseCompartmentDefinition).map(c => c.url -> c).toMap

    //Initialize fhir config with base profiles and value sets to prepare for validation
    fhirConfig.profileRestrictions = baseProfiles
    fhirConfig.valueSetRestrictions = fhirVersionConfigurator.parseValueSetAndCodeSystems(baseValueSetsAndCodeSystems)

    logger.info("Validating given FHIR foundation resources for base specification conformance ...")
    //Validations
    validateGivenInfrastructureResources(fhirConfig, fhirVersionConfigurator.FHIR_CONFORMANCE, Seq(conformanceResource))
    validateGivenInfrastructureResources(fhirConfig, fhirVersionConfigurator.FHIR_STRUCTURE_DEFINITION, profileResources)
    validateGivenInfrastructureResources(fhirConfig, fhirVersionConfigurator.FHIR_SEARCH_PARAMETER, searchParameterResources)
    validateGivenInfrastructureResources(fhirConfig, fhirVersionConfigurator.FHIR_OPERATION_DEFINITION, operationDefResources)
    validateGivenInfrastructureResources(fhirConfig, fhirVersionConfigurator.FHIR_COMPARTMENT_DEFINITION, compartmentDefResources)
    validateGivenInfrastructureResources(fhirConfig, fhirVersionConfigurator.FHIR_VALUE_SET, valueSetResources)
    validateGivenInfrastructureResources(fhirConfig, fhirVersionConfigurator.FHIR_CODE_SYSTEM, codeSystemResources)

    logger.info("Parsing given FHIR foundation resources ...")
    //Parsing the Conformance statement into our compact form
    val conformance = fhirVersionConfigurator.parseCapabilityStatement(conformanceResource)
    val profiles = profileResources.flatMap(fhirVersionConfigurator.parseStructureDefinition).map(p => p.url -> p).toMap
    val searchParameters = searchParameterResources.map(fhirVersionConfigurator.parseSearchParameter).map(s => s.url -> s).toMap
    val operationDefs = operationDefResources.map(opDef => fhirVersionConfigurator.parseOperationDefinition(opDef)).map(o => o.url -> o).toMap
    val compartments = compartmentDefResources.map(fhirVersionConfigurator.parseCompartmentDefinition).map(c => c.url -> c).toMap
    //Parse all as bundle
    val valueSets = fhirVersionConfigurator.parseValueSetAndCodeSystems(valueSetResources ++ codeSystemResources ++ baseValueSetsAndCodeSystems)

    logger.info("Configuring supported FHIR resources and profiles ...")
    fhirConfig = validateAndConfigureProfiles(fhirConfig, conformance, profiles, baseProfiles)

    logger.info("Configuring supported FHIR search parameters for supported resources ...")
    fhirConfig = validateAndConfigureSearchParameters(fhirConfig, conformance, searchParameters, baseSearchParameters, baseProfiles ++ profiles)

    logger.info("Configuring supported FHIR operations ...")
    fhirConfig = validateAndConfigureOperations(fhirConfig, conformance, operationDefs, baseOperationDefinitions)

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
    fhirConfig.FHIR_RESULT_PARAMETERS = fhirVersionConfigurator.FHIR_RESULT_PARAMETERS
    fhirConfig.FHIR_SPECIAL_PARAMETERS = fhirVersionConfigurator.FHIR_SPECIAL_PARAMETERS
    fhirConfig.FHIR_JSON_MEDIA_TYPES = fhirVersionConfigurator.FHIR_JSON_MEDIA_TYPES
    fhirConfig.FHIR_XML_MEDIA_TYPES = fhirVersionConfigurator.FHIR_XML_MEDIA_TYPES
    fhirConfig.FHIR_FORMAT_MIME_TYPE_MAP = fhirVersionConfigurator.FHIR_FORMAT_MIME_TYPE_MAP
    fhirConfig.FHIR_JSON_PATCH_MEDIA_TYPE = fhirVersionConfigurator.FHIR_JSON_PATCH_MEDIA_TYPE
    fhirConfig.FHIR_DEFAULT_MEDIA_TYPE = fhirVersionConfigurator.FHIR_DEFAULT_MEDIA_TYPE

    fhirConfig
  }

  /**
   * Setup the platfrom by initializating/updating the database
   * @param fhirConfig
   */
  def setupPlatform(fhirConfig: FhirConfig):Unit = {
    logger.info("Setting up (or updating) the platform as requested ...")
    //Basic preparation for database
    DBInitializer.prepareDatabase()

    //Get supported resources and versioning mechanism
    val supportedResourcesAndVersions =
      fhirConfig
        .resourceConfigurations
        .map(rconf =>
          rconf._1 ->
            rconf._2.versioning
        )
    //Create collections in database for each supported resource
    DBInitializer.createCollections(supportedResourcesAndVersions)
    //Create the indexes in DB
    DBInitializer.createIndexes(supportedResourcesAndVersions, fhirConfig.resourceQueryParameters, fhirConfig.commonQueryParameters, indexConfigurations)
    //Store the infrastructure resources into DB
    logger.info("Storing infrastructure resources to database ...")
    //Store the Conformance statement (Set a fixed id for the conformance)
    DBInitializer.storeInfrastructureResources(fhirVersionConfigurator.FHIR_CONFORMANCE, Seq(FHIRUtil.setId(conformanceResource, SERVER_CONFORMANCE_STATEMENT_ID)))
    //Store the Profiles (Structure Definitions)
    DBInitializer.storeInfrastructureResources(fhirVersionConfigurator.FHIR_STRUCTURE_DEFINITION, profileResources)
    //Store the Search Parameters
    DBInitializer.storeInfrastructureResources(fhirVersionConfigurator.FHIR_SEARCH_PARAMETER, searchParameterResources)
    //Store the CompartmentDefinitions
    DBInitializer.storeInfrastructureResources(fhirVersionConfigurator.FHIR_COMPARTMENT_DEFINITION,  compartmentDefResources)
    //Store the ValueSets
    DBInitializer.storeInfrastructureResources(fhirVersionConfigurator.FHIR_VALUE_SET, valueSetResources)
    //Store the OperationDefinitions
    DBInitializer.storeInfrastructureResources(fhirVersionConfigurator.FHIR_OPERATION_DEFINITION,  operationDefResources)
    //Store the Code systems
    DBInitializer.storeInfrastructureResources(fhirVersionConfigurator.FHIR_CODE_SYSTEM,  codeSystemResources)
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
    //Check if all mentioned profiles within the given profiles also exist in profile set (Profile set is closed)
    val allProfilesAndExtensionsMentionedInProfiles =
      profiles.values.flatMap(p =>
        p.elementRestrictions.map(_._2)
          .flatMap(e =>
            e.restrictions.get(ConstraintKeys.DATATYPE).toSeq.map(_.asInstanceOf[TypeRestriction]).flatMap(_.dataTypesAndProfiles.flatMap(_._2).toSet) ++
              e.restrictions.get(ConstraintKeys.REFERENCE_TARGET).toSeq.map(_.asInstanceOf[ReferenceRestrictions]).flatMap(_.targetProfiles).toSet) ++
          p.baseUrl.toSeq
      ).toSet
    profileDefinitionsNotGiven = allProfilesAndExtensionsMentionedInProfiles.filter(p => !profiles.contains(p) && !baseProfiles.contains(p)).toSeq
    if(profileDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing StructureDefinition in o profile configurations for the referred profiles (${profileDefinitionsNotGiven.mkString(",")}) within the given profiles (e.g. as base profile 'StructureDefinition.baseDefinition', target profile for an element StructureDefinition.differential.element.type.profile or reference StructureDefinition.differential.element.type.targetProfile) ! All mentioned profiles should be given for validation!")

    fhirConfig.supportedProfiles = conformance.restResourceConf.map(restConf => restConf.resource -> restConf.supportedProfiles).toMap
    fhirConfig.resourceConfigurations = conformance.restResourceConf.map(restConf => restConf.resource -> restConf).toMap
    fhirConfig.profileRestrictions =
      profiles ++
        baseProfiles.filterKeys(allProfilesAndExtensionsMentionedInProfiles.contains) //Only add base profiles that are mentioned
    fhirConfig
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
                val searchParamDef:FHIRSearchParameter = searchParameters.getOrElse(sp, baseSearchParameters.get(sp))
                spConfigurator.createSearchParameterConf(searchParamDef)
              }).filter(_.isDefined).map(_.get)
                .map(spc => spc.pname -> spc).toMap
              )
        })
    val spConfigurator = new SearchParameterConfigurator("DomainResource", None, fhirConfig, Set.empty[String])
    fhirConfig.commonQueryParameters =
      conformance.searchParamDefUrls
        .map(spUrl => {
          val searchParamDef:FHIRSearchParameter = searchParameters(spUrl)
          spConfigurator.createSearchParameterConf(searchParamDef)
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
  private def validateAndConfigureOperations(fhirConfig: FhirConfig, conformance:FHIRCapabilityStatement, operationDefs:Map[String, OperationConf], baseOperationDefinitions:Map[String, OperationConf]): FhirConfig = {
    //Check if all operations mentioned in Conformance are given in operation definition configurations
    val operationDefinitionsNotGiven = conformance.operationDefUrls.diff(operationDefs.keySet ++ baseOperationDefinitions.keySet)
    if(operationDefinitionsNotGiven.nonEmpty)
      throw new InitializationException(s"Missing OperationDefinition in operation definition configurations for the operation definitions (${operationDefinitionsNotGiven.mkString(",")}) declared in CapabilityStatement (CapabilityStatement.rest.resource.operation | CapabilityStatement.rest.operation)! ")

    //Check if operation class paths are given correctly
    val opHandler = new FHIROperationHandler()
    operationDefs.values.map(_.classPath).foreach(cpath => opHandler.loadOperationClass(cpath) match {
      case None | Some(cls)  if !cls.newInstance().isInstanceOf[FHIROperationHandlerService] =>
        throw new InitializationException(s"OperationDefinition resources given as configuration for onFhir should have a 'name' element which provides a valid class path implementing FHIROperationHandlerService!")
      case _ =>
      //Nothing
    })

    fhirConfig.supportedOperations = operationDefs.values.toSeq

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
    val fhirContentValidator = FhirContentValidator.apply(baseFhirConfig, s"$FHIR_ROOT_URL_FOR_DEFINITIONS/$rtype")
    val issuesForEachResource = resources.map(resource =>
      (resource \ "url").extractOpt[String] match {
        case None => throw new InitializationException(s"All infrastructure resources used for onFhir configuration shoud have a url!")
        case Some(url) => url ->  fhirContentValidator.validateComplexContent(resource)
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
        IOUtil.readResource(OnfhirConfig.conformancePath, DEFAULT_RESOURCE_PATHS.CONFORMANCE_PATH, fhirVersionConfigurator.FHIR_CONFORMANCE)
      //If not, read from database
      case false =>
        DBInitializer.getConformance(fhirVersionConfigurator.FHIR_CONFORMANCE)
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
        case fhirVersionConfigurator.FHIR_SEARCH_PARAMETER =>
          OnfhirConfig.searchParametersPath -> DEFAULT_RESOURCE_PATHS.SEARCH_PARAMETER
        case fhirVersionConfigurator.FHIR_STRUCTURE_DEFINITION =>
          OnfhirConfig.profilesPath -> DEFAULT_RESOURCE_PATHS.PROFILES_FOLDER
        case  fhirVersionConfigurator.FHIR_OPERATION_DEFINITION =>
          OnfhirConfig.operationDefinitionsPath -> DEFAULT_RESOURCE_PATHS.OPDEFS_PATH
        case fhirVersionConfigurator.FHIR_COMPARTMENT_DEFINITION =>
          OnfhirConfig.compartmentDefinitionsPath -> DEFAULT_RESOURCE_PATHS.COMPARTMENTS_PATH
        case fhirVersionConfigurator.FHIR_VALUE_SET =>
          OnfhirConfig.valueSetsPath -> DEFAULT_RESOURCE_PATHS.VALUESETS_PATH
        case fhirVersionConfigurator.FHIR_CODE_SYSTEM =>
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

package io.onfhir.r4.config

import akka.http.scaladsl.model.{MediaType, MediaTypes}
import ca.uhn.fhir.context.FhirContext
import ca.uhn.fhir.validation.IValidatorModule
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api._
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.config._
import io.onfhir.exception.InitializationException
import io.onfhir.r4.audit.R4AuditCreator
import io.onfhir.r4.validation.OnFhirValidationSupportR4
import org.hl7.fhir.r4.model.Enumerations.BindingStrength
import org.hl7.fhir.r4.model._

import scala.collection.JavaConverters._
import scala.util.Try

class R4Configurator extends AbstractFhirConfigurator[CapabilityStatement, StructureDefinition, SearchParameter, CompartmentDefinition, ValueSet, Bundle, OperationDefinition, CodeSystem](FhirContext.forR4()) {
  //Main Media types
  val FHIR_JSON_MEDIA_TYPE = MediaType.applicationWithOpenCharset("fhir+json")
  val FHIR_XML_MEDIA_TYPE = MediaType.applicationWithOpenCharset("fhir+xml")

  val FHIR_RESULT_PARAMETERS = Seq(
    FHIR_SEARCH_RESULT_PARAMETERS.SORT,
    FHIR_SEARCH_RESULT_PARAMETERS.COUNT,
    FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY,
    FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS,
    FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE,
    FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE,
    FHIR_SEARCH_RESULT_PARAMETERS.PAGE,
    FHIR_SEARCH_RESULT_PARAMETERS.TOTAL,
    FHIR_SEARCH_RESULT_PARAMETERS.CONTAINED,
    FHIR_SEARCH_RESULT_PARAMETERS.CONTAINED_TYPE,
    FHIR_SEARCH_RESULT_PARAMETERS.SINCE,
    FHIR_SEARCH_RESULT_PARAMETERS.AT
  )

  val FHIR_SPECIAL_PARAMETERS = Seq(
    FHIR_SEARCH_SPECIAL_PARAMETERS.ID,
    FHIR_SEARCH_SPECIAL_PARAMETERS.LIST,
    FHIR_SEARCH_SPECIAL_PARAMETERS.QUERY,
    FHIR_SEARCH_SPECIAL_PARAMETERS.FILTER,
    FHIR_SEARCH_SPECIAL_PARAMETERS.HAS,
    FHIR_SEARCH_SPECIAL_PARAMETERS.TEXT,
    FHIR_SEARCH_SPECIAL_PARAMETERS.CONTENT
  )

  /**
    * Validator Module for R4
    * @return
    */
  override def getValidatorModule():IValidatorModule = {
    val instanceValidator = new org.hl7.fhir.r4.hapi.validation.FhirInstanceValidator()
    instanceValidator.setValidationSupport(
      new org.hl7.fhir.r4.hapi.validation.ValidationSupportChain(
        new org.hl7.fhir.r4.hapi.validation.CachingValidationSupport(new org.hl7.fhir.r4.hapi.ctx.DefaultProfileValidationSupport()),
        new OnFhirValidationSupportR4())
    )
    instanceValidator
  }

  /**
    * Return the version specific FHIR AuditEvent creator
    * @return
    */
  override def getAuditCreator(): IFhirAuditCreator = new R4AuditCreator()

  /**
    * Generate necessary configurations in memory (FhirConfig) that are used in FHIR API services
    *
    * @param conformance            Conformance statement for the FHIR repository
    * @param profiles               StructureDefinitions for the profiles (resources, extensions, data types)
    * @param searchParameters       Search Parameter definitions for extra search parameters supported
    * @param compartmentDefinitions CompartmentDefinitions for supported compartments
    * @param operationDefinitions   FHIR Operations supported in server
    */
  override def getPlatformConfigurations(conformance: CapabilityStatement, profiles: Seq[StructureDefinition], searchParameters: Seq[SearchParameter], compartmentDefinitions: Seq[CompartmentDefinition], valueSets: Seq[ValueSet], operationDefinitions: Seq[OperationDefinition], codeSystems: Seq[CodeSystem]): FhirConfig = {
    //Find the Rest definition for server
    val restDef: CapabilityStatement.CapabilityStatementRestComponent = findServerRestDefinition(conformance)
    val fhirConfig: FhirConfig = new FhirConfig(this.fhirContext)
    //Set system level interactions
    fhirConfig.supportedInteractions = extractSupportedInteractions(restDef)

    logger.info("Configuring supported FHIR resources ...")
    //Extract supported resources
    fhirConfig.supportedResources = extractSupportedResources(restDef) ++
      //Add the infrastructure resources if not exist in Conformance
      Set(FHIR_CONFORMANCE,
        FHIR_STRUCTURE_DEFINITION,
        FHIR_SEARCH_PARAMETER,
        FHIR_VALUE_SET,
        FHIR_COMPARTMENT_DEFINITION,
        FHIR_AUDIT_EVENT)
    logger.info(s"${fhirConfig.supportedResources.size} resources found ...")

    logger.info("Configuring supported FHIR profiles ...")
    //Extract supported profiles
    fhirConfig.supportedProfiles = extractSupportedProfiles(conformance, restDef, profiles)
    logger.info(s"${fhirConfig.supportedProfiles.size} profiles found ...")

    logger.info("Configuring FHIR API support for each supported FHIR Resource  ...")
    //Extract configurations for each profile
    fhirConfig.resourceConfigurations = extractProfileConfigurations(restDef)

    //TODO Check if CompartmentDefinitions are provided for all supported compartments
    //Read and parse the base search parameter definitions in STU3
    val baseSearchParameters: Seq[SearchParameter] = getBaseSearchParameters
    val baseStructureDefinitions: Seq[StructureDefinition] = getBaseStructureDefinitions
    val baseOperationDefinitions: Seq[OperationDefinition] = getBaseOperationDefinitions

    logger.info("Configuring supported FHIR compartments ...")
    //Extract the compartment configuration for supported compartments
    fhirConfig.compartmentRelations = extractCompartmentRelations(restDef, compartmentDefinitions)
    logger.info(s"${fhirConfig.compartmentRelations.size} compartments found ...")

    val supportedBaseProfiles = fhirConfig.resourceConfigurations.values.flatMap(_.profile).toSet
    val profilesAndDataTypes = profiles.filter(p => p.getKind.toCode != "resource" ||supportedBaseProfiles.contains(p.getUrl()))
    logger.info("Configuring FHIR API search parameters for each supported FHIR Resource ...")
    //Extract the resource query parameters supported
    fhirConfig.resourceQueryParameters = extractResourceQueryParameters(restDef, baseSearchParameters, searchParameters, baseStructureDefinitions, profilesAndDataTypes, fhirConfig.compartmentRelations)

    logger.info("Configuring FHIR API common search parameters ...")
    //Extract the common query parameters supported
    fhirConfig.commonQueryParameters = extractCommonQueryParameters(restDef, baseSearchParameters, baseStructureDefinitions)

    logger.info("Configuring FHIR API summary parameters ...")
    //Extract the summary parameters for each resource from the base definitions
    fhirConfig.summaryParameters = extractSummaryParameters(baseStructureDefinitions.filter(resourceType => fhirConfig.supportedResources.contains(resourceType.getIdElement.getIdPart)))

    logger.info("Configuring FHIR Operations ...")
    fhirConfig.supportedOperations = extractOperations(restDef, operationDefinitions, baseOperationDefinitions)
    logger.info(s"${fhirConfig.supportedOperations.size} operations found ...")

    logger.info("Configuring FHIR ValueSets ...")
    fhirConfig.supportedValueSets = extractSupportedValueSets(valueSets)

    logger.info("Configuring FHIR Codes Systems ...")
    fhirConfig.supportedCodeSystems = extractSupportedCodeSystems(codeSystems)

    logger.info("Configuring FHIR version specific configurations ...")
    //Set the names of result parameters
    fhirConfig.FHIR_RESULT_PARAMETERS = FHIR_RESULT_PARAMETERS
    //Set the names of special parameters
    fhirConfig.FHIR_SPECIAL_PARAMETERS = FHIR_SPECIAL_PARAMETERS
    // Set FHIR R4 JSON media types
    fhirConfig.FHIR_JSON_MEDIA_TYPES = Seq(
      MediaTypes.`application/json`,
      FHIR_JSON_MEDIA_TYPE
    )
    //Set FHIR R4 XML media types
    fhirConfig.FHIR_XML_MEDIA_TYPES = Seq(
      MediaTypes.`application/xml`,
      FHIR_XML_MEDIA_TYPE
    )
    //Set JSON Patch Media Type
    fhirConfig.FHIR_JSON_PATCH_MEDIA_TYPE = Some(MediaType.applicationWithOpenCharset("json-patch+json"))
    //Set FGIR _format parameter and MimeType map
    fhirConfig.FHIR_FORMAT_MIME_TYPE_MAP = Map(
      "html" -> MediaTypes.`text/html`,
      "text/html" -> MediaTypes.`text/html`,
      "application/json" -> MediaTypes.`application/json`,
      "application/xml" -> MediaTypes.`application/xml`,
      "application/fhir+json" -> FHIR_JSON_MEDIA_TYPE,
      "application/fhir+xml" -> FHIR_XML_MEDIA_TYPE,
      "json" -> FHIR_JSON_MEDIA_TYPE,
      "xml" -> FHIR_XML_MEDIA_TYPE,
      "text/xml" -> FHIR_XML_MEDIA_TYPE
    )
    //Set common search parameter
    fhirConfig.FHIR_DEFAULT_MEDIA_TYPE = FHIR_JSON_MEDIA_TYPE
    fhirConfig
  }

  override protected def extractSearchParametersFromBundle(bundle: Bundle): Seq[SearchParameter] = {
    bundle.getEntry.asScala.map(entry => {
      entry.getResource.asInstanceOf[SearchParameter]
    })
  }

  /**
    * Extract the StructureDefinitions from a definition Bundle
    *
    * @param bundle
    * @return
    */
  override protected def extractStructureDefinitionsFromBundle(bundle: Bundle): Seq[StructureDefinition] = {
    bundle.getEntry.asScala
      .filter(bundleEntry => bundleEntry.getResource.isInstanceOf[StructureDefinition])
      .map(bundleEntry => bundleEntry.getResource.asInstanceOf[StructureDefinition])
  }

  /**
    * Extract the OperationDefinitions from a definition Bundle
    *
    * @param bundle
    * @return
    */
  override protected def extractOperationDefinitionsFromBundle(bundle: Bundle): Seq[OperationDefinition] = {
    bundle.getEntry.asScala
      .filter(bundleEntry => bundleEntry.getResource.isInstanceOf[OperationDefinition])
      .map(bundleEntry => bundleEntry.getResource.asInstanceOf[OperationDefinition])
  }


  /**
    * Check and find if there is a Rest definition as server
    *
    * @param conformance
    * @return
    */
  private def findServerRestDefinition(conformance: CapabilityStatement): CapabilityStatement.CapabilityStatementRestComponent = {
    //Find a Rest service definition with mode "server"
    val serverRestDef: Option[CapabilityStatement.CapabilityStatementRestComponent] = conformance.getRest.asScala.find(restDef => {
      restDef.getMode.toCode.equals("server")
    })
    //Throw exception if not found
    if (serverRestDef.isEmpty) {
      throw new InitializationException(s"Invalid conformance statement; missing a rest service definition with mode 'server'")
    }
    serverRestDef.get
  }

  /**
    * Return system level interactions supported by the system
    *
    * @param restDef
    * @return
    */
  private def extractSupportedInteractions(restDef: CapabilityStatement.CapabilityStatementRestComponent): Set[String] = {
    restDef.getInteraction.asScala.map(_.getCode.toCode).toSet
  }

  /**
    * Return resources defined in the Rest Definition in Conformance Statement
    *
    * @param restDef
    * @return
    */
  private def extractSupportedResources(restDef: CapabilityStatement.CapabilityStatementRestComponent): Set[String] = {
    restDef.getResource.asScala.toList.map(_.getType).distinct.toSet
  }

  /**
    * Extract the supported profiles and generate Resource -> Set(supported profiles)
    *
    * @param conformance
    * @param restDef
    * @param profiles
    * @return
    */
  private def extractSupportedProfiles(conformance: CapabilityStatement, restDef: CapabilityStatement.CapabilityStatementRestComponent, profiles: Seq[StructureDefinition]): Map[String, Set[String]] = {
    //Extract the full URL of supported profiles (given in Conformance.profile)
    val allProfiles = restDef.getResource.asScala.map(resourceDef => {
      val baseProfile = Option(resourceDef.getProfile) flatMap {
        case fhirRoot if fhirRoot.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS) => None
        case relative if relative.startsWith(FHIR_STRUCTURE_DEFINITION) =>
          Some(OnfhirConfig.fhirDefinitionsUrl + "/" + relative)
        case oth => Some(oth)
      }

      val supportedProfiles = resourceDef.getSupportedProfile.asScala.map(sp => sp.getValueAsString).map {
        case relative if relative.startsWith(FHIR_STRUCTURE_DEFINITION) =>
          OnfhirConfig.fhirDefinitionsUrl + "/" + relative
        case oth => oth
      }

      resourceDef.getType -> (supportedProfiles ++ baseProfile.toSeq).toSet
    }).toMap

    //Construct a temporary map from profile definitions ( profileName -> resourceType)
    val temporaryProfileMap = profiles.map(profile => profile.getUrl -> profile.getType).toMap

    //Check if for all the mentioned profiles in Conformance, we have a structure definition provided
    // IMPORTANT: Currently all Structure Definitions (either it is profile defined elsewhere) should exist within profile folder
    // TODO Try to read definitions over network from the given URLs
    val undefinedProfiles = allProfiles.flatMap(_._2).toSet.diff(temporaryProfileMap.keySet)
    if (undefinedProfiles.nonEmpty) {
      logger.error(s"StructureDefinition(s) for $undefinedProfiles are not given in the configurations !!!")
      throw new InitializationException(s"Some StructureDefinition(s) are missing for some of the mentioned profiles in Conformance statement !!!")
    }

    allProfiles
  }

  /**
    * Extract the supported compartments
    *
    * @param restDef
    * @return
    */
  private def extractSupportedCompartments(restDef: CapabilityStatement.CapabilityStatementRestComponent): Set[String] = {
    restDef.getCompartment.asScala.map(compartment => compartment.getValue).toSet
  }

  /**
    * Extract the supported ValueSet URLs apart from the FHIR's own definitions
    *
    * @param valueSets
    * @return
    */
  private def extractSupportedValueSets(valueSets: Seq[ValueSet]): Set[String] = {
    valueSets.map(vs => vs.getUrl).filterNot(url => url.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS)).toSet
  }

  /**
    * Extract supported FHIR Code systems
    *
    * @param codeSystems
    * @return
    */
  private def extractSupportedCodeSystems(codeSystems: Seq[CodeSystem]): Set[String] = {
    codeSystems.map(cs => cs.getUrl).filterNot(url => url.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS) || url.startsWith(FHIR_URL_FOR_EXTERNAL_FHIR_CODE_SYSTEMS)).toSet
  }

  /**
    * Extract the profile configuration and generate ((Resource,Profile)-> ResourceConf())
    *
    * @param restDef
    * @return
    */
  private def extractProfileConfigurations(restDef: CapabilityStatement.CapabilityStatementRestComponent): Map[String, ResourceConf] = {
    restDef.getResource.asScala.toList.map(resource => {
      val resourceType = resource.getType
      val profile = Option(resource.getProfile).filterNot(p => p.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS))

      // getSearchInclude returns empty list even though the field is empty, converted to Option[None]
      val searchInclude: Option[List[String]] = resource.getSearchInclude.asScala.toList match {
        case List() => None
        case someList => Some(someList.map(_.getValue.split('.').last))
      }

      // getSearchRevInclude returns empty list even though the field is empty, converted to Option[None]
      val revSearchInclude: Option[List[String]] = resource.getSearchRevInclude.asScala.toList match {
        case List() => None
        case someList => Some(someList.map(_.getValue.replace('.', ':')))
      }

      val resourceConf =
        new ResourceConf(
          resourceType,
          profile,
          resource.getInteraction.asScala.toList.map(_.getCode.toCode), //Supported interactions
          Try(resource.getVersioning.toCode).getOrElse(OnfhirConfig.fhirDefaultVersioning), //Versioning support
          Try(resource.getReadHistoryElement.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultReadHistory), //vRead support
          Try(resource.getUpdateCreateElement.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultUpdateCreate), //if update can create resources
          Try(resource.getConditionalCreateElement.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultConditionalCreate),
          Try(resource.getConditionalUpdateElement.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultConditionalUpdate),
          Try(resource.getConditionalDelete.toCode).getOrElse(OnfhirConfig.fhirDefaultConditionalDelete),
          searchInclude,
          revSearchInclude
        )
      resourceType -> resourceConf
    }).toMap
  }

  /**
    * Go over the compartments supported given in Conformance and parse CompartmentDefinitions and
    * constructs the configuration to be used in query mechanism
    *
    * @param restDef                Conformance.rest definition for the server
    * @param compartmentDefinitions power2dm.compartment Definitions given
    * @return Map(compartment-name -> Map(resource-type -> List of search parameters)) e.g. Map(Patient -> Map(Observation -> ))
    */
  private def extractCompartmentRelations(restDef: CapabilityStatement.CapabilityStatementRestComponent, compartmentDefinitions: Seq[CompartmentDefinition]): Map[String, Map[String, Set[String]]] = {
    //Temporary map compartment-url -> CompartmentDefinition
    val compartmentMap: Map[String, CompartmentDefinition] = compartmentDefinitions.map(compartmentDef => compartmentDef.getUrl -> compartmentDef).toMap
    restDef.getCompartment.asScala.map(compartmentUrl => {
      logger.debug(s"Extracting compartment relations for $compartmentUrl...")
      val compartmentDef = compartmentMap.get(compartmentUrl.getValue)
      if (compartmentDef.isEmpty)
        throw new InitializationException(s"Compartment definition for ${compartmentUrl.getValue} is not found among the defined compartments ...")

      val parameterMapForResource: Map[String, Set[String]] = compartmentDef.get.getResource.asScala.map(compResource => {
        compResource.getCode -> compResource.getParam.asScala.map(_.getValue).toSet
      }).filter(_._2.nonEmpty).toMap
      //Construct the outer map for each compartment
      compartmentDef.get.getCode.toCode -> parameterMapForResource
    }).toMap
  }

  /**
    * Go over all supported common search parameters defined in Conformance statement (Conformance.rest.searchParam)
    * and create the configurations for them to be used within query mechanism
    *
    * @param restDef                  The rest definition for the FHIR server (Conformance.rest)
    * @param baseSearchParameters     SearchParameter definitions of base FHIR standard
    * @param baseStructureDefinitions Base FHIR Resource and DataType definitions
    * @return
    */
  private def extractCommonQueryParameters(restDef: CapabilityStatement.CapabilityStatementRestComponent,
                                           baseSearchParameters: Seq[SearchParameter],
                                           baseStructureDefinitions: Seq[StructureDefinition]
                                          ): Map[String, SearchParameterConf] = {
    //Temporary map for base parameters defined on Resource ->  (Param-name -> SearchParameter)
    val baseParamMap: Map[String, SearchParameter] =
      baseSearchParameters
        .flatMap(sp => {
          sp.getBase.asScala.toList.map(b => (b.getValue, sp.getCode, sp))
        })
        //.map(sp => (sp.getBase, sp.getCode, sp))
        .groupBy(_._1).map(rp => rp._1 -> rp._2.map(p => p._2 -> p._3).toMap)
        .filter(g => g._1 == "Resource" || g._1 == "DomainResource")
        .values.reduce((m1, m2) => m1 ++ m2)
    //Filter only Resource and Meta definitions that are related with common search parameters
    val baseDefinitions = baseStructureDefinitions.filter(bsd => bsd.getIdElement.getIdPart == FHIR_RESOURCE || bsd.getIdElement.getIdPart == FHIR_TYPES_META)
    if (baseDefinitions.length != 2)
      throw new InitializationException("Base Resource or Meta definition not found in supplied definition packages!!!")

    val baseDefinitionsMap: Map[String, Map[String, (String, Boolean)]] =
      constructElementPathMapForBase(baseDefinitions) ++ Map("Resource" -> Map("text" -> ("Narrative", false), "meta" -> ("Meta", false))) //This can be defined in DomainResource which is abstract

    val searchParameterConfigurator = new SearchParameterConfigurator("Resource", baseDefinitionsMap, Map.empty, Set.empty)

    val parameterConfigurations =
      restDef.getSearchParam.asScala
        .filterNot(searchParam => FHIR_RESULT_PARAMETERS.contains(searchParam.getName) || FHIR_SPECIAL_PARAMETERS.contains(searchParam.getName)) //Filter result parameters
        .flatMap(searchParam => {
          baseParamMap.get(searchParam.getName) match {
            case None => throw new InitializationException(s"The parameter definition for ${searchParam.getName} is not found in base definitions...")
            case Some(searchParameter) =>
              //Construct the parameter configuration for SearchParam definition
              searchParameterConfigurator.createSearchParameterConf(
                searchParameter.getName,
                searchParameter.getType.toCode,
                searchParameter.getXpath,
                searchParameter.getTarget.asScala.map(_.getValue),
                searchParameter.getModifier.asScala.map(_.getValue.toCode).toSet
              )
          }
      })
    //Convert it to map and return
    parameterConfigurations.map(parameterConf => parameterConf.pname -> parameterConf).toMap
  }


  /**
    * Go over all supported search parameters defined in Conformance statement and create the onFhir configurations to be
    * used within query mechanism
    * e.g. Observation -> Map(date -> ..., code -> ...)
    *
    * @param restDef                  The rest definition for the FHIR server (Conformance.rest)
    * @param baseSearchParameters     SearchParameter definitions of base FHIR standard
    * @param searchParameters         Extra SearchParameter definitions supplied to onFhir with configurations
    * @param baseStructureDefinitions Base FHIR Resource and DataType definitions
    * @param profiles                 Profiles supplied to onFhir with configurations
    * @param compartmentRelations     Compartments and their configurations supplied
    * @return
    */
  private def extractResourceQueryParameters(restDef: CapabilityStatement.CapabilityStatementRestComponent,
                                             baseSearchParameters: Seq[SearchParameter],
                                             searchParameters: Seq[SearchParameter],
                                             baseStructureDefinitions: Seq[StructureDefinition],
                                             profiles: Seq[StructureDefinition],
                                             compartmentRelations: Map[String, Map[String, Set[String]]]): Map[String, Map[String, SearchParameterConf]] = {
    //Temporary map for base parameters Resource-> (Param-name) -> SearchParameter)
    val baseParamMap: Map[String, Map[String, SearchParameter]] =
      baseSearchParameters
        .flatMap(sp => {
          sp.getBase.asScala.toList.map(b => (b.getValue, sp.getCode, sp))
        })
        //.map(sp => (sp.getBase, sp.getCode, sp))
        .groupBy(_._1).map(rp => rp._1 -> rp._2.map(p => p._2 -> p._3).toMap)

    //Temporary map for extra parameters (Param-url) -> SearchParameter
    val paramMap: Map[String, SearchParameter] = searchParameters.map(sp => sp.getUrl -> sp).toMap

    //Temporary map for Structure definitions (resource -> (element-path -> element-type, isArray)
    val baseDefinitionsMap: Map[String, Map[String, (String, Boolean)]] = constructElementPathMapForBase(baseStructureDefinitions)

    //Temporary map for profiles (resource -> (element-path -> element-type)
    val profilesMap: Map[String, Map[String, String]] = constructElementPathMapForProfiles(profiles)

    //For each Resource definition in Conformance.rest
    restDef.getResource.asScala.toList.map(resource => {
      logger.debug(s"Extracting search parameters of ${resource.getType}...")
      // Get the all parameter names defined in the Conformance
      val allParamNamesInDefinition = resource.getSearchParam.asScala.map(_.getName).toSet

      val searchParameterConfigurator = new SearchParameterConfigurator(resource.getType, baseDefinitionsMap, profilesMap, allParamNamesInDefinition)

      //For each SearchParam defined in the resource
      val searchParameterConfigurations: Seq[SearchParameterConf] = resource.getSearchParam.asScala.toList.flatMap(searchParam => {
        //Get the name of search parameter defined in Conformance
        val searchParameterName = FHIRUtil.transformSearchParameterName(searchParam.getName)
        //Check if parameter is defined in base or newly defined
        val searchParameter = searchParam.getDefinition match {
          case null =>
            Try(baseParamMap(resource.getType)(searchParameterName)).toOption
          //If the search parameter is defined in base specification
          case fhirUrl if fhirUrl.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS) =>
            Try(baseParamMap(resource.getType)(searchParameterName)).toOption
          //If the search parameter is defined within the scope of this repository
          case localUrl if localUrl.startsWith(OnfhirConfig.fhirDefinitionsUrl) =>
            paramMap.get(localUrl)
          case otherUrl =>
            throw new InitializationException(s"Currently we couldn't reach SearchParameter definitions from the given urls ($otherUrl). The feature is not implemented yet!!!")
        }

        if (searchParameter.isEmpty)
          throw new InitializationException(s"Parameter '$searchParameterName' not defined in base FHIR specification or not within supplied search parameter definitions for ${resource.getType}")

        val parameterType = searchParameter.get.getType.toCode
        //Construct the configurations for the defined Search Parameter
        searchParameterConfigurator.createSearchParameterConf(
          searchParameterName,
          parameterType,
          if (parameterType == FHIR_PARAMETER_TYPES.COMPOSITE) searchParameter.get.getExpression else searchParameter.get.getXpath, //For composites we put common element path expression
          if (parameterType == FHIR_PARAMETER_TYPES.COMPOSITE) searchParameter.get.getComponent.asScala.map(_.getDefinition) else searchParameter.get.getTarget.asScala.map(_.getValue), //For composites we put the definitions of other elements
          searchParameter.get.getModifier.asScala.map(_.getValue.toCode).toSet
        )
      })
      //Extract further search parameter configurations required to handle Compartments
      val furtherCompartmentParameters = extractCompartmentParametersForResource(
        resource.getType,
        searchParameterConfigurations.map(_.pname).toSet,
        compartmentRelations,
        baseParamMap.getOrElse(resource.getType, Map.empty),
        paramMap,
        searchParameterConfigurator
      )
      //Merge the resource specific parameters defined in Conformance.rest and further compartment parameters
      val allParameters = searchParameterConfigurations ++ furtherCompartmentParameters
      logger.debug(s"Search Parameters supported by the resource type ${resource.getType}: " + allParameters)
      //Construct the inner map (search-parameter-code -> SearchParameterConf)
      val parameterConfMapForResource = allParameters.map(searchParameterConf => searchParameterConf.pname -> searchParameterConf).toMap
      //Construct the outer map resource -> Map(search-parameter-code -> SearchParameterConf)
      resource.getType -> parameterConfMapForResource
    }).toMap
  }

  /**
    *
    * @param resourceType                Type of resource e.g. Observation
    * @param resourceParameterNames      All defined search parameters for the resource
    * @param compartmentRelations        Compartment relations extracted from Compartment Definitions
    * @param baseParamMap                search parameter map for base search parameter definitions
    * @param paramMap                    search parameter map
    * @param searchParameterConfigurator search param configurator object
    * @return
    */
  protected def extractCompartmentParametersForResource(resourceType: String,
                                                        resourceParameterNames: Set[String],
                                                        compartmentRelations: Map[String, Map[String, Set[String]]],
                                                        baseParamMap: Map[String, SearchParameter],
                                                        paramMap: Map[String, SearchParameter],
                                                        searchParameterConfigurator: SearchParameterConfigurator
                                                       ): Seq[SearchParameterConf] = {

    //For each compartment, extract further search parameters needed for the resource to handle compartment search
    val allCompartmentParamsForResource =
      compartmentRelations
        .map(compartment => compartment._2.getOrElse(resourceType, Set.empty))
        .fold(Set.empty[String])((s1, s2) => s1.union(s2))
    //Only find the parameters not defined for the resource
    val parameterNames = allCompartmentParamsForResource.diff(resourceParameterNames)

    parameterNames.toSeq.flatMap(paramName => {
      val searchParam: SearchParameter = baseParamMap.getOrElse(paramName, paramMap.getOrElse(paramName, null))
      if (searchParam == null)
        throw new InitializationException(s"Parameter $paramName is not defined for $resourceType!!! Compartment definitions requires this search parameter for the resource..")
      searchParameterConfigurator.createSearchParameterConf(
        searchParam.getCode,
        searchParam.getType.toCode,
        searchParam.getXpath,
        searchParam.getTarget.asScala.map(_.getValue),
        Set.empty[String]
      )
    })
  }


  /**
    * Construct a map for each base resource for their element paths
    * (Resource ->
    * elem1Path -> (element-type, isArray)
    * elem2Path -> (element-type, isArray)
    *
    * @param baseStructureDefinitions Base FHIR standard definitions
    * @return
    */
  private def constructElementPathMapForBase(baseStructureDefinitions: Seq[StructureDefinition]): Map[String, Map[String, (String, Boolean)]] = {
    baseStructureDefinitions.map(structureDefinition => {
      val resourceType = structureDefinition.getIdElement.getIdPart
      //Prepare initial path map; [path] -> targetType, IsArray
      val elementPathTypeMap: Map[String, (String, Boolean)] =
        structureDefinition.getSnapshot.getElement.asScala
          .flatMap(elemDef => {
            //Remove the type from the beginning of the path; "Observation.id" -> "id"
            var path = elemDef.getPath.replace(resourceType + ".", "")
            val types = elemDef.getType.asScala.map(_.getCode)

            val isArray = elemDef.getMax != "1"
            //If ends with an [x], add all the paths e.g. Observation.component.value[x]
            if (path.endsWith("[x]")) {
              path = path.replace("[x]", "")
              types.map(typ => (path + typ.capitalize) -> (typ, isArray))
            } else {
              Seq(path -> (types.headOption.getOrElse("Resource"), isArray))
            }
          }).toMap

      resourceType -> elementPathTypeMap
    }).toMap
  }

  /**
    * Construct a map for each profile for their element path
    *
    * @param profiles StructureDefinitions for defined profiles
    * @return
    */
  private def constructElementPathMapForProfiles(profiles: Seq[StructureDefinition]): Map[String, Map[String, String]] = {
    profiles.map(profile => {
      val resourceType = profile.getBaseDefinition.split("/").last

      var elementDefinitions = profile.getSnapshot.getElement.asScala
      if (elementDefinitions.isEmpty)
        elementDefinitions = profile.getDifferential.getElement.asScala

      val elementPathTypeMap: Map[String, String] =
        elementDefinitions
          .flatMap(elemDef => {
            //Remove the type from the beginning of the path; "Observation.id" -> "id"
            var path = elemDef.getPath.replace(resourceType + ".", "")
            val types = elemDef.getType.asScala.map(_.getCode)
            //If ends with an [x], add all the paths e.g. Observation.component.value[x]
            if (path.endsWith("[x]")) {
              path = path.replace("[x]", "")
              types.map(typ => (path + typ.capitalize) -> typ)
            } else {
              Seq(path -> types.headOption.getOrElse("Resource"))
            }
          }).toMap
      resourceType -> elementPathTypeMap
    }).toMap
  }

  /**
    * Parse the Structure Definitions and extract the summary parameter paths for each resource
    * !!! IMPORTANT: Only use for Base Standard Definitions for now*
    *
    * @param structureDefinitions
    * @return
    */
  private def extractSummaryParameters(structureDefinitions: Seq[StructureDefinition]): Map[String, Set[String]] = {
    structureDefinitions.map(structureDefinition => {
      logger.debug(s"Extracting summary parameters for ${structureDefinition.getName} ...")
      //If there is a snapshot definition go with that, else go with differential
      val elementDefinitions = Option(structureDefinition.getSnapshot.getElement.asScala)
        .getOrElse(structureDefinition.getDifferential().getElement.asScala)
      //Find isSummary elements and list their paths
      val summaryPaths: Set[String] = elementDefinitions.tail.flatMap(elementDef => {
        Try(elementDef.getIsSummary.booleanValue()).toOption.getOrElse(false) match {
          case true =>
            //Remove the resource name from the path e.g. Observation.code -> code
            val elementPath = elementDef.getPath.dropWhile(_ != '.').tail
            //If path is generic (e.g. Observation.effective[x])
            if (elementPath.contains("[x]")) {
              //Get the possible types, make the first letter capitilize and append it to the path
              //e.g. Observation.effectiveDat
              elementDef.getType.asScala.map(etype => {
                elementPath.replace("[x]", "") + etype.getCode.capitalize
              })
            } else {
              Seq(elementPath)
            }
          case false => Nil
        }
      }).toSet
      //Construct the map for each resource
      structureDefinition.getName -> summaryPaths
    }).toMap
  }

  /**
    * Extract Operation configurations from Operation Definitions
    *
    * @param restDef
    * @param operationDefinitions
    * @param baseOperationDefinitions
    * @return
    */
  def extractOperations(restDef: CapabilityStatement.CapabilityStatementRestComponent, operationDefinitions: Seq[OperationDefinition], baseOperationDefinitions: Seq[OperationDefinition]): Seq[OperationConf] = {
    //Temporary map Operation Definition URL (the last element)
    val operationDefinitionMap: Map[String, OperationDefinition] = operationDefinitions.map(opDef => opDef.getUrl -> opDef).toMap
    val baseOperationDefinitionMap: Map[String, OperationDefinition] = baseOperationDefinitions.map(opDef => opDef.getUrl -> opDef).toMap

    val typeLevelOperations = restDef.getResource.asScala.flatMap(r => r.getOperation.asScala)
    val systemLevelOperations = restDef.getOperation.asScala

    (typeLevelOperations ++ systemLevelOperations).map(operationDef => {
      val operationName = operationDef.getName
      val operationDefinitionUrl = operationDef.getDefinition

      var operationDefinition = operationDefinitionMap.get(operationDefinitionUrl)
      //If it does not a provided definition, we will try from FHIR operation definitions
      if (operationDefinition.isEmpty)
        operationDefinition = baseOperationDefinitionMap.get(operationDefinitionUrl)
      //If we still not find it, throw exception
      if (operationDefinition.isEmpty)
        throw new InitializationException(s"Operation definition for ${operationName} with URL (${operationDefinitionUrl}) stated in CapabilityStatement is not found among the defined operation definitions or base FHIR operation definitions ...")

      //Supported levels for operation
      val levels = Seq(
        if (operationDefinition.get.getSystem) Some("system") else None,
        if (operationDefinition.get.getType) Some("type") else None,
        if (operationDefinition.get.getInstance) Some("instance") else None
      ).flatten
      //Kind of operation
      val kind = operationDefinition.get.getKind.toCode

      //Operation parameter definition parser
      def parseParamDefinition(params: List[OperationDefinition.OperationDefinitionParameterComponent]): Seq[OperationParamDef] = {
        params.map(paramDef => {
          OperationParamDef(
            name = paramDef.getName,
            min = paramDef.getMin,
            max = paramDef.getMax,
            pType = kind match {
              case "operation" => paramDef.getType
              case "query" => paramDef.getSearchType.toCode
            },
            //TODO now R4 can define multiple profiles on parameters
            pProfile = paramDef.getTargetProfile.asScala.map(_.getValue),
            parts = parseParamDefinition(paramDef.getPart.asScala.toList),
            binding = if(paramDef.getBinding.getStrength == BindingStrength.REQUIRED) Some(paramDef.getBinding.getValueSet) else None
          )
        })
      }

      //Divide the parameters into input and output
      val inputParams = operationDefinition.get.getParameter.asScala.filter(_.getUse.toCode == "in")
      val outputParams = operationDefinition.get.getParameter.asScala.filter(_.getUse.toCode == "out")

      //Find the class implementing the operation
      val classPath = if (operationDefinitionMap.isDefinedAt(operationDefinitionUrl)) Option(operationDefinition.get.getName) else DEFAULT_IMPLEMENTED_FHIR_OPERATIONS.get(operationDefinition.get.getCode)
      if (classPath.isEmpty)
        throw new InitializationException(s"Operation ${operationName} defined in URL (${operationDefinitionUrl}) is not implemented yet, class path not found!!")

      if (loadOperationClass(classPath.get).isEmpty)
        throw new InitializationException(s"Implementation for operation ${operationName} defined in URL (${operationDefinitionUrl}) not found in classPath: ${classPath.get}!!")

      OperationConf(
        name = operationDefinition.get.getCode,
        classPath = classPath.get,
        kind = operationDefinition.get.getKind.toCode,
        levels = levels.toSet,
        resources = operationDefinition.get.getResource.asScala.map(_.getValue),
        inputParams = parseParamDefinition(inputParams.toList),
        outputParams = parseParamDefinition(outputParams.toList)
      )
    })
  }


}

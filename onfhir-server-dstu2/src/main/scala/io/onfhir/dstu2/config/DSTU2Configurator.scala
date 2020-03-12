package io.onfhir.dstu2.config

import akka.http.scaladsl.model.{HttpCharsets, MediaType, MediaTypes}
import ca.uhn.fhir.context.FhirContext
import ca.uhn.fhir.model.dstu2.resource._
import ca.uhn.fhir.parser.IParser
import ca.uhn.fhir.validation.IValidatorModule
import io.onfhir.api._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.config._
import io.onfhir.exception.InitializationException
import io.onfhir.stu3.audit.STU3AuditCreator
import org.hl7.fhir.dstu3.model.{CodeSystem, CompartmentDefinition}

import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Created by tuncay on 11/14/2016.
  * Class that configures the system and FHIR API based on the DSTU2
  * Fills the FhirConfig object with the extracted configurations
  */
class DSTU2Configurator extends AbstractFhirConfigurator[Conformance, StructureDefinition, SearchParameter, CompartmentDefinition, ValueSet, Bundle, OperationDefinition, CodeSystem](FhirContext.forDstu2()) {
  /**
    * Resource names in DSTU2 for infrastructure resources
    */
  override def FHIR_CONFORMANCE = "Conformance"

  val FHIR_JSON_MEDIA_TYPE = MediaType.applicationWithFixedCharset("json+fhir", HttpCharsets.`UTF-8`).withComp(MediaType.Compressible)
  val FHIR_XML_MEDIA_TYPE = MediaType.applicationWithFixedCharset("xml+fhir", HttpCharsets.`UTF-8`).withComp(MediaType.Compressible)

  val FHIR_RESULT_PARAMETERS = Seq(
    FHIR_SEARCH_RESULT_PARAMETERS.SORT,
    FHIR_SEARCH_RESULT_PARAMETERS.COUNT,
    FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY,
    FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS,
    FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE,
    FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE,
    FHIR_SEARCH_RESULT_PARAMETERS.PAGE,
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

  //In order to support compartments, code systems and audits for DSTU2
  override def getXmlParser(rtype:String = "Bundle"):IParser = rtype match {
    case "AuditEvent" | "CompartmentDefinition" | "CodeSystem" =>
      FhirContext.forDstu3().newXmlParser().setStripVersionsFromReferences(false)
    case  _ => super.getXmlParser(rtype)
  }

  override def getJsonParser(rtype:String = "Bundle"):IParser = rtype match {
    case "AuditEvent" | "CompartmentDefinition" | "CodeSystem" =>
      FhirContext.forDstu3().newJsonParser().setStripVersionsFromReferences(false)
    case  _ => super.getJsonParser(rtype)
  }


  override def getValidatorModule():IValidatorModule = {
    val instanceValidator = new org.hl7.fhir.instance.hapi.validation.FhirInstanceValidator()
    instanceValidator.setValidationSupport(new org.hl7.fhir.instance.hapi.validation.CachingValidationSupport(new org.hl7.fhir.instance.hapi.validation.DefaultProfileValidationSupport()))
    instanceValidator
  }
  /**
    * Configuration of the platform and FHIR API based on DSTU2 infrastructure resource definitions
    * @param conformance
    * @param profiles
    * @param searchParameters
    * @param compartmentDefinitions
    */
  override def getPlatformConfigurations(conformance:Conformance,
                                          profiles:Seq[StructureDefinition],
                                          searchParameters:Seq[SearchParameter],
                                          compartmentDefinitions: Seq[CompartmentDefinition],
                                          valueSets:Seq[ValueSet],
                                          operationDefinitions:Seq[OperationDefinition],
                                          codeSystems:Seq[CodeSystem]):FhirConfig = {
    //Find the Rest definition for server
    val restDef:Conformance.Rest = findServerRestDefinition(conformance)
    val fhirConfig: FhirConfig = new FhirConfig(this.fhirContext)
    logger.info("Configuring supported FHIR resources ...")
    // Extract supported interactions
    fhirConfig.supportedInteractions = extractSupportedInteractions(restDef)
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
    fhirConfig.profileConfigurations = extractProfileConfigurations(restDef)

    //TODO Check if CompartmentDefinitions are provided for all supported compartments
    //Read and parse the base search parameter definitions in DSTU2
    val baseSearchParameters:Seq[SearchParameter] = getBaseSearchParameters
    val baseStructureDefinitions:Seq[StructureDefinition] = getBaseStructureDefinitions
    val baseOperationDefinitions:Seq[OperationDefinition] = getBaseOperationDefinitions

    logger.info("Configuring supported FHIR compartments ...")
    //Extract the compartment configuration for supported compartments
    fhirConfig.compartmentRelations = extractCompartmentRelations(restDef, compartmentDefinitions)
    logger.info(s"${fhirConfig.compartmentRelations.size} compartments found ...")

    logger.info("Configuring FHIR API search parameters for each supported FHIR Resource ...")
    //Extract the resource query parameters supported
    fhirConfig.resourceQueryParameters = extractResourceQueryParameters(restDef, baseSearchParameters, searchParameters, baseStructureDefinitions, profiles, fhirConfig.compartmentRelations)

    logger.info("Configuring FHIR API common search parameters ...")
    //Extract the common query parameters supported
    fhirConfig.commonQueryParameters = extractCommonQueryParameters(restDef, baseSearchParameters, baseStructureDefinitions)

    logger.info("Configuring FHIR API summary parameters ...")
    //Extract the summary parameters for each resource from the base definitions
    fhirConfig.summaryParameters = extractSummaryParameters(baseStructureDefinitions.filter(resourceType => fhirConfig.supportedResources.contains(resourceType.getId.getIdPart)))

    //logger.info("Configuring FHIR base patient related resources ...")
    //FhirConfig.basePatientResources = extractBasePatientResources(baseStructureDefinitions, FhirConfig.supportedResources, baseSearchParameters)

    logger.info("Configuring FHIR operations ...")
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
    fhirConfig.FHIR_JSON_MEDIA_TYPES =Seq(
      FHIR_JSON_MEDIA_TYPE,
      MediaTypes.`application/json`
    )
    //Set FHIR R4 XML media types
    fhirConfig.FHIR_XML_MEDIA_TYPES =  Seq(
      FHIR_XML_MEDIA_TYPE,
      MediaTypes.`application/xml`
    )
    //Set JSON Patch Media Type
    fhirConfig.FHIR_JSON_PATCH_MEDIA_TYPE = None
    //Set FGIR _format parameter and MimeType map
    fhirConfig.FHIR_FORMAT_MIME_TYPE_MAP =  Map(
      "html" -> MediaTypes.`text/html`,
      "text/html" -> MediaTypes.`text/html`,
      "application/json"-> MediaTypes.`application/json`,
      "application/xml"-> MediaTypes.`application/xml`,
      "application/json+fhir" -> FHIR_JSON_MEDIA_TYPE,
      "application/xml+fhir" -> FHIR_XML_MEDIA_TYPE,
      "json" -> FHIR_JSON_MEDIA_TYPE,
      "xml" -> FHIR_XML_MEDIA_TYPE,
      "text/xml" -> FHIR_XML_MEDIA_TYPE)
    //Set common search parameter
    fhirConfig.FHIR_DEFAULT_MEDIA_TYPE = FHIR_JSON_MEDIA_TYPE
    //Set summarization indicator
    fhirConfig.FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM = "http://hl7.org/fhir/v3/ObservationValue"

    fhirConfig
  }

  /**
    * Extract SearchParameters from bundle
    * @param bundle
    * @return
    */
  def extractSearchParametersFromBundle(bundle:Bundle):Seq[SearchParameter] = {
    bundle.getEntry.asScala.toList.map( entry => {
      entry.getResource.asInstanceOf[SearchParameter]
    })
  }

  /**
    * Extract StructureDefinitions from bundle
    * @param bundle
    * @return
    */
  def extractStructureDefinitionsFromBundle(bundle:Bundle):Seq[StructureDefinition] = {
    bundle.getEntry.asScala.toList
      .filter(bundleEntry => bundleEntry.getResource.isInstanceOf[StructureDefinition])
      .map(bundleEntry => bundleEntry.getResource.asInstanceOf[StructureDefinition])
  }

  /**
    * Extract operation Definitions from bundle
    * @param bundle
    * @return
    */
  def extractOperationDefinitionsFromBundle(bundle:Bundle):Seq[OperationDefinition] = {
    bundle.getEntry.asScala.toList
      .filter(bundleEntry => bundleEntry.getResource.isInstanceOf[OperationDefinition])
      .map(bundleEntry => bundleEntry.getResource.asInstanceOf[OperationDefinition])
  }

  protected def extractValueSetsFromBundle(bundle: Bundle):Seq[ValueSet] = {
    bundle.getEntry.asScala
      .filter(bundleEntry => bundleEntry.getResource.isInstanceOf[ValueSet])
      .map(bundleEntry => bundleEntry.getResource.asInstanceOf[ValueSet])
  }

  protected def extractCodeSystemsFromBundle(bundle:Bundle):Seq[CodeSystem] = {
    bundle.getEntry.asScala
      .filter(bundleEntry => bundleEntry.getResource.isInstanceOf[CodeSystem])
      .map(bundleEntry => bundleEntry.getResource.asInstanceOf[CodeSystem])
  }


  /**
    * Check and find if there is a Rest definition as server
    * @param conformance
    * @return
    */
  private def findServerRestDefinition(conformance:Conformance):Conformance.Rest = {
    //Find a Rest service definition with mode "server"
    val serverRestDef:Option[Conformance.Rest] = conformance.getRest.asScala.find( restDef => {
      restDef.getMode.equals("server")
    })
    //Throw exception if not found
    if(serverRestDef.isEmpty){
      throw new InitializationException(s"Invalid conformance statement; missing a rest service definition with mode 'server'")
    }
    serverRestDef.get
  }

  /**
    * Return system level interactions supported by the system
    * @param restDef
    * @return
    */
  private def extractSupportedInteractions(restDef:Conformance.Rest):Set[String] = {
    val transactionSet = restDef.getTransactionMode match {
      case null | "not-supported" => Set.empty
      case "both" => Set("transaction", "batch")
      case _ => Set(restDef.getTransactionMode)
    }
    restDef.getInteraction.asScala.map(_.getCode).toSet ++ transactionSet
  }

  /**
    * Return resources defined in the Rest Definition in Conformance Statement
    * @param restDef
    * @return
    */
  private def extractSupportedResources(restDef:Conformance.Rest):Set[String] = {
    restDef.getResource.asScala.toList.map(_.getType).distinct.toSet
  }

  /**
    * Extract the supported ValueSet URLs apart from the FHIR's own definitions
    * @param valueSets
    * @return
    */
  private def extractSupportedValueSets(valueSets:Seq[ValueSet]):Set[String] = {
    valueSets.map(vs => vs.getUrl).filterNot(url => url.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS)).toSet
  }

  /**
    * Extract supported FHIR Code systems
    * @param codeSystems
    * @return
    */
  private def extractSupportedCodeSystems(codeSystems:Seq[CodeSystem]):Set[String] = {
    codeSystems.map(cs => cs.getUrl).filterNot(url => url.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS) || url.startsWith(FHIR_URL_FOR_EXTERNAL_FHIR_CODE_SYSTEMS)).toSet
  }

  /**
    * Extract the supported profiles and generate Resource -> Set(supported profiles)
    * @param conformance
    * @param restDef
    * @param profiles
    * @return
    */
  private def extractSupportedProfiles(conformance:Conformance, restDef:Conformance.Rest, profiles:Seq[StructureDefinition]): Map[String, Set[String]] = {
    //Extract the full URL of supported profiles (given in Conformance.profile)
    val profilesForUseCases:Set[String] = conformance.getProfile.asScala.map(profileReference => {
      profileReference.getReference.getBaseUrl match {
        //If it is relative URL, then it means the profile is newly defined and Structure Definition have to exist in profile folder
        case "" | null =>
          OnfhirConfig.fhirDefinitionsUrl + "/" + profileReference.getReference.getResourceType + "/" + profileReference.getReference.getIdPart
        //Otherwise get the full url (a 3rd party profile is supported)
        case _ =>
          profileReference.getReference.getValue
      }
    }).toSet

    //Extract the URLs of supported base profiles e.g. resource type -> profile
    val baseProfileMap:Map[String, Option[String]] = restDef.getResource.asScala.map(resourceDef  => {
      if(resourceDef.getProfile.isEmpty)
        resourceDef.getType -> None
      else {
        val url = resourceDef.getProfile.getReference.getBaseUrl match {
          //If it is relative URL, then it means the profile is newly defined and will exist in profile folder
          case "" | null =>
            Some(OnfhirConfig.fhirDefinitionsUrl + "/" + resourceDef.getProfile.getReference.getResourceType + "/" + resourceDef.getProfile.getReference.getIdPart)
          //If the base URL is FHIR, it means base specification so skip them
          case FHIR_ROOT_URL_FOR_DEFINITIONS => None
          //Otherwise get the full url
          case _ =>
            Some(resourceDef.getProfile.getReference.getValue)
        }
        resourceDef.getType -> url
      }
    }).toMap

    //Construct a temporary map from profile definitions ( profileName -> resourceType)
    val temporaryProfileMap = profiles.map(profile => profile.getUrl -> profile.getConstrainedType).toMap

    //Check if for all the mentioned profiles in Conformance, we have a structure definition provided
    // IMPORTANT: Currently all Structure Definitions (either it is profile defined elsewhere) should exist within profile folder
    // TODO Try to read definitions over network from the given URLs
    val undefinedProfiles = profilesForUseCases.union(baseProfileMap.values.filter(_.isDefined).map(_.get).toSet).diff(temporaryProfileMap.keySet)
    if(undefinedProfiles.nonEmpty){
      logger.error(s"StructureDefinition(s) for $undefinedProfiles are not given in the configurations !!!")
      throw new InitializationException(s"Some StructureDefinition(s) are missing for some of the mentioned profiles in Conformance statement !!!")
    }
    //Construct the map for supported profiles for different use cases  (given in Conformance.profile)
    val profileMapForUseCases:Map[String, Set[String]] = profilesForUseCases.map(profileName => temporaryProfileMap(profileName) -> profileName)
      .groupBy(_._1).map(resourceProfileTuple => resourceProfileTuple._1 -> resourceProfileTuple._2.map(_._2))

    //Merge and return the map
    baseProfileMap.map(bpm => {
      val useCaseProfileSet:Set[String] = profileMapForUseCases.getOrElse(bpm._1, Set.empty[String])
      if(bpm._2.isDefined)
        bpm._1 -> useCaseProfileSet.union(Set(bpm._2.get))
      else
        bpm._1 -> useCaseProfileSet
    })
  }

  /**
    * Extract the supported compartments
    * @param restDef
    * @return
    */
  private def extractSupportedCompartments(restDef:Conformance.Rest):Set[String] = {
    restDef.getCompartment.asScala.toList.map(compartment => compartment.getValue).toSet
  }

  /**
    * Extract the profile configuration and generate ((Resource,Profile)-> ResourceConf())
    * @param restDef
    * @return
    */
  private def extractProfileConfigurations(restDef:Conformance.Rest):Map[String, ResourceConf] = {
    restDef.getResource.asScala.toList.map(resource => {
      val resourceType = resource.getType
      val profile = Option(resource.getProfile.getReference.getValue)

      // getSearchInclude returns empty list even though the field is empty, converted to Option[None]
      val searchInclude:Option[List[String]] = resource.getSearchInclude.asScala.toList match {
        case List() => None
        case someList => Some(someList.map(_.getValue.split('.').last))
      }

      // getSearchRevInclude returns empty list even though the field is empty, converted to Option[None]
      val revSearchInclude:Option[List[String]] = resource.getSearchRevInclude.asScala.toList match {
        case List() => None
        case someList => Some(someList.map(_.getValue.replace('.', ':')))
      }

      val resourceConf =
        ResourceConf(
          resourceType,
          profile,
          resource.getInteraction.asScala.toList.map(_.getCode), //Supported interactions
          Option(resource.getVersioning).getOrElse(OnfhirConfig.fhirDefaultVersioning), //Versioning support
          Try(resource.getReadHistory.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultReadHistory), //vRead support
          Try(resource.getUpdateCreate.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultUpdateCreate), //if update can create resources
          Try(resource.getConditionalCreate.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultConditionalCreate),
          Try(resource.getConditionalUpdate.booleanValue()).getOrElse(OnfhirConfig.fhirDefaultConditionalUpdate),
          Option(resource.getConditionalDelete).getOrElse(OnfhirConfig.fhirDefaultConditionalDelete),
          searchInclude,
          revSearchInclude
        )
      resourceType -> resourceConf
    }).toMap
  }

  /**
    * Go over all supported search parameters defined in Conformance statement and create the configurations to be
    * used within query mechanism
    * e.g.
    *    Map ((fhir-resource, profile) -> Map(parameter-name -> SearchParameterConf)
    * @param restDef The rest definition for the FHIR server (Conformance.rest)
    * @param baseSearchParameters - SearchParameter definitions of in base FHIR standard
    * @param searchParameters - Extra SearchParameter definitions
    * @return
    */
  private def extractResourceQueryParameters(restDef:Conformance.Rest,
                                             baseSearchParameters:Seq[SearchParameter],
                                             searchParameters:Seq[SearchParameter],
                                             baseStructureDefinitions:Seq[StructureDefinition],
                                             profiles:Seq[StructureDefinition],
                                             compartmentRelations:Map[String, Map[String, Set[String]]]) : Map[String,  Map[String, SearchParameterConf]] = {
    //Temporary map for base parameters Resource-> (Param-name) -> SearchParameter)
    val baseParamMap:Map[String, Map[String, SearchParameter]] =
      baseSearchParameters
        .map(sp => (sp.getBase, sp.getCode, sp))
        .groupBy(_._1).map( rp => rp._1 -> rp._2.map( p => p._2 -> p._3).toMap)

    //Temporary map for extra parameters (Param-url) -> SearchParameter
    val paramMap:Map[String, SearchParameter] = searchParameters.map(sp => sp.getUrl -> sp).toMap

    //Temporary map for Structure definitions (resource -> (element-path -> element-types)
    val baseDefinitionsMap:Map[String, Map[String, (String, Boolean)]] = constructElementPathMapForBase(baseStructureDefinitions)

    //Temporary map for profiles (resource -> (element-path -> element-types)
    val profilesMap:Map[String, Map[String, String]] = constructElementPathMapForProfiles(profiles)

    //For each Resource definition in Conformance.rest
    restDef.getResource.asScala.toList.map(resource => {
      logger.debug(s"Extracting search parameters of ${resource.getType}...")
       // Get the all parameter names defined in the Conformance
       val allParamNamesInDefinition = resource.getSearchParam.asScala.map(_.getName).toSet

       val searchParameterConfigurator = new SearchParameterConfigurator(resource.getType, baseDefinitionsMap, profilesMap, allParamNamesInDefinition)

      //For each SearchParam defined in the resource
       val searchParameterConfigurations:Seq[SearchParameterConf] = resource.getSearchParam.asScala.toList.flatMap(searchParam => {
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
         if(searchParameter.isEmpty)
           throw new InitializationException(s"Parameter '$searchParameterName' not defined in base FHIR specification or not within supplied search parameter definitions for ${resource.getType}")

         searchParameter.get.getType match {
           case FHIR_PARAMETER_TYPES.COMPOSITE =>
             handleCompositeParameter(searchParameterName, searchParam.getType, searchParameter.get.getTarget.asScala.map(_.getValue).toSet, searchParam.getModifier.asScala.map(_.getValue).toSet, allParamNamesInDefinition.diff(Set(searchParameterName)))
           case parameterType =>
             //Construct the configurations for the defined Search Parameter
             searchParameterConfigurator.createSearchParameterConf(
               searchParameterName,
               parameterType,
               searchParameter.get.getXpath, //For composites we put common element path expression
               searchParameter.get.getTarget.asScala.map(_.getValue),
               searchParam.getModifier.asScala.map(_.getValue).toSet,
             )
         }
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
       logger.debug(""+allParameters)
       //Construct the inner map (search-parameter-code -> SearchParameterConf)
       val parameterConfMapForResource = allParameters.map( searchParameterConf => searchParameterConf.pname -> searchParameterConf).toMap
      //Construct the outer map resource -> Map(search-parameter-code -> SearchParameterConf)
      resource.getType -> parameterConfMapForResource
    }).toMap
  }

  /**
    * Returns a list of search parameter configuration by parsing the Xpath and converting it to JSON path
    * e.g. (Case 0): 'related' composite parameter in Observation which refers related-target and related-type
    *      --> 1 SearchParameterConf with path --> Seq(related-target, related-type)
    * e.g. (Case 2): 'code-value-[x]' composite parameter in Observation which refers code-value-quantity, code-value-concept, etc
    *      --> Seq(SearchParameterConf(code-value-quantity), SearchParameterConf(code-value-concept), etc )
    *      where each configuration has such following path --> Seq (code, value-quantity)
    * e.g. (Case 3): 'component-code-value-[x]' composite parameter in Observation
    *      --> Seq(SearchParameterConf(component-code-value-quantity), ...)
    *      where each configuration has such following path --> Seq(component-code, component-value-quantity, component-value-concept, etc)
    * @param pname Name of the FHIR search parameter
    * @param ptype Type of the FHIR search parameter (e.g. composite, token, etc)
    * @param targets If type is reference, the resource names this reference refers
    * @param modifiers Supported modifiers for this parameter
    * @param otherParameters Names of other parameters that is supported for the same resource
    * @return
    */
  private def handleCompositeParameter(pname:String,  ptype:String, targets:Set[String], modifiers:Set[String],  otherParameters:Set[String]):Seq[SearchParameterConf] = {
    pname.count(_ == '-') match {
      // Composite parameter refers to 2 values with the common prefix
      // e.g. Within Observation "related" parameter combines related-target and related-type
      case 0 =>
        // Get parameters with the common prefix and put it to paths
        Seq(SearchParameterConf(pname, ptype, Seq(""), otherParameters.filter(_.startsWith(pname)).toSeq, modifiers, Seq("Resource")))
      //Composite parameter refers to 2 values combined e.g. characteristic-value in Group
      case 1 =>
        Seq(SearchParameterConf(pname, ptype, Seq(""), pname.split("-"), modifiers, Seq("Resource")))
      // Composite parameter refers to combination of one parameter with multiple parameters
      // e.g. Withing Observation (code-value[x] -> (code, value[x]))
      case 2 =>
        // Split parameter name for generating required fields
        val compositeElements = pname.split("-")
        // Parameter that loops (e.g. value[x])
        val compositeElementLoop = compositeElements.tail.head
        // Stable parameter(e.g. code)
        val compositeElement = compositeElements.head
        // Construct a composite SearchParameter for each possible alternative (e.g. code-value-date, code-value-quantity)
        otherParameters.toSeq.filter(_.startsWith(compositeElementLoop + "-")).map( otherParam => {
          SearchParameterConf(compositeElement + "-" + otherParam, FHIR_PARAMETER_TYPES.COMPOSITE, Seq(""), Seq(compositeElement, otherParam), Set.empty, Seq("Resource"))
        })
      // Composite parameter refers to combination of one parameter with multiple parameters that has same prefix(e.g. component-code-value[x] -> (component-code, component-value[x]))
      case 3 =>
        // Split parameter name for generating required fields
        val compositeElements = pname.split("-")
        // Combine prefix with looping parameter(e.g. component(prefix)-value)
        val compositeElementLoop = compositeElements.head + "-" + compositeElements.dropRight(1).last
        // Combine prefix with stable parameter(e.g. component-code)
        val compositeElement = compositeElements.head + "-" + compositeElements.tail.head
        // For each parameter that starts with loop parameter, generate composite pair
        otherParameters.toSeq.filter(_.startsWith(compositeElementLoop + "-")).map(otherParam => {
          // Generate composite parameter name by trimming the loop parameter(e.g.component-code + '-' + (component-value-quantity -> value-quantity))
          val generateParameterName = compositeElement + "-" + otherParam.dropWhile(_ != '-').tail
          SearchParameterConf(generateParameterName, FHIR_PARAMETER_TYPES.COMPOSITE, Seq(""), Seq(compositeElement, otherParam), Set.empty, Seq("Resource"))
        })
    }
  }

  /**
    *
    * @param resourceType Type of resource e.g. Observation
    * @param resourceParameterNames All defined search parameters for the resource
    * @param compartmentRelations Compartment relations extracted from Compartment Definitions
    * @param baseDefinitionPathTypeMap Path -> Element Type map for each resource in base definition
    * @param profilePathTypeMap Path -> Element Type map for each profile
    * @param baseParamMap search parameter map for base search parameter definitions
    * @param paramMap search parameter map
    * @return
    */
  protected def extractCompartmentParametersForResource(resourceType:String,
                                                        resourceParameterNames:Set[String],
                                                        compartmentRelations:Map[String, Map[String, Set[String]]],
                                                        baseParamMap: Map[String, SearchParameter],
                                                        paramMap: Map[String, SearchParameter],
                                                        searchParameterConfigurator:SearchParameterConfigurator): Seq[SearchParameterConf] = {

    //For each compartment, extract further search parameters needed for the resource to handle compartment search
    val allCompartmentParamsForResource = compartmentRelations.map(compartment => compartment._2.getOrElse(resourceType, Set.empty[String])).fold(Set.empty[String])((s1, s2) => s1.union(s2))
    //Only find the parameters not defined for the resource
    val parameterNames = allCompartmentParamsForResource.diff(resourceParameterNames)

    parameterNames.toSeq.flatMap(paramName => {
      val searchParam: SearchParameter = baseParamMap.getOrElse(paramName, paramMap.getOrElse(paramName, null))
      if (searchParam == null)
        throw new InitializationException(s"Parameter $paramName is not defined for $resourceType!!! Compartment definitions requires this search parameter for the resource..")
      searchParameterConfigurator.createSearchParameterConf(
        searchParam.getCode,
        searchParam.getType,
        searchParam.getXpath,
        searchParam.getTarget.asScala.map(_.getValue),
        Set.empty[String])
    })
  }

  /**
    * Construct a map for each base resource for their element paths
    * (Resource ->
    *   elem1Path -> Seq(possible-type)
    *   elem2Path -> Seq(possible-type)
    * @param baseStructureDefinitions
    * @return
    */
  private def constructElementPathMapForBase(baseStructureDefinitions:Seq[StructureDefinition]): Map[String, Map[String, (String, Boolean)]] = {
      baseStructureDefinitions.map(structureDefinition => {
        val resourceType = structureDefinition.getId.getIdPart
        val elementPathTypeMap:Map[String, (String, Boolean)] =
          structureDefinition.getSnapshot.getElement.asScala
            .flatMap(elemDef => {
              //Remove the type from the beginning of the path; "Observation.id" -> "id"
              var path = elemDef.getPath.replace(resourceType+".","")
              val types = elemDef.getType.asScala.map(_.getCode)
              val isArray = elemDef.getMax != "1"
              //If ends with an [x], add all the paths e.g. Observation.component.value[x]
              if(path.endsWith("[x]")) {
                path = path.replace("[x]", "")
                types.map(typ=> (path + typ.capitalize) -> (typ, isArray))
              }else{
                Seq(path -> ( types.headOption.getOrElse("Resource"), isArray))
              }
            }).toMap
        resourceType -> elementPathTypeMap
      }).toMap
  }

  /**
    * Construct a map for each profile for their element path
    * @param profiles StructureDefinitions for defined profiles
    * @return
    */
  private def constructElementPathMapForProfiles(profiles:Seq[StructureDefinition]): Map[String, Map[String, String]] = {
    profiles.map(profile => {
      val resourceType = profile.getBase.split("/").last
      var elementDefinitions = profile.getSnapshot.getElement.asScala
      if(elementDefinitions.isEmpty)
        elementDefinitions = profile.getDifferential.getElement.asScala
      val elementPathTypeMap:Map[String, String] =
        elementDefinitions
          .flatMap(elemDef => {
            //Remove the type from the beginning of the path; "Observation.id" -> "id"
            var path = elemDef.getPath.replace(resourceType+".","")
            val types = elemDef.getType.asScala.map(_.getCode)
            //If ends with an [x], add all the paths e.g. Observation.component.value[x]
            if(path.endsWith("[x]")) {
              path = path.replace("[x]", "")
              types.map(typ=> (path + typ.capitalize) -> typ)
            }else{
              Seq(path ->  types.headOption.getOrElse("Resource"))
            }
          }).toMap
      resourceType -> elementPathTypeMap
    }).toMap
  }

  /**
    * Go over all supported common search parameters defined in Conformance statement (Conformance.rest.searchParam)
    * and create the configurations for them to be used within query mechanism
    * @param restDef
    * @param baseSearchParameters
    * @return
    */
  private def extractCommonQueryParameters(restDef:Conformance.Rest,
                                           baseSearchParameters:Seq[SearchParameter],
                                           baseStructureDefinitions:Seq[StructureDefinition]
                                          ):Map[String, SearchParameterConf] = {
    //Temporary map for base parameters Resource -> (Param-name -> SearchParameter)
    val baseParamMap:Map[String, SearchParameter] =
      baseSearchParameters
        .map(sp => (sp.getBase, sp.getCode, sp))
        .groupBy(_._1).map( rp => rp._1 -> rp._2.map( p => p._2 -> p._3).toMap)
        .filter(g => g._1 == "Resource" || g._1 == "DomainResource")
        .values.reduce((m1, m2) => m1 ++ m2)

    //Filter only Resource and Meta definitions that are related with common search parameters
    val baseDefinitions = baseStructureDefinitions.filter(bsd => bsd.getIdElement.getIdPart == FHIR_RESOURCE || bsd.getIdElement.getIdPart == FHIR_TYPES_META)
    if(baseDefinitions.length != 2)
      throw new InitializationException("Base Resource or Meta definition not found in supplied definition packages!!!")

    val baseDefinitionsMap:Map[String, Map[String, (String,Boolean)]] =
      constructElementPathMapForBase(baseDefinitions)

    val parameterConfigurations = restDef.getSearchParam.asScala.flatMap( searchParam => {

      val searchParameterConfigurator = new SearchParameterConfigurator("Resource", baseDefinitionsMap, Map.empty, Set.empty)

      baseParamMap.get(searchParam.getName) match {
        case None => throw new InitializationException(s"The parameter definition for ${searchParam.getName} is not found in base definitions...")
        case Some(searchParameter) =>
          //Construct the parameter configuration for SearchParam definition
          searchParameterConfigurator.createSearchParameterConf(
            searchParam.getName,
            searchParam.getType,
            searchParameter.getXpath,
            searchParam.getTarget.asScala.map(_.getValue),
            searchParam.getModifier.asScala.map(_.getValue).toSet
          )
      }
    })
    //Convert it to map and return
    parameterConfigurations.map(parameterConf => parameterConf.pname -> parameterConf).toMap
  }

  /**
    * Parse the Structure Definitions and extract the summary parameter paths for each resource
    * !!! IMPORTANT: Only use for Base Standart Definitions for now*
    * @param structureDefinitions
    * @return
    */
  private def extractSummaryParameters(structureDefinitions:Seq[StructureDefinition]):Map[String, Set[String]] = {
    structureDefinitions.map( structureDefinition => {
      logger.debug(s"Extracting summary parameters for ${structureDefinition.getName} ...")
      //If there is a snapshot definition go with that, else go with differential
      val elementDefinitions = Option(structureDefinition.getSnapshot.getElement.asScala)
        .getOrElse(structureDefinition.getDifferential.getElement.asScala)
      //Find isSummary elements and list their paths
      val summaryPaths:Set[String] = elementDefinitions.tail.flatMap(elementDef => {
        Try(elementDef.getIsSummary.booleanValue()).toOption.getOrElse(false) match {
          case true =>
            //Remove the resource name from the path e.g. Observation.code -> code
            val elementPath = elementDef.getPath.dropWhile(_ != '.').tail
            //If path is generic (e.g. Observation.effective[x])
            if(elementPath.contains("[x]")) {
              //Get the possible types, make the first letter capitilize and append it to the path
              //e.g. Observation.effectiveDat
              elementDef.getType.asScala.map( etype => {
                elementPath.replace("[x]","") + etype.getCode.capitalize
              })
            } else{
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
    * Go over the compartments supported given in Conformance and parse CompartmentDefinitions and
    * constructs the configuration to be used in query mechanism
    * @param restDef Conformance.rest definition for the server
    * @param compartmentDefinitions power2dm.compartment Definitions given
    * @return
    */
  private def extractCompartmentRelations(restDef:Conformance.Rest, compartmentDefinitions:Seq[CompartmentDefinition]):Map[String, Map[String, Set[String]]] = {
    //Temporary map compartment-url -> CompartmentDefinition
    val compartmentMap:Map[String, CompartmentDefinition] = compartmentDefinitions.map(compartmentDef => compartmentDef.getUrl -> compartmentDef).toMap
    restDef.getCompartment.asScala.map(compartmentUrl => {
      logger.debug(s"Extracting compartment relations for $compartmentUrl...")
      val compartmentDef = compartmentMap.get(compartmentUrl.getValue)
      if(compartmentDef.isEmpty)
        throw new InitializationException(s"Comparment definition for ${compartmentUrl.getValue} is not found among the defined compartments ...")

      val parameterMapForResource:Map[String, Set[String]] = compartmentDef.get.getResource.asScala.map( compResource => {
        compResource.getCode -> compResource.getParam.asScala.map(_.getValue).toSet
      }).filter(_._2.nonEmpty).toMap
      //Construct the outer map for each compartment
      compartmentDef.get.getCode.toCode -> parameterMapForResource
    }).toMap
  }

  /**
    *
    * @param restDef
    * @param operationDefinitions
    * @param baseOperationDefinitions
    * @return
    */
  def extractOperations(restDef:Conformance.Rest, operationDefinitions:Seq[OperationDefinition], baseOperationDefinitions:Seq[OperationDefinition]):Seq[OperationConf] = {
    //Temporary map Operation Definition URL (the last element)
    val operationDefinitionMap:Map[String, OperationDefinition] = operationDefinitions.map(opDef => opDef.getUrl -> opDef).toMap
    val baseOperationDefinitionMap:Map[String, OperationDefinition] = baseOperationDefinitions.map(opDef => opDef.getUrl -> opDef).toMap

    restDef.getOperation.asScala.map(operationDef => {
      val operationName = operationDef.getName
      val operationDefinitionUrl = operationDef.getDefinition.getReference.getValue

      var operationDefinition:Option[OperationDefinition] = operationDefinitionMap.get(operationDefinitionUrl)
      //If it does not a provided definition, we will try from FHIR operation definitions
      if(operationDefinition.isEmpty)
        operationDefinition = baseOperationDefinitionMap.get(operationDefinitionUrl)
      //If we still not find it, throw exception
      if(operationDefinition.isEmpty)
        throw new InitializationException(s"Operation definition for ${operationName} with URL (${operationDefinitionUrl}) stated in CapabilityStatement is not found among the defined operation definitions or base FHIR operation definitions ...")

      //Supported levels for operation
      val levels = Seq(
        if(operationDefinition.get.getSystem) Some("system") else None,
        if(operationDefinition.get.getType.asScala.nonEmpty) Some("type") else None,
        if(operationDefinition.get.getInstance()) Some("instance") else None
      ).flatten
      //Kind of operation
      val kind = operationDefinition.get.getKind

      //Operation parameter definition parser
      def parseParamDefinition(params:List[OperationDefinition.Parameter]):Seq[OperationParamDef] = {
        params.map(paramDef => {
          OperationParamDef(
            name = paramDef.getName,
            min = paramDef.getMin,
            max = paramDef.getMax,
            pType = paramDef.getType,
            pProfile = Option(paramDef.getProfile.getReference.getIdPart).toSeq,
            parts = parseParamDefinition(paramDef.getPart.asScala.toList)
          )
        })
      }
      //Divide the parameters into input and output
      val inputParams = operationDefinition.get.getParameter.asScala.filter(_.getUse == "in")
      val outputParams = operationDefinition.get.getParameter.asScala.filter(_.getUse == "out")

      val forResources = operationDefinition.get.getType.asScala.map(_.getValue)

      //Find the class implementing the operation
      val classPath = if(operationDefinitionMap.isDefinedAt(operationDefinitionUrl)) Option(operationDefinition.get.getName) else DEFAULT_IMPLEMENTED_FHIR_OPERATIONS.get(operationDefinition.get.getCode)
      if(classPath.isEmpty)
        throw new InitializationException(s"Operation ${operationName} defined in URL (${operationDefinitionUrl}) is not implemented yet, class path not found!!")

      if(loadOperationClass(classPath.get).isEmpty)
        throw new InitializationException(s"Implementation for operation ${operationName} defined in URL (${operationDefinitionUrl}) not found in classPath: ${classPath.get}!!")

      OperationConf(
        name = operationDefinition.get.getCode,
        classPath = classPath.get,
        kind = kind,
        levels = levels.toSet,
        resources = forResources,
        inputParams = parseParamDefinition(inputParams.toList),
        outputParams = parseParamDefinition(outputParams.toList)
      )
    })
  }

  /**
    * Return the version specific FHIR AuditEvent creator
    *
    * @return
    */
  override def getAuditCreator(): IFhirAuditCreator = new STU3AuditCreator()
}

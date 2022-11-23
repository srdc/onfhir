package io.onfhir.config

import io.onfhir.api.DEFAULT_IMPLEMENTED_FHIR_OPERATIONS
import io.onfhir.api.parsers.{FHIRResultParameterResolver, FHIRSearchParameterValueParser}
import io.onfhir.api.util.FHIRServerUtil
import io.onfhir.api.validation.{FHIRResourceValidator, IFhirResourceValidator, IFhirTerminologyValidator}
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.authz.AuthzManager
import io.onfhir.db.{MongoDBInitializer, ResourceManager}
import io.onfhir.event.{FhirEventBus, IFhirEventBus}
import io.onfhir.validation.FhirTerminologyValidator
import org.slf4j.{Logger, LoggerFactory}

/**
 * Created by tuncay on 11/14/2016.
 * Central OnFhir configurations defining FHIR server capabilities
 */
object FhirConfigurationManager extends IFhirConfigurationManager {
  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)
  //FHIR capabilities configurations
  var fhirConfig: FhirServerConfig = _
  //FHIR Resource validator
  var fhirValidator: IFhirResourceValidator = _
  //FHIR terminology validator
  var fhirTerminologyValidator: IFhirTerminologyValidator = _
  //Audit generator in FHIR AuditEvent format based on the specific version
  var fhirAuditCreator: IFhirAuditCreator = _
  //Authorization manager
  var authzManager:AuthzManager = _
  //FHIR resource manager
  var resourceManager:ResourceManager = _
  //FHIR event manager
  var eventManager:IFhirEventBus = _
  //Other utilities
  var fhirSearchParameterValueParser:FHIRSearchParameterValueParser = _
  var fhirResultParameterResolver:FHIRResultParameterResolver = _
  var fhirServerUtil:FHIRServerUtil = _
  /**
   * Read FHIR foundational definitions and configure the platform
   *
   * @param fhirConfigurator Specific FHIR configurator for the FHIR version to be supported
   */
  def initialize(fhirConfigurator: IFhirServerConfigurator, fhirOperationImplms: Map[String, String] = Map.empty[String, String]): Unit = {
    val fsConfigReader = new FSConfigReader(
      fhirStandardZipFilePath = OnfhirConfig.baseDefinitions,
      conformancePath = OnfhirConfig.conformancePath,
      profilesPath = OnfhirConfig.profilesPath,
      valueSetsPath = OnfhirConfig.valueSetsPath,
      codeSystemsPath = OnfhirConfig.codeSystemsPath,
      searchParametersPath = OnfhirConfig.searchParametersPath,
      operationDefinitionsPath = OnfhirConfig.operationDefinitionsPath,
      compartmentDefinitionsPath = OnfhirConfig.compartmentDefinitionsPath
    )
    //Initialize platform, and save the configuration
    fhirConfig = fhirConfigurator.initializeServerPlatform(fsConfigReader, DEFAULT_IMPLEMENTED_FHIR_OPERATIONS ++ fhirOperationImplms)

    authzManager = new AuthzManager(this)
    eventManager = new FhirEventBus(fhirConfig)
    resourceManager = new ResourceManager(fhirConfig,eventManager)
    //If it is the first setup or update of the platform (definition of new profile, etc), apply the setups
    if (OnfhirConfig.fhirInitialize) {
      //Read the Value sets
      fhirConfigurator.setupPlatform(fsConfigReader, new MongoDBInitializer(resourceManager), fhirConfig)
    }
    //Initialize FHIR Resource and terminology Validator
    fhirValidator = new FHIRResourceValidator(this)
    fhirTerminologyValidator = new FhirTerminologyValidator(fhirConfig)
    //Initialize FHIR Audit creator if necessary
    if (OnfhirConfig.fhirAuditingRepository.equalsIgnoreCase("local") || OnfhirConfig.fhirAuditingRepository.equalsIgnoreCase("remote"))
      fhirAuditCreator = fhirConfigurator.getAuditCreator()

    fhirSearchParameterValueParser = new FHIRSearchParameterValueParser(fhirConfig)
    fhirResultParameterResolver = new FHIRResultParameterResolver(fhirConfig)
    fhirServerUtil = new FHIRServerUtil(fhirConfig)
    printSystemRuntime()
  }

  /**
   * Print Memory and Processor details
   */
  private def printSystemRuntime() = {
    logger.info("** Initialization completed ...")
    val mb = 1024 * 1024
    val runtime = Runtime.getRuntime
    logger.info("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb + " MB")
    logger.info("** Free Memory:  " + runtime.freeMemory / mb + " MB")
    logger.info("** Total Memory: " + runtime.totalMemory / mb + " MB")
    logger.info("** Max Memory:   " + runtime.maxMemory / mb + " MB")
    logger.info("** Available Processors:" + runtime.availableProcessors())
  }
}

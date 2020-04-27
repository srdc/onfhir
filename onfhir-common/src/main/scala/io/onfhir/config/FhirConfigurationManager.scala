package io.onfhir.config

import io.onfhir.api.validation.{IFhirResourceValidator, IFhirTerminologyValidator}
import io.onfhir.audit.IFhirAuditCreator
import org.slf4j.{Logger, LoggerFactory}
import io.onfhir.api.DEFAULT_IMPLEMENTED_FHIR_OPERATIONS
/**
  * Created by tuncay on 11/14/2016.
  * Central OnFhir configurations defining FHIR server capabilities
  */
object FhirConfigurationManager {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)
  //FHIR capabilities configurations
  var fhirConfig:FhirConfig = _
  //FHIR Resource validator
  var fhirValidator:IFhirResourceValidator = _
  //FHIR terminology validator
  var fhirTerminologyValidator:IFhirTerminologyValidator = _
  //Audit generator in FHIR AuditEvent format based on the specific version
  var fhirAuditCreator:IFhirAuditCreator = _
  /**
    * Read FHIR foundational definitions and configure the platform
    * @param fhirConfigurator Specific FHIR configurator for the FHIR version to be supported
    */
  def initialize(fhirConfigurator:IFhirVersionConfigurator, fhirOperationImplms:Map[String, String] = Map.empty[String, String]) : Unit = {
    //Initialize platform, and save the configuration
    fhirConfig = fhirConfigurator.initializePlatform(OnfhirConfig.fhirInitialize, DEFAULT_IMPLEMENTED_FHIR_OPERATIONS ++ fhirOperationImplms)

    //If it is the first setup or update of the platform (definition of new profile, etc), apply the setups
    if(OnfhirConfig.fhirInitialize) {
      //Read the Value sets
      fhirConfigurator.setupPlatform(fhirConfig)
    }
    //Initialize FHIR Resource and terminology Validator
    fhirValidator = fhirConfigurator.getResourceValidator(fhirConfig)
    fhirTerminologyValidator = fhirConfigurator.getTerminologyValidator(fhirConfig)
    //Initialize FHIR Audit creator if necessary
    if(OnfhirConfig.fhirAuditingRepository.equalsIgnoreCase("local") || OnfhirConfig.fhirAuditingRepository.equalsIgnoreCase("remote"))
      fhirAuditCreator = fhirConfigurator.getAuditCreator()

    printSystemRuntime
  }

  /**
    * Print Memory and Processor details
    */
  private def printSystemRuntime() = {
    logger.info("** Initialization completed ...")
    val mb = 1024*1024
    val runtime = Runtime.getRuntime
    logger.info("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb + " MB")
    logger.info("** Free Memory:  " + runtime.freeMemory / mb + " MB")
    logger.info("** Total Memory: " + runtime.totalMemory / mb + " MB")
    logger.info("** Max Memory:   " + runtime.maxMemory / mb + " MB")
    logger.info("** Available Processors:" + runtime.availableProcessors())
  }
}

package io.onfhir.config

import io.onfhir.api.validation.IFhirResourceValidator
import io.onfhir.audit.IFhirAuditCreator
import org.slf4j.{Logger, LoggerFactory}

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
  //Audit generator in FHIR AuditEvent format based on the specific version
  var fhirAuditCreator:IFhirAuditCreator = _
  /**
    * Read FHIR foundational definitions and configure the platform
    * @param fhirConfigurator Specific FHIR configurator for the FHIR version to be supported
    */
  def initialize(fhirConfigurator:IFhirConfigurator) : Unit = {
    //Initialize platform, and save the configuration
    val platformConfigs = fhirConfigurator.initializePlatform(OnfhirConfig.fhirInitialize)
    fhirConfig = platformConfigs._1

    //If it is the first setup or update of the platform (definition of new profile, etc), apply the setups
    if(OnfhirConfig.fhirInitialize) {
      //Read the Value sets
      fhirConfigurator.setupPlatform(fhirConfig, platformConfigs._2, platformConfigs._3, platformConfigs._4, platformConfigs._5, platformConfigs._6, platformConfigs._7, platformConfigs._8, platformConfigs._9)
    }
    //Initialize FHIR Resource Validator
    fhirValidator = fhirConfigurator.getResourceValidator()
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

package io.onfhir.config

import io.onfhir.api.parsers.{FHIRResultParameterResolver, FHIRSearchParameterValueParser}
import io.onfhir.api.util.FHIRServerUtil
import io.onfhir.api.validation.{IFhirResourceValidator, IFhirTerminologyValidator}
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.authz.AuthzManager
import io.onfhir.db.ResourceManager
import io.onfhir.operation.IFhirOperationLibrary

/**
 * Interface for onfhir.io configuration manager
 */
trait IFhirConfigurationManager {
  //FHIR capabilities configurations
  var fhirConfig: FhirServerConfig
  //FHIR Resource validator
  var fhirValidator: IFhirResourceValidator
  //FHIR terminology validator
  var fhirTerminologyValidator: IFhirTerminologyValidator
  //Audit generator in FHIR AuditEvent format based on the specific version
  var fhirAuditCreator: IFhirAuditCreator
  //Authorization manager
  var authzManager: AuthzManager
  //Resource manager
  var resourceManager:ResourceManager
  //All factories/libraries providing the FHIR Operation Handler service implementations
  var fhirOperationImplLibraries:Seq[IFhirOperationLibrary]
  //Other utilities
  var fhirSearchParameterValueParser: FHIRSearchParameterValueParser
  var fhirResultParameterResolver: FHIRResultParameterResolver
  var fhirServerUtil: FHIRServerUtil
}

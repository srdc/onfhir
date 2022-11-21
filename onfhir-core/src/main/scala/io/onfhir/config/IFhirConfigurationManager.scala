package io.onfhir.config

import io.onfhir.api.parsers.{FHIRResultParameterResolver, FHIRSearchParameterValueParser}
import io.onfhir.api.util.FHIRServerUtil
import io.onfhir.api.validation.{IFhirResourceValidator, IFhirTerminologyValidator}
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.authz.AuthzManager
import io.onfhir.db.ResourceManager

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
  //Other utilities
  var fhirSearchParameterValueParser: FHIRSearchParameterValueParser
  var fhirResultParameterResolver: FHIRResultParameterResolver
  var fhirServerUtil: FHIRServerUtil
}

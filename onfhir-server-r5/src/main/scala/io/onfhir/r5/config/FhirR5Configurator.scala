package io.onfhir.r5.config

import io.onfhir.api.FOUNDATION_RESOURCES_FILE_SUFFIX
import io.onfhir.r4.config.FhirR4Configurator

class FhirR5Configurator extends FhirR4Configurator {
  override val VALUESET_AND_CODESYSTEM_BUNDLE_FILES = Seq(s"valuesets$FOUNDATION_RESOURCES_FILE_SUFFIX")
}

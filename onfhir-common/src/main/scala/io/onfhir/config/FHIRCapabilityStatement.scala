package io.onfhir.config

/**
 * Compact form for FHIR CapabilityStatement
 * @param fhirVersion                 FHIR numeric version supporting e.g. 4.0.1
 * @param restResourceConf        REST configurations for each supported resource
 * @param searchParamDefUrls      Common search parameter definition URLs
 * @param operationDefUrls        All operation definition URLs
 * @param systemLevelInteractions System level interactions supported (e.g. transaction, batch)
 * @param compartments            Definition url of compartments supported
 * @param formats                 Formats (mime types) supported by FHIR repository
 * @param patchFormats            Patch formats supported by FHIR repository
 */
case class FHIRCapabilityStatement(
                                    fhirVersion:String,
                                    restResourceConf:Seq[ResourceConf],
                                    searchParamDefUrls:Set[String],
                                    operationDefUrls:Set[String],
                                    systemLevelInteractions:Set[String],
                                    compartments:Set[String],
                                    formats:Set[String],
                                    patchFormats:Set[String]
                                  )

package io.onfhir.config

import io.onfhir.api.model.InternalEntity

/**
 * OnFhir configuration for a supported resource; (FHIR CapabilityStatement.rest.resource)
 *
 * @param resource          FHIR Resource type
 * @param profile           Base profile for all uses of resource
 * @param supportedProfiles Profiles for use cases supported
 * @param interactions      FHIR interactions supported
 * @param searchParams      Names and Urls of SearchParameter definitions for supported search parameters
 * @param versioning        Supported versioning code i.e no-version, versioned, versioned-update
 * @param readHistory       Whether vRead can return past versions
 * @param updateCreate      If update can create a new resource
 * @param conditionalCreate If allows/uses conditional create
 * @param conditionalRead   How to support conditional read i.e. not-supported | modified-since | not-match | full-support
 * @param conditionalUpdate If allows/uses conditional update
 * @param conditionalDelete Conditional delete status code i.e. not-supported | single | multiple
 * @param searchInclude     List of supported include parameters
 * @param searchRevInclude  List of supported reverse include parameters
 * @param referencePolicies How this resource type uses FHIR references i.e. literal | logical | resolves | enforced | local
 */
case class ResourceConf(resource:String,
                        profile:Option[String] = None,
                        supportedProfiles:Set[String] = Set.empty,
                        interactions:Set[String] =Set.empty,
                        searchParams:Set[(String, String)] = Set.empty,
                        versioning:String = "no-version",
                        readHistory:Boolean = false,
                        updateCreate:Boolean = false,
                        conditionalCreate:Boolean = false,
                        conditionalRead:String = "not-supported",
                        conditionalUpdate:Boolean = false,
                        conditionalDelete:String = "not-supported",
                        searchInclude:Set[String] = Set.empty,
                        searchRevInclude:Set[String] = Set.empty,
                        referencePolicies:Set[String] = Set.empty[String]
                       ) extends InternalEntity

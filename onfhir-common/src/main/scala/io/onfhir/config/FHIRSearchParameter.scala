package io.onfhir.config


/**
 * Compact form of a FHIR Search Parameter definition
 * @param name          Name/code of search parameter
 * @param url           URL of definition
 * @param base          Resource Types that this parameter is defined on
 * @param ptype         Search parameter type i.e. number | date | string | token | reference | composite | quantity | uri | special
 * @param expression    FHIR Expression for parameter paths
 * @param xpath         XPath expression for parameter paths
 * @param target        If type is reference, possible target Resource Types
 * @param multipleOr    If it allows multiple values per parameter (or)
 * @param multipleAnd   If it allows multiple parameters (and)
 * @param comparators   Allowed comparators used with parameter
 * @param modifiers     Allowed modifiers used with parameter
 * @param components    URL of search parameter definitions for children of composite parameters
 */
case class FHIRSearchParameter(
                                name:String,
                                url:String,
                                base:Set[String],
                                ptype:String,
                                expression:Option[String],
                                xpath:Option[String],
                                target:Set[String],
                                multipleOr:Option[Boolean],
                                multipleAnd:Option[Boolean],
                                comparators:Set[String],
                                modifiers:Set[String],
                                components:Set[String]
                              )

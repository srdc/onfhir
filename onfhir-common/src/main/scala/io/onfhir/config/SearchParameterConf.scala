package io.onfhir.config

import io.onfhir.api.model.InternalEntity


/**
  * onFHIR configuration for a Search Parameter
 *
 *  @param url            Definition url for the search parameter
 *  @param pname          Name of the parameter
  * @param ptype          FHIR search parameter type (number, date, string, token, etc)
  * @param paths          Extracted element paths for the parameters  e.g. subject, compose.include.concept.code
  * @param targets        Target types or composite parameter names;
  *                         - For reference parameter types, the possible target Resource types
  *                         - For composite parameter types, the names of the parameters to combine in the same order of query usage
  * @param modifiers      Supported modifiers of the parameter
  * @param targetTypes    Seq of target type for each path (indices are inline with paths)
  * @param restrictions   Further restriction on the search for each path; (indices are inline with paths)
  *                          First element provides the sub path to the property with 0 or more prefixes '@.' indicating the position of restriction from right. 0 prefix means the restriction is on the final path element.
  *                          Second element provides the expected value for the property
  *                          e.g. For "f:OrganizationAffiliation/f:telecom[system/@value=&#39;email&#39;]" --> Seq(system -> email)
  *                          e.g. /Goal/extension[@url='http://hl7.org/fhir/StructureDefinition/goal-target']/extension[@url='measure']/valueCodeableConcept --> Seq(@.@.url -> http://hl7.org/fhir/StructureDefinition/goal-target, @.url -> measure)
 * @param multipleOr     If or on parameter is supported
  * @param multipleAnd    If and on parameter is supported
  * @param comparators    Supported comparators for parameter
  */
case class SearchParameterConf(url:String,
                               pname:String,
                               ptype:String,
                               paths:Seq[String],
                               targets:Seq[String] = Nil,
                               modifiers:Set[String]=Set.empty[String],
                               targetTypes:Seq[String] = Nil,
                               //onExtension:Boolean = false,
                               restrictions:Seq[Seq[(String, String)]] = Nil,
                               multipleOr:Boolean = true,
                               multipleAnd:Boolean = true,
                               comparators:Set[String] = Set.empty[String]
                              ) extends InternalEntity {

  /**
    * Extract the possible element paths for the search parameter
    * @param withArrayIndicators If true, it returns the path with array indicators ([i]); e.g. component[i].code
    * @return
    */
  def extractElementPaths(withArrayIndicators:Boolean = false):Seq[String] ={
    if(withArrayIndicators) paths else paths.map(_.replace("[i]", ""))
  }

  def extractElementPathsAndTargetTypes(withArrayIndicators:Boolean = false):Seq[(String, String)] = {
    extractElementPaths(withArrayIndicators)
      .zip(targetTypes)
  }

  def extractElementPathsTargetTypesAndRestrictions(withArrayIndicators:Boolean = false):Seq[(String, String, Seq[(String,String)])] = {
    extractElementPaths(withArrayIndicators).lazyZip(targetTypes).lazyZip(restrictions).toSeq
  }
}

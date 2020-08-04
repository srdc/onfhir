package io.onfhir.r4.config

import io.onfhir.api.{FHIR_PARAMETER_CATEGORIES, Resource}
import io.onfhir.api.validation.{ProfileRestrictions, ValueSetRestrictions}
import io.onfhir.audit.IFhirAuditCreator
import io.onfhir.config._
import io.onfhir.r4.audit.R4AuditCreator
import io.onfhir.r4.parsers.StructureDefinitionParser
import io.onfhir.util.JsonFormatter.formats
import io.onfhir.validation.TerminologyParser
import org.json4s._

/**
 * Configurator for FHIR R4 standard (parsing R4 foundation resources, etc)
 */
class FhirR4Configurator extends BaseFhirConfigurator {

  def getAuditCreator():IFhirAuditCreator = new R4AuditCreator()

  /**
   * Parse a FHIR Capability Statement into our compact form
   *
   * @param capabilityStmt CapabilityStatement resource in parsed JSON format
   * @return
   */
  override def parseCapabilityStatement(capabilityStmt: Resource): FHIRCapabilityStatement = {
    val restDef =
      (capabilityStmt \ "rest").asInstanceOf[JArray].arr.find(r => (r \ "mode").extract[String] == "server").get

    val resourceDefs = (restDef \ "resource").asInstanceOf[JArray]

    FHIRCapabilityStatement(
      restResourceConf = resourceDefs.arr.map(_.asInstanceOf[JObject]).map(resourceDef =>
        ResourceConf(
          resource = (resourceDef \ "type").extract[String],
          profile = (resourceDef \ "profile").extractOpt[String],
          supportedProfiles = (resourceDef \ "supportedProfile").extractOrElse[Seq[String]](Nil).toSet,
          interactions = (resourceDef \ "interaction" \ "code").extractOrElse[Seq[String]](Nil).toSet,
          searchParams = (resourceDef \ "searchParam" \ "definition").extractOrElse[Seq[String]](Nil).toSet,
          versioning = (resourceDef \ "versioning").extractOrElse[String](OnfhirConfig.fhirDefaultVersioning),
          readHistory = (resourceDef \ "readHistory").extractOrElse[Boolean](OnfhirConfig.fhirDefaultReadHistory),
          updateCreate = (resourceDef \ "updateCreate").extractOrElse[Boolean](OnfhirConfig.fhirDefaultUpdateCreate),
          conditionalCreate = (resourceDef \ "conditionalCreate").extractOrElse[Boolean](OnfhirConfig.fhirDefaultConditionalCreate),
          conditionalRead = (resourceDef \ "conditionalRead").extractOrElse[String](OnfhirConfig.fhirDefaultConditionalRead),
          conditionalUpdate = (resourceDef \ "conditionalUpdate").extractOrElse[Boolean](OnfhirConfig.fhirDefaultConditionalUpdate),
          conditionalDelete = (resourceDef \ "conditionalDelete").extractOrElse[String](OnfhirConfig.fhirDefaultConditionalDelete),
          referencePolicies = (resourceDef \ "referencePolicy").extractOrElse[Seq[String]](Nil).toSet,
          searchInclude = (resourceDef \ "searchInclude").extractOrElse[Seq[String]](Nil).toSet,
          searchRevInclude = (resourceDef \ "searchRevInclude").extractOrElse[Seq[String]](Nil).toSet,
        )
      ),
      searchParamDefUrls = extractCommonSearchParameterDefinitionUrls(capabilityStmt).toSet,
      operationDefUrls = extractOperationDefinitionUrls(capabilityStmt).toSet,
      systemLevelInteractions = (capabilityStmt \ "rest" \ "interaction" \ "code").extractOrElse[Seq[String]](Nil).toSet,
      compartments = (restDef \ "compartment").extractOrElse[Seq[String]](Nil).toSet
    )
  }

  /**
   * Extract common search parameter definition URLs
   * @param capabilityStmt CapabilityStatement resource in parsed JSON format
   * @return
   */
  protected def extractCommonSearchParameterDefinitionUrls(capabilityStmt:Resource):Seq[String] = {
    (capabilityStmt \  "rest" \ "searchParam" \ "definition")
      .extractOrElse[Seq[String]](Nil)
  }

  /**
   * Extract definition URLs for system level operations
   * @param capabilityStmt    Parsed JSON object for CapabilityStatement
   * @return
   */
  protected def extractOperationDefinitionUrls(capabilityStmt:Resource):Seq[String] = {
    val systemLevelOperations = (capabilityStmt \  "rest" \ "operation" \ "definition").extractOrElse[Seq[String]](Nil)

    val typeAndInstanceLevelOperations = (capabilityStmt \  "rest" \ "resource" \ "operation" \ "definition").extractOrElse[Seq[String]](Nil)

    systemLevelOperations ++ typeAndInstanceLevelOperations
  }

  /**
   * Parse a FHIR SearchParameter definition into our compact form
   * @param searchParameter SearchParameter resource in parsed JSON format
   * @return
   */
  override def parseSearchParameter(searchParameter: Resource): FHIRSearchParameter = {
    val searchParamName = (searchParameter \ "code").extract[String]
    FHIRSearchParameter(
      name = searchParamName,
      url = (searchParameter \ "url").extract[String],
      base = (searchParameter \ "base").extract[Seq[String]].toSet,
      ptype = if(searchParamName == "_text") FHIR_PARAMETER_CATEGORIES.SPECIAL else (searchParameter \ "type").extract[String], //_text is not given as special in FHIR R4 conf
      expression = (searchParameter \ "expression").extractOpt[String],
      xpath = (searchParameter \ "xpath").extractOpt[String],
      target = (searchParameter \ "target").extractOrElse[Seq[String]](Nil).toSet,
      multipleOr = (searchParameter \ "multipleOr").extractOpt[Boolean],
      multipleAnd = (searchParameter \ "multipleAnd").extractOpt[Boolean],
      comparators = (searchParameter \ "comparator").extractOrElse[Seq[String]](Nil).toSet,
      modifiers = (searchParameter \ "modifier").extractOrElse[Seq[String]](Nil).toSet,
      components = (searchParameter \ "component" \ "definition").extractOrElse[Seq[String]](Nil).toSet
    )
  }

  /**
   * Parse a FHIR OperationDefinition  into our compact form
   *
   * @param operationDefinition OperationDefinition resource in parsed JSON format
   * @return
   */
  override def parseOperationDefinition(operationDefinition: Resource): OperationConf = {
    val paramDefObjs = (operationDefinition \ "parameter").asInstanceOf[JArray].arr.map(_.asInstanceOf[JObject])
    //Parse all parameter definitions
    val paramDefs = paramDefObjs.map(parseOperationParamDefinition)

    OperationConf(
        url = (operationDefinition \  "url").extract[String],
        name =  (operationDefinition \  "code").extract[String],
        kind = (operationDefinition \  "kind").extract[String],
        levels = Seq(
          "system" ->  (operationDefinition \  "system").extract[Boolean],
          "type" -> (operationDefinition \  "type").extract[Boolean],
          "instance" -> (operationDefinition \  "instance").extract[Boolean],
         ).filter(_._2).map(_._1).toSet,
        resources = (operationDefinition \  "resource").extractOrElse[Seq[String]](Nil).toSet,
        inputParams = paramDefs.filter(_._1 == "in").map(_._2),
        outputParams = paramDefs.filter(_._1 == "out").map(_._2),
        inputParamsProfile = (operationDefinition \  "inputProfile").extractOpt[String],
        affectsState = (operationDefinition \  "affectsState").extractOrElse[Boolean](false)
      )
  }

  /**
   * Parse a Operation parameter definition object
   * @param paramDef  OperationDefinition.parameter element
   * @return
   */
  protected def parseOperationParamDefinition(paramDef:JObject):(String, OperationParamDef) = {
   val binding = paramDef \ "binding"  match {
      case obj:JObject =>
       Some( (obj \ "strength").extract[String] ->  (obj \ "valueSet").extract[String])
      case _ => None
    }

    (paramDef \ "use").extract[String] ->
      OperationParamDef(
        name =  (paramDef \ "name").extract[String],
        min = (paramDef \ "min").extract[Int],
        max = (paramDef \ "max").extract[String],
        pType = (paramDef \ "type").extractOpt[String],
        pProfile = (paramDef \ "targetProfile").extractOrElse[Seq[String]](Nil),
        pSearchType = (paramDef \ "searchType").extractOpt[String],
        parts = (paramDef \ "part") match {
          case JArray(arr) => arr.map(p => parseOperationParamDefinition(p.asInstanceOf[JObject])._2)
          case _ => Nil
        },
        binding = binding
      )
  }

  /**
   * Parse a FHIR CompartmentDefinition into our compact form
   *
   * @param compartmentDefinition CompartmentDefinition resource in parsed JSON format
   * @return
   */
  override def parseCompartmentDefinition(compartmentDefinition: Resource): FHIRCompartmentDefinition = {

    val relations =
      (compartmentDefinition \ "resource").asInstanceOf[JArray].arr
        .map(r =>
          (r \ "code").extract[String] -> (r \ "param").extractOrElse[Seq[String]](Nil).toSet
        ).toMap

    FHIRCompartmentDefinition(
      (compartmentDefinition \ "url").extract[String],
      (compartmentDefinition \ "code").extract[String],
      relations
    )
  }

  override def extractResourcesFromBundle(bundle: Resource, rtype: String): Seq[Resource] = {
    val resources =  (bundle \ "entry" \ "resource").extract[Seq[JObject]]
    resources.filter(r => (r \ "resourceType").extract[String] == rtype)
  }

  /**
   * Parse a FHIR StructureDefinition into our compact form
   *
   * @param structureDefinition   Parsed JSON object for FHIR StructureDefinition
   * @return
   */
  override def parseStructureDefinition(structureDefinition: Resource): ProfileRestrictions = {
    new StructureDefinitionParser(FHIR_COMPLEX_TYPES, FHIR_PRIMITIVE_TYPES).parseProfile(structureDefinition)
  }

  /**
   * Parse a bundle of FHIR ValueSet and CodeSystem into a compact form for validation
   *
   * @param valueSetOrCodeSystems   Parsed JSON objects for all ValueSet and CodeSystem resources that will be related with server
   * @return
   */
  override def parseValueSetAndCodeSystems(valueSetOrCodeSystems: Seq[Resource]): Map[String, Map[String, ValueSetRestrictions]] = {
    new TerminologyParser().parseValueSetBundle(valueSetOrCodeSystems)
  }
}

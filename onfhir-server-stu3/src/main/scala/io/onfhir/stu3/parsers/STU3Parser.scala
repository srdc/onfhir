package io.onfhir.stu3.parsers

import io.onfhir.api.Resource
import io.onfhir.api.validation.ProfileRestrictions
import io.onfhir.config.{FHIRCapabilityStatement, FHIRSearchParameter, OnfhirConfig, OperationConf, OperationParamDef, ResourceConf}
import io.onfhir.r4.parsers.R4Parser
import org.json4s.{JArray, JObject}
import org.json4s._
import io.onfhir.util.JsonFormatter.formats

class STU3Parser(complexTypes:Set[String], primitiveTypes:Set[String]) extends R4Parser(complexTypes, primitiveTypes) {
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
      fhirVersion = (capabilityStmt \ "fhirVersion").extract[String],
      restResourceConf = resourceDefs.arr.map(_.asInstanceOf[JObject]).map(resourceDef =>
        ResourceConf(
          resource = (resourceDef \ "type").extract[String],
          profile = (resourceDef \ "profile" \ "reference").extractOpt[String],
          supportedProfiles = Set.empty[String], //TODO
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
      compartments = (capabilityStmt \ "rest" \ "compartment").extractOrElse[Seq[String]](Nil).toSet,
      formats = (capabilityStmt \ "format").extract[Seq[String]].toSet,
      patchFormats = (capabilityStmt \ "patchFormat").extract[Seq[String]].toSet
    )
  }

  /**
   * Parse a FHIR SearchParameter definition into our compact form
   *
   * @param searchParameter SearchParameter resource in parsed JSON format
   * @return
   */
  override def parseSearchParameter(searchParameter: Resource): FHIRSearchParameter = {
    FHIRSearchParameter(
      name = (searchParameter \ "code").extract[String],
      url = (searchParameter \ "url").extract[String],
      base = (searchParameter \ "base").extract[Seq[String]].toSet,
      ptype = (searchParameter \ "type").extract[String],
      expression = (searchParameter \ "expression").extractOpt[String],
      xpath = (searchParameter \ "xpath").extractOpt[String],
      target = (searchParameter \ "target").extractOrElse[Seq[String]](Nil).toSet,
      multipleOr = None,
      multipleAnd = None,
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
      inputParamsProfile = None
    )
  }

  override def parseStructureDefinition(structureDefinition: Resource): ProfileRestrictions = {
    new STU3StructureDefinitionParser(complexTypes, primitiveTypes).parseProfile(structureDefinition)
  }

  /**
   * Parse a Operation parameter definition object
   * @param paramDef  OperationDefinition.parameter element
   * @return
   */
  override protected def parseOperationParamDefinition(paramDef:JObject):(String, OperationParamDef) = {
    val binding = paramDef \ "binding"  match {
      case obj:JObject =>
        Some( (obj \ "strength").extract[String] ->
          (obj \ "valueSetUri")
            .extractOpt[String]
            .getOrElse((obj \ "valueSetReference" \ "reference").extract[String])
        )
      case _ => None
    }

    (paramDef \ "use").extract[String] ->
      OperationParamDef(
        name =  (paramDef \ "name").extract[String],
        min = (paramDef \ "min").extract[Int],
        max = (paramDef \ "max").extract[String],
        pType = (paramDef \ "type").extractOpt[String],
        pProfile = (paramDef \ "profile").extractOrElse[Seq[String]](Nil),
        pSearchType = (paramDef \ "searchType").extractOpt[String],
        parts = (paramDef \ "part") match {
          case JArray(arr) => arr.map(p => parseOperationParamDefinition(p.asInstanceOf[JObject])._2)
          case _ => Nil
        },
        binding = binding
      )
  }
}

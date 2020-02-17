package io.onfhir.r4.parsers

import io.onfhir.api.Resource
import io.onfhir.api.parsers.{FHIRCapabilityStatement, FHIRSearchParameter, IFHIRFoundationResourceParser}
import io.onfhir.config.{OnfhirConfig, OperationConf, OperationParamDef, ResourceConf}
import io.onfhir.util.JsonFormatter.formats
import org.json4s._

class FHIRR4FoundationResourceParser extends IFHIRFoundationResourceParser{
  /**
   * Parse a FHIR Capability Statement into our compact form
   *
   * @param capabilityStmt CapabilityStatement resource in parsed JSON format
   * @return
   */
  override def parseCapabilityStatement(capabilityStmt: Resource): FHIRCapabilityStatement = {

    var resourceDefs = (capabilityStmt \ "rest" \ "resource").asInstanceOf[JArray]

    FHIRCapabilityStatement(
      restResourceConf = resourceDefs.arr.map(_.asInstanceOf[JObject]).map(resourceDef =>
        ResourceConf(
          resource = (resourceDef \ "type").extract[String],
          profile = (resourceDef \ "profile").extractOpt[String],
          supportedProfiles = (resourceDef \ "supportedProfile").extractOrElse[Seq[String]](Nil).toSet,
          interactions = (resourceDef \ "interaction" \ "code").extractOrElse[Seq[String]](Nil).toSet,
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
      operationDefUrls = extractOperationDefinitionUrls(capabilityStmt).toSet
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
   * @param capabilityStmt
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
    FHIRSearchParameter(
      name = (searchParameter \ "code").extract[String],
      url = (searchParameter \ "url").extract[String],
      base = (searchParameter \ "base").extract[Seq[String]].toSet,
      ptype = (searchParameter \ "type").extract[String],
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
  override def parseOperationDefinition(operationDefinition: Resource, cp:Option[String] = None): OperationConf = {
    val paramDefObjs = (operationDefinition \ "parameter").asInstanceOf[JArray].arr.map(_.asInstanceOf[JObject])
    //Parse all parameter definitions
    val paramDefs = paramDefObjs.map(parseOperationParamDefinition)
    //Get input or output profile
    val inputProfile = (operationDefinition \  "inputProfile").extractOpt[String]
    val outputProfile = (operationDefinition \  "outputProfile").extractOpt[String]

    OperationConf(
        name =  (operationDefinition \  "code").extract[String],
        classPath = cp.getOrElse((operationDefinition \  "name").extract[String]),
        kind = (operationDefinition \  "kind").extract[String],
        levels = Seq(
          "system" ->  (operationDefinition \  "system").extract[Boolean],
          "type" -> (operationDefinition \  "type").extract[Boolean],
          "instance" -> (operationDefinition \  "instance").extract[Boolean],
         ).filter(_._2).map(_._1).toSet,
        resources = (operationDefinition \  "resource").extractOrElse[Seq[String]](Nil).toSet,
        inputParams = inputProfile match {
          case None => Left(paramDefs.filter(_._1 == "in").map(_._2))
          case Some(ip) => Right(ip)
        },
        outputParams = outputProfile match {
          case None => Left(paramDefs.filter(_._1 == "out").map(_._2))
          case Some(op) => Right(op)
        },
        affectsState = (operationDefinition \  "affectsState").extractOrElse[Boolean](false)
      )
  }

  /**
   * Parse a Operation parameter definition object
   * @param paramDef  OperationDefinition.parameter element
   * @return
   */
  protected def parseOperationParamDefinition(paramDef:JObject):(String, OperationParamDef) = {
   val binding = (paramDef \ "binding" ) match {
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
        parts = (paramDef \ "part").asInstanceOf[JArray].arr.map(p => parseOperationParamDefinition(p.asInstanceOf[JObject])._2),
        binding = binding
      )
  }

  /**
   * Parse a FHIR CompartmentDefinition into our compact form
   *
   * @param compartmentDefinition CompartmentDefinition resource in parsed JSON format
   * @return
   */
  override def parseCompartmentDefinition(compartmentDefinition: Resource): (String, Map[String, Set[String]]) = {
    val compartmentCode = (compartmentDefinition \ "code").extract[String]

    val relations =
      (compartmentDefinition \ "resource").asInstanceOf[JArray].arr
        .map(r =>
          (r \ "code").extract[String] -> (r \ "param").extractOrElse[Seq[String]](Nil).toSet
        ).toMap

    compartmentCode -> relations
  }
}

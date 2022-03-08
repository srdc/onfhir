package io.onfhir.api.util

import io.onfhir.api._
import io.onfhir.api.model.{FHIRResponse, FhirLiteralReference, OutcomeIssue, Parameter}
import io.onfhir.config.SearchParameterConf
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.db.ResourceManager
import io.onfhir.exception.NotImplementedException

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Try

/**
  * Utility class that runs FHIR queries in memory on a specific resource
  */
object ResourceChecker {
  /**
    * Check if the given resource satisfies the restriction given search query
    * @param resourceType Resource type to be queried
    * @param parameters parameters to search
    * @param resource given Resource
    * @return true if the resource satisfies the query, otherwise false
    */
  def checkIfResourceSatisfies(resourceType:String, parameters:List[Parameter], resource: Resource):Boolean = {
    //Get valid query parameters for resource type
    val validQueryParameters: Map[String, SearchParameterConf] = fhirConfig.getSupportedParameters(resourceType)

    //First check common query params if exist
    handleCommonQueryParams(parameters, resource) &&
      //Then check others
      handleResourceSpecificParams(resourceType, parameters, validQueryParameters, resource)
  }

  /**
    * Handle checking of common query parameters
    * @param parameters
    * @param resource
    * @return
    */
  private def handleCommonQueryParams(parameters: List[Parameter], resource: Resource): Boolean = {
    parameters.filter(_.name.startsWith("_")).forall(parameter => { //query should be satisfy for all
      val parameterName = parameter.name
      val searchParameConf = fhirConfig.commonQueryParameters.get(parameterName)
      if (searchParameConf.isEmpty)
        throw new NotImplementedException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
            None,
            Some(s"Search operation doesn't support parameter ($parameterName). Please check the conformance statement of the server !!!"),
            Nil
          )
        ))

        (parameter.paramType, parameter.suffix) match {
          case (_, FHIR_PREFIXES_MODIFIERS.MISSING) =>
            InMemoryPrefixModifierHandler.missingHandler(searchParameConf.get.extractElementPaths().toSeq, parameter.valuePrefixList.head._2, resource)
          case (_, _) =>
            handleSimpleParameter(parameter, searchParameConf.get, resource)
        }
    })
  }

  /**
    *
    * @param parameters
    * @param validQueryParameters
    * @param resource
    * @return
    */
  private def handleResourceSpecificParams(rtype:String, parameters:List[Parameter], validQueryParameters:Map[String, SearchParameterConf], resource: Resource) : Boolean = {
    // Eliminate common parameters which starts with underscore and construct query for each parameter
    parameters.filterNot(_.name.startsWith("_")).forall(parameter => {
      parameter.paramCategory match {
        case FHIR_PARAMETER_CATEGORIES.COMPARTMENT =>
          val compartmentParameters = parameter.chain.map(_._2)
          //compartmentParameters.map(cpName => Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.))
          val compartmentParamConfs = compartmentParameters
            .flatMap(cp => fhirConfig.findSupportedSearchParameter(rtype, cp)).toList

          handleCompartment(parameter.valuePrefixList.head._1, parameter.valuePrefixList.head._2, compartmentParamConfs, resource)
        case FHIR_PARAMETER_CATEGORIES.NORMAL =>
          val searchParamConf = validQueryParameters.get(parameter.name)
          if (searchParamConf.isEmpty)
            throw new NotImplementedException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
                None,
                Some(s"Search operation doesn't support parameter (${parameter.name}). Please check the conformance statement of the server !!!"),
                Nil
              )
            ))

          parameter.paramType match {
            //Handle composite parameter
            case FHIR_PARAMETER_TYPES.COMPOSITE =>
              handleCompositeParameter(rtype, parameter, searchParamConf.get, validQueryParameters, resource)
            case _ =>
                handleSimpleParameter(parameter, searchParamConf.get, resource)
          }

        case FHIR_PARAMETER_CATEGORIES.CHAINED =>
          handleChainParameter(parameter, validQueryParameters, resource)
      }
    })
  }

  /**
   * Handle chain search parameter
   * e.g.
   *  - /DiagnosticReport?subject:Patient.name=peter   (Single chain)
   *  - /DiagnosticReport?subject:Patient.general-practitioner.name=peter (Deep chain)
   * @param parameter
   * @param validQueryParameters
   * @param resource
   * @return
   */
  private def handleChainParameter(parameter:Parameter, validQueryParameters:Map[String, SearchParameterConf], resource: Resource): Boolean ={
    val (mainChainType, mainChainParam) = parameter.chain.head
    val searchParameterConf = validQueryParameters.get(mainChainParam)
    if(searchParameterConf.isEmpty)
      throw new NotImplementedException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
          None,
          Some(s"Search operation doesn't support parameter (${parameter.name}). Please check the conformance statement of the server !!!"),
          Nil
        )
      ))
   //Get the paths for the chaining reference parameter
   val referencePaths =
     searchParameterConf.get
       .extractElementPaths()
   val referencedResources =
     referencePaths
       .flatMap(p =>
         FHIRUtil.applySearchParameterPath(p, resource) //Extract the reference value from path
       )
       .map(v => FHIRUtil.parseReference(v)) //Parse the reference object
       .filter(r => r.isInstanceOf[FhirLiteralReference]) //Chaining is only supported on literal references
       .map(_.asInstanceOf[FhirLiteralReference])
       .filter(_.rtype == mainChainType)  //Filter only the requested chain type
       .map(_.rid)  //get the resource id

    //If there is no referenced resource, query is not satisfied
    if(referencedResources.isEmpty)
      false
    else {
      //Otherwise we should query the first chained type
      val newSearchParam = {
        //If this is a single chain, convert the parameter to a normal parameter
        if (parameter.chain.length == 1)
          parameter.copy(paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL, chain = Nil)
        else //Otherwise drop the first element from chain
          parameter.copy(chain = parameter.chain.drop(1))
      }
      val idQueryParam = Parameter(FHIR_PARAMETER_CATEGORIES.SPECIAL, FHIR_PARAMETER_TYPES.TOKEN, "_id", referencedResources.map(r => "" -> r))
      //TODO Change method signature to return Future
      Try(Await.result(ResourceManager.countResources(mainChainType, List(idQueryParam, newSearchParam)), 5 seconds))
        .toOption
        .exists(_ > 0)  //At least one reference should satisfied
    }
  }

  private def handleSimpleParameter(parameter:Parameter, searchParameterConf:SearchParameterConf, resource: Resource): Boolean ={
    val values = ImMemorySearchUtil.extractValuesAndTargetTypes(searchParameterConf, resource)
    ImMemorySearchUtil.handleSimpleParameter(parameter, searchParameterConf, values)
  }


  /**
    * Handle the compartment parameter query
    * @param compartmentType
    * @param compartmentId
   *  @param compartmentParamConfs
   *  @param resource
    * @return
    */
  private def handleCompartment(compartmentType:String, compartmentId:String, compartmentParamConfs:List[SearchParameterConf], resource: Resource):Boolean = {
    compartmentParamConfs.exists(cpf => {
      val param = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.REFERENCE, cpf.pname, Seq("" -> s"$compartmentType/$compartmentId"))
      handleSimpleParameter(param, cpf, resource)
    })
  }

  /**
    * Handle the query on composite parameters wrt to provided
    * list of paths and parameter object
    * @param parameter Parameter object
    * @param searchParamConf Configuration of search parameter corresponding to Composite parameter
    * @param validQueryParameters All query parameters supported for this resource
    */
  private def handleCompositeParameter(rtype:String, parameter:Parameter, searchParamConf:SearchParameterConf, validQueryParameters:Map[String, SearchParameterConf], resource: Resource):Boolean = {
    val commonParts = ImMemorySearchUtil.extractValuesAndTargetTypes(searchParamConf, resource)
    ImMemorySearchUtil.handleCompositeParameter(parameter, searchParamConf, commonParts, validQueryParameters)
  }
}

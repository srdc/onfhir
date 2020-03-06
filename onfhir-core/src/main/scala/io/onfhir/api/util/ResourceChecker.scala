package io.onfhir.api.util

import io.onfhir.api._
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.util.ImMemorySearchUtil.typeHandlerFunction
import io.onfhir.config.SearchParameterConf
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.exception.NotImplementedException
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JNothing, JObject, JValue}

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
            handleParameter(parameter, searchParameConf.get, resource.asInstanceOf[JValue])
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
              if(searchParamConf.get.onExtension)
                handleExtensionParameter(parameter, searchParamConf.get, resource)
              else
                handleSimpleParameter(parameter, searchParamConf.get, resource)
          }
      }
    })
  }

  private def handleSimpleParameter(parameter:Parameter, searchParameterConf:SearchParameterConf, resource: Resource): Boolean ={
    parameter.suffix match {
      //Common handling for missing prefix
      case FHIR_PREFIXES_MODIFIERS.MISSING =>
        InMemoryPrefixModifierHandler.missingHandler(searchParameterConf.extractElementPaths().toSeq, parameter.valuePrefixList.head._2, resource)
      case _ =>
        handleParameter(parameter, searchParameterConf, resource.asInstanceOf[JValue])
    }
  }


  /**
    * Handle a search on a parameter
    * @param parameter
    * @param searchParameterConf
    * @param resource
    * @return
    */
  private def handleParameter(parameter:Parameter, searchParameterConf:SearchParameterConf, resource: JValue):Boolean = {
    parameter.valuePrefixList.exists {
      case (prefix, value) =>
        // A parameter can have either a prefix or modifier
        val prefixModifier = (parameter.suffix, prefix) match {
          case ("", "") => ""
          case (_, "") => parameter.suffix
          case ("", _) => prefix
          case _ => ""//Not Possible
        }

        //Split the values given for the parameters e.g. /Patient?language=FR,NL
        typeHandlerFunction(parameter.paramType)(value, prefixModifier, searchParameterConf,resource)
    }
  }


  /**
    * Handle the compartment parameter query
    * @param compartmentParameters
    * @param validQueryParameters
    * @return
    */
  private def handleCompartment(compartmentType:String, compartmentId:String, compartmentParamConfs:List[SearchParameterConf], resource: Resource):Boolean = {
    compartmentParamConfs.exists(cpf => {
      val param = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.REFERENCE, cpf.pname, Seq("" -> s"$compartmentType/$compartmentId"))
      handleParameter(param, cpf, resource.asInstanceOf[JValue])
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
    val composites = searchParamConf.extractElementPaths()
    val result = parameter.valuePrefixList exists { case (prefix, value) =>
      // Extract parameter names and values of composite pair
      val parameterName1 = composites.head
      val parameterName2 = composites.last
      //Extract the values split by $
      val valuesArr = value.split('$')
      val parameterValue1 = valuesArr.head
      val parameterValue2 = valuesArr.last

      // Generate parameters for the recursive call
      val parameter1 = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, validQueryParameters(parameterName1).ptype, parameterName1, Seq(prefix -> parameterValue1))
      val parameter2 = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, validQueryParameters(parameterName2).ptype, parameterName2, Seq(prefix -> parameterValue2))

      handleResourceSpecificParams(rtype, List(parameter1), validQueryParameters, resource) &&
        handleResourceSpecificParams(rtype, List(parameter2), validQueryParameters, resource)
    }
    result
  }

  /**
    * Handle the query on search parameter defined on extensions
    * @param parameter
    * @param searchParameterConf
    * @param validQueryParameters
    * @return
    */
  private def handleExtensionParameter(parameter:Parameter, searchParameterConf: SearchParameterConf,  resource: Resource):Boolean = {
   //Get the paths
    val paths = searchParameterConf.paths.asInstanceOf[Seq[Seq[(String, String)]]]
    paths.exists(eachPath => {
      //Seq of uri restrictions on the extension , except the last one (last one is the element for value)

      //Extension URL list
      val uriList = eachPath.dropRight(1)
      val lastExtension = uriList.foldLeft(resource.asInstanceOf[JValue])((v,uri) => {
        (v \ "extension") match {
          case JNothing => JNothing
          case org.json4s.JsonAST.JArray(arr) => arr.find(v => (v \ "url").extract[String] == uri._2).getOrElse(JNothing)
          case oth => if((oth \ "url").extract[String] == uri._2) oth else JNothing
        }
      })

      //The last of the Seq is the path for the value element (remove the extensions as we will use it as inner query)
      val valueElementPath = eachPath.last._1.replaceAll("extension.", "")
      //Extension elements are like valueCodeableConcept, valueCoding, etc. So remove it to find the type
      val targetType = valueElementPath.replace("value","")

      //Construct temporary Parameter and SearchParameterConf objects for recursive call
      val extParameter = Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, searchParameterConf.ptype, parameter.name, parameter.valuePrefixList, parameter.suffix)
      val extSearchParameterConf = SearchParameterConf(parameter.name, searchParameterConf.ptype, Seq(valueElementPath), searchParameterConf.targets, searchParameterConf.modifiers, Seq(targetType))

      //Query on the extension element
      handleSimpleParameter(extParameter, extSearchParameterConf, lastExtension.asInstanceOf[JObject])
    })
  }

}

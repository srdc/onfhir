package io.onfhir.api.parsers

import io.onfhir.api._
import io.onfhir.api.model.Parameter
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.{InvalidParameterException, UnsupportedParameterException}

/**
  * Utilty class to resolve several Result parameters
  */
object FHIRResultParameterResolver {
  /**
    * Handle FHIR _sort parameters and return (param name, sorting direction, List of alternative paths and target types)
    * @param resultParameters Given result category parameters
    * @return (param name, sorting direction, List of alternative paths and target types) e.g. date, 1, Seq((effectiveDateTime, DateTime), (effectiveInstant, Instant), etc)
    */
  def resolveSortingParameters(rtype:String, resultParameters:List[Parameter]):Seq[(String, Int, Seq[(String, String)])] = {
    resultParameters
      .filter(_.name == FHIR_SEARCH_RESULT_PARAMETERS.SORT)
      .flatMap(p =>
        p.valuePrefixList.map( pv =>
          fhirConfig.findSupportedSearchParameter(rtype, pv._2) match {
            case Some(spConf) if spConf.ptype != FHIR_PARAMETER_TYPES.COMPOSITE =>
              (
                spConf.pname, //Name of the parameter
                if(pv._1 == FHIR_PREFIXES_MODIFIERS.DESCENDING) -1 else 1,  //Either ascending or descending
                spConf.extractElementPathsAndTargetTypes() //Zip the path and target types
              )
            case _ => throw new UnsupportedParameterException(s"Search parameter ${p.name} is not supported for resource type $rtype, or you can not use it for sorting! Check conformance statement of server!")
          }
        )
      )
  }

  /**
    * Resolve FHIR _summary parameter to infer including and excluding fields
    * @param rtype Resource type
    * @param resultParameters Given result category parameters
    * @return Specifically
    */
  def resolveSummaryParameter(rtype:String, resultParameters:List[Parameter]):Option[(Boolean, Set[String])] = {
    resultParameters
      .find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY)
      .flatMap(sp =>
        //Get value of parameter
        resolveSummary(rtype, sp.valuePrefixList.head._2)
      )
  }

  /**
    * Handle FHIR _elements parameter
    * @param resultParameters
    * @return
    */
  def resolveElementsParameter(resultParameters:List[Parameter]):Set[String] = {
    resultParameters
      .filter(_.name == FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS)
      .flatMap(
        _.valuePrefixList.map(_._2)
      ).toSet
  }

  /**
    * Resolve FHIR _page and _count parameter
    * @param resultParameters
    * @return page -> count
    */
  def resolveCountPageParameters(resultParameters:List[Parameter]):(Int, Int) = {
    val count =
      resultParameters
        .find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.COUNT)
        .map(_.valuePrefixList.head._2.toInt)
        .getOrElse(OnfhirConfig.fhirDefaultPageCount)

    val page = resultParameters
      .find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.PAGE)
      .map(_.valuePrefixList.head._2.toInt)
      .getOrElse(1)

    page -> count
  }

  /**
    * Reolve _total parameter
    * @param resultParameters
    * @return If client needs total number or not
    */
  def resolveTotalParameter(resultParameters:List[Parameter]):Boolean = {
    resultParameters
      .find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.TOTAL)
      .forall(_.valuePrefixList.head._2 != "none")
  }

  /**
    * Resolve sumamary parameter
    * @param rtype
    * @param summary
    * @return
    */
  def resolveSummary(rtype:String, summary:String):Option[(Boolean, Set[String])] = {
    //Summary fields for resource type
    val summaryFields = fhirConfig.getSummaryElements(rtype)

    summary match {
      case "false" =>
        None
      case "true" =>
        Some(true -> summaryFields) //only include sumamry fields
      case "data" =>
        Some(false -> Set(FHIR_COMMON_FIELDS.TEXT)) //Only exclude text
      case "text" =>
        Some(true -> (FHIR_MANDATORY_SUMMARY_FIELDS ++ Seq(FHIR_COMMON_FIELDS.TEXT)).toSet) //only include mandatory and text
      case "count" =>
        Some(true, Set.empty)
      case _ =>
        throw new InvalidParameterException("{" + summary + "}" + " is invalid for _summary parameter.")
    }
  }
}

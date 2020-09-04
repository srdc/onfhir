package io.onfhir.subscription.util

import io.onfhir.api.{FHIR_PARAMETER_TYPES, Resource}
import io.onfhir.api.model.Parameter
import io.onfhir.api.util.{ImMemorySearchUtil}
import io.onfhir.config.SearchParameterConf
import org.json4s.JsonAST.JValue
import org.slf4j.LoggerFactory

class MultipleQueryResourceChecker(rtype:String, searchParameters:Map[String, SearchParameterConf]) {

  val log = LoggerFactory.getLogger(classOf[MultipleQueryResourceChecker])
  /**
   * Return which criteria are satisfied for the resource
   * @param criteriaSubscriptions
   * @param resource
   * @return
   */
  def filterSatisfiedCriteriaForResource(criteriaSubscriptions:Seq[Seq[Parameter]], resource: Resource):Seq[Boolean] = {
    //Find out all referred parameters
    val allReferredParameters =  criteriaSubscriptions.flatten.map(_.name).toSet
    //Get the search parameter configurations
    val allReferredParameterConfigurations = allReferredParameters.map(p => p -> searchParameters.get(p))

    if(allReferredParameterConfigurations.exists(_._2.isEmpty))
      throw new RuntimeException(s"Search parameter configuration not found for parameters ${allReferredParameterConfigurations.filter(_._2.isEmpty).map(_._1).mkString(",")} ...")

    val referredParametersAndValues:Map[String, (SearchParameterConf, Seq[(Seq[JValue], String)])] =
      allReferredParameterConfigurations
        .toMap
        .mapValues(_.get)
        .mapValues(spConfig =>
          spConfig -> ImMemorySearchUtil.extractValuesAndTargetTypes(spConfig, resource)
        )

    val result =
      criteriaSubscriptions.map(c =>
        checkIfResourceSatisfiesCriteria(c, referredParametersAndValues)
      ).toArray.toSeq

    result
  }

  /**
   * Check if a criteria is satisfied by the record
   * @param criteria          Parsed FHIR search expression
   * @param parameterValues   All extracted elements for each search param e.g. code -> (search param config of code, and extracted values for each path for code parameter)
   * @return
   */
  def checkIfResourceSatisfiesCriteria(criteria:Seq[Parameter], parameterValues:Map[String, (SearchParameterConf, Seq[(Seq[JValue], String)])]):Boolean = {
    log.debug(s"Criteria: $criteria ...")

    val result = criteria.forall(parameter => {
      val paramValue = parameterValues(parameter.name)
      parameter.paramType match {
        //Handle composite parameter
        case FHIR_PARAMETER_TYPES.COMPOSITE =>
          handleCompositeParameter(parameter,  paramValue._1, paramValue._2)
        case _ =>
          ImMemorySearchUtil.handleSimpleParameter(parameter, paramValue._1, paramValue._2)
      }
    })

    result
  }


  /**
   *
   * @param parameter
   * @param sp
   * @param values
   * @return
   */
  private def handleCompositeParameter(parameter:Parameter, sp:SearchParameterConf, values:Seq[(Seq[JValue], String)]):Boolean = {
    val childParamConfs =
      sp.targets
        .map(p => p -> searchParameters.get(p))
        .toMap

    if(childParamConfs.exists(_._2.isEmpty))
      throw new RuntimeException(s"One of the parameters (${childParamConfs.filter(_._2.isEmpty).keys.mkString(",")}) referenced in composite parameter ${parameter.name} is missing in cache!")

    ImMemorySearchUtil.handleCompositeParameter(parameter, sp, values, childParamConfs.mapValues(_.get))
  }
}

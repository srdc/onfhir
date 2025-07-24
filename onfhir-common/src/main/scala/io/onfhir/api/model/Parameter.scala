package io.onfhir.api.model

import io.onfhir.api.{FHIR_PARAMETER_CATEGORIES, FHIR_SEARCH_RESULT_PARAMETERS}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/**
  * Parsed FHIR Search Parameter
 *
  * @param paramType FHIR Parameter type (+ our special parameter types) See tr.com.srdc.fhir.api.FHIR_PARAMETER_TYPES
  * @param name Name of the parameter
  * @param valuePrefixList Expected prefix and value tuples for the parameter e.g. 'eq5' -> (eq,5)
  * @param suffix Modifier for the parameter e.g. ':missing' , ':exact'
  * @param chain For chained and reverse chained(_has) parameters, ResourceType and parameter to chain  e.g. ?subject:Patient.name=peter  --> Patient,subject (parameter name is name)
  */
case class Parameter(paramCategory:String,
                     paramType:String,
                     name:String,
                     valuePrefixList:Seq[(String, String)],
                     suffix:String = "",
                     chain:Seq[(String, String)] = Nil ) extends InternalEntity {

  /**
   * Encode the search parameter into string
   * e.g. gender=male
   *
   * @return
   */
  def encode:String = {
    //Construct name part
    val namePart = paramCategory match {
      case FHIR_PARAMETER_CATEGORIES.CHAINED =>
        chain.map(c => c._2 + ":" + c._1).mkString(".") + "." + name
      case FHIR_PARAMETER_CATEGORIES.REVCHAINED =>
        chain.map(c => "_has" + c._1 + ":" + c._2).mkString(":") + ":" + name
      case _ =>
        name + suffix
    }
    //Construct the value part
    val valuePart =
      if (paramCategory == FHIR_PARAMETER_CATEGORIES.RESULT && (name == FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE || name == FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE))
        valuePrefixList.map { case (typ, prName) => s"$typ:$prName"}.head
      else
        valuePrefixList.map(vp => vp._1 + vp._2).mkString(",")

    //Return name and value part
    namePart + "=" + URLEncoder.encode(valuePart, StandardCharsets.UTF_8.toString)
  }

}
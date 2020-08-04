package io.onfhir.api.model

/**
  * Parsed FHIR Search Parameter
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
                     chain:Seq[(String, String)] = Nil ) {

}
package io.onfhir.config

/**
 * Onfhir configuration for a supported Operation parameter
 * @param name           Parameter name
 * @param min            Min cardinality
 * @param max            Max cardinality (*) or a integer
 * @param pType          parameter's FHIR type
 * @param pProfile       If this is a resource parameter, URLs of the profiles it should conform, or if type is a reference or canonical; the profile of target resource
 * @param pSearchType    If parameter is a search parameter, its type e.g. reference, date, token
 * @param parts          Child parameter definitions if exist
 * @param binding        URLs of valueset bindings
 */
case class OperationParamDef(name:String, //Parameter name
                             min:Int, //Min cardinality
                             max:String, //Max cardinality (*) or a integer
                             pType:Option[String], //Parameter type
                             pProfile:Seq[String] = Nil, //
                             pSearchType:Option[String] = None,
                             parts:Seq[OperationParamDef] = Nil,
                             binding:Option[(String,String)] = None
                            )

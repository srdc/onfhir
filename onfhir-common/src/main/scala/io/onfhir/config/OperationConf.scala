package io.onfhir.config

import io.onfhir.api.model.InternalEntity

/**
 * Onfhir configuration for a supported FHIR operation
 *
 * @param url            URL of the definition
 * @param name           Name of the operation to use e.g. validate --> $validate (parsed from OperationDefinition.code)
 * @param classPath      Full class path for the implementation of operation (parsed from OperationDefinition.name)
 * @param kind           Kind of operation "operation" or "query"
 * @param levels         Levels this operation is supported "system", "type", "instance"
 * @param resources      Resources this operation is supported for (for type and instance level operations)
 * @param inputParams    Input parameter definitions or profile for input as a whole (Parameters resource)
 * @param outputParams   Output parameter definitions or profile for output
 */
case class OperationConf(url:String,
                         name:String,
                         var classPath:String = "",
                         kind:String,
                         levels:Set[String],
                         resources:Set[String],
                         inputParams:Seq[OperationParamDef],
                         outputParams:Seq[OperationParamDef],
                         inputParamsProfile:Option[String] = None,
                         affectsState:Boolean = false
                        ) extends InternalEntity {
  //If HTTP Get is allowed for operation; if it does not affect state of resources and all required input parameters are primitive
  def isHttpGetAllowed(level:String) = !affectsState && getInputParams(level).filter(_.min > 0).forall(ip => ip.pType.isDefined &&  ip.pType.get.head.isLower)

  /**
   * Get input parameters defined for level
   * @param level The level of operation parameters are requested (instance | type | system)
   * @return
   */
  def getInputParams(level:String):Seq[OperationParamDef] =
    inputParams.filter(pdef => pdef.scopes.isEmpty || pdef.scopes.contains(level))

  /**
   * Get output parametes defined for level
   * @param level The level of operation parameters are requested (instance | type | system)
   * @return
   */
  def getOutputParams(level:String):Seq[OperationParamDef] =
    outputParams.filter(pdef => pdef.scopes.isEmpty || pdef.scopes.contains(level))
}

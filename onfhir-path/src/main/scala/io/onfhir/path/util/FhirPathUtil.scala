package io.onfhir.path.util

import io.onfhir.path.grammar.FhirPathExprParser.FunctionContext

object FhirPathUtil {

  def getFunctionName(functionContext: FunctionContext):(Option[String], String) = {
    val prefixAndName = functionContext.functionName().identifier()
    if(prefixAndName.size() == 1)
      None -> prefixAndName.get(0).getText
    else
      Some(prefixAndName.get(0).getText) ->  prefixAndName.get(1).getText
  }




}

package io.onfhir.path

import io.onfhir.api.service.IFhirIdentityService
import io.onfhir.path.annotation.FhirPathFunction
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext

import scala.concurrent.Await

/**
 * Function library to access identity service functionalities
 *
 * @param context FHIR Path context
 */
class FhirPathIdentityServiceFunctions(context: FhirPathEnvironment) extends AbstractFhirPathFunctionLibrary {

  /**
   * Resolve the given business identifier and return the FHIR Resource identifier (together with the resource type) by using the supplied identity service
   *
   * @param resourceTypeExpr     FHIR resource type
   * @param identifierValueExpr  Business identifier value (Identifier.value)
   * @return Identifier for the resource e.g. Patient/455435464698
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Resolves the given business identifier and return the FHIR Resource identifier (together with the resource type) by using the supplied identity service.  \n⚠\uFE0F To use this function, the mapping must be executed with a mapping job that includes a configured Identity Service.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`resourceType`**  \nThe FHIR resource type for which the business identifier is being resolved. This should be a valid FHIR resource type.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`identifierValue`**  \nThe value of the business identifier value (Identifier.value) that needs to be resolved.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \nThe method returns the identifier for the resource in the format `<resourceType>/<resourceId>`.\n```json\n\"Patient/455435464698\"\n``` \n\uD83D\uDCA1 **E.g.** idxs:resolveIdentifier('Patient', '12345')",
    insertText = "idxs:resolveIdentifier(<resourceType>, <identifierValue>)",detail = "idxs", label = "idxs:resolveIdentifier", kind = "Function", returnType = Seq("string"), inputType = Seq())
  def resolveIdentifier(resourceTypeExpr: ExpressionContext, identifierValueExpr: ExpressionContext): Seq[FhirPathResult] = resolveIdentifier(resourceTypeExpr, identifierValueExpr, None)

  /**
   * Resolve the given business identifier and return the FHIR Resource identifier (together with the resource type) by using the supplied identity service
   *
   * @param resourceTypeExpr     FHIR resource type
   * @param identifierValueExpr  Business identifier value (Identifier.value)
   * @param identifierSystemExpr Business identifier system (Identifier.system)
   * @return Identifier for the resource e.g. Patient/455435464698
   */
  @FhirPathFunction(documentation = "\uD83D\uDCDC Resolves the given business identifier+system and return the FHIR Resource identifier (together with the resource type) by using the supplied identity service.  \n⚠\uFE0F To use this function, the mapping must be executed with a mapping job that includes a configured Identity Service.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`resourceType`**  \nThe FHIR resource type for which the business identifier is being resolved. This should be a valid FHIR resource type.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`identifierValue`**  \nThe value of the business identifier value (Identifier.value) that needs to be resolved.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`identifierSystem`**  \nThe value of the business identifier system (Identifier.system) that needs to be resolved.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \nThe method returns the identifier for the resource in the format `<resourceType>/<resourceId>`.\n```json\n\"Patient/455435464698\"\n``` \n\uD83D\uDCA1 **E.g.** idxs:resolveIdentifier('Patient', '12345')",
    insertText = "idxs:resolveIdentifier(<resourceType>, <identifierValue>, <identifierSystem>)",detail = "idxs", label = "idxs:resolveIdentifier", kind = "Function", returnType = Seq("string"), inputType = Seq())
  def resolveIdentifier(resourceTypeExpr: ExpressionContext, identifierValueExpr: ExpressionContext, identifierSystemExpr: ExpressionContext): Seq[FhirPathResult] = resolveIdentifier(resourceTypeExpr,identifierValueExpr,Some(identifierSystemExpr))

  /**
   * Resolve the given business identifier and return the FHIR Resource identifier (together with the resource type) by using the supplied identity service
   *
   * @param resourceTypeExpr     FHIR resource type
   * @param identifierValueExpr  Business identifier value (Identifier.value)
   * @param identifierSystemExpr Business identifier system (Identifier.system)
   * @return Identifier for the resource e.g. Patient/455435464698
   */
  private def resolveIdentifier(resourceTypeExpr: ExpressionContext, identifierValueExpr: ExpressionContext, identifierSystemExpr: Option[ExpressionContext]): Seq[FhirPathResult] = {
    val identityService = checkIdentityService()

    val resourceType = evaluateToSingleString(resourceTypeExpr)
    val identifierValue = evaluateToSingleString(identifierValueExpr)
    val identifierSystem = identifierSystemExpr.flatMap(evaluateToSingleString)
    if (resourceType.isEmpty || identifierValue.isEmpty)
      throw FhirPathException(s"Invalid function call resolveIdentifier. The 'identifierValue' and 'resourceType' parameters are mandatory!")

    try {
      Await.result(identityService.findMatching(resourceType.get, identifierValue.get, identifierSystem), identityService.getTimeout)
        .map(matchingId => FhirPathString(s"${resourceType.get}/$matchingId"))
        .toSeq
    } catch {
      case t: Throwable => throw FhirPathException("Problem while calling identity service!", t)
    }
  }

  /**
   * Check if terminology service is supplied
   *
   * @return
   */
  private def checkIdentityService(): IFhirIdentityService = {
    if (context.identityService.isEmpty)
      throw FhirPathException("Identity service function calls are not supported with the current configuration of onfhir FHIR Path engine!")
    context.identityService.get
  }

  private def evaluateToSingleString(expr: ExpressionContext): Option[String] = {
    // Evaluate code
    val exprResult = new FhirPathExpressionEvaluator(context, context._this).visit(expr)
    if (exprResult.length > 1 || !exprResult.forall(_.isInstanceOf[FhirPathString]))
      throw FhirPathException(s"Invalid function call. The expression '${expr.getText}' (the function parameter) should evaluate to a single string value providing the code value.")
    exprResult.headOption.map(_.asInstanceOf[FhirPathString].s)
  }

}

/**
 * Function library factory for this library
 */
object FhirPathIdentityServiceFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix: String = "idxs"

  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathIdentityServiceFunctions(context)
}

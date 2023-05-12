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
   * @param identifierValueExpr  Business identifier  value (Identifier.value)
   * @param identifierSystemExpr Business identifier system (Identifier.system)
   * @return Identifier for the resource e.g. Patient/455435464698
   */
  @FhirPathFunction(documentation = "Resolves the given business identifier and return the FHIR Resource identifier (together with the resource type) by using the supplied identity service. Ex: idxs:resolveIdentifier('Patient', '12345', 'urn:oid:1.2.36.146.595.217.0.1')",
    insertText = "idxs:resolveIdentifier(<resourceTypeExpr>,<identifierValueExpr>,<identifierSystemExpr>)",detail = "idxs", label = "idxs:resolveIdentifier", kind = "Function", returnType = Seq("string"), inputType = Seq())
  def resolveIdentifier(resourceTypeExpr: ExpressionContext, identifierValueExpr: ExpressionContext, identifierSystemExpr: Option[ExpressionContext]): Seq[FhirPathResult] = {
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

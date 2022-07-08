package io.onfhir.path

import io.onfhir.api.service.{IFhirIdentityService, IFhirTerminologyService}
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext

import scala.concurrent.Await

class FhirPathIdentityServiceFunctions(context:FhirPathEnvironment) extends AbstractFhirPathFunctionLibrary {
  /**
   * Resolve the given business identifier and return the FHIR Resource identifier by using the supplied identity service
   * @return
   */
  def resolveIdentifier(resourceTypeExpr:ExpressionContext, identifierValueExpr:ExpressionContext, identifierSystemExpr:ExpressionContext):Seq[FhirPathResult] = {
    val identityService = checkIdentityService()

    val resourceType = evaluateToSingleString(resourceTypeExpr)
    val identifierValue = evaluateToSingleString(identifierValueExpr)
    val identifierSystem = evaluateToSingleString(identifierSystemExpr)
    if(resourceType.isEmpty || identifierValue.isEmpty)
      throw FhirPathException(s"Invalid function call resolveIdentifier. The 'identifierValue' and 'resourceType' parameters are mandatory!")

    try {
      Await.result(identityService.findMatching(resourceType.get, identifierValue.get, identifierSystem), identityService.getTimeout)
        .map(matchingId => FhirPathString(matchingId))
        .toSeq
    } catch {
      case t:Throwable => throw FhirPathException("Problem while calling identity service!", t)
    }
  }

  /**
   * Check if terminology service is supplied
   * @return
   */
  private def checkIdentityService():IFhirIdentityService = {
    if(context.identityService.isEmpty)
      throw FhirPathException("Identity service function calls are not supported with the current configuration of onfhir FHIR Path engine!")
    context.identityService.get
  }

  private def evaluateToSingleString(expr:ExpressionContext):Option[String] = {
    // Evaluate code
    val exprResult = new FhirPathExpressionEvaluator(context, context._this).visit(expr)
    if (exprResult.length > 1 || !exprResult.forall(_.isInstanceOf[FhirPathString]))
      throw FhirPathException(s"Invalid function call. The expression '${expr.getText}' (the function parameter) should evaluate to a single string value providing the code value.")
    exprResult.headOption.map(_.asInstanceOf[FhirPathString].s)
  }

}

object FhirPathIdentityServiceFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix:String = "idxs"
  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathIdentityServiceFunctions(context)
}
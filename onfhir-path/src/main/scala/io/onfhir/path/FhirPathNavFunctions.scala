package io.onfhir.path

import io.onfhir.path.annotation.FhirPathFunction
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext

/**
 * Default FHIR Path function library for onFhir to navigate through FHIR content
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathNavFunctions(context:FhirPathEnvironment, current:Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {

  /**
   *  If the current expression returns Nil, then evaluates the else expression from start, otherwise return current
   * @param elseExpr
   * @return
   */
  @FhirPathFunction(documentation = "If the current expression returns Nil, then evaluates the else expression from start, otherwise return current. Ex: startTime.nav:orElse(plannedTime)",
    insertText = "nav:orElse(<expression>)",detail = "nav", label = "nav:orElse")
  def orElse(elseExpr:ExpressionContext):Seq[FhirPathResult] = {
    current match {
      case Nil =>
        val elsePart = new FhirPathExpressionEvaluator(context, context._this).visit(elseExpr)
        elsePart
      case oth => oth
    }
  }
}

object FhirPathNavFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix:String = "nav"
  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathNavFunctions(context, current)
}
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC If the current expression returns Nil, then evaluates the else expression from start, otherwise returns the current.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`elseExpr`**  \nThe else expression to be evaluated if the current expression returns Nil.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \nCurrent expression if it is not Nil, else expression otherwise.\n\n\uD83D\uDCA1 **E.g.** startTime.nav:orElse(plannedTime)",
    insertText = "nav:orElse(<expression>)",detail = "nav", label = "nav:orElse", kind = "Method", returnType = Seq(), inputType = Seq())
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
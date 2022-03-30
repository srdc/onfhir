package io.onfhir.path

import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext

/**
 * Default FHIR Path function library for onFhir for terminology handling
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathTerminologyFunctions(context:FhirPathEnvironment, current:Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {
  /**
   * For FHIR coding elements, filter the ones that match the given coding list as FHIR code query
   * @param codingList  Expression that evaluates to Seq[FhirPathString] in FHIR token query format [system]|[code]
   * @return
   */
  def isCodingIn(codingList:ExpressionContext):Seq[FhirPathResult] = {
    val systemAndCodes:Seq[(Option[String], Option[String])] = new FhirPathExpressionEvaluator(context, current).visit(codingList) match {
      case s:Seq[_] if s.forall(i =>i.isInstanceOf[FhirPathString]) =>
        s
          .map(_.asInstanceOf[FhirPathString])
          .map(i =>
            try {
              FHIRUtil.parseTokenValue(i.s)
            } catch {
              case t:Throwable => throw new FhirPathException(s"Invalid function call 'filterCodings', first expression ${codingList.getText} does not evaluate to sequence of FHIR Path Strings in FHIR token query format!", Some(t))
            }
          )
      case _ => throw new FhirPathException(s"Invalid function call 'filterCodings', first expression ${codingList.getText} does not evaluate to sequence of FHIR Path Strings!")
    }

    val codingMatch =
      current
        .filter(_.isInstanceOf[FhirPathComplex])
        .map(_.asInstanceOf[FhirPathComplex])
        .exists(coding =>
          systemAndCodes
            .exists {
              // |code query
              case (Some(""), Some(c)) =>
                FHIRUtil.extractValueOption[String](coding.json, "system").isEmpty &&
                  FHIRUtil.extractValueOption[String](coding.json, "code").contains(c)
              // system|code
              case (Some(s), Some(c)) =>
                FHIRUtil.extractValueOption[String](coding.json, "system").contains(s) &&
                  FHIRUtil.extractValueOption[String](coding.json, "code").contains(c)
              // code
              case (None, Some(c)) =>
                FHIRUtil.extractValueOption[String](coding.json, "code").contains(c)
              // system|
              case (Some(s), None) =>
                FHIRUtil.extractValueOption[String](coding.json, "system").contains(s)
              case _ =>
                throw new FhirPathException("Invalid given system codes pairings!")
            }
        )

    Seq(FhirPathBoolean(codingMatch))
  }
}

object FhirPathTerminologyFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix:String = "trm"
  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathTerminologyFunctions(context, current)
}
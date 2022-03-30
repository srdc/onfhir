package io.onfhir.path

import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext

import java.lang.reflect.InvocationTargetException

/**
 * Abstract class to implement for external function libraries
 */
abstract class AbstractFhirPathFunctionLibrary {
  /**
   * Return function signatures (name and number of parameters)
   * @return
   */
  def getFunctionSignatures():Seq[(String, Int)] = getClass.getMethods.filterNot(_.getName.startsWith("$anonfun$")).map(m => m.getName -> m.getParameterCount).toSeq
  /**
   * Method that will be used to call a FHIR Path function from the function library
   * Function library should define the function handlers as public methods with the name of the function that gets 0 or more ExpressionContext as parameters and return Seq of FhirPathResult
   *
   * @param fname   Name of the function
   * @param params  Supplied parameters
   * @return
   */
  @throws[FhirPathException]
  def callFhirPathFunction(fname:String, params:Seq[ExpressionContext]):Seq[FhirPathResult] = {
    try {
      val method = getClass.getMethod(fname, params.map(_ => classOf[ExpressionContext]): _*)
      val result = method.invoke(this, params:_*)
      val fhirPathResult = result.asInstanceOf[Seq[FhirPathResult]]
      fhirPathResult
    } catch {
      case n:NoSuchMethodException =>
        throw new FhirPathException(s"Invalid FHIR Path function call, function $fname does not exist or take ${params.length} arguments !!!")
      case ite:InvocationTargetException =>
        ite.getTargetException match {
          case fpe:FhirPathException => throw fpe
          case e:Throwable => throw FhirPathException.apply(s"Invalid FHIR Path function call $fname!", e)
        }
    }
  }
}

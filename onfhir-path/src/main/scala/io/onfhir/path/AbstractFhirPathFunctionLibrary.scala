package io.onfhir.path

import io.onfhir.api.{FHIR_DATA_TYPES, FHIR_PARAMETER_TYPES}
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext

import java.lang.reflect.InvocationTargetException
import io.onfhir.path.annotation.{FhirPathFunction, FhirPathFunctionDocumentation, FhirPathFunctionParameter, FhirPathFunctionReturn}

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

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

  /**
   * Returns documentations of functions in this library. It searches for the methods having FhirPathFunction annotation
   * and returns them.
   *
   * @return a list of FhirPathFunction representing the documentation of functions
   * */
  def getFunctionDocumentation():Seq[FhirPathFunction] = {
    var currentFunctionDocumentationField: FhirPathFunctionDocumentation = null

    // Using reflection to inspect the methods annotated with @FhirPathFunction
    currentMirror.classSymbol(Class.forName(getClass.getName))
      .toType
      .decls
      // filter the methods having FhirPathFunction annotation
      .filter(symbol => {
        symbol.isMethod && symbol.annotations.exists(_.tree.tpe =:= typeOf[FhirPathFunction])
      })
      .map(method => {
        // retrieve annotation fields
        val annotationFields = method.annotations
          .find(_.tree.tpe =:= typeOf[FhirPathFunction]).head
          .tree.children.tail
          .collect({
            // Extract documentation details from the annotation
            case field if field.tpe.toString.contentEquals("io.onfhir.path.annotation.FhirPathFunctionDocumentation") =>
              currentFunctionDocumentationField = getFhirPathDocumentation(field);

            // matches 'String' fields
            case Literal(Constant(s: String)) => s

            // matches 'Seq[String]' fields
            case Apply(_: Tree, args: List[Tree]) =>
              args.collect({
                // matches 'String's in the sequence
                case Literal(Constant(s: String)) => s

                case rest =>
                  // matches 'FHIR_DATA_TYPES' and 'FHIR_PARAMETER_TYPES' fields
                  getFhirDataTypeValue(rest.toString())
              })
          })
        // create an instance of FhirPathFunction
        new FhirPathFunction(documentation = currentFunctionDocumentationField,
          insertText = annotationFields.lift(1).get.toString,
          detail = annotationFields.lift(2).get.toString,
          label = annotationFields.lift(3).get.toString,
          kind = annotationFields.lift(4).get.toString,
          returnType = annotationFields.lift(5).get.asInstanceOf[Seq[String]],
          inputType = annotationFields.lift(6).get.asInstanceOf[Seq[String]])
      }).toSeq
  }

  /**
   * Retrieves the corresponding value of a FHIR data type from its string representation.
   * This function uses reflection to access fields in the FHIR_DATA_TYPES/FHIR_PARAMETER_TYPES class dynamically.
   * Because the values of FHIR_DATA_TYPES/FHIR_PARAMETER_TYPES that comes from ToFHIR's api usage are not accessible otherwise.
   * @param fhirDataTypeString The string representation of the FHIR data type.
   * @return An Option containing the string value of the FHIR data type if found, or None if not.
   */
  private def getFhirDataTypeValue(fhirDataTypeString: String): Option[String] = {
    // Regular expression to extract the object type and method name from the reference string
    val pattern = """.*(FHIR_DATA_TYPES|FHIR_PARAMETER_TYPES)\.(\w+)""".r

    // Match the input string against the pattern to find the corresponding object and type
    fhirDataTypeString match {
      case pattern(objectType, methodName) =>
        try {
          // Resolve the object based on the prefix
          val targetObject = objectType match {
            case "FHIR_DATA_TYPES" => FHIR_DATA_TYPES
            case "FHIR_PARAMETER_TYPES" => FHIR_PARAMETER_TYPES
          }
          // Use reflection to invoke the method corresponding to the type name
          val value = targetObject.getClass.getMethod(methodName).invoke(targetObject)
          Some(value.asInstanceOf[String]) // Return the value as an Option
        } catch {
          case _: Exception => None // Return None if any exception occurs during reflection
        }

      case _ =>
        None // Return None if the input string does not match the expected pattern
    }
  }

  /**
   * Parses the annotation syntax tree to extract the documentation for a FHIR path function.
   * This method processes the fields of the annotation to populate the details, warnings, parameters,
   * return value, and examples for the function's documentation.
   *
   * @param annotationSyntaxTree The syntax tree of the annotation from which the documentation is extracted.
   * @return An instance of FhirPathFunctionDocumentation populated with the extracted fields.
   */
  def getFhirPathDocumentation(annotationSyntaxTree: Tree): FhirPathFunctionDocumentation = {
    // Initializing variables to store extracted information
    var detail: String = ""
    var warnings: Option[Seq[String]] = None
    var parameters: Option[Seq[FhirPathFunctionParameter]] = None
    var returnValue: FhirPathFunctionReturn = FhirPathFunctionReturn(None, Seq())
    var examples: Seq[String] = Seq()

    annotationSyntaxTree match {
      case Apply(_: Tree, args: List[Tree]) =>
        // Extract the 'detail' field
        args.lift(0).foreach {
          case Literal(Constant(s: String)) => detail = s
        }

        // Extract the 'warnings' field
        args.lift(1).foreach {
          case Apply(_, warningsArgs: List[Tree]) =>
            val extractedWarnings = warningsArgs.head.collect {
              case Literal(Constant(warning: String)) => warning
            }
            warnings = if (extractedWarnings.nonEmpty) Some(extractedWarnings) else None
          case _ => // Do nothing
        }

        // Extract the 'parameters' field
        args.lift(2).foreach {
          case Apply(_: Tree, parametersArgs: List[Tree]) =>
            parametersArgs.foreach({
              case Apply(_: Tree, args: List[Tree]) =>
                parameters = readFhirPathFunctionParameter(args)
              case _ => // Do nothing
            })
          case _ => // Do nothing
        }

        // Extract the 'returnValue' field
        args.lift(3).foreach {
          case Apply(_: Tree, returnValueArgs: List[Tree]) =>
            returnValue = readFhirPathFunctionReturn(returnValueArgs)
          case _ => // Do nothing
        }

        // Extract 'examples' field
        args.lift(4).foreach {
          case Apply(_: Tree, examplesArgs: List[Tree]) =>
            examples = examplesArgs.collect {
              case Literal(Constant(example: String)) => example
            }
        }

      case _ => // Do nothing
    }

    // Return the populated FhirPathFunctionDocumentation object
    FhirPathFunctionDocumentation(
      detail = detail,
      usageWarnings = warnings,
      parameters = parameters,
      returnValue = returnValue,
      examples = examples
    )
  }

  /**
   * Parses a list of trees to extract parameters for a FHIR path function.
   * This method processes each field in the list, extracting the name, detail, and examples for each parameter.
   *
   * @param parameterSyntaxTreeList A list of trees representing the fields of the parameter annotation.
   * @return An `Option` containing a sequence of `FhirPathFunctionParameter` objects if parameters are found,
   *         otherwise `None`.
   */
  private def readFhirPathFunctionParameter(parameterSyntaxTreeList: List[Tree]): Option[Seq[FhirPathFunctionParameter]] = {
    // Process each field in the list
    val parameters = parameterSyntaxTreeList.collect {
      case Apply(_: Tree, args: List[Tree]) =>
        // Extract values from `args`
        val name = args.headOption.collect {
          case Literal(Constant(value: String)) => value
        }.getOrElse("")

        val detail = args.lift(1).collect {
          case Literal(Constant(value: String)) => value
        }.getOrElse("")

        val examples = args.lift(2) match {
          case Some(Apply(_: Tree, exampleArgs: List[Tree])) =>
            exampleArgs.collect {
              case Literal(Constant(value: String)) => value
            }
          case _ => Seq.empty
        }

        // Construct the parameter object
        FhirPathFunctionParameter(name, detail, if (examples.nonEmpty) Some(examples) else None)
    }

    // Return the collected parameters wrapped in an Option
    if (parameters.nonEmpty) Some(parameters) else None
  }

  /**
   * Extracts the return value details for a FHIR path function from a list of trees.
   * This method processes the list of fields, extracting the `detail` and `examples` for the return value.
   * If no valid details are found, it defaults to an empty return value.
   *
   * @param returnValueSyntaxTreeList A list of trees representing the fields of the return annotation.
   * @return An instance of `FhirPathFunctionReturn` containing the extracted `detail` and `examples`.
   */
  private def readFhirPathFunctionReturn(returnValueSyntaxTreeList: List[Tree]): FhirPathFunctionReturn = {
    // Extract the `detail` and `examples`
    val detail = returnValueSyntaxTreeList.headOption match {
      case Some(Literal(Constant(detail: String))) => Some(detail)
      case _ => None
    }

    val examples = returnValueSyntaxTreeList.lift(1) match {
      case Some(Apply(_: Tree, exampleArgs: List[Tree])) =>
        exampleArgs.collect {
          case Literal(Constant(example: String)) => example
        }
      case _ => Seq.empty
    }

    FhirPathFunctionReturn(detail, examples)
  }
}

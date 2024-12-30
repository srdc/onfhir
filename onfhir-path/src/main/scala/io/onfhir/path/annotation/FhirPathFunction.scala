package io.onfhir.path.annotation

import scala.annotation.StaticAnnotation

/**
 * Custom annotation to mark a method as FhirPath function. It provides a documentation of the function which can be used
 * by a code editor to suggest available functions to the user.
 *
 * @param documentation Detailed documentation about the function, including parameters, return information, and examples.
 * @param insertText    A string or snippet that should be inserted in a document when selecting this completion.
 * @param detail        The library prefix to which FhirPath function belongs.
 * @param label         The label (i.e. name) of function.
 * @param kind          The type of function. It can be one of the followings:
 *                        - Method: FhirPath functions that operate on a collection of values (i.e. input collection) such as name.exists()
 *                        - Function: FhirPath functions that do not need an input collection such as today()
 * @param returnType    Possible return types of a function / method. It could be an empty sequence indicating that it returns a collection of any data types.
 * @param inputType     Possible input types of a method. It could be an empty sequence indicating that it accepts a collection of any data types.
 *                      For functions (i.e. the ones whose kind are 'Function'), it should be an empty sequence because they do not need any input collection to run.
 */
class FhirPathFunction(val documentation: FhirPathFunctionDocumentation,
                       val insertText: String,
                       val detail: String,
                       val label: String,
                       val kind: String,
                       val returnType: Seq[String],
                       val inputType: Seq[String]
) extends StaticAnnotation

/**
 * Documentation details for a FhirPath function, including parameters, return information, and examples.
 *
 * To add a JSON as an example, use the following syntax:
 * - Wrap your JSON in triple quotation marks with the "json" language identifier for proper formatting:
 *   """<JSON>[{"reference":"Observation/123"},{"reference":"Observation/456"}]"""
 * - Note: JSON formatting is supported only for parameters and return value examples. <JSON> tag will not affect other fields.
 *
 * @param detail     A detailed explanation of the function, its behavior, or its purpose.
 * @param usageWarnings A collection of warnings or notes regarding the function's usage. These might include
 *                      constraints, edge cases, or common pitfalls to avoid when using the function.
 * @param parameters The parameters that the function accepts, each with its own explanation and examples.
 * @param returnValue Information about the return type of the function, including its description and examples.
 * @param examples   Examples illustrating how to use the function in practice.
 */
case class FhirPathFunctionDocumentation(
                                          detail: String,
                                          usageWarnings: Option[Seq[String]],
                                          parameters: Option[Seq[FhirPathFunctionParameter]],
                                          returnValue: FhirPathFunctionReturn,
                                          examples: Seq[String]
                                        )

/**
 * Parameter information for a FhirPath function.
 *
 * @param name     The name of the parameter.
 * @param detail   A description of the parameter, explaining its purpose and how it is used.
 * @param examples Examples illustrating valid values or usage for the parameter.
 */
case class FhirPathFunctionParameter(
                                      name: String,
                                      detail: String,
                                      examples: Option[Seq[String]]
                                    )

/**
 * Information about the return type of a FhirPath function.
 *
 * @param detail   A description of the return value, explaining what the function returns.
 * @param examples Examples illustrating the structure or content of the return value.
 */
case class FhirPathFunctionReturn(
                                   detail: Option[String],
                                   examples: Seq[String]
                                 )
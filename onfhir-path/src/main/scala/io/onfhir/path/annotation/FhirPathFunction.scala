package io.onfhir.path.annotation

import scala.annotation.StaticAnnotation

/**
 * Custom annotation to mark a method as FhirPath function. It provides a documentation of the function which can be used
 * by a code editor to suggest available functions to the user.
 *
 * @param documentation A human-readable string that represents a doc-comment.
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
class FhirPathFunction(val documentation: String,
                       val insertText: String,
                       val detail: String,
                       val label: String,
                       val kind: String,
                       val returnType: Seq[String],
                       val inputType: Seq[String]
) extends StaticAnnotation

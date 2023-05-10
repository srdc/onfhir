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
 */
class FhirPathFunction(val documentation: String, val insertText: String, val detail: String, val label: String, val kind: String) extends StaticAnnotation

package io.onfhir.mapping

package object model {

  /**
   * Codes used for transform operations (http://hl7.org/fhir/map-transform)
   */
  object TransformOperations {
    val CREATE = "create"
    val COPY = "copy"
    val TRUNCATE = "truncate"
    val ESCAPE = "escape"
    val CAST = "cast"
    val APPEND = "append"
    val TRANSLATE = "translate"
    val REFERENCE = "reference"
    val DATE_OP = "dateOp"
    val UUID = "uuid"
    val POINTER = "pointer"
    val EVALUATE = "evaluate"
    val CREATE_CODEABLE_CONCEPT = "cc"
    val CREATE_CODING  = "c"
    val CREATE_QUANTITY = "qty"
    val CREATE_IDENTIFIER = "id"
    val CREATE_CONTACT_DETAILS = "cp"
  }

  object ListModes {
    val FIRST = "first"
    val NOT_FIRST = "not_first"
    val LAST = "last"
    val NOT_LAST = "not_last"
    val ONLY_ONE = "only_one"
    val SHARE = "share"
    val COLLATE = "collate"
  }

}

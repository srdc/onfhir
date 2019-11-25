package io.onfhir.api.validation

import org.json4s.JsonAST.JValue

//Type of FHIR constraints
object ConstraintKeys {
  val MIN = "min"
  val MAX = "max"
  val MINVALUE = "minValue"
  val MAXVALUE = "maxValue"
  val ARRAY = "array"
  val BINDING = "binding"
  val CONSTRAINT = "constraint"
  val MAXLENGTH = "maxLength"
  val PATTERN = "pattern"
  val REFERENCE_TARGET = "target"
}

/**
  *
  * @param errorOrWarningMessage Message if it is a warning or failure
  * @param isWarning If warning or not
  */
case class ConstraintFailure(errorOrWarningMessage:String, isWarning:Boolean = false)

/**
  * A FHIR StructureDefinition (Profile or base definition for resource types or data types)
  * @param url                  URL of the profile
  * @param baseUrl              Base profile that this extends if exist
  * @param elementRestrictions  Defined restrictions on elements with path as key e.g. component.value[x] -> ...
  * @param constraints          Root constraints defined for the content
  * @param isAbstract           If this is a abstract definition
  */
case class ProfileRestrictions(
                                url:String,
                                baseUrl:Option[String],
                                elementRestrictions:Seq[(String, ElementRestrictions)],
                                constraints:Option[FhirRestriction] = None,
                                isAbstract:Boolean = false)

/**
  * Restrictions defined on a element
  * @param path                     Path for the element e.g. Observation.component[x] --> component[x], Observation.status --> status
  * @param dataTypes                Defined data types and profiles for this field
  * @param restrictions             FHIR Restrictions for different categories
  * @param slicing                  FHIR slicing if defined
  * @param sliceName                Name of the slice if this is the element restriction for a slice e.g. Observation.component:m1 -> m1
  * @param profileDefinedIn         Profile url that this element restriction is defined (used for validation result building)
  */
case class ElementRestrictions(path:String, dataTypes: Seq[(String, Seq[String])], restrictions:Map[String, FhirRestriction], slicing:Option[FhirSlicing] = None, sliceName:Option[String], profileDefinedIn:Option[String] = None) {

}

/**
  * Fhir Slicing definition
  * @param discriminators Discriminator type and path
  * @param ordered        If the elements of slices are ordered
  * @param rule           Rule for slicing (closed | open | openAtEnd)
  */
case class FhirSlicing(discriminators:Seq[(String, String)], ordered:Boolean, rule:String)


/**
 * Abstract restriction defined for elements or profiles
 */
trait FhirRestriction {
  /**
   * Evaluate a restriction on given value
   * @param value Json value
   * @return
   */
  def evaluate(value:JValue):Seq[ConstraintFailure]
}

/**
 * Compact ValueSet definition for validation
 * @param includes  Included codes
 * @param excludes  Excluded codes
 */
case class ValueSetRestrictions(includes:ValueSetDef, excludes:Option[ValueSetDef] = None)

/**
 * Defines the contents of a value set inclusion or exclusion
 * @param codes       Listed codes in a CodeSystem
 * @param valueSets   Select the contents of these value sets
 */
case class ValueSetDef(codes:Map[String, Set[String]], valueSets:Set[String] = Set.empty[String])
package io.onfhir.api.validation

import io.onfhir.api.model.InternalEntity
import org.json4s.JsonAST.JValue

//Type of FHIR constraints
object ConstraintKeys {
  val MIN = 0
  val MAX = 1
  val ARRAY = 2
  val DATATYPE= 3
  val MINVALUE = 4
  val MAXVALUE = 5
  val BINDING = 6
  val CONSTRAINT = 7
  val MAXLENGTH = 8
  val PATTERN = 9
  val REFERENCE_TARGET = 10
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
  * @param summaryElements      Paths of the elements that are defined as summary element
  * @param constraints          Root constraints defined for the content
  * @param isAbstract           If this is a abstract definition
  */
case class ProfileRestrictions(
                                url:String,
                                baseUrl:Option[String],
                                elementRestrictions:Seq[(String, ElementRestrictions)],
                                summaryElements:Set[String],
                                constraints:Option[FhirRestriction] = None,
                                isAbstract:Boolean = false) extends InternalEntity

/**
  * Restrictions defined on a element
  * @param path                     Path for the element e.g. Observation.component[x] --> component[x], Observation.status --> status
  * @param restrictions             FHIR Restrictions for different categories
  * @param slicing                  FHIR slicing if defined
  * @param sliceName                Name of the slice if this is the element restriction for a slice e.g. Observation.component:m1 -> m1
  * @param profileDefinedIn         Profile url that this element restriction is defined (used for validation result building)
  */
case class ElementRestrictions(path:String, restrictions:Map[Int, FhirRestriction], slicing:Option[FhirSlicing] = None, sliceName:Option[String], contentReference:Option[String],profileDefinedIn:Option[String] = None)

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
  def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure]

  /**
   * Check if a element is matching with the restriction (used for slice matching)
   * @param value
   * @param fhirContentValidator
   * @return
   */
  def matches(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Boolean = {
    evaluate(value, fhirContentValidator).forall(_.isWarning)
  }
}

/**
 * Compact ValueSet definition for validation
 * @param includes  Included codes
 * @param excludes  Excluded codes
 */
case class ValueSetRestrictions(includes:ValueSetDef, excludes:Option[ValueSetDef] = None) extends InternalEntity

/**
 * Defines the contents of a value set inclusion or exclusion
 * @param codes       Listed codes in a CodeSystem
 * @param valueSets   Select the contents of these value sets
 */
case class ValueSetDef(codes:Map[String, Set[String]], valueSets:Set[String] = Set.empty[String])
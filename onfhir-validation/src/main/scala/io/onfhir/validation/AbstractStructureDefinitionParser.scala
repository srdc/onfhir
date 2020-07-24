package io.onfhir.validation

import io.onfhir.api.{Resource}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.util.FHIRUtil.decapitilize
import io.onfhir.api.validation.{ElementRestrictions, FhirRestriction, FhirSlicing, ProfileRestrictions}
import io.onfhir.path.FhirPathEvaluator
import org.json4s.JsonAST.{JObject, JValue}

abstract class AbstractStructureDefinitionParser(fhirComplexTypes:Set[String], fhirPrimitiveTypes:Set[String]) {
  /**
   * Parse a StructureDefinition resource
   * @param structureDef
   * @return
   */
  def parseProfile(structureDef:Resource):ProfileRestrictions
  /**
   * Parse a element definition and construct a set of restrictions for the element
   * @param elemDef       JSON object for Element Definition
   * @param resourceType  Resource type that this element is defined
   * @param profileUrl    URL of the profile that this element is defined (if not exist, it means base profile)
   * @return
   */
  def parseElementDef(elemDef:JObject, resourceType:String, profileUrl:Option[String]):(ElementRestrictions, Boolean)


  /**
   * Create a cardinality min restriction from int value
   * @param n
   * @return
   */
  protected def createMinRestriction(n:Int):Option[FhirRestriction] = {
    n match {
      case 0 => None
      case i => Some(CardinalityMinRestriction(i))
    }
  }

  /**
   * Create a max cardinality restriction from string n value
   * @param n
   * @return
   */
  protected def createMaxRestriction(n:String):Option[FhirRestriction]  = {
    n match {
      case "*" => None
      case i => Some(CardinalityMaxRestriction(i.toInt))
    }
  }

  /**
   * Create a array restriction (element should be an array or not)
   * @param isBase
   * @param n
   * @return
   */
  protected def createArrayRestriction(isBase:Boolean, n:Option[String]):Option[FhirRestriction] = {
    if(isBase && n.exists(m=> m  == "*" || m.toInt > 1)) Some(ArrayRestriction()) else None
  }

  /**
   * Create Code binding restriction
   * @param bindingDef
   * @return
   */
  protected def createBindingRestriction(bindingDef:JObject):Option[FhirRestriction] = {
    val bindingStrength = FHIRUtil.extractValueOption[String](bindingDef, "strength").get
    if(bindingStrength == "required" || bindingStrength == "extensible" || bindingStrength == "preferred"){
      FHIRUtil.extractValueOption[String](bindingDef, "valueSet")
        .map(v => FHIRUtil.parseCanonicalValue(v))
        .map { case (vsUrl, version) => CodeBindingRestriction(vsUrl, version, bindingStrength == "required")}
    } else None
  }

  /**
   * Create Min value or max value restriction
   * @param dataType
   * @param value
   * @param isMin
   * @return
   */
  protected def createMinMaxValueRestriction(dataType:String, value:JValue, isMin:Boolean):FhirRestriction = {
    MinMaxValueRestriction(value, isMin)
  }

  /**
   * Create fixed or pattern restriction
   * @param dataType
   * @param fixedOrPatternValue
   * @param isFixed
   * @return
   */
  protected def createFixedPatternRestriction(dataType:String, fixedOrPatternValue:JValue, isFixed:Boolean) = {
    FixedOrPatternRestriction(fixedOrPatternValue,  isFixed)
  }

  /**
   * Parse the Constraint definition within the element definitions of FHIR
   * @param constraintDef parsed json definition content
   * @return
   */
  protected def parseConstraint(constraintDef: JObject):Option[FhirConstraint] = {
    FHIRUtil.extractValueOption[String](constraintDef, "expression")
      .flatMap(expression =>
        expression match {
          //This is not a FHIR path expression, but they use it for xhtml type
          //TODO implement this as a function
          case "htmlChecks()" => None
          //Go on
          case _ => FHIRUtil.extractValue[String](constraintDef, "key") match {
            //This is a common constraint that forces elements to have childs (we already check it)
            case "ele-1" => None
            case ckey =>
              Some(FhirConstraint(
                key = ckey,
                desc = FHIRUtil.extractValue[String](constraintDef, "human"),
                expr = FhirPathEvaluator().parse(expression),
                isWarning = FHIRUtil.extractValueOption[String](constraintDef, "severity").get == "warning"
              ))
          }
        }
      )
  }

  /**
   * Parse slicing definition
   * @param slicing JSON content of slicing def
   * @return
   */
  protected def parseSlicing(slicing: JObject):Option[FhirSlicing] = {
    val isOrdered = FHIRUtil.extractValueOption[Boolean](slicing, "ordered").getOrElse(false)
    val discr= FHIRUtil.extractValueOption[Seq[JObject]](slicing, "discriminator").getOrElse(Nil)
    if(isOrdered || discr.nonEmpty)
      Some(FhirSlicing(
        discriminators = discr.map(d => FHIRUtil.extractValueOption[String](d, "type").get -> FHIRUtil.extractValueOption[String](d, "path").get),
        ordered = isOrdered,
        rule =  FHIRUtil.extractValueOption[String](slicing, "rules").get
      ))
    else
      None
  }

  /**
   * In FHIR, some elements may have multiple types in which case the data type is appended to element name for JSON serialization to indicate the data type
   * This method is to find such an element, and its data type
   * @param field     Field name e.g. Observation.value[x] --> value
   * @param content   JSON content to search the field
   * @return  Actual field name, data type and the JSON content of the found field e.g. valueQuantity, Quantity, ...
   */
  protected def findElementWithMultipleFhirTypes(field:String, content:JObject):Option[(String, String, JValue)] = {
    content.obj
      .find(f => f._1.startsWith(field)) //Find the field starts with the given field name
      .map(f => (f._1, f._1.replace(field, ""), f._2))  //Extract the data type field
      .flatMap(f =>
        if (fhirComplexTypes.contains(f._2))
          Some(f)
        else if (fhirPrimitiveTypes.contains(decapitilize(f._2)))
          Some(f._1, decapitilize(f._2), f._3) //if it is a primitive, decapitilize it
        else
          None
      )
  }
}

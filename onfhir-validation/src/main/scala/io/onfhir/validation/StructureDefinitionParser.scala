package io.onfhir.validation

import java.io.{File, FileInputStream, InputStream, InputStreamReader}
import java.util.zip.{ZipEntry, ZipInputStream}

import io.onfhir.api.{FHIR_DATA_TYPES, FHIR_ROOT_URL_FOR_DEFINITIONS, Resource, validation}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintKeys, ElementRestrictions, FhirRestriction, FhirSlicing, ProfileRestrictions}
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.InitializationException
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.util.OnFhirZipInputStream
import org.apache.commons.io.input.BOMInputStream
import org.json4s.JsonAST.{JObject, JValue}
import org.json4s.jackson.JsonMethods
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

object StructureDefinitionParser {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
   *
   * @param structureDef Parsed FHIR StructureDefinition
   * @return
   */
  def parseProfile(structureDef:Resource):Option[ProfileRestrictions] = {
    //Get resource type
    val rtype =  FHIRUtil.extractValueOption[String](structureDef, "type").get
    //Do not get primitive type definitions
    if(rtype.apply(0).isLower){
      None
    } else {
      val profileUrl = FHIRUtil.extractValueOption[String](structureDef, "url").get
      //Get the element definitions
      var elementDefs =
        FHIRUtil
          .extractValueOptionByPath[Seq[JObject]](structureDef, s"differential.element")
          .getOrElse(Nil)
      //First element definition generally provide constraints about resource type
      val baseResourceElemDefinition =
        elementDefs
          .find(e => FHIRUtil.extractValueOption[String](e, "path").get == rtype)

      //If resource constraints are defined in the first element, drop it
      if (baseResourceElemDefinition.isDefined)
        elementDefs = elementDefs.drop(1)

      //Parse element definitions (without establishing child relationship)
      val elemDefs =
        elementDefs
        .map(parseElementDef(_, rtype, if(profileUrl.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS)) None else Some(profileUrl))) //Parse the element definitions
        .map(e => e.path -> e)


      Some(validation.ProfileRestrictions(
        url = FHIRUtil.extractValueOption[String](structureDef, "url").get,
        baseUrl = FHIRUtil.extractValueOption[String](structureDef, "baseDefinition"),
        elementRestrictions = elemDefs,
        constraints =
          baseResourceElemDefinition
            .flatMap(e =>
              FHIRUtil.extractValue[Seq[JObject]](e, "constraint") match {
                case Nil => None
                case cs =>  Some(ConstraintsRestriction(cs.flatMap(parseConstraint)))//Parse the constraint definition
              } //Get constraint elements
            ),
        isAbstract = FHIRUtil.extractValueOption[Boolean](structureDef, "abstract").get
      ))
    }
  }

  /**
   * Parse FHIR Element definition to generate our internal model to keep restrictions on element
   * @param elemDef       FHIR Element definition to parse
   * @param profileUrl    URL of the profile that this element definition is defined (If not FHIR base)
   * @return
   */
  private def parseElementDef(elemDef:JObject, resourceType:String, profileUrl:Option[String]):ElementRestrictions = {
    val dataTypeAndProfile =
      FHIRUtil
        .extractValueOption[Seq[JObject]](elemDef, "type")
        .getOrElse(Nil)
        .map(typeDef =>
          (
            FHIRUtil.extractValue[String](typeDef, "code") match {
              case "http://hl7.org/fhirpath/System.String" => "string" // Some base definitions have these
              case oth => oth
            },
            FHIRUtil.extractValue[Seq[String]](typeDef, "profile"),
            FHIRUtil.extractValue[Seq[String]](typeDef, "targetProfile"),
            FHIRUtil.extractValueOption[String](typeDef, "versioning"),
            FHIRUtil.extractValue[Seq[String]](typeDef, "aggregation")
          )
        )

    ElementRestrictions(
      path = FHIRUtil.extractValueOption[String](elemDef, "id").get.dropWhile( _ != '.').drop(1),
      restrictions =
        Seq(
          ConstraintKeys.DATATYPE -> (if(dataTypeAndProfile.isEmpty) None else Some(TypeRestriction(dataTypeAndProfile.map(dt => dt._1 -> dt._2)))),
          ConstraintKeys.MIN -> FHIRUtil.extractValueOption[Int](elemDef, "min").flatMap(createMinRestriction),
          ConstraintKeys.MAX -> FHIRUtil.extractValueOption[String](elemDef, "max").flatMap(createMaxRestriction),
          ConstraintKeys.ARRAY -> createArrayRestriction(profileUrl.isEmpty,  FHIRUtil.extractValueOption[String](elemDef, "max")),
          ConstraintKeys.BINDING -> FHIRUtil.extractValueOption[JObject](elemDef, "binding").flatMap(createBindingRestriction),
          ConstraintKeys.MINVALUE ->
            FHIRUtil.findElementWithMultipleFhirTypes("minValue", elemDef)
              .map(f => createMinMaxValueRestriction(f._2, f._3, isMin = true)),
          ConstraintKeys.MINVALUE ->
            FHIRUtil.findElementWithMultipleFhirTypes("maxValue", elemDef)
              .map(f => createMinMaxValueRestriction(f._2, f._3, isMin = false)),
          ConstraintKeys.PATTERN ->
            (
              FHIRUtil.findElementWithMultipleFhirTypes("fixed", elemDef) match {
                case Some((_, dt, v)) =>  Some(createFixedPatternRestriction(dt, v, isFixed = true))
                case None =>
                  FHIRUtil.findElementWithMultipleFhirTypes("pattern", elemDef)
                    .map(f => createFixedPatternRestriction(f._2, f._3, isFixed = false))
              }
            ),
          ConstraintKeys.MAXLENGTH ->
            FHIRUtil.extractValueOption[Int](elemDef, "maxLength")
              .map(l => MaxLengthRestriction(l)),
          ConstraintKeys.CONSTRAINT ->
            (FHIRUtil.extractValueOption[Seq[JObject]](elemDef, "constraint").getOrElse(Nil)
              .flatMap(c => parseConstraint(c)) match {
                case Nil => None
                case constraints => Some(ConstraintsRestriction(constraints))
            }),
          ConstraintKeys.REFERENCE_TARGET ->
            dataTypeAndProfile
              .find(_._1 == FHIR_DATA_TYPES.REFERENCE)
              .map(dt => (dt._3, dt._4, dt._5))
              .map {
                case (targetProfiles, versioning, aggregation) =>
                  ReferenceRestrictions(targetProfiles,
                    versioning.map {
                      case "specific" => true
                      case "independent" => false
                    },
                    aggregation
                )
              }
        )
        .filter(_._2.isDefined).map(r => r._1 -> r._2.get)
        .toMap,
      slicing = FHIRUtil.extractValueOption[JObject](elemDef, "slicing")
                  .flatMap(s => parseSlicing(s)),
      sliceName = FHIRUtil.extractValueOption[String](elemDef, "sliceName"),
      contentReference =
        FHIRUtil.extractValueOption[String](elemDef, "contentReference")
          .map(cr => cr.dropWhile( _ != '.').drop(1)),
      profileDefinedIn = profileUrl
    )
  }

  /**
    * Create a cardinality min restriction from int value
    * @param n
    * @return
    */
  private def createMinRestriction(n:Int):Option[FhirRestriction] = {
    n match {
      case 0 => None
      case i => Some(CardinalityMinRestriction(i))
    }
  }

  /**
    * Create a max cardinality restriction from string
    * @param n
    * @return
    */
  private def createMaxRestriction(n:String):Option[FhirRestriction]  = {
      n match {
        case "*" => None
        case i => Some(CardinalityMaxRestriction(i.toInt))
      }
  }

  /**
    * Create a array restriction
    * @param isBase
    * @param n
    * @return
    */
  private def createArrayRestriction(isBase:Boolean, n:Option[String]):Option[FhirRestriction] = {
    if(isBase && n.exists(m=> m  == "*" || m.toInt > 1)) Some(ArrayRestriction()) else None
  }

  /**
   * Create Binding restriction
   * @param bindingDef
   * @return
   */
  private def createBindingRestriction(bindingDef:JObject):Option[FhirRestriction] = {
    val bindingStrength = FHIRUtil.extractValueOption[String](bindingDef, "strength").get
    if(bindingStrength == "required" || bindingStrength == "extensible" || bindingStrength == "preferred"){
      FHIRUtil.extractValueOption[String](bindingDef, "valueSet")
        .map(v => FHIRUtil.parseCanonicalValue(v))
        .map { case (vsUrl, version) => CodeBindingRestriction(vsUrl, version, bindingStrength == "required")}
    } else None
  }

  private def createMinMaxValueRestriction(dataType:String, value:JValue, isMin:Boolean):FhirRestriction = {
    MinMaxValueRestriction(value, isMin)
  }

  private def createFixedPatternRestriction(dataType:String, fixedOrPatternValue:JValue, isFixed:Boolean) = {
    FixedOrPatternRestriction(fixedOrPatternValue,  isFixed)
  }

  /**
   * Parse the Constraint definition within the element definitions of FHIR
   * @param constraintDef parsed json definition content
   * @return
   */
  private def parseConstraint(constraintDef: JObject):Option[FhirConstraint] = {
    FHIRUtil.extractValueOption[String](constraintDef, "expression")
      .flatMap(expression =>
        expression match {
          //This is not a FHIR path expression, but they use it for xhtml type
          case "htmlChecks()" => None
          //Go on
          case _ => FHIRUtil.extractValue[String](constraintDef, "key") match {
            //This is a common constraint that forces elements to have childs (we already check it)
            case "ele-1" => None
            case ckey =>
              Some(FhirConstraint(
                key = ckey,
                desc = FHIRUtil.extractValue[String](constraintDef, "human"),
                expr = FhirPathEvaluator.parse(expression),
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
  private def parseSlicing(slicing: JObject):Option[FhirSlicing] = {
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

}

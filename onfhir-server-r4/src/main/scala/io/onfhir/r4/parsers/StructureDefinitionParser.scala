package io.onfhir.r4.parsers

import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation._
import io.onfhir.api.{FHIR_DATA_TYPES, FHIR_ROOT_URL_FOR_DEFINITIONS, Resource, validation}
import io.onfhir.validation.{AbstractStructureDefinitionParser, ConstraintsRestriction, MaxLengthRestriction, ReferenceRestrictions, TypeRestriction}
import org.json4s.JsonAST.{JObject, JValue}
import org.slf4j.{Logger, LoggerFactory}

/**
 * Parser for R4 StructureDefinition resources to convert them to our compact form
 */
class StructureDefinitionParser(fhirComplexTypes:Set[String], fhirPrimitiveTypes:Set[String]) extends AbstractStructureDefinitionParser(fhirComplexTypes, fhirPrimitiveTypes) {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
   *
   * @param structureDef Parsed FHIR StructureDefinition
   * @return
   */
  override def parseProfile(structureDef:Resource):ProfileRestrictions = {
    //Get resource type
    val rtype =  FHIRUtil.extractValueOption[String](structureDef, "type").get
    //Do not get primitive type definitions
    if(rtype.apply(0).isLower){
      validation.ProfileRestrictions(
        url = FHIRUtil.extractValueOption[String](structureDef, "url").get,
        baseUrl = None,
        elementRestrictions = Nil,
        summaryElements = Set.empty[String],
        constraints = None,
        isAbstract = FHIRUtil.extractValueOption[Boolean](structureDef, "abstract").get
      )
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


      validation.ProfileRestrictions(
        url = FHIRUtil.extractValueOption[String](structureDef, "url").get,
        baseUrl = FHIRUtil.extractValueOption[String](structureDef, "baseDefinition"),
        elementRestrictions = elemDefs.map(e => e._1.path -> e._1),
        summaryElements= elemDefs.filter(_._2).map(e => e._1.path).toSet,
        constraints =
          baseResourceElemDefinition
            .flatMap(e =>
              FHIRUtil.extractValue[Seq[JObject]](e, "constraint") match {
                case Nil => None
                case cs =>  Some(ConstraintsRestriction(cs.flatMap(parseConstraint)))//Parse the constraint definition
              } //Get constraint elements
            ),
        isAbstract = FHIRUtil.extractValueOption[Boolean](structureDef, "abstract").get
      )
    }
  }

  /**
   * Parse FHIR Element definition to generate our internal model to keep restrictions on element
   * @param elemDef       FHIR Element definition to parse
   * @param profileUrl    URL of the profile that this element definition is defined (If not FHIR base)
   * @return Parsed Definition and if element is a summary element
   */
  override def parseElementDef(elemDef:JObject, resourceType:String, profileUrl:Option[String]):(ElementRestrictions, Boolean) = {
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
            findElementWithMultipleFhirTypes("minValue", elemDef)
              .map(f => createMinMaxValueRestriction(f._2, f._3, isMin = true)),
          ConstraintKeys.MINVALUE ->
            findElementWithMultipleFhirTypes("maxValue", elemDef)
              .map(f => createMinMaxValueRestriction(f._2, f._3, isMin = false)),
          ConstraintKeys.PATTERN ->
            (
              findElementWithMultipleFhirTypes("fixed", elemDef) match {
                case Some((_, dt, v)) =>  Some(createFixedPatternRestriction(dt, v, isFixed = true))
                case None =>
                  findElementWithMultipleFhirTypes("pattern", elemDef)
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
    ) -> (FHIRUtil.extractValueOption[Boolean](elemDef, "isSummary").getOrElse(false))
  }


}

package io.onfhir.validation

import java.net.{URI, URL}

import io.onfhir.api
import io.onfhir.api.FHIR_DATA_TYPES
import io.onfhir.api.model.FHIRResponse.{OUTCOME_CODES, SEVERITY_CODES}
import io.onfhir.api.model.OutcomeIssue
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintFailure, ConstraintKeys, ElementRestrictions, FhirRestriction, FhirSlicing, ProfileRestrictions}
import io.onfhir.config.FhirConfigurationManager
import io.onfhir.exception.InitializationException
import io.onfhir.path.{FhirPathEvaluator, FhirPathValueTransformer}
import org.json4s.JInt
import org.json4s.JsonAST.{JArray, JBool, JDecimal, JDouble, JLong, JObject, JString, JValue}
import io.onfhir.util.JsonFormatter.formats
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.util.Try
import scala.util.matching.Regex

object FhirContentValidator {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /** Regular expressions for some primitive */
  private val ID_REGEX: Regex = """\A[A-Za-z0-9\-\.]{1,64}$""".r
  private val CODE_REGEX: Regex = """\A[^\s]+([\s]?[^\s]+)*$""".r
  private val DATE_REGEX: Regex = """\A-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1]))?)?$""".r
  private val DATETIME_REGEX: Regex = """\A-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)))?)?)?$""".r
  private val INSTANT_REGEX: Regex = """\A([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))$""".r
  private val TIME_REGEX: Regex = """\A([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?$""".r
  private val OID_REGEX: Regex = """\Aurn:oid:[0-2](\.[1-9]\d*)+$""".r
  private val UUID_REGEX: Regex = """\Aurn:uuid:[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$""".r
  private val VERSION_REGEX: Regex = """\A[0-9]+(\.[0-9]+)*$""".r


   /**
     * Validate a FHIR complex content
     * @param profileUrl                    URL of the profile as content is validated agains
     * @param value                         Parsed content
     * @param resourceElementRestrictions   For validation against a data type, chain of element restrictions defined in profiles for inner elements
     * @return
     */
  def validateComplexContentAgainstProfile(profileUrl: String, value: JObject, parentPath:Option[String], resourceElementRestrictions:Seq[Seq[(String, ElementRestrictions)]] = Nil): Seq[OutcomeIssue] = {
    logger.debug(s"Validating against profile $profileUrl...")
    //Find profile chain for this profile
    val profileChain = FhirContentValidator.findProfileChain(profileUrl)
    //Chain of possible restrictions for this field
    val allRestrictions = resourceElementRestrictions ++ profileChain.map(_.elementRestrictions)
    //Validated fields within this object
    val validatedFields = new mutable.HashSet[String]()

    val issues =
      value.obj.flatMap {
        case ("resourceType", resourceType) =>
          if(!resourceType.isInstanceOf[JString] || profileUrl.split('/').last != resourceType.extract[String])
            convertToOutcomeIssue("resourceType", Seq(ConstraintFailure(s"Resource type ${resourceType.extract[String]} does not match with the target profile ${profileUrl}!")))
          else
            Nil
        case (field, fieldValue) =>
          //Find out the field name and DataType for the given field
          extractFieldNameAndDataType(field, allRestrictions) match {
            //If there is no definition for the element, return error
            case None =>
              convertToOutcomeIssue(FHIRUtil.mergeElementPath(parentPath, field) ,Seq(ConstraintFailure(s"Unrecognized element $field !")))
            //If we found such a defined field e.g. valueQuantity -> value, Quantity
            case Some((fieldName, dataType)) =>
              //Add the field to validated fields
              validatedFields += fieldName
              //Find all restrictions chain defined for the field (in priority order)
              val elementRestrictions = findElementRestrictionsChain(fieldName,  allRestrictions)
              //Validate element
              validateElement(FHIRUtil.mergeElementPath(parentPath, field), fieldName, dataType, fieldValue, elementRestrictions)
          }
      }
    //Find required elements
    val requiredElements = findRequiredElements(allRestrictions).toSeq
    //Find required but, not given elements
    val requiredButMissingElements =
      requiredElements
      .filterNot(rq =>
        if(rq._2.length == 1)
          validatedFields.contains(rq._1) //if field is validated
        else
          rq._2
            .map(dt => rq._1 + dt) //merge the field name and data type to produce possible paths
            .exists(f => validatedFields.contains(f)) //if one of them exists in the set
      )
    //Convert them to issues
    val requiredConstraintIssues =
      requiredButMissingElements
        .flatMap(el => {
          val path = FHIRUtil.mergeElementPath(parentPath, el._1)
          convertToOutcomeIssue(path, Seq(ConstraintFailure(s"Element ${path} is required, but does not exist!")))
        })

    val fhirConstraintIssues =
      profileChain
        .flatMap(_.constraints)
        .flatMap(fc => convertToOutcomeIssue(parentPath.getOrElse("$this"), fc.evaluate(value)))

    //Combine them
    issues ++ requiredConstraintIssues ++ fhirConstraintIssues
  }

  /**
    *
    * @param path                   Full actual path of the element e.g. Observation.valueQuantity
    * @param fieldName              Field name or prefix (for multi type elements e.g. Observation.value[x] -> value) in the definition for the given element
    * @param dataType               Resolved data type for the given element e.g. Quantity
    * @param value                  Given element content
    * @param elementRestrictions    Element definition chain found for this element (with also sub element definitions under that path)
    * @return
    */
  def validateElement(path:String, fieldName: String, dataType: String, value: JValue, elementRestrictions: Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])]): Seq[OutcomeIssue] = {
        logger.debug(s"Validating element at path $path with field $fieldName and data type $dataType...")
        //First perform cardinality validations
        val cardinalityValidations = evaluateCardinalityConstraints(dataType, value, elementRestrictions.flatMap(_._1))
        //If there is an error in cardinality directly return it
        if (cardinalityValidations.nonEmpty)
          convertToOutcomeIssue(path, cardinalityValidations)
        else {
           //If element is sliced, construct a slice map with matchers
           //Fist find all slices defined on the field
           val allSlicingDefs = findSlicingDefinitions(fieldName, elementRestrictions)
           //If there is any slicing on field
           if(allSlicingDefs.nonEmpty) {
             furtherValidateSlicedElement(allSlicingDefs, path, fieldName, dataType, value, elementRestrictions)
           } else {
             //Normalize paths for sub elements
             val normalDefinitions = normalizePathsForSubElements(fieldName, elementRestrictions)
             value match {
               case JArray(arr) =>
                 //all elements should be valid based on the restrictions
                 arr.zipWithIndex
                   .flatMap(v => furtherValidateElement(path + s"[${v._2}]", fieldName, dataType, v._1, normalDefinitions))

               //Non array
               case oth =>
                 furtherValidateElement(path, fieldName, dataType, oth, normalDefinitions)
             }
           }
        }
  }

  /**
   * If the element is sliced, this will continue to validate element with the given slice definitions
   * @param allSlicingDefs         All slicing definitions on the element
   * @param path                   Full actual path of the element e.g. Observation.valueQuantity
   * @param fieldName              Field name or prefix (for multi type elements e.g. Observation.value[x] -> value) in the definition for the given element
   * @param dataType               Resolved data type for the given element e.g. Quantity
   * @param value                  Given element content
   * @param elementRestrictions    Element definition chain found for this element (with also sub element definitions under that path)
   * @return
   */
  def furtherValidateSlicedElement(allSlicingDefs:Seq[FhirSlicing], path:String, fieldName: String, dataType: String, value: JValue, elementRestrictions: Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])]):Seq[OutcomeIssue] = {
    //Find if there is a ordered constraint in some where in the chain
    val isOrdered = allSlicingDefs.exists(_.ordered)
    //Last rule is the more restrictive e.g. open, close, openAtEnd
    val rule = allSlicingDefs.head.rule
    //Construct the slice map
    val slices = constructSliceMap(fieldName, allSlicingDefs, elementRestrictions)
    //Find the matching slice for each value (or if not match any)
    val valueSliceMatching:Seq[((JValue, Int), Option[(String, Int, Map[String, Seq[FhirRestriction]], ElementRestrictions, Seq[Seq[(String, ElementRestrictions)]])])] =
      value match {
        case JArray(arr) =>
          arr.zipWithIndex.map(v => v -> slices.find(s => evaluateMatcher(v._1, dataType, s._3)))
        case oth =>
          Seq((oth,0) -> slices.find(s => evaluateMatcher(oth, dataType, s._3)))
      }

    //Group values into slices in (slice definition order)
    val slicesValuesMatchings:Seq[( Option[(String, Int, Map[String, Seq[FhirRestriction]], ElementRestrictions, Seq[Seq[(String, ElementRestrictions)]])], Seq[(JValue, Int)])] =
      valueSliceMatching
        .groupBy(g => g._2.map(_._1)).toSeq
        .sortWith((g1, g2) =>
          g1._1.isDefined && //Defined slices are in priority to remaining
            !g1._1.contains("$default") && //Default slice should be at end
            g1._2.head._2.map(_._2).get > g2._2.headOption.flatMap(_._2.map(_._2)).getOrElse(-1) //Slices should be ordered in terms of their order in the definitions
        )
        .map(g => g._2.head._2 -> g._2.map(_._1).sortWith((i1, i2) => i1._2 < i2._2))

    var arrIndex = -1
    val slicingErrors = slicesValuesMatchings
      .flatMap(sliceValuesMatchings => {
        //Get the corresponding slice values
        val sliceValues = sliceValuesMatchings._2
        val sliceErrors  = sliceValuesMatchings._1 match {
          //Value that does not match any slice
          case None =>
            rule match {
              //This means there should be no other value not matching any slice
              case "closed" =>
                Seq(ConstraintFailure(s"Slicing rule on the element is defined as 'closed' and element at index(es) ${sliceValues.map(_._2).mkString(",")} does not match any slice!"))
              case "openAtEnd" =>
                findFirstUnorderedElementForSlicing(arrIndex, sliceValues.map(_._2)) match {
                  case None => Nil
                  case Some((i1,i2)) => Seq(ConstraintFailure(s"Slicing rule on the element is defined as 'openAtEnd' and element at index $i1 should be at the end (at index $i2)!"))
                }
              case "open" =>
                Nil //If it is open, so element can be anywhere
            }
          case Some(svm) =>
            var failures:Seq[ConstraintFailure] = Nil
            //If the slicing is ordered, then values should match to slice in order
            val orderFailure =
              if(isOrdered)
                findFirstUnorderedElementForSlicing(arrIndex, sliceValues.map(_._2)) match {
                  case Some((i1, i2)) => Some(ConstraintFailure(s"Problem in order of values matched to slice ${svm._1}. The element within the array with index $i1 should be in index ${i2}"))
                  case None => None
                }
              else
                None
            failures = failures ++ orderFailure.toSeq

            if(sliceValuesMatchings._1.isDefined)
            //Cardinality checks on the slice values
              failures = failures ++
                evaluateCardinalityConstraints(dataType, JArray(sliceValues.map(_._1).toList), Seq(svm._4))
                  .map(cf => ConstraintFailure(s"Based on the slice definition ${svm._1}: ${cf.errorOrWarningMessage}"))

           failures
        }
        //Update the array index for the values
        if(sliceValues.nonEmpty)
          arrIndex = sliceValues.last._2
        sliceErrors
      })
    //If there is any error in cardinality of slices
    if(slicingErrors.nonEmpty)
      convertToOutcomeIssue(path, slicingErrors)
    else
      slicesValuesMatchings
        .flatMap(sliceValuesMatchings => {
          //Normalize paths for sub elements
          val normalDefinitions = normalizePathsForSubElements(fieldName, elementRestrictions)
          sliceValuesMatchings._1 match {
            //If element does not match any slice, just validate it against common restrictions
            case None =>
              sliceValuesMatchings._2.flatMap(v => furtherValidateElement(path + s"[${v._2}]", fieldName, dataType, v._1, normalDefinitions))
            case Some(svm) =>
              sliceValuesMatchings._2.flatMap(v => furtherValidateElement(path + s"[${v._2}]", fieldName, dataType, v._1, (Some(svm._4) -> svm._5.flatten) +: normalDefinitions))
          }
        })
  }

  /**
   * Find the first unordered element's index within the sliced element (also the index it should be located)
   * @param lastSliceFinalElementIndex  Last element index in previously defined slice
   * @param thisSliceElementIndexes     Element indexes in order that match this slice
   * @return  Unordered element index and the index it should located
   */
  private def findFirstUnorderedElementForSlicing(lastSliceFinalElementIndex:Int, thisSliceElementIndexes:Seq[Int]):Option[(Int, Int)] = {
    (lastSliceFinalElementIndex +: thisSliceElementIndexes)
      .sliding(2)
      .find(sl => sl.head + 1 != sl.apply(1))
      .map(s => s.apply(1) -> (s.head + 1))
  }

  /**
   * @param path                    Actual path to the element to be validated e.g. valueQuantity
   * @param fieldName               Field name (as defined) e.g. value[x] --> value
   * @param dataType                FHIR DataType of the field e.g. Quantity
   * @param value                   Parsed JSON content of the element
   * @param elementRestrictionChain Chain of definitions for the element (coming from profile chain)  (together with sub element definitions)
   * @return
   */
  def furtherValidateElement(path:String, fieldName: String, dataType: String, value: JValue, elementRestrictionChain: Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])]): Seq[OutcomeIssue] ={
    val isPrimitive = dataType.apply(0).isLower
    val thisElementRestrictions = elementRestrictionChain.flatMap(_._1)
    //Go over all possible restrictions in some order, and collect errors and warnings
    val allWarningsOrErrorsForThis = {
      //If the dataType is primitive, validate it and if there is an error do not check other constraints
      if (isPrimitive && !validatePrimitive(value, dataType)) {
        Seq(ConstraintFailure(s"Invalid value '${value.extract[String]}' for FHIR primitive type '$dataType'!"))
      } else {
        //If data type is complex, it should be a JObject and not empty
        if (!isPrimitive && (!value.isInstanceOf[JObject] || value.asInstanceOf[JObject].obj.isEmpty))
          Seq(ConstraintFailure(s"Invalid or empty value for FHIR complex type '$dataType'!"))
        else {

          Seq(
            evaluateMinMaxConstraints(dataType, value, thisElementRestrictions),
            evaluateFixedPatternConstraints(dataType, value, thisElementRestrictions),
            evaluateMaxLengthConstraints(dataType, value, thisElementRestrictions),
            evaluateFhirConstraints(dataType, value, thisElementRestrictions),
            evaluateBindingConstraint(dataType, value, thisElementRestrictions),
            evaluateReferenceConstraint(dataType, value, thisElementRestrictions)
          ).flatten
        }
      }

    }

    //If there is an error on this element return it
    if(allWarningsOrErrorsForThis.nonEmpty && allWarningsOrErrorsForThis.exists(!_.isWarning))
      convertToOutcomeIssue(path, allWarningsOrErrorsForThis)
    //Otherwise, and if it is complex continue with childs
    else if(!isPrimitive) {
      val profileUrls = findExpectedProfilesUrl(dataType, thisElementRestrictions)
      //Otherwise
      validateComplexContentAgainstProfile(profileUrls.head, value.asInstanceOf[JObject], Some(path), elementRestrictionChain.map(_._2))
      //TODO validate against other profiles
    } else {
      Nil
    }
  }

  /**
   * Evaluate a matcher against the given value if they match
   * @param value
   * @param dataType
   * @param matchers
   * @return
   */
  private def evaluateMatcher(value:JValue, dataType:String, matchers:Map[String, Seq[FhirRestriction]]): Boolean = {
    //The value should satisfy for each matcher
    matchers.forall {
      //If a slicing over the type of current element, just check if type is correct
      case ("$this",Seq(TypeRestriction(dataTypes))) => dataTypes.contains(dataType)
      //Otherwise
      case m =>
        FhirPathEvaluator.evaluate(m._1, value) //Evaluate the Discriminator path
          .exists(cv => //A result should exist, which match the the given restriction
            m._2
              .forall(
                _.evaluate(cv.toJson) //Evaluate the restriction
                  .isEmpty) //No error
          )
    }
  }

  /**
   * Find in the chain for all slice definitions for an element if exists
   * @param elementRestrictions   Chain of element restrictions
   * @return
   */
  private def findSlicingDefinitions(fieldName: String, elementRestrictions: Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])]):Seq[FhirSlicing] = {
    elementRestrictions.flatMap(e =>
      e._1.flatMap(_.slicing) match {
        case None if e._2.map(_._1).contains(fieldName+":") =>
          //If there is no explicitly defined slicing, but it has slices, then it should be a type slicing for a multi typed value
          Some(FhirSlicing(Seq("type" -> "$this"), false, "open"))
        case oth => oth
      }
    )
  }

  /**
   * Find out all slices; slice name, slice index, their matchers, element restriction that defines the slice, and all sub element restrictions under the slice
   * @param fieldName             Defined field name
   * @param fhirSlicingDefs       All defined slices on the field coming from profiles
   * @param elementRestrictions   Element restrictions and sub restrictions defined under the field
   */
  def constructSliceMap(fieldName: String, fhirSlicingDefs: Seq[FhirSlicing], elementRestrictions: Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])]):Seq[(String, Int, Map[String, Seq[FhirRestriction]], ElementRestrictions, Seq[Seq[(String, ElementRestrictions)]])] = {
    //Get all discriminators
    val discriminators = fhirSlicingDefs.flatMap(_.discriminators).toSet

    val subElementRestrictions = elementRestrictions.map(_._2).filter(_.nonEmpty)
    //Get all slices; slice name -> Element restriction for that slice
    val allSlices = findSlices(fieldName, subElementRestrictions)
    allSlices.zipWithIndex.map {
      case ((sliceName, sliceRestriction), i) =>
        //Find subelement restrictions for that slice
        val sliceSubElements = findAndNormalizeSliceSubElementRestrictions(fieldName, sliceName, subElementRestrictions)
        //Find related matching restrictions (path of the restriction -> FhirRestriction)
        val sliceMatchers = findRelatedMatchers(discriminators, sliceRestriction, sliceSubElements)
        (sliceName, i, sliceMatchers, sliceRestriction, sliceSubElements)
    }
  }

  /**
   * Find matcher restrictions related with all of the discriminators for the slice (path of the restriction -> Seq[FhirRestriction])
   * @param discriminators    All discriminators defined for this slicing (discriminator type and path)
   * @param sliceRestriction  Element definition which is the root of the slice e.g. Observation.value[x]:valueQuantity or Observation.component:m1
   * @param sliceSubElements  Sub element restrictions for the given slice e.g. Observation.component:m1.code --> code-> ElementRestriction
   * @return
   */
  def findRelatedMatchers(discriminators:Set[(String, String)], sliceRestriction:ElementRestrictions, sliceSubElements:Seq[Seq[(String, ElementRestrictions)]]):Map[String, Seq[FhirRestriction]] = {
      val allRestrictions = discriminators.toSeq.flatMap(disc => {
        //Find matchers path prefix
        val matcherPathPrefix = findPrefixOfDiscriminatorPath(disc._2, sliceSubElements)

        val discriminatorRestrictions:Seq[Seq[(String, FhirRestriction)]] =
          disc._1 match {
            case "value" | "pattern" =>
              findSubElementRestrictions(matcherPathPrefix, sliceSubElements)
                .map(
                  _.map(er => er._1 ->
                    er._2.restrictions
                      .filterKeys(k => k == ConstraintKeys.PATTERN || k == ConstraintKeys.BINDING || k == ConstraintKeys.MAX).values.headOption //These restrictions can be related with value or pattern
                  )
                    .filter(_._2.isDefined)
                    .map(er => er._1 -> er._2.get)
                )
            case "exists" =>
              findSubElementRestrictions(matcherPathPrefix, sliceSubElements, includeSubsAndSlices = false)
                .map(
                  _.map(er => er._1 ->
                    er._2.restrictions
                      .filterKeys(k => k == ConstraintKeys.MIN || k == ConstraintKeys.MAX).values.headOption
                  ) //
                    .filter(_._2.isDefined)
                    .map(x => x._1 -> x._2.get )
                )
            case "type" =>
              //Path $this is used with type discriminator on multi typed elements
              if(disc._2 == "$this")
                sliceRestriction.dataTypes.map(_._1) match {
                  case Nil => Nil
                  case dts => Seq(Seq("$this" -> TypeRestriction(dts.toSet)))
                }
              else
                findSubElementRestrictions(matcherPathPrefix, sliceSubElements, includeSubsAndSlices = false)
                    .map(
                      _
                        .map(er => er._1 -> er._2.dataTypes.map(_._1))
                        .filter(_._2.nonEmpty)
                        .map(e => e._1 -> TypeRestriction(e._2.toSet))
                    )
            case "profile" =>
              findSubElementRestrictions(matcherPathPrefix, sliceSubElements, includeSubsAndSlices = false)
                  .map(
                    _
                      .map(er => er._1 -> er._2.dataTypes)
                      .filter(_._2.nonEmpty)
                      .map(e => e._1 -> ProfileRestriction(e._2.head._1, e._2.head._2.toSet))
                  )
            case _ => Nil
          }

        discriminatorRestrictions.flatten
      })

    allRestrictions.groupBy(_._1).map(g => g._1 -> g._2.map(_._2)).toMap
  }

  /**
   * Find element path prefix that will include restrictions related with discriminator path
   * @param discriminatorPath
   * @param sliceSubElements
   * @return
   */
  private def findPrefixOfDiscriminatorPath(discriminatorPath:String, sliceSubElements:Seq[Seq[(String, ElementRestrictions)]]):String = {
    //If there is some resolve() on the path, just get the prefix of it e.g. item.resolve(), item.resolve().name --> item
    if(discriminatorPath.contains("resolve()"))
      discriminatorPath.substring(0, discriminatorPath.indexOf("resolve()"))
    else if(discriminatorPath.contains("extension(") || discriminatorPath.contains("ofType(")){
      var prefix:Option[String] = None
      discriminatorPath.split('.')
        .foreach(pitem =>
          if(pitem.startsWith("extension(")){
            //Find extension url
            val url = pitem.replace("extension(", "").dropRight(1)
            //There should be definition of extension within the element definitions
            val extensionPrefix = FHIRUtil.mergeElementPath(prefix, "extension") + ":"
            val extensionName = sliceSubElements
              .flatMap(elementRestrictions =>
                elementRestrictions
                  .find(er => er._1.startsWith(extensionPrefix) && er._2.dataTypes.exists(_._2.contains(url)))
                  .map(_._2.sliceName)
              ).head
            prefix = Some(FHIRUtil.mergeElementPath(prefix,  "extension:" + extensionName))
          } else if(pitem.startsWith("ofType(")) {
            //Just skip ofType
          } else {
            prefix = Some(FHIRUtil.mergeElementPath(prefix,pitem))
          }
        )
      prefix.get
    } else
      discriminatorPath
  }

  /**
   * Find defined slices in all chains
   * @param fieldName               Name of the sliced field
   * @param subElementRestrictions  All sub element restrictions of the field
   * @return
   */
  private def findSlices(fieldName: String, subElementRestrictions:Seq[Seq[(String, ElementRestrictions)]]):Seq[(String, ElementRestrictions)] = {
    subElementRestrictions
      .reverse //Order of slice definitions are important, so we start from the parent
      .flatMap(rm => rm.find(i => i._2.sliceName.isDefined && !i._1.contains('.'))) // Slice should be on this element not childs
      .map(s => s._2.sliceName.get -> s._2)
  }

  /**
   * Find element restrictions on and under the given path
   * @param pathPrefix
   * @param subElementRestrictions
   * @return
   */
  private def findSubElementRestrictions(pathPrefix:String, subElementRestrictions:Seq[Seq[(String, ElementRestrictions)]], includeSubsAndSlices:Boolean = true):Seq[Seq[(String, ElementRestrictions)]] = {
    subElementRestrictions
      .map(elementRestrictions => {
        val s = elementRestrictions.indexWhere(er => isSubElement(pathPrefix, er._1)) //Find the starting index for the path
        elementRestrictions
          .slice(s, elementRestrictions.length) //Go to there
          .takeWhile(er => isSubElement(pathPrefix, er._1)) //Take all elements under that (as they are sequential)
      }).filter(_.nonEmpty)
  }

  /**
   * If the given element path is defined under the path prefix
   * @param pathPrefix
   * @param elementPath
   * @return
   */
  private def isSubElement(pathPrefix:String, elementPath:String, includeSubsAndSlices:Boolean = true) =
    pathPrefix == elementPath ||
      (includeSubsAndSlices &&                 // Either same
        (
          elementPath.startsWith(pathPrefix + ".") || // Or a sub element
          elementPath.startsWith(pathPrefix + ":")
        )
      )   // Or a slice

  /**
   * Filter the subelements under the given slice and normalize the paths
   * @param parentField             Field name of the sliced element
   * @param sliceName               Name of the slice
   * @param subElementRestrictions  All sub element defs under this element path
   * @return
   */
  private def findAndNormalizeSliceSubElementRestrictions(parentField:String, sliceName:String, subElementRestrictions:Seq[Seq[(String, ElementRestrictions)]]):Seq[Seq[(String, ElementRestrictions)]] = {
    //This is the prefix for sub element definitions for the slice
    val prefix = s"$parentField:$sliceName."
    subElementRestrictions
      .map(_.filter(_._1.startsWith(prefix)))
      .map(sr => sr.map(e => e._1.replace(prefix, "") -> e._2))
      .filter(_.nonEmpty)
  }

  /**
   * Normalize the path map for element restrictions by removing the parent field from the maps
   * @param parentField             Parent field name
   * @param elementRestrictions     Element and sub element restriction chain
   */
  private def normalizePathsForSubElements(parentField:String, elementRestrictions:Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])]):Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])] = {
    elementRestrictions
      .map(e => e._1 ->
        e._2
          .filter(e => e._1.startsWith(parentField + ".")) // Only get child elements
          .map(e => e._1.replace(parentField + ".", "") -> e._2)
      ).filter(e => e._1.isDefined || e._2.nonEmpty)
  }



  /**
    * Convert failures to FHIR OutcomeIssue objects
    * @param path
    * @param warningsAndErrors
    * @return
    */
   def convertToOutcomeIssue(path:String, warningsAndErrors:Seq[ConstraintFailure]):Seq[OutcomeIssue] = {
      warningsAndErrors.map(error =>
         OutcomeIssue(
            severity = if(error.isWarning) SEVERITY_CODES.WARNING else SEVERITY_CODES.ERROR,
            code = OUTCOME_CODES.INVALID,
            details = None,
            diagnostics = Some(error.errorOrWarningMessage),
            location = Seq(path)
         )
      )
   }

  /**
    * Validate FHIR Primitive Data Types
    *
    * @param value Json value
    * @param ptype FHIR type expected
    * @return
    */
  def validatePrimitive(value: JValue, ptype: String): Boolean = {
    ptype match {
      case FHIR_DATA_TYPES.STRING => value.extract[String].length > 0 //OK
      case FHIR_DATA_TYPES.INTEGER => value.isInstanceOf[JInt]
      case FHIR_DATA_TYPES.URI => Try(new URI(value.extract[String])).isSuccess
      case FHIR_DATA_TYPES.URL => Try(new URL(value.extract[String])).isSuccess
      case FHIR_DATA_TYPES.CANONICAL => value.extract[String].split('|') match {
        case Array(url) => Try(new URI(url)).isSuccess
        case Array(url, version) =>
          Try(new URI(url)).isSuccess && VERSION_REGEX.findFirstMatchIn(version).isDefined
        case _ => false
      }
      case FHIR_DATA_TYPES.BOOLEAN => value.isInstanceOf[JBool]
      case FHIR_DATA_TYPES.DECIMAL =>
         (value.isInstanceOf[JDecimal] || value.isInstanceOf[JLong] || value.isInstanceOf[JDouble] || value.isInstanceOf[JInt]) &&
           Try(value.extract[Double]).isSuccess
      case FHIR_DATA_TYPES.CODE => CODE_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.DATE => DATE_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.DATETIME => DATETIME_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.INSTANT => INSTANT_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.TIME => TIME_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.ID => ID_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.OID => OID_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.UUID => UUID_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.UNSIGNEDINT => value.isInstanceOf[JInt] && value.extract[Int].intValue() >= 0 //"""[0]|([1-9][0-9]*)$""".r.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.POSITIVEINT => value.isInstanceOf[JInt] && value.extract[Int].intValue() >= 1 //"""+?[1-9][0-9]*$""".r.findFirstMatchIn(value).isDefined
      case FHIR_DATA_TYPES.BASE64BINARY => true
      case FHIR_DATA_TYPES.MARKDOWN => value.extract[String].length > 0
      case FHIR_DATA_TYPES.XHTML => true //TODO Check FHIR XHTML restrictions
      case _ => true
    }
  }


  /**
    * Evaluate cardinality constraints on the element
    *
    * @param value
    * @param elementRestrictions
    * @return
    */
  private def evaluateCardinalityConstraints(dataType:String, value: JValue, elementRestrictions: Seq[ElementRestrictions], testArray:Boolean = true): Seq[ConstraintFailure] = {
    Seq(
      //Evaluate Minimum cardinality restriction if exist
      findFirstFhirRestriction(ConstraintKeys.MIN, elementRestrictions).map(_.evaluate(value)).getOrElse(Nil),
      //Evaluate Maximum cardinality restriction if exist
      findFirstFhirRestriction(ConstraintKeys.MAX, elementRestrictions).map(_.evaluate(value)).getOrElse(Nil),
      //As array elements are defined in Base
      if(testArray)
        getBaseFhirRestriction(ConstraintKeys.ARRAY, elementRestrictions)
          .getOrElse(ArrayRestriction(isArray = false))//If it is not array, we should also force this
          .evaluate(value) else Nil,
    ).flatten
  }


  private def evaluateMinMaxConstraints(dataType: String, value: JValue, elementRestrictions: Seq[ElementRestrictions]): Seq[ConstraintFailure] = {
    Seq(
      findFirstFhirRestriction(ConstraintKeys.MINVALUE, elementRestrictions)
        .map(_.evaluate(value)).getOrElse(Nil),
      findFirstFhirRestriction(ConstraintKeys.MAXVALUE, elementRestrictions)
        .map(_.evaluate(value)).getOrElse(Nil)
    ).flatten
  }

   /**
     * Evaluate maxLength restriction in element definition
      * @param dataType
     * @param value
     * @param elementRestrictions
     * @return
     */
  private def evaluateMaxLengthConstraints(dataType: String, value: JValue, elementRestrictions: Seq[ElementRestrictions]):Seq[ConstraintFailure] = {
     if(Set(FHIR_DATA_TYPES.STRING, FHIR_DATA_TYPES.ID, FHIR_DATA_TYPES.CODE, FHIR_DATA_TYPES.MARKDOWN).contains(dataType))
        findFirstFhirRestriction(ConstraintKeys.MAXLENGTH, elementRestrictions)
          .map(_.evaluate(value))
          .getOrElse(Nil)
     else
        Nil
  }

   /**
     * Evaluate Fixed or pattern constraint
      * @param dataType
     * @param value
     * @param elementRestrictions
     * @return
     */
  private def evaluateFixedPatternConstraints(dataType: String, value: JValue, elementRestrictions: Seq[ElementRestrictions]):Seq[ConstraintFailure]  = {
    findFirstFhirRestriction(ConstraintKeys.PATTERN, elementRestrictions)
       .map(_.evaluate(value))
       .getOrElse(Nil)
  }

   /**
     *
     * @param dataType
     * @param value
     * @param elementRestrictions
     * @return
     */
   private def evaluateFhirConstraints(dataType: String, value: JValue, elementRestrictions: Seq[ElementRestrictions]):Seq[ConstraintFailure] = {
      //We need to run all constraints defined in the chain
      getAllRestrictions(ConstraintKeys.CONSTRAINT, elementRestrictions)
        .flatMap(_.evaluate(value))
   }

   /**
     *
     * @param dataType
     * @param value
     * @param elementRestrictions
     * @return
     */
   private def evaluateBindingConstraint(dataType: String, value: JValue, elementRestrictions: Seq[ElementRestrictions]):Seq[ConstraintFailure] = {
      if(Set(FHIR_DATA_TYPES.CODEABLE_CONCEPT, FHIR_DATA_TYPES.CODING, FHIR_DATA_TYPES.CODE, FHIR_DATA_TYPES.QUANTITY).contains(dataType))
         findFirstFhirRestriction(ConstraintKeys.BINDING, elementRestrictions)
           .map(_.evaluate(value))
            .getOrElse(Nil)
      else
         Nil
   }

   private def evaluateReferenceConstraint(dataType: String, value: JValue, elementRestrictions: Seq[ElementRestrictions]):Seq[ConstraintFailure] = {
      if(dataType == FHIR_DATA_TYPES.REFERENCE){
        findFirstFhirRestriction(ConstraintKeys.REFERENCE_TARGET, elementRestrictions)
          .map(_.evaluate(value))
          .getOrElse(Nil)
      } else Nil
   }

  /**
    * Find expected profile for a FHIR complex element
    * @param dataType
    * @param elementRestrictions
    * @return
    */
  private def findExpectedProfilesUrl(dataType:String, elementRestrictions: Seq[ElementRestrictions]):Seq[String] = {
    elementRestrictions
      .view
      .flatMap(_.dataTypes)
      .find(_._1 == dataType)
      .map(_._2)
      .filter(_.nonEmpty)
      .getOrElse(Seq(s"${api.FHIR_ROOT_URL_FOR_DEFINITIONS}/StructureDefinition/$dataType"))
  }



/*  private def findAllSubElementRestrictions(elementRestrictions: Seq[ElementRestrictions]):Seq[Map[String,ElementRestrictions]] = {
    elementRestrictions
      .dropRight(1) //Do not get base definition
      .map(e => e.subElementRestrictions)
      .filter(_.nonEmpty)
  }*/

  /**
    * Given a profile chain find required elements and their data types
    * @param allElementRestrictions
    * @return
    */
  private def findRequiredElements(allElementRestrictions:Seq[Seq[(String, ElementRestrictions)]]):Map[String, Seq[String]] = {
    allElementRestrictions
      .map(er => er.filter(!_._1.contains('.'))) //Only get the children not descendants
      .map(er => er.filter(_._2.restrictions.isDefinedAt(ConstraintKeys.MIN)))  // Get the ones that has minimum cardinality restriction (which is created if it exists and more than 0)
      .map(er => er.map(e => e._1 -> e._2.dataTypes.map(_._1)).toMap)
      .foldRight(Map.empty[String, Seq[String]])((m1, m2) => m1 ++ m2) //Merge them by overriding (left overrides as it is a further restriction)
  }

  /**
    * Find the first restriction found in the chain (For those restrictions that directly decide on the issue e.g. min, max cardinality)
    *
    * @param restrictionKey      Key of the restriction
    * @param elementRestrictions Element restriction chain
    * @return
    */
  private def findFirstFhirRestriction(restrictionKey: String, elementRestrictions: Seq[ElementRestrictions]): Option[FhirRestriction] = {
    elementRestrictions.find(er => er.restrictions.isDefinedAt(restrictionKey)).map(_.restrictions(restrictionKey))
  }

  /**
    *
    * @param restrictionKey
    * @param elementRestrictions
    * @param constraint
    * @return
    */
  private def findFirstFhirRestrictionWithExtraConstraint(restrictionKey: String, elementRestrictions: Seq[ElementRestrictions], constraint: FhirRestriction => Boolean): Option[FhirRestriction] = {
    elementRestrictions
      .find(er => er.restrictions.isDefinedAt(restrictionKey) && constraint(er.restrictions(restrictionKey)))
      .map(_.restrictions(restrictionKey))
  }

   /**
     * Get the restriction in base FHIR standart definition
     * @param restrictionKey
     * @param elementRestrictions
     * @return
     */
  private def getBaseFhirRestriction(restrictionKey: String, elementRestrictions: Seq[ElementRestrictions]): Option[FhirRestriction] = {
    elementRestrictions.last.restrictions.get(restrictionKey)
  }

   /**
     * Get all restrictions in the chain
      * @param restrictionKey
     * @param elementRestrictions
     * @return
     */
  private def getAllRestrictions(restrictionKey: String, elementRestrictions: Seq[ElementRestrictions]):Seq[FhirRestriction] = {
     elementRestrictions.flatMap(er => er.restrictions.get(restrictionKey))
  }

  /**
    * Find a chain of ElementRestrictions and sub element restrictions under that element for a given field in priority order of profiles
    *
    * @param field                      Field name
    * @param elementRestrictions        All possibly related element restrictions chain
    * @return Element restrictions in order of evaluation
    */
  private def findElementRestrictionsChain(field: String, elementRestrictions: Seq[Seq[(String, ElementRestrictions)]]): Seq[(Option[ElementRestrictions], Seq[(String, ElementRestrictions)])] = {
    //Restrictions defined in the Resource profiles are first in that order, Then the data type profile chain (Or root elements in resource profile)
    elementRestrictions
      .map(rr =>
        findElementRestriction(field, rr) -> //find the restriction for the field if exist in each set of element defs coming from a profile
          rr.filter(p => p._1.startsWith(field +".") || p._1.startsWith(field+":")) //find child definitions in each set of element defs coming from a profile
      ).filter(e => e._1.isDefined || e._2.nonEmpty)
  }

  /**
    * Extract Field name and DataType of field as defined in profiles e.g. Observation.effective[x];  effectiveDateTime ->   (effective, dateTime)
    *
    * @param field        Field name
    * @param profileChain Profile chain that should include dataType definition for the field
    * @return
    */
  private def extractFieldNameAndDataType(field: String, profileChain:Seq[Seq[(String, ElementRestrictions)]]): Option[(String, String)] = {
    profileChain
      .reverse //Start from the back as there will be the base data type or resource definitions which provides the type of field
      .view //lazy evaluation
      .flatten
      .find(er =>
        er._2.dataTypes.nonEmpty && //If there is a data type definition
          (
            er._1 == field || //If the profile has element definition for the field and has given the data types
            (er._1.endsWith("[x]") && field.startsWith(er._1.dropRight(3))) //Or is a multi valued field matching the name
          )
      ).flatMap(er =>
        if(er._1.endsWith("[x]")) { //If multi valued
          val fieldName = er._1.dropRight(3) //remove the [x] to find the defined field root
          field.replace(fieldName, "") match { //replace the root to find the Data type part
            case ct if api.FHIR_COMPLEX_TYPES.contains(ct) => Some(er._1 -> ct) //If it is a complex type
            case st if api.FHIR_PRIMITIVE_TYPES.contains(decapitilize(st)) =>   Some(er._1 -> decapitilize(st)) //Or check if it is simple type
            case _ => None
          }
        } else
          Some(er._1 ->er._2.dataTypes.head._1)
      )
  }

  /**
    * Decapitalize a string
    *
    * @param s
    * @return
    */
  def decapitilize(s: String): String = {
    s.apply(0).toLower + s.substring(1)
  }

  /**
    * Find an element definition for a given path
    *
    * @param field               Field name for the the element
    * @param elementRestrictions Element restrictions to search
    * @return
    */
  def findElementRestriction(field: String, elementRestrictions: Seq[(String, ElementRestrictions)]): Option[ElementRestrictions] = {
    elementRestrictions.find(_._1 == field).map(_._2)
  }

  /**
    * Find a chain of parent profiles until the base FHIR spec given the profile
    *
    * @param profileUrl Profile definition
    * @return Profiles in order of evaluation (inner profile - base profile)
    */
  private def findProfileChain(profileUrl: String): Seq[ProfileRestrictions] = {
    FhirConfigurationManager.fhirConfig.profileRestrictions.get(profileUrl) match {
      case None => throw new InitializationException(s"Resource profile with url $profileUrl is nor recognized! Please check conformance statement of the server!")
      case Some(profile) => findChain(FhirConfigurationManager.fhirConfig.profileRestrictions)(profile)
    }
  }


  /**
    * Supplementary method for profile chain finding
    *
    * @param restrictions
    * @param profile
    * @return
    */
  private def findChain(restrictions: Map[String, ProfileRestrictions])(profile: ProfileRestrictions): Seq[ProfileRestrictions] = {
    profile
      .baseUrl
      .map(burl =>
        restrictions
          .get(burl)
          .fold[Seq[ProfileRestrictions]](Seq(profile))(parent => profile +: findChain(restrictions)(parent))
      )
      .getOrElse(Seq(profile))
  }

}



package io.onfhir.api.util

import java.time.Instant

import io.onfhir.api._
import io.onfhir.api.model.Parameter
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.config.{OnfhirConfig, SearchParameterConf}
import io.onfhir.exception.{InternalServerException, InvalidParameterException}
import io.onfhir.util.DateTimeUtil
import org.json4s.JsonAST.{JObject, JValue}
import io.onfhir.util.JsonFormatter._

import scala.collection.immutable.HashMap
import scala.util.Try

/**
  * Implementation of FHIR search in memory over a resource
  */
object ImMemorySearchUtil {
  //Function map based on search parameter type (provides a function that gets search parameter conf and whole resource to evaluate)
  /*val typeHandlerFunction = HashMap[String, (String, String, SearchParameterConf, JValue) => Boolean] (
    FHIR_PARAMETER_TYPES.NUMBER -> numberQuery,
    FHIR_PARAMETER_TYPES.DATE -> dateQuery,
    FHIR_PARAMETER_TYPES.STRING -> stringQuery,
    FHIR_PARAMETER_TYPES.URI -> uriQuery,
    FHIR_PARAMETER_TYPES.TOKEN -> tokenQuery,
    FHIR_PARAMETER_TYPES.QUANTITY -> quantityQuery,
    FHIR_PARAMETER_TYPES.REFERENCE -> referenceQuery
  )*/

  //Function map based on serach parameter type (provides a function that gets element values and target types to evaluate)
  val typeHandlerFunctionOnValues =  HashMap[String, (String, String, Seq[(Seq[JValue], String)], Seq[String]) => Boolean] (
    FHIR_PARAMETER_TYPES.NUMBER -> numberQueryOnValues,
    FHIR_PARAMETER_TYPES.DATE -> dateQueryOnValues,
    FHIR_PARAMETER_TYPES.STRING -> stringQueryOnValues,
    FHIR_PARAMETER_TYPES.URI -> uriQueryOnValues,
    FHIR_PARAMETER_TYPES.TOKEN -> tokenQueryOnValues,
    FHIR_PARAMETER_TYPES.QUANTITY -> quantityQueryOnValues,
    FHIR_PARAMETER_TYPES.REFERENCE -> referenceQueryOnValues
  )

  /**
   * Handle simple parameter
   * @param parameter
   * @param sp
   * @param values
   * @return
   */
  def handleSimpleParameter(parameter:Parameter, sp:SearchParameterConf, values:Seq[(Seq[JValue], String)]): Boolean ={
    parameter.suffix match {
      //Common handling for missing prefix
      case FHIR_PREFIXES_MODIFIERS.MISSING =>
        InMemoryPrefixModifierHandler.misssingHandler(parameter.valuePrefixList.map(_._2).head, values.map(_._1))
      case _ =>
        handleParameter(parameter, sp, values)
    }
  }

  /**
   * Handle composite search parameter
   * @param parameter
   * @param sp
   * @param values
   * @param validSearchParamConfs
   * @return
   */
  def handleCompositeParameter(parameter:Parameter, sp:SearchParameterConf, values:Seq[(Seq[JValue], String)], validSearchParamConfs:Map[String, SearchParameterConf]):Boolean = {
    val result = parameter.valuePrefixList exists {
      //Prefix is not parsed in composite
      case (_, value) =>
        //Extract the values split by $
        val valuesArr = value.split('$')

        val childParamConfs =
          sp.targets
            .map(p => p -> validSearchParamConfs.get(p))

        if(childParamConfs.exists(_._2.isEmpty))
          throw new RuntimeException(s"One of the parameters (${childParamConfs.filter(_._2.isEmpty).map(_._1).mkString(",")}) referenced in composite parameter ${parameter.name} is missing in cache!")

        val childParamsConstructed =
          childParamConfs
            .zip(valuesArr)
            .map {
              case (spc, v) =>
                spc._2.get ->
                  Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, spc._2.get.ptype, spc._1, FHIRSearchParameterValueParser.parseSimpleValue(v, spc._2.get.ptype))
            }

        childParamsConstructed.forall(p =>
          ImMemorySearchUtil.handleSimpleParameter(p._2, p._1, values)
        )
    }
    result
  }


  /**
   * Handle a search on a parameter
   * @param parameter
   * @param values
   * @return
   */
  private def handleParameter(parameter:Parameter,  sp:SearchParameterConf, values:Seq[(Seq[JValue], String)]):Boolean = {
    parameter.valuePrefixList.exists {
      case (prefix, value) =>
        // A parameter can have either a prefix or modifier
        val prefixModifier = (parameter.suffix, prefix) match {
          case ("", "") => ""
          case (_, "") => parameter.suffix
          case ("", _) => prefix
          case _ => ""//Not Possible
        }

        ImMemorySearchUtil
          .typeHandlerFunctionOnValues(parameter.paramType)(value, prefixModifier, values, sp.targets)
    }
  }


  /**
   * Based on the paths defined in the search parameter extract the values of elements (more than one if the path indicates an array) and target type for each path
   * @param queryConfig Search parameter configuration
   * @param resource    FHIR Resource to evaluate
   * @return
   */
  def extractValuesAndTargetTypes(queryConfig:SearchParameterConf, resource:JValue):Seq[(Seq[JValue], String)] = {
    queryConfig
       .extractElementPathsTargetTypesAndRestrictions()
        .map(ptr =>
           ptr._3 match {
             //If there is no restriction extract the value from the path
             case Nil =>
               if(ptr._1 == "") //If no path get the resource itself
                 Seq(resource) -> ptr._2
               else
                  FHIRUtil.applySearchParameterPath(ptr._1, resource) -> ptr._2
             //If there are restrictions
             case r =>
               val pathParts = ptr._1.split('.')
                val indexOfRestrictions = FHIRUtil.findIndexOfRestrictionsOnPath(pathParts, ptr._3)
               extractValuesBasedOnRestrictions(pathParts, indexOfRestrictions, Seq(resource)) -> ptr._2
           }
        )
  }

  /**
   * Recursively extract values for a path with restrictions
   * @param pathParts             Splitted path parts e.g. component.valueQuantity, telecom
   * @param indexOfRestrictions   Restrictions and their indexes; 0 -> Seq(system, phone) -->
   * @param resources             Resources to evaluate
   * @return
   */
  private def extractValuesBasedOnRestrictions(pathParts:Seq[String], indexOfRestrictions:Seq[(Int, Seq[(String, String)])], resources:Seq[JValue]):Seq[JValue] = {
    val restriction = indexOfRestrictions.head
    val parentPath = pathParts.slice(0, restriction._1 + 1).mkString(".")

    val tempValues  =
      resources
        .flatMap(resource =>
          FHIRUtil
            .applySearchParameterPath(parentPath, resource) //Get the values targeted by the path that restriction is about
            .filter(v =>
              restriction._2. //all restriction should be satisfied
                forall(r =>
                  FHIRUtil.applySearchParameterPath(r._1, v)  //Get the element that restriction is about
                    .exists(_.extractOpt[String].contains(r._2))) //Compare the value
            )
        )

    val childPathParts = pathParts.drop(restriction._1 + 1)
    //If there is no more restriction
    if(indexOfRestrictions.tail.isEmpty) {
      val childPath = childPathParts.mkString(".")
      //If there is no child path after restriction (restriction is on the last path item), return the evaluated values
      if(childPath == "")
        tempValues
      else //Otherwise go get those elements
        tempValues.flatMap(v => FHIRUtil.applySearchParameterPath(childPath, v))
    } else {
      //If there are further restrictions, evaluate them recursively
      extractValuesBasedOnRestrictions(childPathParts, indexOfRestrictions.tail, tempValues)
    }
  }

  /**
   * Handle FHIR number queries
   * @param number                Compared number
   * @param prefix                FHIR Search prefix
   * @param valuesAndTargetType   Values extracted for each path and its target FHIR data type
   * @return
   */
  private def numberQueryOnValues(number:String, prefix:String, valuesAndTargetType:Seq[(Seq[JValue], String)], referenceTargets:Seq[String] = Nil):Boolean = {
    valuesAndTargetType.exists(valuesAndTarget =>
      valuesAndTarget._1.exists(value => {
        valuesAndTarget._2 match {
          case FHIR_DATA_TYPES.RANGE => InMemoryPrefixModifierHandler.rangePrefixHandler(number, prefix, value)
          case FHIR_DATA_TYPES.INTEGER => InMemoryPrefixModifierHandler.intPrefixHandler(number, prefix, value.extract[Int])
          case FHIR_DATA_TYPES.DECIMAL => InMemoryPrefixModifierHandler.decimalPrefixHandler(number, prefix, value.extract[Double])
          case other => throw new InternalServerException(s"Unknown target element type $other !!!")
        }
      })
    )
  }

  /*
  /**
    * Handle FHIR number queries
    * @param number Compared number
    * @param prefix
    * @param queryConfig Defined Search parameter
    * @param resource resource that query will be executed on
    * @return
    */
  private def numberQuery(number:String, prefix:String, queryConfig:SearchParameterConf, resource:JValue):Boolean = {
    val valuesAndTargetTypes = extractValuesAndTargetTypes(queryConfig, resource)
    numberQueryOnValues(number,prefix, valuesAndTargetTypes)
  }*/

  /**
   *
   * @param value
   * @param modifier
   * @param valuesAndTargetType
   * @return
   */
  private def stringQueryOnValues(value:String, modifier:String, valuesAndTargetType:Seq[(Seq[JValue], String)], referenceTargets:Seq[String] = Nil):Boolean = {
    valuesAndTargetType
      .flatMap(valueAndType =>
        STRING_TYPE_SEARCH_SUBPATHS
          .get(valueAndType._2)//If there are subpaths get those and get their values
          .map(subpaths =>
            subpaths.flatMap(sp =>
              valueAndType._1.flatMap(v => FHIRUtil.applySearchParameterPath(sp, v))
            )
          ).getOrElse(valueAndType._1) //Otherwise return the given values
      ).exists(actualValue => {
        InMemoryPrefixModifierHandler.stringQueryHandler(actualValue, modifier, value)
      })
  }


  /*
  /**
    * Handle FHIR string queries
    * @param value
    * @param modifier
    * @param queryConfig
    * @param resource
    * @return
    */
  private def stringQuery(value:String, modifier:String, queryConfig:SearchParameterConf, resource: JValue):Boolean = {
    //Get the JSON paths and target types for the query parameter
    val pathsAndTargetTypes = queryConfig.extractElementPathsAndTargetTypes()

    pathsAndTargetTypes
      .flatMap(valueAndType => //Find extra subpath if it is a
        STRING_TYPE_SEARCH_SUBPATHS
          .getOrElse(valueAndType._2, Seq(""))
          .map(subpath => FHIRUtil.mergeElementPath(valueAndType._1, subpath))
      )
      .flatMap(p => FHIRUtil.applySearchParameterPath(p, resource))
      .exists (actualValue => {
        InMemoryPrefixModifierHandler.stringQueryHandler(actualValue, modifier, value)
      })
  }*/

  /**
   *  Handles FHIR URI query
   * @param uri
   * @param modifier
   * @param valuesAndTargetType
   * @param referenceTargets
   * @return
   */
  private def uriQueryOnValues(uri:String, modifier:String, valuesAndTargetType:Seq[(Seq[JValue], String)], referenceTargets:Seq[String] = Nil):Boolean = {
    valuesAndTargetType
      .flatMap(_._1)
      .exists(actualValue => InMemoryPrefixModifierHandler.uriQueryHandler(actualValue, modifier, uri))
  }

  /*
  /**
    * Handles FHIR URI query
    * @param uri
    * @param modifier
    * @param queryConfig
    * @param resource
    * @return
    */
  private def uriQuery(uri:String, modifier:String, queryConfig:SearchParameterConf, resource: JValue):Boolean = {
    val valuesAndTargetTypes = extractValuesAndTargetTypes(queryConfig, resource)
    uriQueryOnValues(uri, modifier, valuesAndTargetTypes)
  }
  */

  /**
   *
   * @param date
   * @param prefix
   * @param valuesAndTargetTypes
   * @return
   */
  private def dateQueryOnValues(date:String, prefix:String, valuesAndTargetTypes:Seq[(Seq[JValue], String)], referenceTargets:Seq[String] = Nil):Boolean = {
    // '+' in time zone field is replaced with ' ' by the server
    val dateValue = date.replace(" ", "+")

    val qrange = DateTimeUtil.populateImplicitDateTimeRanges(dateValue)
    val qtimeRange = Instant.parse(qrange._1) -> Instant.parse(qrange._2)

    valuesAndTargetTypes
      .exists(valuesAndTargetType =>
        valuesAndTargetType._2 match {
          case FHIR_DATA_TYPES.STRING |
               FHIR_DATA_TYPES.DATE |
               FHIR_DATA_TYPES.DATETIME |
               FHIR_DATA_TYPES.INSTANT =>

            valuesAndTargetType._1
              .exists(actualTime => {
                val atime = DateTimeUtil.populateImplicitDateTimeRanges(actualTime.extract[String])
                if (atime._1.equals(atime._2))
                  InMemoryPrefixModifierHandler.dateTimeQueryBuilder(qtimeRange, prefix, Instant.parse(atime._1))
                else
                  InMemoryPrefixModifierHandler.dateTimeQueryBuilder(qtimeRange, prefix, Some(Instant.parse(atime._1)) -> Some(Instant.parse(atime._2)))
              })
          case FHIR_DATA_TYPES.PERIOD =>
            valuesAndTargetType._1
              .exists(actualTime => InMemoryPrefixModifierHandler.periodPrefixHandler(qtimeRange, prefix, actualTime))
          case FHIR_DATA_TYPES.TIMING =>
            valuesAndTargetType._1
              .exists( v =>
                FHIRUtil
                  .applySearchParameterPath(s"${FHIR_COMMON_FIELDS.REPEAT}.${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}", v)
                  .exists(actualTime => InMemoryPrefixModifierHandler.periodPrefixHandler(qtimeRange, prefix, actualTime))
              )
          case other =>
            throw new InternalServerException(s"Unknown target element type $other !!!")
        }
      )
  }

  /*
  /**
    * Handle FHIR date queries
    * @param date
    * @param prefix
    * @param queryConfig
    * @param resource
    * @return
    */
  private def dateQuery(date:String, prefix:String, queryConfig:SearchParameterConf, resource: JValue):Boolean = {
    val valuesAndTargetTypes = extractValuesAndTargetTypes(queryConfig, resource)
    dateQueryOnValues(date, prefix, valuesAndTargetTypes)
  }*/

  /*/**
    * Handle token query with a search parameter
    * @param token
    * @param modifier
    * @param queryConfig
    * @param resource
    * @return
    */
  private def tokenQuery(token:String, modifier:String, queryConfig:SearchParameterConf, resource:JValue): Boolean = {
    val valuesAndTargetTypes = extractValuesAndTargetTypes(queryConfig, resource)
    tokenQueryOnValues(token, modifier, valuesAndTargetTypes)
  }*/

  /**
   * Handle token query
   * @param token                 Given token value
   * @param modifier              Search modifier
   * @param valuesAndTargetTypes  Extracted values and target types from the resource
   * @return
   */
  private def tokenQueryOnValues(token:String, modifier:String, valuesAndTargetTypes:Seq[(Seq[JValue], String)], referenceTargets:Seq[String] = Nil):Boolean = {
    val result = valuesAndTargetTypes.exists(valuesAndTargetType =>
      valuesAndTargetType._2 match {
        case FHIR_DATA_TYPES.STRING | FHIR_DATA_TYPES.ID |  FHIR_DATA_TYPES.URI | FHIR_DATA_TYPES.CODE =>
          valuesAndTargetType._1
            .exists(_.extractOpt[String].contains(token)) //Check if it is equal the given value

        case FHIR_DATA_TYPES.BOOLEAN =>
          valuesAndTargetType._1
            .exists(_.extractOpt[Boolean].contains(token.toBoolean))//Check if it is equal the given value

        case FHIR_DATA_TYPES.IDENTIFIER if modifier == FHIR_PREFIXES_MODIFIERS.OF_TYPE =>
          val (typeSystem, typeCode, value) = FHIRUtil.parseTokenOfTypeValue(token)
          valuesAndTargetType._1
            .filter(_.isInstanceOf[JObject])
            .map(_.asInstanceOf[JObject])
            .exists(idenfierElement =>
              FHIRUtil.applySearchParameterPath(FHIR_COMMON_FIELDS.VALUE, idenfierElement) //Get the value part
                .exists(_.extractOpt[String].contains(value)) && //Check if it is equal to given velue
                FHIRUtil
                  .applySearchParameterPath(s"${FHIR_COMMON_FIELDS.TYPE}.${FHIR_COMMON_FIELDS.CODING}", idenfierElement) //get all codings
                  .map(_.asInstanceOf[JObject]) //Check if one of them is ok with the system and code
                  .exists(codingElement => //one should match with the given code
                    InMemoryPrefixModifierHandler.tokenModifierHandler(FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, Some(typeSystem), Some(typeCode), "", codingElement)
                  )
            )
        case _ =>
          modifier match {
            case FHIR_PREFIXES_MODIFIERS.TEXT =>
              valuesAndTargetType._1
                .exists(v => InMemoryPrefixModifierHandler.handleTokenTextModifier(token, valuesAndTargetType._2, v))
            case _ =>
              val actualValues:Seq[JValue] =
                //2 level complex type, so add path for the the last complex type
                if(valuesAndTargetType._2 == FHIR_DATA_TYPES.CODEABLE_CONCEPT)
                  valuesAndTargetType._1.flatMap(v => FHIRUtil.applySearchParameterPath(FHIR_COMMON_FIELDS.CODING, v))
                else
                  valuesAndTargetType._1

              //Get the field paths
              val (systemField, codeField) = TOKEN_TYPE_SEARCH_SUBPATHS(valuesAndTargetType._2)

              val (system, code) = FHIRUtil.parseTokenValue(token)
              actualValues
                .filter(_.isInstanceOf[JObject])
                .map(_.asInstanceOf[JObject])
                .exists(parentElement =>
                  InMemoryPrefixModifierHandler.tokenModifierHandler(systemField, codeField, system, code, modifier, parentElement)
                )
          }
      }
    )

    modifier match {
      case FHIR_PREFIXES_MODIFIERS.NOT => !result
      case _ => result
    }
  }


  /*private def tokenQuery(token:String, modifier:String, path:String, targetType:String, resource:JValue):Boolean = {
    targetType match {
      case FHIR_DATA_TYPES.STRING | FHIR_DATA_TYPES.ID |  FHIR_DATA_TYPES.URI | FHIR_DATA_TYPES.CODE =>
        FHIRUtil.applySearchParameterPath(path, resource) //Get the value of the path
          .exists(_.extractOpt[String].contains(token)) //Check if it is equal the given value

      case FHIR_DATA_TYPES.BOOLEAN =>
        FHIRUtil.applySearchParameterPath(path, resource)//Get the value of the path
          .exists(_.extractOpt[Boolean].contains(token.toBoolean))//Check if it is equal the given value

      case FHIR_DATA_TYPES.IDENTIFIER if modifier == FHIR_PREFIXES_MODIFIERS.OF_TYPE =>
        val (typeSystem, typeCode, value) = FHIRUtil.parseTokenOfTypeValue(token)
        FHIRUtil.applySearchParameterPath(path, resource) //Get the identifier element
          .map(_.asInstanceOf[JObject])
          .exists(idenfierElement =>
            FHIRUtil.applySearchParameterPath(FHIR_COMMON_FIELDS.VALUE, idenfierElement) //Get the value part
              .exists(_.extractOpt[String].contains(value)) && //Check if it is equal to given velue
            FHIRUtil
              .applySearchParameterPath(s"${FHIR_COMMON_FIELDS.TYPE}.${FHIR_COMMON_FIELDS.CODING}", idenfierElement) //get all codings
              .map(_.asInstanceOf[JObject]) //Check if one of them is ok with the system and code
              .exists(codingElement => //one should match with the given code
                InMemoryPrefixModifierHandler.tokenModifierHandler(FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, Some(typeSystem), Some(typeCode), "", codingElement)
              )
          )
      case _ =>
        modifier match {
          case FHIR_PREFIXES_MODIFIERS.TEXT =>
            InMemoryPrefixModifierHandler.handleTokenTextModifier(path, token, targetType, resource)
          case _ =>
            var complextElementPath = path
            //2 level complex type, so add path for the the last complex type
            if(targetType == FHIR_DATA_TYPES.CODEABLE_CONCEPT)
              complextElementPath = FHIRUtil.mergeElementPath(complextElementPath, FHIR_COMMON_FIELDS.CODING)
            //Get the field paths
            val (systemField, codeField) = TOKEN_TYPE_SEARCH_SUBPATHS(targetType)

            val (system, code) = FHIRUtil.parseTokenValue(token)
              FHIRUtil
                .applySearchParameterPath(complextElementPath, resource)
                .map(_.asInstanceOf[JObject])
                .exists(parentElement =>
                  InMemoryPrefixModifierHandler.tokenModifierHandler(systemField, codeField, system, code, modifier, parentElement)
                )
        }
    }
  }*/

  /**
   * Handle query on references
   * @param reference               Expected value
   * @param modifier                Search modifier
   * @param valuesAndTargetTypes    Values and target types extracted from evaluated resource
   * @param referenceTargetTypes    Possible target types for reference
   * @return
   */
  private def referenceQueryOnValues(reference:String, modifier:String, valuesAndTargetTypes:Seq[(Seq[JValue], String)], referenceTargetTypes:Seq[String]):Boolean = {
    valuesAndTargetTypes.exists(valuesAndTargetType =>
      valuesAndTargetType._2 match {
        case FHIR_DATA_TYPES.REFERENCE =>
          //Parse the given reference
          val (url, rtype, rid, version) = FHIRUtil.resolveReferenceValue(reference, modifier, referenceTargetTypes)

          modifier match {
            //If modifier is identifier, search like a token on identifier element (Reference.identifier)
            case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
              val identifierValues =
                valuesAndTargetType._1
                  .map(v => FHIRUtil.applySearchParameterPath(FHIR_COMMON_FIELDS.IDENTIFIER, v) -> FHIR_DATA_TYPES.IDENTIFIER)
              tokenQueryOnValues(rid, "", identifierValues)
            //Query only on resource type to refer
            case FHIR_PREFIXES_MODIFIERS.TYPE =>
              valuesAndTargetType._1
                .flatMap(v =>  FHIRUtil.applySearchParameterPath(FHIR_COMMON_FIELDS.REFERENCE, v)) //Get the reference element
                .exists(
                  _.extractOpt[String]
                    .flatMap(ref => Try(FHIRUtil.parseReferenceValue(ref)).toOption) //parse it
                    .exists { case (_, actualResourceType, _, _) =>
                      actualResourceType == rtype
                    }
                )
            //No modifier
            case _ =>
              valuesAndTargetType._1
                .flatMap(v =>  FHIRUtil.applySearchParameterPath(FHIR_COMMON_FIELDS.REFERENCE, v)) //Get the reference element
                .exists(
                  _.extractOpt[String]
                    .flatMap(ref => Try(FHIRUtil.parseReferenceValue(ref)).toOption) //parse it
                    .exists { case (actualUrl, actualResourceType, actualRid, actualVersion) =>
                      actualResourceType == rtype &&
                        actualRid == rid &&
                        isUrlMatching(actualUrl, url) &&
                        version == actualVersion
                    }
                )
          }

        //Handle canonical target type
        case   FHIR_DATA_TYPES.CANONICAL =>
          //As for canonical, we only look at one field we don't need to care arrays in the path
          modifier match {
            case FHIR_PREFIXES_MODIFIERS.BELOW =>
              val (canonicalUrl, canonicalVersion) = FHIRUtil.parseCanonicalValue(reference)
              valuesAndTargetType._1
                .flatMap(_.extractOpt[String])
                .exists(canonical => {
                  val (actualUrl, actualVersion) = FHIRUtil.parseCanonicalValue(canonical)
                  actualUrl == canonicalUrl && actualVersion.forall(av => canonicalVersion.forall(cv => av.startsWith(cv + ".")))
                })
            case _ =>
              val (canonicalUrl, canonicalVersion) = FHIRUtil.parseCanonicalValue(reference)
              valuesAndTargetType._1
                .flatMap(_.extractOpt[String])
                .exists(canonical => {
                  val (actualUrl, actualVersion) = FHIRUtil.parseCanonicalValue(canonical)
                  actualUrl == canonicalUrl && //URLs should match
                    canonicalVersion.forall(cv => actualVersion.contains(cv)) //if given version should match
                })
          }

        //Not possible
        case other =>
          throw new InvalidParameterException(s"Reference search is not supported on $other data type!")
      }
    )

  }

  /*
  /**
    * Handle FHIR reference query on resource
    * @param reference Compared reference
    * @param modifier search modifier
    * @param queryConfig search parameter config
    * @param resource Resource itself
    * @return
    */
  private def referenceQuery(reference:String, modifier:String, queryConfig:SearchParameterConf, resource:JValue):Boolean = {
    val valuesAndTargetTypes = extractValuesAndTargetTypes(queryConfig, resource)
    referenceQueryOnValues(reference, modifier, valuesAndTargetTypes, queryConfig.targets)
  }*/

  /**
    * Check if URL for reference values matches
    * @param actualUrl
    * @param queryUrl
    * @return
    */
  private def isUrlMatching(actualUrl:Option[String], queryUrl:Option[String]) = {
    (actualUrl, queryUrl) match {
      case (None, None) => true //if not url in both it is ok
      case (Some(OnfhirConfig.fhirRootUrl), None) => true //if one of them is FHIR root URL and other is missing, it is OK
      case (None, Some(OnfhirConfig.fhirRootUrl)) => true
      case (Some(x), Some(y)) if x==y => true //if they are equal it is ok
      case _ => false
    }
  }

  /**
   * Handle quantity queries
   * @param quantity
   * @param prefix
   * @param valuesAndTargetTypes
   * @param referenceTargetTypes
   * @return
   */
  private def quantityQueryOnValues(quantity:String, prefix:String, valuesAndTargetTypes:Seq[(Seq[JValue], String)], referenceTargetTypes:Seq[String] = Nil):Boolean = {
    //Parse the given value
    val (value,system, code) = FHIRUtil.parseQuantityValue(quantity)

    valuesAndTargetTypes
      .exists(valuesAndTargetType =>
        valuesAndTargetType._1
          .filter(_.isInstanceOf[JObject])
          .map(_.asInstanceOf[JObject])
          .exists(parentElement =>
            valuesAndTargetType._2 match {
              case FHIR_DATA_TYPES.QUANTITY |
                   FHIR_DATA_TYPES.SIMPLE_QUANTITY |
                   FHIR_DATA_TYPES.MONEY_QUANTITY |
                   FHIR_DATA_TYPES.AGE |
                   FHIR_DATA_TYPES.DISTANCE |
                   FHIR_DATA_TYPES.COUNT |
                   FHIR_DATA_TYPES.DURATION =>

                    FHIRUtil
                      .applySearchParameterPath(FHIR_COMMON_FIELDS.VALUE, parentElement)
                      .flatMap(_.extractOpt[Double])
                      .exists(v =>  InMemoryPrefixModifierHandler.decimalPrefixHandler(value, prefix, v)) && //Value equality
                      InMemoryPrefixModifierHandler
                        .handleQuantityCodeSystemQuery(system, code, FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT, parentElement)


              case FHIR_DATA_TYPES.SAMPLED_DATA =>
                InMemoryPrefixModifierHandler.rangePrefixHandler(value, prefix, parentElement, isSampleData = true) &&
                  InMemoryPrefixModifierHandler
                    .handleQuantityCodeSystemQuery(system, code, s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.SYSTEM}", s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.CODE}",s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.UNIT}", parentElement)
            }
      ))
  }

  /*
  /**
    * Handle FHIR Quantity queries
    * @param quantity query string
    * @param prefix prefix used
    * @param queryConfig search parameter config
    * @param resource
    * @return
    */
  private def quantityQuery(quantity:String, prefix:String, queryConfig:SearchParameterConf, resource:JValue):Boolean = {
    val valuesAndTargetTypes = extractValuesAndTargetTypes(queryConfig, resource)
    quantityQueryOnValues(quantity, prefix, valuesAndTargetTypes)
  }*/

}

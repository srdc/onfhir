package io.onfhir.api.util

import java.time.Instant

import io.onfhir.api._
import io.onfhir.config.{OnfhirConfig, SearchParameterConf}
import io.onfhir.exception.{InternalServerException, InvalidParameterException}
import io.onfhir.util.DateTimeUtil
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JObject, JValue}

import scala.collection.immutable.HashMap
import scala.util.Try

/**
  * Implementation of FHIR search in memory over a resource
  */
object ImMemorySearchUtil {
  //Function map based on search parameter type ->  (value, prefix, config, resource) ->Boolean
  val typeHandlerFunction = HashMap[String, (String, String, SearchParameterConf, JValue) => Boolean] (
    FHIR_PARAMETER_TYPES.NUMBER -> numberQuery,
    FHIR_PARAMETER_TYPES.DATE -> dateQuery,
    FHIR_PARAMETER_TYPES.STRING -> stringQuery,
    FHIR_PARAMETER_TYPES.URI -> uriQuery,
    FHIR_PARAMETER_TYPES.TOKEN -> tokenQuery,
    FHIR_PARAMETER_TYPES.QUANTITY -> quantityQuery,
    FHIR_PARAMETER_TYPES.REFERENCE -> referenceQuery
  )

  /**
    * Handle FHIR number queries
    * @param number Compared number
    * @param prefix search prefix
    * @param queryConfig Defined Search parameter
    * @param resource resource that query will be executed on
    * @return
    */
  private def numberQuery(number:String, prefix:String, queryConfig:SearchParameterConf, resource:JValue):Boolean = {
    //Get the JSON paths and target types for the query parameter
    val paths = queryConfig.extractElementPaths()
    val targetTypes = queryConfig.targetTypes
    paths
      .map(p => FHIRUtil.applySearchParameterPath(p, resource))
      .zip(targetTypes)
      .exists(valuesAndTarget =>
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
    val paths = queryConfig.extractElementPaths()
    val targetTypes = queryConfig.targetTypes
    paths
      .zip(targetTypes)
      .flatMap(valueAndType => //Find extra subpath if it is a
        STRING_TYPE_SEARCH_SUBPATHS
          .getOrElse(valueAndType._2, Seq(""))
          .map(subpath => FHIRUtil.mergeElementPath(valueAndType._1, subpath))
      )
      .flatMap(p => FHIRUtil.applySearchParameterPath(p, resource))
      .exists (actualValue => {
          modifier match {
            case FHIR_PREFIXES_MODIFIERS.EXACT =>
              // Exact match provided with $eq mongo operator
             actualValue.extract[String].equalsIgnoreCase(value)
            case FHIR_PREFIXES_MODIFIERS.CONTAINS =>
              actualValue.extract[String].toLowerCase.contains(value.toLowerCase())
            case _ =>
              // Case insensitive, partial matches at the end of string
              actualValue.extract[String].toLowerCase.startsWith(value.toLowerCase())
          }
      })
  }


  /**
    * Handles FHIR URI query
    * @param uri
    * @param modifier
    * @param queryConfig
    * @param resource
    * @return
    */
  private def uriQuery(uri:String, modifier:String, queryConfig:SearchParameterConf, resource: JValue):Boolean = {
    //Get the JSON paths and target types for the query parameter
    val paths = queryConfig.extractElementPaths()
    paths
      .flatMap(p => FHIRUtil.applySearchParameterPath(p, resource))
      .exists(actualValue => {
        modifier match {
          case FHIR_PREFIXES_MODIFIERS.ABOVE =>
            val parentUri = uri.split("/").dropRight(2).mkString("/")
            actualValue.extract[String].startsWith(parentUri)
          case FHIR_PREFIXES_MODIFIERS.BELOW =>
            actualValue.extract[String].startsWith(uri)
          case "" =>
            actualValue.extract[String].equals(uri)
        }
      })
  }


  /**
    * Handle FHIR date queries
    * @param date
    * @param prefix
    * @param queryConfig
    * @param resource
    * @return
    */
  private def dateQuery(date:String, prefix:String, queryConfig:SearchParameterConf, resource: JValue):Boolean = {
    // '+' in time zone field is replaced with ' ' by the server
    val dateValue = date.replace(" ", "+")
    //Get the JSON paths and target types for the query parameter
    val paths = queryConfig.extractElementPaths()
    val targetTypes = queryConfig.targetTypes

    val qrange = DateTimeUtil.populateImplicitDateTimeRanges(dateValue)
    val qtimeRange = Instant.parse(qrange._1) -> Instant.parse(qrange._2)

    paths.zip(targetTypes).exists(pathTypes => {
      pathTypes._2 match {
        case FHIR_DATA_TYPES.STRING |
             FHIR_DATA_TYPES.DATE |
             FHIR_DATA_TYPES.DATETIME |
             FHIR_DATA_TYPES.INSTANT =>

          FHIRUtil
            .applySearchParameterPath(pathTypes._1, resource)
            .exists(actualTime => {
              val atime = DateTimeUtil.populateImplicitDateTimeRanges(actualTime.extract[String])
              if (atime._1.equals(atime._2))
                InMemoryPrefixModifierHandler.dateTimeQueryBuilder(qtimeRange, prefix, Instant.parse(atime._1))
              else
                InMemoryPrefixModifierHandler.dateTimeQueryBuilder(qtimeRange, prefix, Some(Instant.parse(atime._1)) -> Some(Instant.parse(atime._2)))
            })
        case FHIR_DATA_TYPES.PERIOD =>
          FHIRUtil
            .applySearchParameterPath(pathTypes._1, resource)
            .exists(actualTime => {
              InMemoryPrefixModifierHandler.periodPrefixHandler(qtimeRange, prefix, actualTime)
            })
        case FHIR_DATA_TYPES.TIMING =>
          FHIRUtil
            .applySearchParameterPath(pathTypes._1 + s".${FHIR_COMMON_FIELDS.REPEAT}.${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}", resource)
            .exists(actualTime => {
              InMemoryPrefixModifierHandler.periodPrefixHandler(qtimeRange, prefix, actualTime)
            })
        case other =>
          throw new InternalServerException(s"Unknown target element type $other !!!")
      }
    })
  }

  /**
    * Handle token query with a search parameter
    * @param token
    * @param modifier
    * @param queryConfig
    * @param resource
    * @return
    */
  private def tokenQuery(token:String, modifier:String, queryConfig:SearchParameterConf, resource:JValue): Boolean ={
    //Get the JSON paths and target types for the query parameter
    val paths = queryConfig.extractElementPaths()
    val targetTypes = queryConfig.targetTypes

    modifier match {
      case FHIR_PREFIXES_MODIFIERS.NOT =>
        ! paths.zip(targetTypes).exists(pathTypes => {
          tokenQuery(token, "", pathTypes._1, pathTypes._2, resource)
        })

      case  _ =>
        paths.zip(targetTypes).exists(pathTypes => {
          tokenQuery(token, modifier, pathTypes._1, pathTypes._2, resource)
        })
    }
  }

  /**
    * Handle token query on a single path
    * @param token Given value
    * @param modifier Search modifier
    * @param path Path to the element to query on
    * @param targetType Target element's type
    * @param resource Resource to qun query on
    * @return
    */
  private def tokenQuery(token:String, modifier:String, path:String, targetType:String, resource:JValue):Boolean = {
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
  }

  /**
    * Handle FHIR reference query on resource
    * @param reference Compared reference
    * @param modifier search modifier
    * @param queryConfig search parameter config
    * @param resource Resource itself
    * @return
    */
  private def referenceQuery(reference:String, modifier:String, queryConfig:SearchParameterConf, resource:JValue):Boolean = {
    //Get paths and target ResourceTypes
    val paths = queryConfig.extractElementPaths()

    paths.zip(queryConfig.targetTypes).exists {
      //Reference query
      case (path, FHIR_DATA_TYPES.REFERENCE) =>
        //Parse the given reference
        val (url, rtype, rid, version) = FHIRUtil.resolveReferenceValue(reference, modifier, queryConfig.targets)

        modifier match {
          //If modifier is identifier, search like a token on identifier element (Reference.identifier)
          case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
            tokenQuery(rid, modifier = "", FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.IDENTIFIER), FHIR_DATA_TYPES.IDENTIFIER, resource)
          //Query only on resource type to refer
          case FHIR_PREFIXES_MODIFIERS.TYPE =>
            FHIRUtil
              .applySearchParameterPath(FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.REFERENCE), resource) //Get the reference value
              .exists(
                _.extractOpt[String]
                  .flatMap(ref => Try(FHIRUtil.parseReferenceValue(ref)).toOption) //parse it
                  .exists { case (_, actualResourceType, _, _) =>
                    actualResourceType == rtype
                  }
              )
          //No modifier
          case _ =>
            FHIRUtil
              .applySearchParameterPath(FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.REFERENCE), resource)
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

      case   (path, FHIR_DATA_TYPES.CANONICAL) =>
        //As for canonical, we only look at one field we don't need to care arrays in the path
        modifier match {
          case FHIR_PREFIXES_MODIFIERS.BELOW =>
            val (canonicalUrl, canonicalVersion) = FHIRUtil.parseCanonicalValue(reference)
            FHIRUtil
              .applySearchParameterPath(path, resource)
              .flatMap(_.extractOpt[String])
              .exists(canonical => {
                val (actualUrl, actualVersion) = FHIRUtil.parseCanonicalValue(canonical)
                actualUrl == canonicalUrl && actualVersion.forall(av => canonicalVersion.forall(cv => av.startsWith(cv + ".")))
              })
          case _ =>
            val (canonicalUrl, canonicalVersion) = FHIRUtil.parseCanonicalValue(reference)
            FHIRUtil
              .applySearchParameterPath(path, resource)
              .flatMap(_.extractOpt[String])
              .exists(canonical => {
                val (actualUrl, actualVersion) = FHIRUtil.parseCanonicalValue(canonical)
                actualUrl == canonicalUrl && //URLs should match
                  canonicalVersion.forall(cv => actualVersion.contains(cv)) //if given version should match
              })
        }

      case (path, other) =>
        throw new InvalidParameterException(s"Reference search is not supported on $other data type!")
    }
  }

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
    * Handle FHIR Quantity queries
    * @param quantity query string
    * @param prefix prefix used
    * @param queryConfig search parameter config
    * @param resource
    * @return
    */
  private def quantityQuery(quantity:String, prefix:String, queryConfig:SearchParameterConf, resource:JValue):Boolean = {
    //Parse the given value
    val (value,system, code) = FHIRUtil.parseQuantityValue(quantity)

    //Get the JSON paths and target types for the query parameter
    val paths = queryConfig.extractElementPaths()

    paths
      .zip(queryConfig.targetTypes)
      .exists {
        case (path, targetType) =>
          FHIRUtil.applySearchParameterPath(path, resource)
            .map(_.asInstanceOf[JObject])
            .exists(parentElement =>
              targetType match {
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
                    .exists(v =>  InMemoryPrefixModifierHandler.decimalPrefixHandler(value, prefix, v)) &&
                    InMemoryPrefixModifierHandler
                      .handleQuantityCodeSystemQuery(system, code, FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT, parentElement)
                case FHIR_DATA_TYPES.SAMPLED_DATA =>
                  InMemoryPrefixModifierHandler.rangePrefixHandler(value, prefix, parentElement, isSampleData = true) &&
                    InMemoryPrefixModifierHandler
                      .handleQuantityCodeSystemQuery(system, code, s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.SYSTEM}", s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.CODE}",s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.UNIT}", parentElement)
              }
            )
      }
  }

}

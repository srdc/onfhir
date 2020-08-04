package io.onfhir.db

import io.onfhir.api._
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters._

import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.{InternalServerException, InvalidParameterException}
import org.mongodb.scala.bson.conversions.Bson


import scala.collection.immutable.HashMap

/**
  * Handles the searching according to type of Search Parameter
  */
object SearchUtil {
  //Function map based on search parameter type ->  (value, prefix, path, target FHIR type) -> BSON
  val typeHandlerFunction = HashMap[String, (String, String, String, String, Seq[String]) => Bson] (
    FHIR_PARAMETER_TYPES.NUMBER -> numberQuery,
    FHIR_PARAMETER_TYPES.DATE -> dateQuery,
    FHIR_PARAMETER_TYPES.STRING -> stringQuery,
    FHIR_PARAMETER_TYPES.URI -> uriQuery,
    FHIR_PARAMETER_TYPES.TOKEN -> tokenQuery,
    FHIR_PARAMETER_TYPES.QUANTITY -> quantityQuery,
    FHIR_PARAMETER_TYPES.REFERENCE -> referenceQuery
  )

  /**
    * Search based on numerical values and range
    *
    * @param number Query value
    * @param prefix Prefix to be handled
    * @param path Path to the target element to be queried
    * @param targetType FHIR Type of the target element
    * @return respective BsonDocument for target query
    */
  private def numberQuery(number:String, prefix:String, path:String, targetType:String, targetReferences:Seq[String]  = Nil):Bson = {
    targetType match{
      case FHIR_DATA_TYPES.RANGE =>
        //Split the parts
        val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
        //Main query on Range object
        val mainQuery = PrefixModifierHandler.rangePrefixHandler(queryPath.getOrElse(""), number, prefix)
        //If an array exist, use elemMatch otherwise return the query
        elemMatchPath match {
          case None => mainQuery
          case Some(emp) => elemMatch(emp, mainQuery)
        }
      //Ad these are simple searches we don't need to handle arrays with elemMatch
      case FHIR_DATA_TYPES.INTEGER => PrefixModifierHandler.intPrefixHandler(FHIRUtil.normalizeElementPath(path), number, prefix)
      case FHIR_DATA_TYPES.DECIMAL => PrefixModifierHandler.decimalPrefixHandler(FHIRUtil.normalizeElementPath(path), number, prefix)
      case other =>  throw new InternalServerException(s"Unknown target element type $other !!!")
    }
  }

  /**
    * Date parameter searches on a date/time or period. As is usual for date/time related functionality
    * has the following form;
    *
    * yyyy-mm-ddThh:mm:ss[Z|(+|-)hh:mm] (the standard XML format).
    *
    * Some parameters of date type could be defined as both period or regular date, therefore, a query
    * that match each case is defined.
    *
    * @param date Query value of the date parameter
    * @param prefix Prefix to be handled
    * @param path Path to the target element to be queried
    * @param targetType FHIR Type of the target element
    * @return equivalent BsonDocument for the target query
    */
  private def dateQuery(date:String, prefix:String, path:String, targetType:String, targetReferences:Seq[String]  = Nil):Bson = {
    //Split the parts
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
    // '+' in time zone field is replaced with ' ' by the server
    val dateValue = date.replace(" ", "+")
    // Construct main query on date object
    val mainQuery = targetType match {
      case FHIR_DATA_TYPES.DATE |
           FHIR_DATA_TYPES.DATETIME |
           FHIR_DATA_TYPES.INSTANT =>
        PrefixModifierHandler.dateRangePrefixHandler(queryPath.getOrElse(""), dateValue, prefix)
      case FHIR_DATA_TYPES.PERIOD =>
        PrefixModifierHandler.periodPrefixHandler(queryPath.getOrElse(""), dateValue, prefix, isTiming = false)
      case FHIR_DATA_TYPES.TIMING =>
        //TODO Handle event case better by special query on array (should match for all elements, not or)
        Filters.or(
          PrefixModifierHandler.timingEventHandler(queryPath.getOrElse(""), dateValue, prefix),
          //PrefixModifierHandler.dateRangePrefixHandler(FHIRUtil.mergeElementPath(queryPath, "event"), dateValue, prefix),
          PrefixModifierHandler.periodPrefixHandler(queryPath.getOrElse(""), dateValue, prefix, isTiming = true)
        )
      case other =>
        throw new InternalServerException(s"Unknown target element type $other !!!")
    }
    //If an array exist, use elemMatch otherwise return the query
    elemMatchPath match {
      case None => mainQuery
      case Some(emp) => elemMatch(emp, mainQuery)
    }
  }

  /**
   * String parameter serves as the input for a case- and accent-insensitive search against sequences of
   * characters, :exact modifier is used for case and accent sensitive search, :contains modifier is
   * used for case and accent insensitive partial matching search.
   * @param value             Query value of the string parameter
   * @param modifier          Prefix to be handled
   * @param path              Path to the target element to be queried
   * @param targetType        FHIR Type of the target element
   * @param targetReferences
   * @return
   */
  private def stringQuery(value:String, modifier:String,  path:String, targetType:String, targetReferences:Seq[String]  = Nil):Bson = {
    targetType match {
      case FHIR_DATA_TYPES.STRING =>
        PrefixModifierHandler.stringModifierHandler(FHIRUtil.normalizeElementPath(path), value, modifier)
      case FHIR_DATA_TYPES.HUMAN_NAME | FHIR_DATA_TYPES.ADDRESS =>
        //Split the parts
        val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
        val queries =
          STRING_TYPE_SEARCH_SUBPATHS(targetType)
            .map(subpath => FHIRUtil.mergeElementPath(queryPath, subpath))
            .map(p => PrefixModifierHandler.stringModifierHandler(p, value, modifier))
        val mainQuery = or(queries:_*)
        //If an array exist, use elemMatch otherwise return the query
        elemMatchPath match {
          case None => mainQuery
          case Some(emp) => elemMatch(emp, mainQuery)
        }
      case other =>
        throw new InvalidParameterException(s"String type search is not supported on target type $other!")
    }
  }

  /**
    * The uri parameter refers to a URI (RFC 3986) element. Matches are precise (e.g. case, accent, and escape)
    * sensitive, and the entire URI must match. The modifier :above or :below can be used to indicate that
    * partial matching is used.
    *
    * @param uri value of the uri parameter
    * @param modifier Modifier to be handled
    * @param path Path to the target element to be queried
    * @param targetType FHIR Type of the target element
    * @return equivalent BsonDocument for the target query
    */
  private def uriQuery(uri:String, modifier:String, path:String, targetType:String, targetReferences:Seq[String]  = Nil):Bson = {
    PrefixModifierHandler.uriModifierHandler(FHIRUtil.normalizeElementPath(path), uri, modifier)
  }

  /**
    * A token type is a parameter that searches on a URI/value pair. It is used against a code or identifier
    * data type where the value may have a URI that scopes its meaning. The search is performed against the
    * pair from a Coding or an Identifier. The syntax for the value is one of the following:

    * [parameter]=[code]: the value of [code] matches a Coding.code or Identifier.value
    * irrespective of the value of the system property
    *
    * [parameter]=[system]|[code]: the value of [code] matches a Coding.code or Identifier.value,
    * and the value of [system] matches the system property of the Identifier or Coding
    *
    * [parameter]=|[code]: the value of [code] matches a Coding.code or Identifier.value, and the
    * Coding/Identifier has no system property
    *
    * e.g. GET [base]/Condition?code=http://acme.org/conditions/codes|ha125
    * e.g  GET [base]/Patient?gender:not=male
    * e.g. GET [base]/Condition?code:text=headache
    *
    * // TODO Missing Modifiers Requires Value-Sets (above, below, in, not-in)
    *
    * @param token The token part of the query
    * @param modifier The modifier part of the query
    * @param path The path to the element for the corresponding query parameter
    *             e.g. One of the path for 'value-date' search parameter in Observation is 'valueDateTime'
    * @param targetType Type of the target element that path goes to e.g. 'dateTime', 'Period'
    * @return corresponding MongoDB Query for single path of the query
    */
  private def tokenQuery(token:String, modifier:String, path:String, targetType:String, targetReferences:Seq[String] = Nil):Bson = {
    targetType match {
      //Simple types, only one field to match (so we don't need to evaluate elemMatch options)
      case FHIR_DATA_TYPES.STRING | FHIR_DATA_TYPES.ID |  FHIR_DATA_TYPES.URI | FHIR_DATA_TYPES.CODE =>
        Filters.eq(FHIRUtil.normalizeElementPath(path), token)
      case FHIR_DATA_TYPES.BOOLEAN =>
        PrefixModifierHandler.tokenBooleanModifierHandler(FHIRUtil.normalizeElementPath(path), token, modifier)
      //A special modifier case
      case FHIR_DATA_TYPES.IDENTIFIER if modifier == FHIR_PREFIXES_MODIFIERS.OF_TYPE =>
        val (typeSystem, typeCode, value) = FHIRUtil.parseTokenOfTypeValue(token)
        //Split the parts
        val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
        //Construct main query, it both checks the Identifier.type.coding and Identifier.value
        val mainQuery =
              and(
                elemMatch(
                  FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.TYPE}.${FHIR_COMMON_FIELDS.CODING}"), //Path for type codes in Identifier
                  PrefixModifierHandler.tokenModifierHandler(FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, Some(typeSystem), Some(typeCode), modifier="")
                ),
                Filters.eq(
                  FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.VALUE),
                  value)
              )
        //If an array exist, use elemMatch otherwise return the query
         elemMatchPath match {
           case None => mainQuery
           case Some(emp) => elemMatch(emp, mainQuery)
         }
      //For the complex types, we should consider array elements within the path
      case _ =>
        modifier match {
          case FHIR_PREFIXES_MODIFIERS.TEXT =>
            TOKEN_TYPE_SEARCH_DISPLAY_PATHS.get(targetType) match {
              case None => throw new InvalidParameterException(s"Modifier _text cannot be used on $targetType data type for token type pearameters!")
              case Some(textFields) =>
                val queries = textFields.map(textField =>
                  PrefixModifierHandler.handleTokenTextModifier(FHIRUtil.normalizeElementPath(FHIRUtil.mergeElementPath(path, textField)), token)
                )
                if(queries.length > 1) or(queries:_*) else queries.head
            }
          //Otherwise (no modifier, or any other)
          case _ =>
            var complextElementPath = path
            //2 level complex type, so add path for the the last complex type
            if(targetType == FHIR_DATA_TYPES.CODEABLE_CONCEPT)
              complextElementPath = FHIRUtil.mergeElementPath(complextElementPath,s"${FHIR_COMMON_FIELDS.CODING}[i]") //coding is array, so we add [i]
            //Find out the elemMatch and query parts of the path
            val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(complextElementPath)
            // Based on the target type, decide on system, code and text fields
            val (systemField, codeField) = TOKEN_TYPE_SEARCH_SUBPATHS(targetType)

            // Extract system and code parts from query value
            val (system, code) = FHIRUtil.parseTokenValue(token)
            //Resolve main query on the fields
            val mainQuery =
              PrefixModifierHandler
                .tokenModifierHandler(FHIRUtil.mergeElementPath(queryPath, systemField), FHIRUtil.mergeElementPath(queryPath,codeField), system, code, modifier)

            //If an array exist, use elemMatch otherwise return the query
            elemMatchPath match {
              case None => mainQuery
              case Some(emp) => elemMatch(emp, mainQuery)
            }
        }
    }
  }

  /**
   * A quantity parameter searches on the Quantity data type. The syntax for the
   * value follows the form:
   *
   * [parameter]=[prefix][number]|[system]|[code] matches a quantity with the given unit
   *
   * @param quantity            quantity parameter
   * @param prefix              prefix for the quantity parameter
   * @param path                Path for the element
   * @param targetType
   * @param targetReferences
   * @return
   */
  private def quantityQuery(quantity:String, prefix:String, path:String, targetType:String, targetReferences:Seq[String] = Nil):Bson = {
    //Parse the given value
    val (value,system, code) = FHIRUtil.parseQuantityValue(quantity)

    //Find out the elemMatch and query parts of the path
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)

    //Try to construct main query
    val mainQuery = targetType match {
        case FHIR_DATA_TYPES.QUANTITY |
             FHIR_DATA_TYPES.SIMPLE_QUANTITY |
             FHIR_DATA_TYPES.MONEY_QUANTITY |
             FHIR_DATA_TYPES.AGE |
             FHIR_DATA_TYPES.DISTANCE |
             FHIR_DATA_TYPES.COUNT |
             FHIR_DATA_TYPES.DURATION =>
          //Query on the quentity
          val valueQuery = PrefixModifierHandler.decimalPrefixHandler(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.VALUE), value, prefix)
          //Also merge it with query on system and code
          val sysCodeQuery = unitSystemCodeQuery(system, code, queryPath, FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT)
          sysCodeQuery.map(sq => and(valueQuery, sq)).getOrElse(valueQuery)

        case FHIR_DATA_TYPES.MONEY =>
          //Query on the quatity
          val valueQuery = PrefixModifierHandler.decimalPrefixHandler(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.VALUE), value, prefix)
          //Also merge it with query on currency code
          val sysCodeQuery = code.map(c => Filters.eq(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.CURRENCY), c))
          sysCodeQuery.map(sq => and(valueQuery, sq)).getOrElse(valueQuery)

        //Handle range
        case FHIR_DATA_TYPES.RANGE =>
          //Query on range values
          val valueQuery = PrefixModifierHandler.rangePrefixHandler(queryPath.getOrElse(""), value, prefix)
          val lowPath = FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.LOW)
          val highPath =FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.HIGH)

          val lq = unitSystemCodeQuery(system, code, Some(lowPath), FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT).map(sq => or(exists(lowPath, exists = false), sq))
          val hq = unitSystemCodeQuery(system, code, Some(highPath), FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, FHIR_COMMON_FIELDS.UNIT).map(sq => or(exists(highPath, exists = false), sq))
          //Merge all (both lq and hq should be SOME or NONE
          lq.map(and(_, hq.get, valueQuery)).getOrElse(valueQuery)

        case FHIR_DATA_TYPES.SAMPLED_DATA =>
          //For SampleData, we should check for lowerLimit and upperLimit like a range query
          val valueQuery = PrefixModifierHandler.rangePrefixHandler(queryPath.getOrElse(""), value, prefix, isSampleData = true)
          val sysCodeQuery = unitSystemCodeQuery(system, code, queryPath,
            systemPath = s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.SYSTEM}",
            codePath=s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.CODE}",
            unitPath= s"${FHIR_COMMON_FIELDS.ORIGIN}.${FHIR_COMMON_FIELDS.UNIT}")
          sysCodeQuery.map(sq => and(valueQuery, sq)).getOrElse(valueQuery)
      }

    //If an array exist, use elemMatch otherwise return the query
    elemMatchPath match {
      case None => mainQuery
      case Some(emp) => elemMatch(emp, mainQuery)
    }
  }

  /**
    * Merge the query ont the Quantity value with system and code restrictions
    * @param system Expected system
    * @param code Expected code/unit
    * @param queryPath Main path to the FHIR quantity element
    * @param systemPath system field path within the element
    * @param codePath code field path within the element
    * @param unitPath unit field path within the element
    * @return
    */
  private def unitSystemCodeQuery(system:Option[String], code:Option[String], queryPath:Option[String], systemPath:String, codePath:String, unitPath:String):Option[Bson] = {
    (system, code) match {
      //Only query on value
      case (None, None) =>
        None
      //Query on value + unit
      case (None, Some(c)) =>
        Some(
          or(
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, codePath), c),
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, unitPath), c)
          )
        )
      //query on value + system + code
      case (Some(s), Some(c)) =>
        Some(
          and(
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, codePath), c),
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, systemPath), s)
          )
        )
      case _ => throw new InternalServerException("Invalid state!")
    }
  }

  /**
   * A reference parameter refers to references between resources. The interpretation of a reference
   * parameter is either:
   *
   * [parameter]=[id] the logical [id] of a resource using a local reference (i.e. a relative reference)
   *
   * [parameter]=[type]/[id] the logical [id] of a resource of a specified type using
   * a local reference (i.e. a relative reference), for when the reference can point to different
   * types of resources (e.g. Observation.subject)
   *
   * [parameter]=[url] where the [url] is an absolute URL - a reference to a resource by its absolute location
   *
   * @param reference               a reference to be handled
   * @param modifier                type of the reference(e.g. :type modifier)
   * @param path                    path to the element
   * @param targetType
   * @param targetReferenceTypes    Target reference types
   * @return                        equivalent BsonDocument for the target query
   */
  private def referenceQuery(reference:String, modifier:String, path:String, targetType:String, targetReferenceTypes:Seq[String]):Bson = {
    targetType match {
      //If this is a search on a FHIR Reference type element
      case FHIR_DATA_TYPES.REFERENCE =>
        //Find out the elemMatch and query parts of the path
        val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
        //Parse reference value (URL part, resource type, resource id, version)
        val (url, rtype, rid, version) = FHIRUtil.resolveReferenceValue(reference, modifier, targetReferenceTypes)

        val mainQuery = modifier match {
          //If modifier is identifier, search like a token on identifier element (Reference.identifier)
          case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
            tokenQuery(rid, modifier = "", FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.IDENTIFIER), FHIR_DATA_TYPES.IDENTIFIER)
          //Query only on resource type to refer
          case FHIR_PREFIXES_MODIFIERS.TYPE =>
            Filters.eq(
              FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE}"),
              rid)
          //No modifier
          case _ =>
            //Base queries for reference on resource type and id
            var baseQueries:Seq[Bson] = Seq(
              Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}"), rid),
              Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE}"), rtype),
              url match {
                // If no url is given or it is our root url
                case None | Some(OnfhirConfig.fhirRootUrl) =>
                  Filters.or(
                    Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), OnfhirConfig.fhirRootUrl), //Either it should equal to our root URL
                    Filters.exists(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), false) //Or url part does not exist
                  )
                //If given any other, it should match
                case Some(other) =>
                  Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), other)
              }
            )
            //Add version specific query if given
            baseQueries =
              baseQueries ++
                version.map(v => Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_VERSION}"), v)).toSeq
            //And all queries
            and(baseQueries:_*)
        }
        //If an array exist, use elemMatch otherwise return the query
        elemMatchPath match {
          case None => mainQuery
          case Some(emp) => elemMatch(emp, mainQuery)
        }

      //If this is a search on Canonical references
      case FHIR_DATA_TYPES.CANONICAL =>
        //As for canonical, we only look at one field we don't need to care arrays in the path
        modifier match {
          case FHIR_PREFIXES_MODIFIERS.BELOW =>
            val (canonicalUrl, canonicalVersion) = FHIRUtil.parseCanonicalValue(reference)
            // Escape characters for to have valid regular expression
            val regularExpressionValue = FHIRUtil.escapeCharacters(canonicalUrl) + canonicalVersion.map(v => s"\\|$v(\\.[0-9]*)+").getOrElse("")
            // Match at the beginning of the uri
            regex(FHIRUtil.normalizeElementPath(path), "\\A" + regularExpressionValue + "$")
          case _ =>
            val (canonicalUrl, canonicalVersion) = FHIRUtil.parseCanonicalValue(reference)
            canonicalVersion match{
              case None => //Otherwise should match any version

                regex(FHIRUtil.normalizeElementPath(path), "\\A" + FHIRUtil.escapeCharacters(canonicalUrl) + "(\\|[0-9]+(\\.[0-9]*)*)?$")
              case Some(_) => // Exact match if version exist
                Filters.eq(FHIRUtil.normalizeElementPath(path), reference)
            }

        }
    }
  }

  /**
   *
   * @param pathParts
   * @param restrictionsWithIndexes
   * @param value
   * @param paramType
   * @param targetType
   * @param modifierOrPrefix
   * @param targetReferences
   * @return
   */
  def queryWithRestrictions(pathParts:Seq[String], restrictionsWithIndexes:Seq[(Int, Seq[(String,String)])], value:String,  paramType:String, targetType:String, modifierOrPrefix:String, targetReferences:Seq[String]):Bson = {
    val restriction = restrictionsWithIndexes.head
    val parentPath = pathParts.slice(0, restriction._1 + 1).mkString(".")
    //Find childpaths
    val childPaths = pathParts.drop(restriction._1 + 1)
    //Split the parent path
    val (elemMatchPath,  queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(parentPath)

    if(restrictionsWithIndexes.tail.isEmpty){
      val childPath = childPaths.mkString(".")
      val mainQuery =
        and(
            (
              //Restriction queries
              restriction._2.map(r => Filters.eq(FHIRUtil.mergeElementPath(queryPath, r._1), r._2)) :+
              //Actual query
              SearchUtil
                .typeHandlerFunction(paramType)(value, modifierOrPrefix, FHIRUtil.mergeElementPath(queryPath, childPath), targetType, targetReferences),
            ):_*
        )
      //Apply elem match
      elemMatchPath match {
        case None => mainQuery
        case Some(emp) => elemMatch(emp, mainQuery)
      }
    }
    //If there are still restrictions on child paths
    else {
      val mainQuery =
        and(
          (
            //Restriction queries
            restriction._2.map(r => Filters.eq(FHIRUtil.mergeElementPath(queryPath, r._1), r._2)) :+
            queryWithRestrictions(childPaths,restrictionsWithIndexes.tail, value, paramType, targetType, modifierOrPrefix, targetReferences)
            ):_*
        )
      elemMatch(elemMatchPath.get, mainQuery)
    }
  }


  def extensionQuery(value:String, path:Seq[(String,String)], paramType:String, prefixOrModifier:String, targetReferences:Seq[String]) = {
    //The last of the Seq is the path for the value element (remove the extensions as we will use it as inner query)
    val valueElementPath = path.last._1.replaceAll("extension[i].", "")
    //Extension elements are like valueCodeableConcept, valueCoding, etc. So remove it to find the type
    val targetType = valueElementPath.replace("value","")

    //Query on the extension element
    val mainQuery = SearchUtil.typeHandlerFunction(paramType)(value, prefixOrModifier, valueElementPath, targetType, targetReferences)
    //Extension URL list
    val uriList = path.dropRight(1)
    val query = uriList.foldRight(mainQuery)( (url, query) => {
      elemMatch(FHIR_COMMON_FIELDS.EXTENSION, and(equal(FHIR_COMMON_FIELDS.URL, url._2), query))
    })

    query
  }
}

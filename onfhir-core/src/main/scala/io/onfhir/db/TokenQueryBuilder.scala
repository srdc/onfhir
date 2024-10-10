package io.onfhir.db

import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.{COMMON_SYNTACTICALLY_HIERARCHICAL_CODE_SYSTEMS, FHIR_COMMON_FIELDS, FHIR_DATA_TYPES, FHIR_PREFIXES_MODIFIERS, TOKEN_TYPE_SEARCH_DISPLAY_PATHS, TOKEN_TYPE_SEARCH_SUBPATHS}
import io.onfhir.config.FhirConfigurationManager
import io.onfhir.exception.{InvalidParameterException, UnsupportedParameterException}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

import scala.util.Try

/**
 * Provide utility method for query construction for FHIR token type parameters for a given target path and type
 * e.g. code=http://loing.org|1235-4
 * e.g. code:in=http://my.org/fhir/ValueSet/myValueSet
 *
 *
 * Supporting the following modifiers;
 *  - :text --> Checking if the display parts of element (e.g. CodeableConcept.text, CodeableConcept.coding.display, etc) starts with the given text (case insensitive)
 *  - :in   --> Checking if the code is listed in given ValueSet (only canonical urls are supported, ***literal references to ValueSets should also be supported***) TODO
 *              ValueSet should be given in the configuration and should enumerate the codes. The ones that define the valueset with rules are not supported yet TODO
 *  - :not-in --> Checking if the code is not listed in given ValueSet. Same restrictions apply
 *  - :below  --> Checking if the code is child of given concept (Only supported for CodeSystems that syntactically has this child relationship by using start with query e.g. ATC codes, ICD-10 codes)
 *
 *
 */
object TokenQueryBuilder extends IFhirQueryBuilder {

  /**
   * Convert a token type search parameter into MongoDB query on the given path and data type
   *
   * [parameter]=[code]: the value of [code] matches a Coding.code or Identifier.value
   * irrespective of the value of the system property
   *
   * [parameter]=[system]|[code]: the value of [code] matches a Coding.code or Identifier.value,
   * and the value of [system] matches the system property of the Identifier or Coding
   *
   * [parameter]=|[code]: the value of [code] matches a Coding.code or Identifier.value, and the
   * Coding/Identifier has no system property
   *
   * [parameter]=[system]|: any element where the value of [system] matches the system property of the Identifier or Coding
   *
   * e.g. GET [base]/Condition?code=http://acme.org/conditions/codes|ha125
   * e.g  GET [base]/Patient?gender:not=male
   * e.g. GET [base]/Condition?code:text=headache
   *
   *
   * @param values     Values supplied in search for the token type search parameter
   *                   e.g. http://loinc.org|85354-9
   * @param modifier   Modifier used e.g. :in, :above, :not
   * @param path       The path to the target element for the corresponding query parameter
   *                   e.g. One of the path for 'value-date' search parameter in Observation is 'valueDateTime'
   * @param targetType Type of the target element that path goes to e.g. 'dateTime', 'Period'
   * @return           Bson representation of MongoDB statement
   */
  def getQuery(values:Seq[String], modifier:String, path:String, targetType:String):Bson = {
    targetType match {
      //Simple types, only one field to match (so we don't need to evaluate elemMatch options)
      case FHIR_DATA_TYPES.STRING | FHIR_DATA_TYPES.ID | FHIR_DATA_TYPES.URI | FHIR_DATA_TYPES.CODE =>
        getQueryForSimpleTypeTarget(values, modifier, path)
      case FHIR_DATA_TYPES.BOOLEAN =>
        getQueryForBooleanTarget(values, modifier, path)
      //A special modifier case
      case FHIR_DATA_TYPES.IDENTIFIER if modifier == FHIR_PREFIXES_MODIFIERS.OF_TYPE =>
        getQueryForOfTypeModifierOnIdentifier(values, path)
      //For the complex types (e.g. CodeableConcept, Coding, ...), we should consider array elements within the path
      case _ =>
        getQueryForComplexTypeTarget(values, modifier, path, targetType)
    }
  }

  /**
   * Construct token search on a complex type
   *  @param values     Values supplied in search for the token type search parameter
   *                    e.g. http://loinc.org|85354-9
   * @param modifier    Modifier used e.g. :in, :above, :not
   * @param path        The path to the target element for the corresponding query parameter
   *                    e.g. One of the path for 'value-date' search parameter in Observation is 'valueDateTime'
   * @param targetType  Type of the target element that path goes to e.g. CodeableConcept
   * @return
   */
  private def getQueryForComplexTypeTarget(values:Seq[String], modifier:String, path:String, targetType:String):Bson = {
    modifier match {
      case FHIR_PREFIXES_MODIFIERS.TEXT =>
        //Find the paths of textual elements for target type e.g. CodeableConcept.text, CodeableConcept.coding.display
        TOKEN_TYPE_SEARCH_DISPLAY_PATHS
          .get(targetType) match {
            case None => throw new InvalidParameterException(s"Modifier _text cannot be used on $targetType data type for token type pearameters!")
            case Some(textFields) =>
              val queries =
                textFields
                  .flatMap(textField =>
                    values
                      .map(value =>
                        StringQueryBuilder.getQueryOnTargetStringElement(FHIRUtil.normalizeElementPath(FHIRUtil.mergeElementPath(path, textField)), value, "")
                      )
                  )
              orQueries(queries)
          }
      //Modifier :sw  --> Codes start with given prefix, Modifier :nsw --> Codes not start with given prefix
      // Note: Negation part (nsw) is handled on the upstream while merging queries for different paths
      case FHIR_PREFIXES_MODIFIERS.STARTS_WITH | FHIR_PREFIXES_MODIFIERS.NOT_STARTS_WITH  =>
        val (elemMatchPath, systemPath, codePath) =  getElemMatchSystemAndCodePaths(path, targetType)
        val parsedSystemAndCodes = values.map(FHIRUtil.parseTokenValue)
        val mainQuery = orQueries(parsedSystemAndCodes.map(systemAndCode => getQueryForStartsWithModifier(systemPath, codePath, systemAndCode._1, systemAndCode._2)))
        if (modifier == FHIR_PREFIXES_MODIFIERS.NOT_STARTS_WITH)
          Filters.not(getFinalQuery(elemMatchPath, mainQuery))
        else
          getFinalQuery(elemMatchPath, mainQuery)
      //Modifier :in and :not-in (negation part is handled on the upstream while merging queries for different paths)
      case FHIR_PREFIXES_MODIFIERS.IN | FHIR_PREFIXES_MODIFIERS.NOT_IN =>
        if(values.length != 1)
          throw new InvalidParameterException(s"Invalid usage of parameter. A canonical or literal ValueSet reference should be provided in parameter value for ${FHIR_PREFIXES_MODIFIERS.IN} or ${FHIR_PREFIXES_MODIFIERS.NOT_IN} modifiers !!!")
        val (elemMatchPath, systemPath, codePath) =  getElemMatchSystemAndCodePaths(path, targetType)
        val mainQuery = getQueryForInModifier(systemPath, codePath, values.head)
        if (modifier == FHIR_PREFIXES_MODIFIERS.NOT_IN)
          Filters.not(getFinalQuery(elemMatchPath, mainQuery))
        else
          getFinalQuery(elemMatchPath, mainQuery)
      //Modifier :below
      case FHIR_PREFIXES_MODIFIERS.BELOW =>
        val (elemMatchPath, systemPath, codePath) =  getElemMatchSystemAndCodePaths(path, targetType)
        val parsedSystemAndCodes = values.map(FHIRUtil.parseTokenValue)
        //If code system syntactically
        val mainQuery =
          if(parsedSystemAndCodes.forall(sc => sc._1.exists(COMMON_SYNTACTICALLY_HIERARCHICAL_CODE_SYSTEMS.contains))){
            orQueries(
              parsedSystemAndCodes.map {
                case (system, code) => getQueryForStartsWithModifier(systemPath, codePath, system, code)
              }
            )
          } else {
             //TODO Some other mechanism or through terminology service
            throw new UnsupportedParameterException(s"Modifier $modifier is not supported by onFhir.io system for token type parameters!")
          }
        getFinalQuery(elemMatchPath, mainQuery)
      //Modifier above
      case FHIR_PREFIXES_MODIFIERS.ABOVE =>
        throw new UnsupportedParameterException(s"Modifier $modifier is not supported by onFhir.io system for token type parameters!")
      //No modifier
      case "" | FHIR_PREFIXES_MODIFIERS.NOT =>
        val (elemMatchPath, systemPath, codePath) =  getElemMatchSystemAndCodePaths(path, targetType)
        // Extract system and code parts from query value
        val parsedSystemAndCodes =
          values
            .map(FHIRUtil.parseTokenValue)
            .groupBy(_._1).map(g => g._1 -> g._2.map(_._2)) //Group by given system
            .toSeq
            .flatMap {
              case (system, codes) =>
                codes.flatten match {
                  case Nil => Seq(system -> None)
                  case oth if codes.forall(_.nonEmpty) => Seq(system -> Some(oth))
                  case oth =>  Seq(system -> Some(oth), system -> None)
                }
            }
        val mainQuery =
          orQueries(
            parsedSystemAndCodes
              .map {
                case (system, codes) => getTokenCodeSystemQueryForMultipleCodes(systemPath, codePath, system, codes)
              }
          )

        if (modifier == FHIR_PREFIXES_MODIFIERS.NOT)
          Filters.not( getFinalQuery(elemMatchPath, mainQuery))
        else
          getFinalQuery(elemMatchPath, mainQuery)
      //Any other modifier
      case _ =>
        throw new InvalidParameterException(s"Modifier $modifier is not a valid modifier on $targetType elements !")
    }
  }

  /**
   * Find out on elemMatch path (the last array element on the path), path to the system element (after elemMatch path), path to the code element (after elemMatch path)
   * @param path        Path to the target element
   * @param targetType  Target data type
   * @return
   */
  private def getElemMatchSystemAndCodePaths(path:String, targetType:String):(Option[String], String, String) = {
    val complexElementPath =
      //2 level complex type, so add path for the the last complex type
      if (targetType == FHIR_DATA_TYPES.CODEABLE_CONCEPT)
        FHIRUtil.mergeElementPath(path, s"${FHIR_COMMON_FIELDS.CODING}[i]") //coding is array, so we add [i]
      else
        path
    //Find out the elemMatch and query parts of the path
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(complexElementPath)
    // Based on the target type, decide on system, code and text fields
    val (systemField, codeField) = TOKEN_TYPE_SEARCH_SUBPATHS(targetType)

    (elemMatchPath, FHIRUtil.mergeElementPath(queryPath, systemField),  FHIRUtil.mergeElementPath(queryPath, codeField))
  }

  /**
   * Handle the :in and :not-in modifier for token search and construct query
   *
   * @param systemPath Path to the system element
   * @param codePath   Path to the code element
   * @param vsUrl      URL of the ValueSet to search in
   * @return
   */
  private def getQueryForInModifier(systemPath: String, codePath: String, vsUrl: String): Bson = {
    Try(FHIRUtil.parseCanonicalReference(vsUrl))
      .toOption
      .filter(vs => FhirConfigurationManager.fhirTerminologyValidator.isValueSetSupported(vs.getUrl(), vs.version)) match {
        //If we resolve the ValueSet from canonical reference
        case Some(vs) =>
          //All codes listed in ValueSet (system url -> list of codes)
          val vsCodes: Seq[(String, Set[String])] = FhirConfigurationManager.fhirTerminologyValidator.getAllCodes(vs.getUrl(), vs.version).toSeq

          if(vsCodes.isEmpty)
            throw new UnsupportedParameterException(s"ValueSet given with url '$vsUrl' by 'in' or 'not-in' modifier is empty!")

          val queriesForEachCodeSystem = vsCodes.map(vsc => Filters.and(Filters.eq(systemPath, vsc._1), Filters.in(codePath, vsc._2.toSeq: _*)))
            /*modifier match {
              case FHIR_PREFIXES_MODIFIERS.IN =>
                vsCodes.map(vsc => and(Filters.eq(systemPath, vsc._1), Filters.in(codePath, vsc._2.toSeq: _*)))
              case FHIR_PREFIXES_MODIFIERS.NOT_IN =>
                vsCodes.map(vsc => or(Filters.ne(systemPath, vsc._1), Filters.nin(codePath, vsc._2.toSeq: _*)))
            }*/
          orQueries(queriesForEachCodeSystem)
          /*queriesForEachCodeSystem.length match {
            case 0 =>
            case 1 => queriesForEachCodeSystem.head
            case _ => if (modifier == FHIR_PREFIXES_MODIFIERS.IN) or(queriesForEachCodeSystem: _*) else and(queriesForEachCodeSystem: _*)}*/
        //We don't resolve the ValueSet
        case None =>
          //TODO check if it is a literal reference and hande that
          throw new UnsupportedParameterException(s"ValueSet url '$vsUrl' given with 'in' or 'not-in' modifier is not known!")
    }
  }


  /**
   * Construct query for a target simple type
   * @param values    Given values
   * @param modifier  Supplied modifier
   * @param path      Path to the target element
   * @return
   */
  private def getQueryForSimpleTypeTarget(values:Seq[String], modifier:String, path:String):Bson = {
    modifier match {
      //If no modifier
      case "" =>
        values match {
          //If a single value is given, check equality with the value in the path
          case Seq(single) => Filters.eq(FHIRUtil.normalizeElementPath(path), single)
          //If multiple values, check with in
          case multiple => Filters.in(FHIRUtil.normalizeElementPath(path), multiple:_*)
        }
      //Not modifier
      case FHIR_PREFIXES_MODIFIERS.NOT =>
        Filters.not(
          values match {
            //If a single value is given, check equality with the value in the path
            case Seq(single) => Filters.eq(FHIRUtil.normalizeElementPath(path), single)
            //If multiple values, check with in
            case multiple => Filters.in(FHIRUtil.normalizeElementPath(path), multiple: _*)
          }
        )
      case FHIR_PREFIXES_MODIFIERS.STARTS_WITH | FHIR_PREFIXES_MODIFIERS.NOT_STARTS_WITH =>
        orQueries(values.map(prefix => getQueryForStartsWith(path, prefix)))
      case other =>
        throw new InvalidParameterException(s"Modifier $other is not supported for FHIR token queries on a simple type!")
    }
  }


  /**
   * Handle FHIR token type queries on FHIR boolean values
   *
   * @param values   Supplied values
   * @param path     Path of the target element
   * @param modifier Search modifier
   * @return
   */
  def getQueryForBooleanTarget(values: Seq[String], modifier: String, path: String): Bson = {
    if(modifier!="")
      throw new InvalidParameterException(s"Modifier $modifier is not supported for FHIR token queries on FHIR boolean elements!")

    values match {
      case Seq("true")  => Filters.eq(path, true)
      case Seq("false") => Filters.eq(path, false)
      case _ => throw new InvalidParameterException(s"Invalid usage of parameter. Target element (with path $path) for search parameter is boolean, use either 'false' or 'true' for the parameter value!!!")
    }
  }

  /**
   * Cosntruct the query for :ofType modifier on Identifiers
   * @param values  Supplied parameter values
   * @param path    Path to the Identifier element
   * @return
   */
  def getQueryForOfTypeModifierOnIdentifier(values: Seq[String], path: String):Bson = {
    //Group supplied values into (system, code) -> values
    val parsedValues: Seq[((String, String), Seq[String])] =
      values
        .map(FHIRUtil.parseTokenOfTypeValue)
        .groupBy(v => (v._1, v._2))
        .map(g => g._1 -> g._2.map(_._3))
        .toSeq

    //Split the path to handle paths to array elements (if identifier is on a array path)
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
    //Construct main query, it both checks the Identifier.type.coding and Identifier.value
    val mainQuery =
      orQueries(
        parsedValues
        .map {
          case ((typeSystem, typeCode), values) =>
            Filters.and(
              Filters.elemMatch(
                FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.TYPE}.${FHIR_COMMON_FIELDS.CODING}"), //Path for type codes in Identifier
                getTokenCodeSystemQuery(FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE, Some(typeSystem), Some(typeCode))
              ),
              values match {
                case Seq(single) => Filters.eq(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.VALUE), single)
                case multiple => Filters.in(FHIRUtil.mergeElementPath(queryPath, FHIR_COMMON_FIELDS.VALUE), multiple:_*)
              }
            )
        }
      )

    //If an array exist, use elemMatch otherwise return the query
    elemMatchPath match {
      case None => mainQuery
      case Some(emp) => Filters.elemMatch(emp, mainQuery)
    }
  }


  /**
   * Get query for system and code matching for multiple codes
   *
   * @param systemPath Path to the system part e.g. Coding.system, Identifier.system
   * @param codePath   Path to the code part
   * @param system     Expected system value
   * @param codes       Expected codes
   * @return
   */
  private def getTokenCodeSystemQueryForMultipleCodes(systemPath: String, codePath: String, system: Option[String], codes: Option[Seq[String]]): Bson = {

    def getCodesQuery(codes:Seq[String]) =
      codes match {
        case Seq(single) => Filters.eq(codePath, single)
        case multiple => Filters.in(codePath, multiple: _*)
      }

    system match {
      // Query like [code] -> the value of [code] matches a Coding.code or Identifier.value irrespective of the value of the system property
      case None => getCodesQuery(codes.get)
      // Query like |[code] -> the value of [code] matches a Coding.code or Identifier.value, and the Coding/Identifier has no system property
      case Some("") =>
        Filters.and(Filters.exists(systemPath, exists = false), getCodesQuery(codes.get))
      // Query like [system][code] -> the value of [code] matches a Coding.code or Identifier.value, and the value of [system] matches the system property of the Identifier or Coding
      case Some(sys) =>
        codes match {
          //[system]| --> should macth only systen
          case None =>
            Filters.eq(systemPath, sys)
          // Query like [system][code]
          case Some(cds) =>
            Filters.and(getCodesQuery(cds), Filters.eq(systemPath, sys))
        }
    }
  }

  /**
   * Handle the Token query on system and code fields
   *
   * @param systemPath Path to the system part e.g. Coding.system, Identifier.system
   * @param codePath   Path to the code part
   * @param system     Expected system value
   * @param code       Expected code value
   * @return
   */
  private def getTokenCodeSystemQuery(systemPath: String, codePath: String, system: Option[String], code: Option[String]): Bson = {
    system match {
      // Query like [code] -> the value of [code] matches a Coding.code or Identifier.value irrespective of the value of the system property
      case None =>
        Filters.eq(codePath, code.get)
      // Query like |[code] -> the value of [code] matches a Coding.code or Identifier.value, and the Coding/Identifier has no system property
      case Some("") =>
        Filters.and(Filters.exists(systemPath, exists = false), Filters.eq(codePath, code.get))
      // Query like [system][code] -> the value of [code] matches a Coding.code or Identifier.value, and the value of [system] matches the system property of the Identifier or Coding
      case Some(sys) =>
        code match {
          //[system]| --> should match only system
          case None =>
            Filters.eq(systemPath, sys)
          // Query like [system][code]
          case Some(cd) =>
            Filters.and(Filters.eq(codePath, cd), Filters.eq(systemPath, sys))
        }
    }
  }


  /**
   * Query construction for :sw modifier (onFHIR.io specific)
   *
   * @param systemPath    Path to the system element
   * @param codePath      Path to the code element
   * @param system        Supplied system part in parameter value
   * @param code          Supplied code part in parameter value
   * @return
   */
  private def getQueryForStartsWithModifier(systemPath: String, codePath: String, system: Option[String], code: Option[String]): Bson = {
    if (code.isEmpty)
      throw new InvalidParameterException(s"Code value should be given when modifier ':sw' is used!")
    //val pattern = Pattern.compile("^"+code.get+"")
    val codeStartsWithQuery = getQueryForStartsWith(codePath, code.get)

    system match {
      // Query like [code] -> the value of [code] matches a Coding.code or Identifier.value irrespective of the value of the system property
      case None =>
        codeStartsWithQuery
      // Query like |[code] -> the value of [code] matches a Coding.code or Identifier.value, and the Coding/Identifier has no system property
      case Some("") =>
        Filters.and(Filters.exists(systemPath, exists = false), codeStartsWithQuery)
      // Query like [system][code] -> the value of [code] matches a Coding.code or Identifier.value, and the value of [system] matches the system property of the Identifier or Coding
      case Some(sys) =>
        code match {
          //[system]| --> should macth only systen
          case None =>
            Filters.eq(systemPath, sys)
          // Query like [system][code]
          case Some(_) =>
            Filters.and(Filters.eq(systemPath, sys), codeStartsWithQuery)
        }
    }
  }


  /**
   * Construct MongoDB query statement for prefix search (If the given value on target path starts with given prefix)
   * @param path      Path to the target element
   * @param prefix    Prefix
   * @return
   */
  private def getQueryForStartsWith(path:String, prefix:String):Bson =
    Filters.regex(path,  "^"+prefix+"" )

}

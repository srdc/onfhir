package io.onfhir.db

import java.net.URL
import java.util.regex.Pattern

import io.onfhir.api._
import com.mongodb.client.model.Filters
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.FhirConfigurationManager
import io.onfhir.exception.{InvalidParameterException, UnsupportedParameterException}
import io.onfhir.util.DateTimeUtil
import org.mongodb.scala.bson.{BsonDateTime, BsonValue}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters.{exists, _}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.math.pow
import scala.util.Try

/**
  * Handles prefixes and modifiers over the parameters
  */
object PrefixModifierHandler {
  /**
    * Handles missing modifier
    *
    * @param pathList absolute path of the parameter
    * @param bool boolean value
    * @return BsonDocument for the query
    */
  def missingHandler(pathList: Seq[String], bool: String): Bson = {
    val missingQuery:ListBuffer[Bson] = ListBuffer()
    bool match {
      case MISSING_MODIFIER_VALUES.STRING_TRUE =>
        pathList foreach { path =>
          missingQuery.append(exists(path, exists=false))
        }
        and(missingQuery.toList:_*)
      case MISSING_MODIFIER_VALUES.STRING_FALSE =>
        pathList foreach { path =>
          missingQuery.append(exists(path, exists=true))
        }
        or(missingQuery.toList:_*)
      case _ =>
        throw new InvalidParameterException("Correct Boolean Value Should be Provided")
    }
  }


  /**
    * Handles prefixes for integer values
    *
    * @param path   absolute path of the parameter
    * @param value  value of the parameter
    * @param prefix prefix of the parameter
    * @return BsonDocument for the query
    */
  def intPrefixHandler(path:String, value:String, prefix:String): Bson = {
    //If there is non-zero digits after a decimal point, there cannot be any matches
    if(value.toDouble != value.toDouble.toLong * 1.0)
      equal(path, 0.05)
    else {
      //If the value is given in exponential form, we use precision
      if(value.contains("e") || value.contains("E")){
        val precision = calculatePrecisionDelta(value)
        // Generated function values for comparison
        val floor = value.toDouble - precision
        val ceil = value.toDouble + precision

        prefix match {
          case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => and(gte(path, floor), lt(path, ceil))
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => gt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => lt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => gte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => lte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL =>  or(lt(path, floor), gte(path, ceil))
          case FHIR_PREFIXES_MODIFIERS.APPROXIMATE => and(gte(path, value.toDouble * 0.9), lte(path, value.toDouble * 1.1))
          case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER | FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => throw new IllegalArgumentException("Prefixes sa and eb can not be used with integer values.")
        }
      } else { //Otherwise we need extact integer match
        // Prefix matching and query filter generation
        prefix match {
          case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => equal(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => gt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => lt(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => gte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => lte(path, value.toLong)
          case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => not(equal(path, value.toLong))
          case FHIR_PREFIXES_MODIFIERS.APPROXIMATE => and(gte(path, value.toDouble * 0.9), lte(path, value.toDouble * 1.1))
          case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER | FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => throw new IllegalArgumentException("Prefixes sa and eb can not be used with integer values.")
        }
      }
    }
  }

  /**
   * Calculate the delta for precision issues
   * @param value
   * @return
   */
  private def calculatePrecisionDelta(value:String):Double = {
    val preprocessedValue = if(value.startsWith("-")) value.drop(1)  else value
    preprocessedValue match {
      case d if(d.contains('.')) =>
        val parts = if(d.contains('e')) d.split('e') else d.split('E')
        //Find the precision e.g. 5.4 --> -2 -->  0.05 -->  5.4 +- 0.05
        var i = (parts.apply(0).length - parts.apply(0).indexOf('.')) * -1  + 1
        //Also include the power part e.g. 5.4e-2 --> -4 --> 0.0005 -> [5.35e-2,5.45e-2)
        if(parts.length > 1) {
          if(parts.apply(1).startsWith("-"))
            i = i + (parts.apply(1).drop(1).toInt * -1)
          else
            i = i + parts.apply(1).replace("+", "").toInt
        }
        pow(10, i) * 0.5

      case n if(!n.contains('.')) =>
        val parts = if(n.contains('e')) n.split('e') else n.split('E')
        var i = if(parts.apply(0).length != 1) 0 else -1
        if(parts.length > 1){
          if(!parts.apply(1).startsWith("-")){
            val p = parts.apply(1).replace("+", "").toInt
            i = i + p
          } else {
            val p = parts.apply(1).drop(1).toInt
            i = i - p
          }

        }

        pow(10, i) * 0.5
    }
  }

  /**
    * Handles prefixes for decimal values
    *
    * @param path absolute path of the parameter
    * @param prefix prefix of the parameter
    * @return BsonDocument for the query
    */
  def decimalPrefixHandler(path:String, value:String, prefix:String): Bson = {
    // Calculation of precision to generate implicit ranges
    val precision = calculatePrecisionDelta(value)
    //if(!value.contains('.')) 0.5 else pow(0.1, value.length - (value.indexOf(".") + 1)) * 0.5
    // Generated function values for comparison
    val floor = value.toDouble - precision
    val ceil = value.toDouble + precision
    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => and(gte(path, floor), lt(path, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => gt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => lt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => gte(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => lte(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => or(lt(path, floor), gte(path, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER =>  gt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => lt(path, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        val approximateLow = decimalPrefixHandler(path, (value.toDouble*0.9).toString, FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL)
        val approximateHigh = decimalPrefixHandler(path, (value.toDouble*1.1).toString, FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL)
        and(approximateLow, approximateHigh)
    }
  }

  /**
    * Handles prefixes for range type
    *
    * @param path absolute path of the parameter
    * @param value value of the parameter
    * @param prefix prefix of the parameter
    * @return BsonDocument for the query
    */
  def rangePrefixHandler(path:String, value:String, prefix:String, isSampleData:Boolean = false): Bson = {
    // Calculation of precision to generate implicit ranges
    val precision = calculatePrecisionDelta(value)
    // Paths to the range structure's high and low values
    val pathLow = if(isSampleData) FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.LOWER_LIMIT) else  FHIRUtil.mergeElementPath(path,s"${FHIR_COMMON_FIELDS.LOW}.${FHIR_COMMON_FIELDS.VALUE}")
    val pathHigh = if(isSampleData) FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.UPPER_LIMIT) else FHIRUtil.mergeElementPath(path, s"${FHIR_COMMON_FIELDS.HIGH}.${FHIR_COMMON_FIELDS.VALUE}")
    // Implicit input value ranges
    val floor = value.toDouble - precision
    val ceil = value.toDouble + precision
    // BsonDocuments to represent nonexistence of high and low values
    val fieldHighNotExist = and(exists(pathLow, exists=true), exists(pathHigh, exists=false))
    val fieldLowNotExist = and(exists(pathLow, exists=false), exists(pathHigh, exists=true))
    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => and(gte(pathLow, floor), lt(pathHigh, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => or(gt(pathHigh, value.toDouble), fieldHighNotExist)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => or(lt(pathLow, value.toDouble), fieldLowNotExist)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => or(gte(pathHigh, value.toDouble), fieldHighNotExist)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => or(lte(pathLow, value.toDouble), fieldLowNotExist)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => or(lt(pathLow, floor), gte(pathHigh, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => gt(pathLow, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => lt(pathHigh, value.toDouble)
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        val approximateLow = decimalPrefixHandler(FHIRUtil.mergeElementPath(path,FHIR_COMMON_FIELDS.LOW), (value.toDouble*0.9).toString, FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL)
        val approximateHigh = decimalPrefixHandler(FHIRUtil.mergeElementPath(path,FHIR_COMMON_FIELDS.HIGH), (value.toDouble*1.1).toString, FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL)
        and(approximateLow, approximateHigh)
    }
  }

  /**
    * FHIR string type query handler (including modifiers)
    * @param path absolute path of the target element
    * @param value value of the parameter
    * @param modifier Search modifier
    * @return
    */
  def stringModifierHandler(path:String, value:String, modifier:String):Bson = {
    //TODO Ignorance of accents or other diacritical marks, punctuation and non-significant whitespace is not supported yet

    // Escape characters for to have valid regular expression
    val regularExpressionValue = FHIRUtil.escapeCharacters(value)
    // Generator for regular expression queries(Only regex fields empty)
    val caseInsensitiveStringRegex = regex(path, _:String, "i")
    modifier match {
      case FHIR_PREFIXES_MODIFIERS.EXACT =>
        // Exact match provided with $eq mongo operator
        equal(path, value)
      case FHIR_PREFIXES_MODIFIERS.CONTAINS =>
        // Partial match
        caseInsensitiveStringRegex(".*" + regularExpressionValue + ".*")
      //No modifier
      case "" =>
        // Case insensitive, partial matches at the end of string
        caseInsensitiveStringRegex("^" + regularExpressionValue)
      case other =>
        throw new InvalidParameterException(s"Modifier $other is not supported for FHIR string queries!")
    }
  }

  /**
    * FHIR uri type query handler (including modifiers)
    * @param path absolute path of the target element
    * @param uri Uri value
    * @param modifier Search modifier
    * @return
    */
  def uriModifierHandler(path:String, uri:String, modifier:String):Bson = {
    modifier match {
      case FHIR_PREFIXES_MODIFIERS.ABOVE if uri.contains("/")=>
        val url =  Try(new URL(uri)).toOption
        if(url.isEmpty)
          throw new InvalidParameterException(s"Modifier ${FHIR_PREFIXES_MODIFIERS.ABOVE} is only supported for URLs not URNs or OIDs!")

        val initialPart = url.get.getProtocol + "://" + url.get.getHost + (if(uri.contains(url.get.getHost + ":"+url.get.getPort.toString)) ":" + url.get.getPort else "")
        var urlPath = url.get.getPath
        if(urlPath.length == 0 || urlPath == "/") urlPath = "" else urlPath = urlPath.drop(1)
        val parts = urlPath.split("/")

        def constructRegexForAbove(parts:Seq[String]):String = {
          parts match {
            case Nil => ""
            case oth => "(" + FHIRUtil.escapeCharacters("/"+ parts.head) + constructRegexForAbove(parts.drop(1))  +  ")?"
          }
        }
        //Constuct the regex to match any url above
        val regularExpressionValue = FHIRUtil.escapeCharacters(initialPart) + constructRegexForAbove(parts)
        regex(path, "\\A" + regularExpressionValue + "$")
      case FHIR_PREFIXES_MODIFIERS.BELOW if uri.contains("/") =>
        val url =  Try(new URL(uri)).toOption
        if(url.isEmpty)
          throw new InvalidParameterException(s"Modifier ${FHIR_PREFIXES_MODIFIERS.ABOVE} is only supported for URLs not URNs or OIDs!")
        // Escape characters for to have valid regular expression
        val regularExpressionValue = FHIRUtil.escapeCharacters(uri) + "("+ FHIRUtil.escapeCharacters("/")+".*)*"
        // Match at the beginning of the uri
        regex(path, "\\A" + regularExpressionValue + "$")
      case "" =>
        //If this is a query on  Canonical URLs of the conformance and knowledge resources (e.g. StructureDefinition, ValueSet, PlanDefinition etc) and a version part is given |[version]
        if(path == "url" && uri.contains("|")){
          val canonicalRef = Try(FHIRUtil.parseCanonicalReference(uri)).toOption
          if(canonicalRef.exists(_.version.isDefined))
            and(equal(path, canonicalRef.get.getUrl()), equal("version", canonicalRef.get.version.get))
          else
            equal(path, uri)
        } else {
          // Exact match
          equal(path, uri)
        }
      case other =>
        throw new InvalidParameterException(s"Modifier $other is not supported for FHIR uri queries!")
    }
  }

  /**
    * Handle FHIR token type queries on FHIR boolean values
    * @param path absolute path of the target element
    * @param boolean Boolean value of the parameter
    * @param modifier Search modifier
    * @return
    */
  def tokenBooleanModifierHandler(path:String, boolean:String, modifier:String):Bson = {

    def handleTokenBooleanQuery(path:String, value:String, isNot:Boolean = false):Bson = {
      if( value.equalsIgnoreCase("false") |
        value.equalsIgnoreCase("true"))
        Filters.eq(path, if(isNot) !value.toBoolean else value.toBoolean)
      else
        throw new InvalidParameterException(s"Invalid usage of parameter. Target element (with path $path) for search parameter is boolean, use either 'false' or 'true' for the parameter value!!!")
    }

    modifier match {
      case FHIR_PREFIXES_MODIFIERS.NOT =>
        handleTokenBooleanQuery(path, boolean, true)
      case "" =>
        handleTokenBooleanQuery(path, boolean)
      case other =>
        throw new InvalidParameterException(s"Modifier $other is not supported for FHIR token queries on FHIR boolean elements!")
    }
  }

  /**
    * Handle FHIR token type search with modifiers
    * @param systemPath Path to the system field
    * @param codePath Path to the code field
    * @param system Expected system value if exist;
    *               None means don't care
    *               Some("") means system should not exist
    *               Some(x) means system should match to x
    * @param code Expected code
    * @param modifier Modifier
    * @return
    */
  def tokenModifierHandler(systemPath:String, codePath:String, system:Option[String], code:Option[String], modifier:String):Bson = {
    modifier match {
      //Without modifier
      case "" =>
        handleTokenCodeSystemQuery(systemPath, codePath, system, code)
      case FHIR_PREFIXES_MODIFIERS.STARTS_WITH | FHIR_PREFIXES_MODIFIERS.NOT_STARTS_WITH =>
        handleTokenStartsWithModifier(systemPath, codePath, system, code, modifier == FHIR_PREFIXES_MODIFIERS.NOT_STARTS_WITH)
      case FHIR_PREFIXES_MODIFIERS.IN | FHIR_PREFIXES_MODIFIERS.NOT_IN =>
        handleTokenInModifier(systemPath, codePath, code.get, modifier)
      case FHIR_PREFIXES_MODIFIERS.BELOW | FHIR_PREFIXES_MODIFIERS.ABOVE =>
        throw new UnsupportedParameterException("Modifier is not supported by onFhir.io system yet!")
      case other =>
        throw new InvalidParameterException(s"Modifier $other is not supported for FHIR token queries!")
    }
  }

  /**
   * onFHIR specific starts with modifier
   * @param systemPath
   * @param codePath
   * @param system
   * @param code
   * @return
   */
  private def handleTokenStartsWithModifier(systemPath:String, codePath:String, system:Option[String], code:Option[String], isNot:Boolean):Bson = {
    if(code.isEmpty)
      throw new InvalidParameterException(s"Code value should be given when modifier ':sw' is used!")
    val pattern = Pattern.compile("^"+code.get+"")
    var codeStartsWithQuery = Filters.regex(codePath,  pattern )
    if(isNot)
      codeStartsWithQuery = Filters.not(codeStartsWithQuery)

    system match {
      // Query like [code] -> the value of [code] matches a Coding.code or Identifier.value irrespective of the value of the system property
      case None =>
        codeStartsWithQuery
      // Query like |[code] -> the value of [code] matches a Coding.code or Identifier.value, and the Coding/Identifier has no system property
      case Some("") =>
        and(exists(systemPath, exists = false), codeStartsWithQuery)
      // Query like [system][code] -> the value of [code] matches a Coding.code or Identifier.value, and the value of [system] matches the system property of the Identifier or Coding
      case Some(sys) =>
        code match {
          //[system]| --> should macth only systen
          case None =>
            Filters.eq(systemPath, sys)
          // Query like [system][code]
          case Some(cd) =>
            and(Filters.eq(systemPath, sys), codeStartsWithQuery)
        }
    }
  }

  /**
   * Handle the in modifier for token search
   * @param systemPath  Path to the system element
   * @param codePath    Path to the code element
   * @param vsUrl       URL of the ValueSet to search in
   * @return
   */
  private def handleTokenInModifier(systemPath:String, codePath:String,  vsUrl:String, modifier:String) :Bson = {
    val vs = FHIRUtil.parseCanonicalReference(vsUrl)
    if(FhirConfigurationManager.fhirTerminologyValidator.isValueSetSupported(vs.getUrl(), vs.version)){
      val vsCodes = FhirConfigurationManager.fhirTerminologyValidator.getAllCodes(vs.getUrl(), vs.version).toSeq

      val queriesForEachCodeSystem = modifier match {
        case FHIR_PREFIXES_MODIFIERS.IN =>
          vsCodes.map(vsc => and(Filters.eq(systemPath, vsc._1), Filters.in(codePath, vsc._2.toSeq :_* )))
        case FHIR_PREFIXES_MODIFIERS.NOT_IN =>
          vsCodes.map(vsc => or(Filters.ne(systemPath, vsc._1), Filters.nin(codePath, vsc._2.toSeq :_* )))
      }

      queriesForEachCodeSystem.length match {
        case 0 =>  throw new UnsupportedParameterException(s"ValueSet given with url '$vsUrl' by 'in' or 'not-in' modifier is empty!")
        case 1 => queriesForEachCodeSystem.head
        case _ => if(modifier == FHIR_PREFIXES_MODIFIERS.IN) or(queriesForEachCodeSystem:_*) else and(queriesForEachCodeSystem:_*)
      }
    }
    //If it is not a canonical reference
    else {
      //TODO check if it is a literal reference and hande that
      throw new UnsupportedParameterException(s"ValueSet url '$vsUrl' given with 'in' or 'not-in' modifier is not known!")
    }
  }

  /**
    * Handles text modifier
    *
    * @param path  Absolute path for the query parameter
    * @param value value of the parameter
    * @return BsonDocument for the query
    */
  def handleTokenTextModifier(path: String, value: String): Bson = {
    // Regular expression query definition for partial matching
    val textQuery = regex(_:String, ".*" + FHIRUtil.escapeCharacters(value) + ".*", "i")
    textQuery(path)
    /*// Get the token :text field for target type
    val textFields = TOKEN_DISPLAY_PATHS.get(targetType)
    if(textFields.isEmpty)
      throw new InitializationException(s"The modifier :text cannot be used for elements with type $targetType !!!")

    val queries = textFields.get.map(textField => {
      textQuery(path + textField)
    })
    if(queries.length > 1) or(queries:_*) else queries.head*/
  }
  /**
    * Handle the Token query on system and code fields
    * @param systemPath Path to the system part e.g. Coding.system, Identifier.system
    * @param codePath Path to the code part
    * @param system Expected system value
    * @param code Expected code value
    * @return
    */
  private def handleTokenCodeSystemQuery(systemPath:String, codePath:String, system:Option[String], code:Option[String]):Bson = {
    system match {
      // Query like [code] -> the value of [code] matches a Coding.code or Identifier.value irrespective of the value of the system property
      case None =>
          Filters.eq(codePath, code.get)
      // Query like |[code] -> the value of [code] matches a Coding.code or Identifier.value, and the Coding/Identifier has no system property
      case Some("") =>
        and(exists(systemPath, exists = false), Filters.eq(codePath, code.get))
      // Query like [system][code] -> the value of [code] matches a Coding.code or Identifier.value, and the value of [system] matches the system property of the Identifier or Coding
      case Some(sys) =>
        code match {
          //[system]| --> should macth only systen
          case None =>
            Filters.eq(systemPath, sys)
          // Query like [system][code]
          case Some(cd) =>
            and(Filters.eq(codePath, cd), Filters.eq(systemPath, sys))
        }
    }
  }

  def handleOfTypeModifier(typeSystemPath:String, typeCodePath:String, valuePath:String, typeSystem:String, typeCode:String, value:String):Bson = {
    and(Filters.eq(typeSystemPath, typeSystem), Filters.eq(typeCodePath, typeCode), Filters.eq(valuePath, value))
  }



  /**
    * Handles prefix for date values(implicit range) for date parameters.
    * For further information about using prefixes with range values
    * please refer to prefix table's third column in page;
    * https://www.hl7.org/fhir/search.html#prefix
    *
    * @param path absolute path of the parameter
    * @param value value of the parameter
    * @param prefix prefix of the parameter
    * @return BsonDocument for the query
    */
  // TODO Missing ap for date queries
  def dateRangePrefixHandler(path:String, value:String, prefix:String): Bson = {
    // Populate Implicit ranges(e.g. 2010-10-10 represents the range 2010-10-10T00:00Z/2010-10-10T23:59ZZ)
    val implicitRanges = DateTimeUtil.populateImplicitDateTimeRanges(value)
    // Generate the implicit range paths(i.e. the paths created by the server)
    val rangePaths = (FHIRUtil.mergeElementPath(path,FHIR_EXTRA_FIELDS.TIME_RANGE_START), FHIRUtil.mergeElementPath(path,FHIR_EXTRA_FIELDS.TIME_RANGE_END))

    //If it is a datetime or instant base query
    if(value.contains("T")){
      // Build dateTime query on date time and period query on implicit ranges and combine them.
      or(dateTimeQueryBuilder(FHIRUtil.mergeElementPath(path,FHIR_EXTRA_FIELDS.TIME_TIMESTAMP), prefix, implicitRanges), periodQueryBuilder(rangePaths, prefix, implicitRanges))
    } else {
      //If it is year, year-month, or date query
      //Query over the sub date field
      or(dateTimeQueryBuilder(FHIRUtil.mergeElementPath(path,FHIR_EXTRA_FIELDS.TIME_DATE), prefix, implicitRanges), periodQueryBuilder(rangePaths, prefix, implicitRanges))
    }
  }

  /**
    * Handle prefixes for period parameters. For further information
    * about the prefixes with range values please refer to prefix table's
    * third column in page; https://www.hl7.org/fhir/search.html#prefix
    *
    * @param path absolute path of the parameter
    * @param value value of the parameter
    * @param prefix prefix of the parameter
    * @param isTiming determines if the field is timing
    * @return BsonDocument for the query
    */
  def periodPrefixHandler(path:String, value:String, prefix:String, isTiming:Boolean): Bson = {
    // Generate period fields
    val periodPath = if(isTiming) FHIRUtil.mergeElementPath(path,s"${FHIR_COMMON_FIELDS.REPEAT}.${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}") else path
    val periodRanges = (
      FHIRUtil.mergeElementPath(periodPath, s"${FHIR_COMMON_FIELDS.START}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
      FHIRUtil.mergeElementPath(periodPath, s"${FHIR_COMMON_FIELDS.END}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}")
    )
    // Populate implicit date ranges(e.g. 2010 represents the range 2010-01-01/2010-12-31)
    //val implicitDate = DateTimeUtil.populateImplicitDateRanges(value)
    // Populate implicit date time ranges(i.e. same process with the time ranges)
    val implicitDateTime = DateTimeUtil.populateImplicitDateTimeRanges(value)
    // Generate queries for both date and date time ranges
    periodQueryBuilder(periodRanges, prefix, implicitDateTime)
    //or(periodQueryBuilder(periodRanges, prefix, implicitDate), periodQueryBuilder(periodRanges, prefix, implicitDateTime))
  }

  /**
   * Special processing for Timing.event; all elements should satisfy the query
   * @param path
   * @param value
   * @param prefix
   * @return
   */
  def timingEventHandler(path:String, value:String, prefix:String):Bson = {
      // Populate Implicit ranges(e.g. 2010-10-10 represents the range 2010-10-10T00:00Z/2010-10-10T23:59ZZ)
      val implicitRanges = DateTimeUtil.populateImplicitDateTimeRanges(value)
      // Convert implicit range to dateTime objects(inputted values have already been converted to dataTime format)
      var (floor, ceil) = (BsonTransformer.dateToISODate(implicitRanges._1), BsonTransformer.dateToISODate(implicitRanges._2))

      val subpath = if (value.contains("T")) FHIR_EXTRA_FIELDS.TIME_TIMESTAMP else FHIR_EXTRA_FIELDS.TIME_DATE
      dateRangePrefixHandler(subpath, value, prefix)
      val oppositeQuery = prefix match {
        case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => or(lt(subpath, floor), gt(subpath, ceil))
        case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => or(lt(subpath, floor), and(gte(subpath, floor), lt(subpath, ceil)), equal(path, floor))
        case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => or(gt(subpath, ceil), and(gte(subpath, floor), lt(subpath, ceil)), equal(subpath, floor))
        case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => lt(subpath, floor)
        case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => gt(subpath, ceil)
        case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => or(lt(subpath, floor), gt(subpath, ceil))
        case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => or(lt(subpath, ceil), equal(subpath, ceil))
        case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => or(gt(subpath, floor), equal(subpath, floor))
        case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
          if (ceil == floor)
            or(lt(subpath, floor), gt(subpath, ceil))
          else {
            val delta: Long = ((ceil.asInstanceOf[BsonDateTime].getValue - floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
            ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
            floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
            or(lt(subpath, floor), gt(subpath, ceil))
          }
      }
      val (fieldStart, fieldEnd) = (FHIR_EXTRA_FIELDS.TIME_RANGE_START, FHIR_EXTRA_FIELDS.TIME_RANGE_END)

      val oppositeQuery2 = prefix match {
        case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => or(lt(fieldStart, floor), gt(fieldEnd, ceil))
        case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => lte(fieldEnd, ceil)
        case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => gte(fieldStart, floor)
        case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => or(lte(fieldEnd, ceil), lt(fieldStart, floor), gt(fieldEnd, ceil))
        case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => or(gte(fieldStart, floor), lt(fieldStart, floor), gt(fieldEnd, ceil))
        case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => or(lt(fieldStart, floor), gt(fieldEnd, ceil))
        case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => lte(fieldStart, ceil)
        case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => gte(fieldEnd, floor)
        case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
          if (ceil == floor)
            or(lt(fieldStart, floor), gt(fieldEnd, ceil))
          else {
            val delta: Long = ((ceil.asInstanceOf[BsonDateTime].getValue - floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
            ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
            floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
            or(lt(fieldStart, floor), gt(fieldEnd, ceil))
          }
      }

      and(
        Filters.exists(FHIRUtil.mergeElementPath(path, "event")),
        if(prefix == FHIR_PREFIXES_MODIFIERS.NOT_EQUAL)
          elemMatch(FHIRUtil.mergeElementPath(path, "event"), or(oppositeQuery, oppositeQuery2))
        else
          Filters.nor(elemMatch(FHIRUtil.mergeElementPath(path, "event"), or(oppositeQuery, oppositeQuery2)))
      )
  }


  /**
    * Query builders for period type searches
    *
    * @param path path to the lower and upper boundaries
    * @param prefix prefix of the date
    * @param valueRange value of lower and upper boundaries
    * @return BsonDocument for the target query
    */
  private def periodQueryBuilder(path:(String, String), prefix:String, valueRange:(String, String)):Bson = {
    val isoDate:(BsonValue, BsonValue) = (BsonTransformer.dateToISODate(valueRange._1), BsonTransformer.dateToISODate(valueRange._2))
    /*try {
      // Try to convert input date to date time(only fails when checking periods for date)
      isoDate = (DateTimeUtil.dateToISODate(valueRange._1), DateTimeUtil.dateToISODate(valueRange._2))
    } catch {
      // If the conversion fails accept it as a string
      case e:IllegalArgumentException => isoDate = (BsonString(valueRange._1), BsonString(valueRange._2))
    }*/
    // Initiliaze start and end fields of the ranges
    val (fieldStart, fieldEnd) = path
    // Implicit date range
    var(floor, ceil) = isoDate
    // BsonDocuments that represent the nonexistence of boundary values
    val fieldEndNotExist = and(exists(fieldStart, exists=true), exists(fieldEnd, exists=false))
    val fieldStartNotExist = and(exists(fieldStart, exists=false), exists(fieldEnd, exists=true))

    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => and(gte(fieldStart, floor), lte(fieldEnd, ceil))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => or(gt(fieldEnd, ceil), fieldEndNotExist)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => or(lt(fieldStart, floor), fieldStartNotExist)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => or(periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => or(periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => or(periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange), periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => gt(fieldStart, ceil)
        //or(periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => lt(fieldEnd, floor)
        //or(periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), periodQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        if(ceil == floor)
          and(gte(fieldStart, floor), lte(fieldEnd, ceil))
        else {
           val delta:Long = ((ceil.asInstanceOf[BsonDateTime].getValue -  floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
          ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
          floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
          and(gte(fieldStart, floor), lte(fieldEnd, ceil))
        }
    }
  }

  /**
    * Query builders for date type searches
    *
    * @param path path to the target value
    * @param prefix prefix of the date
    * @param valueRange value of lower and upper boundaries
    * @return BsonDocument for the target query
    */
  private def dateTimeQueryBuilder(path:String, prefix:String, valueRange:(String, String)):Bson = {
    // Convert implicit range to dateTime objects(inputted values have already been converted to dataTime format)
    var(floor, ceil) = (BsonTransformer.dateToISODate(valueRange._1), BsonTransformer.dateToISODate(valueRange._2))
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => or(and(gte(path, floor), lt(path, ceil)), equal(path, floor))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => gt(path, ceil)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => lt(path, floor)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => or(lt(path, floor), gt(path, ceil))
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER => gt(path, ceil)
        //or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.LESS_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => lt(path, floor)
        //or(dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, valueRange), dateTimeQueryBuilder(path, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, valueRange))
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        if(ceil == floor)
          or(and(gte(path, floor), lt(path, ceil)), equal(path, floor))
        else {
          val delta:Long = ((ceil.asInstanceOf[BsonDateTime].getValue -  floor.asInstanceOf[BsonDateTime].getValue) * 0.1).toLong
          ceil = BsonDateTime.apply(ceil.asInstanceOf[BsonDateTime].getValue + delta)
          floor = BsonDateTime.apply(floor.asInstanceOf[BsonDateTime].getValue - delta)
          or(and(gte(path, floor), lt(path, ceil)), equal(path, floor))
        }
    }
  }
}

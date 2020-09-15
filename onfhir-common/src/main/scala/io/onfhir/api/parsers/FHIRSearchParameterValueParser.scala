package io.onfhir.api.parsers

import akka.http.scaladsl.server.{Directive1, Directives}
import io.onfhir.api._
import io.onfhir.exception._
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.api.model.Parameter
import io.onfhir.config.OnfhirConfig
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

/**
 * Parsers for parsing search parameter values given in queries
 */
object FHIRSearchParameterValueParser {
  private val logger: Logger = LoggerFactory.getLogger("FHIRSearchParameterParser")

  /* Core Parser Definitions */
  private trait Parsers extends RegexParsers{
    /* Core Fhir Value Definitions */
    def endOfString:Parser[String] = """($|\$)""".r ^^ (eos => if (eos == "\\$") "$" else "")
    def delimiter:Parser[String] = """\|""".r ^^ {_.toString}
    def string:Parser[String] = """[\p{N}|\p{L}_\-\.|\s| ]+""".r ^^ {_.toString}
    //def integer:Parser[String] = """[0-9]+""".r ^^ {_.toString}
    def integer:Parser[String] = """[0]|[-+]?[1-9][0-9]*""".r ^^ {_.toString}
    def boolean:Parser[String] = """(true|false)""".r ^^ {_.toString}
    //def decimal:Parser[String] = """[0-9]+\.[0-9]+""".r ^^ {_.toString}
    def decimal:Parser[String] = """-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?""".r ^^ {_.toString}
    def code:Parser[String] = """[^\s\|,]+([\s]+[^\s\|,]+)*""".r ^^ {_.toString}
    def dateTime:Parser[String] = """-?[1-2]{1}[0|1|8|9][0-9]{2}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?(\.[0-9]+)?(Z|(\+|-|\s)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?""".r ^^ {_.toString}
    def uri:Parser[String] = """((?<=\()[A-Za-z][A-Za-z0-9\+\.\-]*:([A-Za-z0-9\.\-_~:/\?#\[\]@!\$&'\(\)\*\+,;=]|%[A-Fa-f0-9]{2})+(?=\)))|([A-Za-z][A-Za-z0-9\+\.\-]*:([A-Za-z0-9\.\-_~:/\?#\[\]@!\$&'\(\)\*\+,;=]|%[A-Fa-f0-9]{2})+)(\|[0-9]+(\.[0-9]+)*)?""".r ^^ {_.toString}
    def identifier:Parser[String] = """[A-Za-z0-9\-\.]{1,64}""".r ^^ {_.toString}

    def refVersionPart = """(\|[0-9]+(\.[0-9]+)*)?"""

    /* Token Type Parser Definitions */
    def tokenType:Parser[String] = uri.? ~ delimiter.? ~ code.? ~ delimiter.? ~ string.? ^^ {
      case someUri ~ someDel ~ code ~ othDel ~ value => someUri.getOrElse("") + someDel.getOrElse("") + code.getOrElse("") + othDel.getOrElse("") + value.getOrElse("")
    }

    /* Base Prefix and Suffix Definitions */
    def suffixes:Parser[String] = (""":""".r ~ string).? ^^ {case Some(colon ~ id) => colon + id case None => ""}
    def prefixes:Parser[String] = """(eq|ne|gt|lt|ge|le|sa|eb|ap|<|>)?""".r ^^ {_.toString}

    /* Return Types */
    def dataType:Parser[String] = endOfString
    def parseName:Parser[(String, String)] = string ~ suffixes ~ endOfString ^^ {case id ~ suf ~ eos => (id, suf)}
    def parseValue:Parser[(String, String)] = prefixes ~ dataType ~ (""",""".r ~ dataType).* ~ endOfString ^^ {case pref ~ num ~ someNum ~ eos =>
      (pref, num + someNum.mkString("").replaceAll("(~|\\(|\\))", ""))
    }
  }

  /* Type Specific Parsers */
  private object NameParser extends Parsers
  private object NumberParser extends Parsers {
    /* Parser for Number Type */
    override def dataType:Parser[String] = decimal | integer | boolean

    /* Suffix Definition for Number Type */
    override def suffixes:Parser[String] = """:missing|$""".r

    /* Number Parser Definition */
    def parseNumberName:Parser[(String, String)] = parseName
    def parseNumberValue:Parser[(String, String)] = parseValue
  }
  private object StringParser extends Parsers{
    /* String Parser Definition */
    override def dataType:Parser[String] = string

    /* Prefixes and Suffixes for String Type */
    override def suffixes:Parser[String] = """(:(missing|exact|contains|text))|$""".r ^^ {_.toString}
    override def prefixes:Parser[String] = """""".r

    def parseStringName:Parser[(String, String)] = parseName
    def parseStringValue:Parser[(String, String)] = parseValue
  }
  private object DateParser extends Parsers{
    /* Date Format Definitions */
    override def dataType:Parser[String] = dateTime | boolean

    /* Suffix definitions for Date Format */
    override def suffixes:Parser[String] = """:missing|$""".r ^^ {_.toString}

    def parseDateName:Parser[(String, String)]  = parseName
    def parseDateValue:Parser[(String, String)]  = parseValue
  }
  // TODO :above, :below values stuck at parsers if the code parser is erased, check later
  private object URIParser extends Parsers {
    /* Uri Parser Definition */
    override def dataType:Parser[String]  = uri | boolean | code

    /* Prefixes and Suffixes for URI type */
    override def suffixes:Parser[String]  =   """(:(missing|below|above))|$""".r ^^ {_.toString}
    override def prefixes:Parser[String]  = """""".r

    def parseUriName:Parser[(String, String)] = parseName
    def parseUriValue:Parser[(String, String)] = parseValue
  }
  private object TokenParser extends Parsers  {

    override def dataType:Parser[String] = tokenType | boolean

    /* Suffix Definitions for Token */
    override def suffixes:Parser[String] = """(:(missing|text|in|below|above|not-in|not|of-type|sw|nsw))|$""".r ^^ {_.toString}
    override def prefixes:Parser[String] = """""".r

    def parseTokenName:Parser[(String, String)]  = parseName
    def parseTokenValue:Parser[(String, String)]  = parseValue
  }
  private object QuantityParser extends Parsers {
    /* Quantity Type Parser Definition */
    def numericType:Parser[String] = decimal | integer
    def quantityType:Parser[String] = numericType ~ delimiter ~ uri.? ~ delimiter ~ code.? ^^ {
      case num ~ del1 ~ uri ~ del2 ~ code => num + del1 + uri.getOrElse("") + del2 + code.getOrElse("")
    }
    override def dataType:Parser[String] = quantityType | numericType | boolean

    /* Suffix Definition for Quantity*/
    override def suffixes:Parser[String] = """:missing|$""".r

    def parseQuantityName:Parser[(String, String)] = parseName
    def parseQuantityValue:Parser[(String, String)] = parseValue
  }
  private object ReferenceParser extends Parsers {
    /* Reference Data Type Definitions */
    def history:Parser[String] = """/_history/""".r ~ string ^^ {case history ~ version => history+version }
    def logicalId:Parser[String] = string ~ """/""".r ~ identifier ~ history.? ^^ {case typ ~ slash ~ id ~ history => typ + slash + id + history.getOrElse("")}
    override def dataType:Parser[String] = tokenType | logicalId | uri | identifier | boolean

    /* Prefix Definition for Quantity*/
    override def prefixes:Parser[String] = """""".r
    override def suffixes:Parser[String] = """^((?!(:(text|in|not-in|exact|contains))).)*$|$""".r ^^ {_.toString}

    def parseReferenceName:Parser[(String, String)] = parseName
    def parseReferenceValue:Parser[(String, String)] = parseValue
  }
  private object CompositeParser extends Parsers {
    override def dataType:Parser[String] = """[^\$]+[\$][^\$]+$""".r^^ {_.toString}

    override def prefixes:Parser[String] = """""".r
    override def suffixes:Parser[String] = """""".r

    def parseCompositeName:Parser[(String, String)] = parseName
    def parseCompositeValue:Parser[(String, String)] = parseValue
  }

  /**
    * Parser for _sort search parameter value
    */
  private object SortExprParser extends Parsers {
    override def prefixes:Parser[String] = """[\-]?""".r
    def parse:Parser[(String, String)]  = prefixes ~ string ~ endOfString  ^^ {case prefix ~ param ~ _ => (prefix, param)}
  }

  /**
    * Parser for include/revinclude
    */
  private object IncludeExprParser extends Parsers {
    override def suffixes:Parser[String] = """:iterate|$""".r
    def parse:Parser[(String, String)]  = """_include|_revinclude""".r ~ suffixes ~ endOfString ^^ {case inc ~ suf ~ _ => (inc, suf) }
  }

  /**
    * Parse the name part of a search paremeter to split it to param name and suffix
    * @param nameExpr Name part of the search expression
    * @param paramType Identified parameter type
    * @return
    */
  private def parseSimpleName(nameExpr:String, paramType:String):(String, String) ={
    paramType match {
      case FHIR_PARAMETER_TYPES.NUMBER => NumberParser.parse(NumberParser.parseNumberName, nameExpr).get
      case FHIR_PARAMETER_TYPES.DATE => DateParser.parse(DateParser.parseDateName, nameExpr).get
      case FHIR_PARAMETER_TYPES.STRING => StringParser.parse(StringParser.parseStringName, nameExpr).get
      case FHIR_PARAMETER_TYPES.URI => URIParser.parse(URIParser.parseUriName, nameExpr).get
      case FHIR_PARAMETER_TYPES.TOKEN => TokenParser.parse(TokenParser.parseTokenName, nameExpr).get
      case FHIR_PARAMETER_TYPES.QUANTITY => QuantityParser.parse(QuantityParser.parseQuantityName, nameExpr).get
      case FHIR_PARAMETER_TYPES.REFERENCE => ReferenceParser.parse(ReferenceParser.parseReferenceName, nameExpr).get
      case FHIR_PARAMETER_TYPES.COMPOSITE => CompositeParser.parse(CompositeParser.parseCompositeName, nameExpr).get
    }
  }

  /**
    * Parse the value part of the given search parameter expression and return prefix and expected value(s)
    * @param valueExpr Value part of search expression
    * @param paramType Identified parameter type
    * @return
    */
  def parseSimpleValue(valueExpr:String, paramType:String):Seq[(String, String)] = {
    valueExpr.split(',').map(eachValue =>
      paramType match {
        case FHIR_PARAMETER_TYPES.NUMBER => NumberParser.parse(NumberParser.parseNumberValue, eachValue).get
        case FHIR_PARAMETER_TYPES.DATE => DateParser.parse(DateParser.parseDateValue, eachValue).get
        case FHIR_PARAMETER_TYPES.STRING => StringParser.parse(StringParser.parseStringValue, eachValue).get
        case FHIR_PARAMETER_TYPES.URI => URIParser.parse(URIParser.parseUriValue, eachValue).get
        case FHIR_PARAMETER_TYPES.TOKEN => TokenParser.parse(TokenParser.parseTokenValue, eachValue).get
        case FHIR_PARAMETER_TYPES.QUANTITY => QuantityParser.parse(QuantityParser.parseQuantityValue, eachValue).get
        case FHIR_PARAMETER_TYPES.REFERENCE => ReferenceParser.parse(ReferenceParser.parseReferenceValue, eachValue).get
        case FHIR_PARAMETER_TYPES.COMPOSITE => CompositeParser.parse(CompositeParser.parseCompositeValue, eachValue).get
      }
    )
  }

  /***
    * Parse and validate a simple category search parameter expression
    * @param nameExpr Name part of query
    * @param valueExpr Value part of query
    * @param rtype Resource type query is executed
    * @return parsed parameter
    */
  private def parseSimpleCategory(nameExpr:String, valueExpr:String, rtype:String):Parameter = {
    //Extract param name
    val paramName = NameParser.parse(NameParser.parseName, nameExpr).get._1
    //Find param type
    val searchParamConf = fhirConfig.findSupportedSearchParameter(rtype, paramName)
    if(searchParamConf.isEmpty)
      throw new UnsupportedParameterException(s"Search parameter $paramName is not supported for resource type $rtype! Check conformance statement.")
    //Parse nameExpr and valueExpr
    val (_, suffix) = parseSimpleName(nameExpr, searchParamConf.get.ptype)
    val prefixAndValues = parseSimpleValue(valueExpr, searchParamConf.get.ptype)
    //Return the parameter
    Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, searchParamConf.get.ptype, paramName, prefixAndValues, suffix)
  }

  /***
    * Parse other special parameters
    * @param name Name of special parameter
    * @param valueExpr Value part of query expression
    * @param rtype Resource type that search is on
    * @return
    */
  private def parseSpecialCategory(name:String, valueExpr:String, rtype:String):Parameter = {
    name match {
      //_list parameter in FHIR
      case FHIR_SEARCH_SPECIAL_PARAMETERS.LIST =>
        //Current lists See  https://build.fhir.org/lifecycle.html#current
        if(valueExpr.startsWith("$current-"))
          Parameter(FHIR_PARAMETER_CATEGORIES.SPECIAL, "", name, Nil, valueExpr)
        else //Otherwise it should be direct identifier
          Parameter(FHIR_PARAMETER_CATEGORIES.SPECIAL, "", name, parseSimpleValue(valueExpr, FHIR_PARAMETER_TYPES.TOKEN))
      // _query (named queries)
      case FHIR_SEARCH_SPECIAL_PARAMETERS.QUERY => Parameter(FHIR_PARAMETER_CATEGORIES.SPECIAL, "", name, Seq("" -> valueExpr))
      //ID param
      case FHIR_SEARCH_SPECIAL_PARAMETERS.ID => Parameter(FHIR_PARAMETER_CATEGORIES.SPECIAL, "", name, parseSimpleValue(valueExpr, FHIR_PARAMETER_TYPES.TOKEN))
      // _filter
      case FHIR_SEARCH_SPECIAL_PARAMETERS.FILTER =>
        throw new UnsupportedParameterException("Parameter _filter is not supported by onFhir.io yet!")
      case FHIR_SEARCH_SPECIAL_PARAMETERS.TEXT =>
        throw new UnsupportedParameterException("Parameter _text is not supported by onFhir.io yet!")
      case FHIR_SEARCH_SPECIAL_PARAMETERS.CONTENT =>
        throw new UnsupportedParameterException("Parameter _content is not supported by onFhir.io yet!")
      case _ =>
        throw new UnsupportedParameterException(s"Parameter $name is not supported by onFhir.io yet!")
    }
  }

  /***
    * Parse FHIR Result parameters
    * @param nameExprs Name part of query expression
    * @param valueExpr Value part of query expression
    * @param rtype Resource type that search is on
    * @return
    */
  private def parseResultCategory(nameExprs:String, valueExpr:String, rtype:String):Parameter ={
    nameExprs match {
      //For these values should be number
      case FHIR_SEARCH_RESULT_PARAMETERS.COUNT | FHIR_SEARCH_RESULT_PARAMETERS.PAGE =>
        if(Try(valueExpr.toLong).toOption.isEmpty)
          throw new InvalidParameterException(s"Parameter $nameExprs does not have a number value $valueExpr !")
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, "", nameExprs, Seq("" -> valueExpr))

      //Parse sort
      case sort if sort.startsWith(FHIR_SEARCH_RESULT_PARAMETERS.SORT) =>
        //Sorting was different at DSTU2
        if(fhirConfig.isDstu2()) {
          val (pname, suffix) = NameParser.parse(NameParser.parseName, nameExprs).get

          if(fhirConfig.findSupportedSearchParameter(rtype, valueExpr).isEmpty)
            throw new UnsupportedParameterException(s"Search parameter $pname is not supported for resource type $rtype, you can not use it for sorting! Check conformance statement of server!")

          if(! (suffix == "" || suffix == FHIR_PREFIXES_MODIFIERS.ASCENDING_OLD || suffix == FHIR_PREFIXES_MODIFIERS.DESCENDING_OLD))
            throw new InvalidParameterException(s"Invalid suffix $suffix for _sort parameter!")
          //Return parameter
          Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, "", pname, Seq((if(suffix ==  FHIR_PREFIXES_MODIFIERS.DESCENDING_OLD ) FHIR_PREFIXES_MODIFIERS.DESCENDING else "") -> valueExpr))
        } else {
          //Parse the prefix and search parameter names
          val prefixAndValues =  valueExpr.split(',').map(v => SortExprParser.parse(SortExprParser.parse, v))
          //Check if we can parse them
          if(prefixAndValues.exists(_.isEmpty))
            throw new InvalidParameterException(s"Invalid parameter value for _sort; $valueExpr !")
          //Check if we support all
          val undefinedParam = prefixAndValues.find(v => fhirConfig.findSupportedSearchParameter(rtype, v.get._2).isEmpty)
          if(undefinedParam.nonEmpty)
            throw new UnsupportedParameterException(s"Search parameter ${undefinedParam.get.get._2} is not supported for resource type $rtype , you can not use it for sorting! Check conformance statement of server!!")
          //Return the parameter
          Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, "", nameExprs, prefixAndValues.map(_.get))
        }

      //Elements is just names of elements to include in search result
      case FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS =>
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, "", nameExprs, parseSimpleValue(valueExpr, FHIR_PARAMETER_TYPES.STRING))

      //Validate summary param
      case FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY =>
        val r = StringParser.parse(StringParser.parseStringValue, valueExpr)
        if(r.isEmpty || r.get._1!="" || !Set(FHIR_SUMMARY_OPTIONS.COUNT, FHIR_SUMMARY_OPTIONS.DATA, FHIR_SUMMARY_OPTIONS.FALSE, FHIR_SUMMARY_OPTIONS.TEXT, FHIR_SUMMARY_OPTIONS.TRUE).contains(r.get._2))
          throw new InvalidParameterException(s"Invalid parameter value for _summary $valueExpr ! Please see https://build.fhir.org/search.html#summary!")
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, "", nameExprs, Seq(r.get))

      //Inclusions (_include or _revinclude)
      case inclusion if inclusion.startsWith(FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE) || inclusion.startsWith(FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE) =>
        val presult = IncludeExprParser.parse(IncludeExprParser.parse, nameExprs)
        if(!presult.successful)
          throw new InvalidParameterException(s"Invalid parameter value for _include or _revinclude; $inclusion ! https://build.fhir.org/search.html#revinclude")

        //Get inclusion type, and iteration suffix if exist
        val (inclusionType, iteration) = presult.get

        //Parse the actual part to find resource type to join, paramter for joining and target type of reference to join
        val (joinedResourceType, joinedParam, targetType) = valueExpr.split(":")  match {
          case Array("*") if inclusionType == FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE => (rtype, "*", None) //include all references, in the given resource type
          case Array(prtype, paramName) => (prtype, paramName, None)
          case Array(prtype, paramName, tType) => (prtype ,paramName, Some(tType))
          case _ => throw new InvalidParameterException("Invalid usage of _include or _revinclude parameter, please check https://www.hl7.org/fhir/search.html#include")
        }

        //Check if given _include or _revinclude supported for resource type
        if(
          inclusionType == FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE && joinedParam!= "*" && !fhirConfig.resourceConfigurations(rtype).searchInclude.exists(_.contains(joinedParam)) ||
            inclusionType == FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE && joinedParam!= "*" && !fhirConfig.resourceConfigurations(rtype).searchRevInclude.exists(_.contains(joinedResourceType + ":" + joinedParam))
        )
          throw new UnsupportedParameterException(s"Search parameter _include or _revinclude is not supported for $valueExpr on resource type $rtype ! Please check conformance statement of server!")

        //Handle if * is used
        val joins =
          (inclusionType, joinedParam) match {
            //Inclusion of all
            case (FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE, "*") =>
              fhirConfig.resourceConfigurations(rtype)
                .searchInclude.toSeq //Get all supported includes
                .map(p => joinedResourceType -> p.split("\\.").last) // Parse and get the last e.g. CarePlan.goal -> goal
            case (FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE, "*") =>
              fhirConfig.resourceConfigurations(rtype)
                .searchRevInclude.toSeq //Get all supported reverse includes
                .map(_.split(":")) //Parse them e.g. AllergyIntolerance:patient
                .map(s => s.head -> s.last)
            case _ =>
              Seq(joinedResourceType -> joinedParam)
          }

        // Return the parsed param
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, targetType.getOrElse(""), presult.get._1, joins, presult.get._2)

      //_total parameter is ignored, we always return the total
      case FHIR_SEARCH_RESULT_PARAMETERS.TOTAL =>
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, "", nameExprs, Seq("" -> valueExpr))
      case FHIR_SEARCH_RESULT_PARAMETERS.SINCE | FHIR_SEARCH_RESULT_PARAMETERS.AT =>
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, "", nameExprs, Seq("" -> valueExpr))
      case _ =>
        throw new UnsupportedParameterException(s"Search parameter $nameExprs is not supported in onFhir.io! ")
    }
  }

  /***
    * Parse and validate reverse chain search expression (_has parameter) e.g. /Patient?_has:Observation:patient:_has:AuditEvent:entity:user=MyUserId
    * @param nameExpr  Name part of expression e.g. _has:Observation:patient:_has:AuditEvent:entity:user
    * @param valueExpr Value part of expression e.g. MyUserId
    * @param rtype Resource type that search in on
    */
  private def parseReversedChainedCategory(nameExpr:String, valueExpr:String, rtype:String):Parameter = {
    val revChainParts =
      nameExpr
        .split("\\_has:")
        .drop(1)
        .map(_.split(':'))

    val queryPart = revChainParts.last.last

    val chainParts = (revChainParts.dropRight(1) ++ Seq(revChainParts.last.dropRight(1))).map(c => c.length match {
      case 2 => (c.apply(0), c.apply(1))
      case _ => throw new InvalidParameterException(s"Invalid usage of _has parameter '$nameExpr'! Syntax is erroneous, please see https://build.fhir.org/search.html#has !")
    })
    //Resource type to query on
    val queryOn = chainParts.last._1
    var tempRType = rtype
    val parsedChainQuery = chainParts.map(c => {
        if(!fhirConfig
            .findSupportedSearchParameter(c._1, c._2) //Get parameter conf
            .filter(_.ptype == FHIR_PARAMETER_TYPES.REFERENCE) //Be sure it is a reference
            .map(_.targets) //get target types for this reference type param
            .getOrElse(Nil).contains(tempRType))
          throw new InvalidParameterException(s"Not supported parameter in _has chain, parameter ${c._2} on resource type ${c._1} is not supported or does not target $tempRType ! Syntax is erroneous, please see https://build.fhir.org/search.html#has !")
        //Set temp to resource type
        tempRType = c._1
        //Return ResourceType that is to be chained and parameter to refer that e.g. Observation -> patient, AuditEvent -> entity
        (c._1, c._2)
    })

    //Parse for the actual query part
    val parameter = parseSimpleCategory(queryPart, valueExpr, queryOn)
    //Set the chain part
    parameter.copy(chain = parsedChainQuery, paramCategory = FHIR_PARAMETER_CATEGORIES.REVCHAINED)
  }

  /***
    * Parse and validate a chained search parameter expression See https://build.fhir.org/search.html#chaining
    * e.g. Patient?general-practitioner.name=Joe
    * @param nameExpr Name part of expression e.g. general-practitioner.name
    * @param valueExpr Value part of expression e.g. Jow
    * @param rtype Resource type that search in on e.g. Patient
    * @return
    */
  private def parseChainedCategory(nameExpr:String, valueExpr:String, rtype:String):Parameter = {
    //Get chaining part
    val chainedParts = nameExpr.split('.')

    val chains =
        chainedParts
          .dropRight(1) //Last part is actual query
          .map(_.split(':')) //split to get the resource type indicators for chain

      //Last part is actual query parameter
     val queryPart = chainedParts.last

     //Parse the chain part of the query
     var lastTempRType = rtype
     val parsedChainQuery = chains.map(c => {
        val pname = c.head //Parameter name in the chain
        //Trying to find the chained resource type
        lastTempRType = c.length match {
          case 1 =>
            val possibleTargets =
              fhirConfig.findSupportedSearchParameter(lastTempRType, pname) //Find parameter definition
                .filter(_.ptype == FHIR_PARAMETER_TYPES.REFERENCE) //Be sure that it is a reference parameter
                .map(_.targets) //get target types for this reference type param
                .getOrElse(Set.empty)
            possibleTargets.size match {
              case 1 =>  possibleTargets.head
              case _ =>  throw new InvalidParameterException(s"Invalid usage of chained search '$nameExpr', need type discriminator for parameter '$pname'! Syntax is erroneous, please see https://build.fhir.org/search.html#chaining !")
            }
          case 2 => chains.last.last
          case _ => throw new InvalidParameterException(s"Invalid usage of chained search '$nameExpr'! Syntax is erroneous, please see https://build.fhir.org/search.html#chaining !")
        }
        (lastTempRType, pname)
      })

      //Parse for the actual query part
      val parameter = parseSimpleCategory(queryPart, valueExpr, parsedChainQuery.last._1)
      //Set the chain part
      parameter.copy(chain = parsedChainQuery, paramCategory = FHIR_PARAMETER_CATEGORIES.CHAINED)
  }

  /**
    * Parses parameters that are provided in the form; parameterName -> list of values
    * return list of parameter objects.
    * @param parameters parsed HTTP parameters from query or entity
    * @param _type ResourceType that search is on
    * @return
    */
  private def parseParameters(parameters: Map[String, List[String]], _type: String, preferHeader:Option[String]): List[Parameter] = {
    parameters.flatMap(eachParameter => {
      eachParameter._2.flatMap( valueExpr =>
        try {
          val parsedParam = eachParameter._1 match {
            //If it contains a ., then it is a chain search
            case chainedParamExpr if chainedParamExpr.contains('.') =>
              parseChainedCategory(chainedParamExpr, valueExpr, _type)
            //If it starts with _, it is commonly defined param
            case commonParamExpr if commonParamExpr.startsWith("_") =>
              //If it is reverse chain search
              if(commonParamExpr.startsWith(FHIR_SEARCH_SPECIAL_PARAMETERS.HAS)){
                parseReversedChainedCategory(commonParamExpr, valueExpr, _type)
              } else {
                //Parse parameter name
                NameParser.parse(NameParser.parseName, commonParamExpr).get._1 match {
                  case specialParam if fhirConfig.FHIR_SPECIAL_PARAMETERS.contains(specialParam) =>
                    parseSpecialCategory(commonParamExpr, valueExpr, _type)
                  case resultParam if fhirConfig.FHIR_RESULT_PARAMETERS.contains(resultParam) =>
                    parseResultCategory(commonParamExpr, valueExpr, _type)
                  case _ =>
                    parseSimpleCategory(commonParamExpr, valueExpr, _type)
                }
              }
            //Othe normal defined parameters
            case normalParamExpr =>
              parseSimpleCategory(normalParamExpr, valueExpr, _type)
          }
          Some(parsedParam)
        } catch {
          case invalidParameterException: InvalidParameterException => throw  invalidParameterException
          case unsupportedParameterException: UnsupportedParameterException =>
            if(preferHeader.getOrElse(OnfhirConfig.fhirSearchHandling)  == FHIR_HTTP_OPTIONS.FHIR_SEARCH_STRICT)
              throw unsupportedParameterException
            else {
              //Just log warning and continue
              logger.warn("Unsupported Parameter: " + unsupportedParameterException.getMessage)
              None
            }
          case other:Exception =>
            logger.debug("Search Parameter Parsing Exception:", other)
            throw new InvalidParameterException("Invalid usage of parameter.Check https://www.hl7.org/fhir/search.html#" + eachParameter._1 + " for valid usage!")
        }
      )
    }).toList
  }

  /**
    * Parse and return all the search parameter by eliminating _format parameter
    * @param _type Resource type
    * @param parameters Spray parsed parameters
    * @param preferHeader Prefer header is search
    * @return
    */
  def parseSearchParameters(_type: String, parameters: Map[String, List[String]], preferHeader:Option[String] = None): List[Parameter] = {
    // Parse parameters
    FHIRSearchParameterValueParser
      .parseParameters(parameters - FHIR_HTTP_OPTIONS.FORMAT, _type, preferHeader)
  }

  /**
    * Directive to parse search parameters from URI
    * @param _type Resource type that search is on
    * @param preferHeader Prefer header is search
    * @return
    */
  def parseSearchParametersFromUri(_type: String, preferHeader:Option[String]):Directive1[List[Parameter]] =
    Directives.parameterMultiMap.map(FHIRSearchParameterValueParser.parseSearchParameters(_type, _, preferHeader))

  /**
    * Directive to parse search parameters from-url-encoded entity
    * @param _type Resource type that search is on
    * @param preferHeader Prefer header is search
    * @return
    */
  def parseSearchParametersFromEntity(_type: String, preferHeader:Option[String]):Directive1[List[Parameter]] =
    Directives.formFieldMultiMap.map(FHIRSearchParameterValueParser.parseSearchParameters(_type, _, preferHeader))


  /**
    * Construct a compartment search parameter
    * @param compartmentType Compartment type e.g. Patient
    * @param compartmentId compartment id
    * @param resourceType Resource type that search is on
    * @return
    */
  def constructCompartmentSearchParameter(compartmentType:String, compartmentId:String, resourceType:String):Parameter = {
    //Extract the name of search parameters that will relate to the compartment
    val correspondingParameterNames = fhirConfig.compartmentRelations.get(compartmentType).flatMap(_.get(resourceType)).getOrElse(Set.empty)
    //Construct the parameter
    Parameter(
      FHIR_PARAMETER_CATEGORIES.COMPARTMENT,
      FHIR_PARAMETER_TYPES.REFERENCE,
      "", //No need to name of the param
      Seq(compartmentType -> compartmentId), //Set the expected value (ResourceType, id)
      "", //No suffix
      correspondingParameterNames.map(pname => ("", pname)).toSeq) //Set the parameterNames to check
  }
}

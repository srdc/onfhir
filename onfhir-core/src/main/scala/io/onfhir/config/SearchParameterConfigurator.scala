package io.onfhir.config

import org.slf4j.{Logger, LoggerFactory}
import io.onfhir.api._
import io.onfhir.api.util.{BaseFhirProfileHandler, FHIRUtil}

import scala.util.parsing.combinator.RegexParsers
import scala.util.{Success, Try}
/**
  * Created by ozan before
  * Update by tuncay on 11/16/2016
  * Supplementary class to help parsing of XPaths defined in FHIR SearchParameter definitions and converting them to
  * some type of JSON paths by creating search parameter configuration objects (SearchParameterConf)
  * @param rtype                Resource type that parameters are configured
  * @param rtypeBaseProfile     Base profile specified for the resource type in CapabilityStatement
  * @param fhirConfig           FhirConfig with configured profiles
  * @param allSearchParameters  URL of all search parameters supported by the resource type
  */
class SearchParameterConfigurator(
                                   rtype:String,
                                   rtypeBaseProfile:Option[String],
                                   fhirConfig:FhirConfig,
                                   allSearchParameters:Set[String]
                                 ) extends BaseFhirProfileHandler(fhirConfig) {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)
  // Find profile definition for base Resource type
  val baseProfileChain = fhirConfig.getBaseProfileChain(rtype)
  //Find profile chain for base profile specific for resource type
  val profileChain = rtypeBaseProfile.map(url => fhirConfig.findProfileChain(url)).getOrElse(baseProfileChain)


  /**
   * Create a search parameter configuration for the given search parameter definition for the specific resource type
   * @param searchParameterDef
   * @return
   */
  def createSearchParameterConf(searchParameterDef:FHIRSearchParameter):Option[SearchParameterConf] = {
    searchParameterDef.ptype match {
      //We never use special parameter definitions, just create it to set we are supporting it
      case FHIR_PARAMETER_CATEGORIES.SPECIAL =>
        Some(SearchParameterConf(searchParameterDef.name, searchParameterDef.ptype, Seq(searchParameterDef.xpath.getOrElse("")), Nil, searchParameterDef.modifiers, Nil))
      //If parameter is composite
      case FHIR_PARAMETER_TYPES.COMPOSITE =>
        if(searchParameterDef.expression.nonEmpty && searchParameterDef.components.nonEmpty)
          handleCompositeParameter(searchParameterDef)
        else {
          logger.warn(s"Composite parameter '${searchParameterDef.name}' for resource type $rtype cannot be processed! It has no component or expression definition!")
          None
        }
      //Otherwise check Expression or Xpath
      case _ =>
        //First check expression
        var pathAndRestrictions =
          Try(
            searchParameterDef
              .expression
              .map(expr =>
                SearchParameterConfigurator.parseMultiplePathExpression(rtype, expr)
              )
          ).recoverWith {
            case ex:Exception =>
              logger.warn(s"Problem while parsing search parameter expression '${searchParameterDef.expression}'! Trying xpath expression to resolve possible target paths...")
              Success(None)
          }.toOption.flatten

        if(pathAndRestrictions.isEmpty){
          pathAndRestrictions = searchParameterDef.xpath
            .map(xpath =>
              SearchParameterConfigurator.parseXpath(rtype, xpath)
            )
        }

        if(pathAndRestrictions.isEmpty)
          logger.warn(s"We cannot process paths for search parameter '${searchParameterDef.name}' for resource type '$rtype' neither from Xpath nor from FHIR Path expression!")

        pathAndRestrictions
          .flatMap(par => {
            val extensionPaths = par.filter(_._1.contains("extension[i]"))
            val extensionTargetTypes = extensionPaths.map(e => SearchParameterConfigurator.findOutTargetTypeForExtensionPath(fhirConfig, e._1))
            val nonExtensionPathDetails = par.diff(extensionPaths)
            //Find out the target types for paths or target type for references
            val (nonExtensionPaths, nonExtensionTargetTypes, restrictions, targetReferencedProfiles) = transformPathsAndExtractTargetTypes(searchParameterDef.ptype, nonExtensionPathDetails)

            val finalPaths = extensionPaths.map(_._1) ++ nonExtensionPaths
            val finalTargetTypes = extensionTargetTypes ++ nonExtensionTargetTypes

            if(finalPaths.isEmpty || finalTargetTypes.isEmpty) {
              logger.warn(s"Problem while processing search parameter '${searchParameterDef.name}' for resource type '$rtype' , invalid definition for FHIR!")
              None
            } else
              Some(constructConfFromDef(
                searchParameterDef,
                finalPaths,   //Alternative Paths for the search parameter
                finalTargetTypes, //Target type for each path
                extensionPaths.map(_._2) ++ restrictions, //Set of restrictions for each path
                targetReferencedProfiles                  //If reference set of target profiles for each path
              )
            )
          })
    }
  }

  /**
   * Construct a search parameter configuration for onFhir.io
   * @param searchParameterDef
   * @param finalPaths
   * @param finalTargetTypes
   * @param restrictions
   * @param targetReferences
   * @param onExtension
   * @return
   */
  private def constructConfFromDef(searchParameterDef:FHIRSearchParameter, finalPaths:Seq[String], finalTargetTypes:Seq[String], restrictions:Seq[Seq[(String, String)]], targetReferences:Seq[Set[String]] = Nil, onExtension:Boolean = false):SearchParameterConf = {
    SearchParameterConf(
      pname = searchParameterDef.name,
      ptype = searchParameterDef.ptype,
      paths = finalPaths,
      targets =
        searchParameterDef.ptype match {
          case FHIR_PARAMETER_TYPES.REFERENCE =>
            if(targetReferences.isEmpty) {
              searchParameterDef.target.toSeq
            } else
              targetReferences.reduce((s1, s2) => s1.union(s2)).flatMap(fhirConfig.findResourceType).toSeq match {
                case Nil => searchParameterDef.target.toSeq
                case oth => searchParameterDef.target.toSeq.intersect(oth)
              }

          case FHIR_PARAMETER_TYPES.COMPOSITE => targetReferences.head.toSeq
          case _ => Nil
        }
      ,
      modifiers = searchParameterDef.modifiers,
      targetTypes = finalTargetTypes,
      restrictions = restrictions,
      multipleOr = searchParameterDef.multipleOr.getOrElse(true),
      multipleAnd = searchParameterDef.multipleAnd.getOrElse(true),
      comparators = searchParameterDef.comparators
    )
  }

  /**
    * Supplementary method to extract target types of elements for each path
    * @param ptype FHIR search parameter type
    * @param pathsAndRestrictions Paths to evaluate (and the restrictions on path)
    * @return (Paths, Target Element Types, Restrictions on paths, referenced target profiles)
    */
  private def transformPathsAndExtractTargetTypes(ptype:String, pathsAndRestrictions:Seq[(String, Seq[(String, String)])]):(Seq[String], Seq[String], Seq[Seq[(String, String)]], Seq[Set[String]]) = {
    //Filter the paths that exist in the given profile
    val filteredPaths:Seq[(String, String, Seq[(String, String)], Set[String])] = pathsAndRestrictions.flatMap { case (path, restriction) =>
      if(path == "") //If this is a composition root path with the resource as root
        Some((path, "Resource", Nil, Set.empty[String]))
      else {
        //Handle search path with array index e.g. Bundle.entry[0].resource
        val parsedPath = SearchParameterConfigurator.parsePathWithArrayIndex(path)
        val normalizedPath = parsedPath.map(_._1).mkString(".")
        //Find the target type for the path
        var pathAndTargetTypeDetails = findTargetTypeOfPath(normalizedPath, profileChain)
        val referenceTargetProfiles = pathAndTargetTypeDetails.head._4


        //Filter out the paths with type that is not compatible to search parameter type
        if(ptype != FHIR_PARAMETER_TYPES.COMPOSITE)
          pathAndTargetTypeDetails = pathAndTargetTypeDetails.filter(tt => FHIR_PARAMETER_TYPE_TARGETS(ptype).contains(tt._2))

        //Transform the paths to our path format (just add [i] to the path parts that are arrays)
        val finalPaths =
          pathAndTargetTypeDetails
            .map {
              case("", t, r, rp) => ("", t,t,rp)
              case(path, t, r, rp) =>
                //Split the path
                val pathParts = path.split('.')
                //Find cardinality of each prefixes of path
                val arrayIndicators =
                  pathParts.indices
                    .reverse
                    .map(i => findPathCardinality(pathParts.dropRight(i).mkString("."), baseProfileChain))
                //If it is an array, add [i] indicator
                val finalPath = pathParts.zip(arrayIndicators).map {
                  case (pp, false) => pp
                  case (pp, true) => if(pp.last != ']') pp+"[i]" else pp
                }.mkString(".") //Merge path again

                (finalPath, t, r, rp)
            }



        finalPaths.map(pathAndTargetType => {
          val finalPath =
            if(parsedPath.exists(_._2.isDefined))
              pathAndTargetType._1.split('.')
                .zip(parsedPath.map(_._2))
                .map(p => p._1 + p._2.map(i => s"[$i]").getOrElse("")).mkString(".")
            else
              pathAndTargetType._1

          (finalPath, pathAndTargetType._2, restriction, if(pathAndTargetType._2 == FHIR_DATA_TYPES.REFERENCE) referenceTargetProfiles else Set.empty[String])
        })
      }
    }

    //Return paths and types separately
    //If there is no target type, it means this path is not valid for this profile (cardinality set to 0, or removed from choices like value[x])
    (filteredPaths.map(_._1), filteredPaths.map(_._2), filteredPaths.map(_._3), filteredPaths.map(_._4))
  }



  /**
   * Returns a list of search parameter configuration by parsing the Xpath and converting it to JSON path
   * @param searchParameterDef Search parameter definition
   * @return
   */
  private def handleCompositeParameter(searchParameterDef:FHIRSearchParameter):Option[SearchParameterConf] = {
    //Extract combined parameters
    val combinedParamUrls =
      searchParameterDef.components
        //.map(c => c.split('/').last) //Take the last part
        //.map(d => d.substring(d.indexOf('-')+1)) //Split it from the first - to get the parameter name

    //Check if all parameters exist
    if(!combinedParamUrls.subsetOf(allSearchParameters)) {
      //throw new InitializationException(s"Some of the parameters ${combinedParamNames.diff(otherParameters)} referred in composite parameter $pname does not exist in defined parameters for the resource type $rtype!")
      logger.warn(s"Some of the parameters ${combinedParamUrls.toSet.diff(allSearchParameters)} referred in composite parameter ${searchParameterDef.name} does not exist in defined parameters for the resource type $rtype!")
      None
    } else {
      // Find common paths for the composite parameters from expression
      val compositionPaths =
        searchParameterDef.expression.get
          .split('|') //Split if there are alternative paths
          .filter(_.startsWith(rtype)) //Filter the related paths
          .map(_.trim().replace(rtype,"")) //Remove the resource type part from the path
          .map(p => if(p!="") p.drop(1) else  p) //Remove the dot after resource type if exist
          .map(p => p -> Nil)

      val (finalPaths, finalTargetTypes, _, _) = transformPathsAndExtractTargetTypes(searchParameterDef.ptype, compositionPaths)

      val combinedParamNames =
        combinedParamUrls
        .map(c => c.split('/').last) //Take the last part
        .map(d => d.substring(d.indexOf('-')+1)) //Split it from the first - to get the parameter name

      Some(constructConfFromDef(searchParameterDef, finalPaths, finalTargetTypes, Nil, Seq(combinedParamNames)))
    }
  }
/*
  /**
    * Parse the XPath of the search parameter including an extension, and returns a single SearchParameterConf
    * with the following JSON path format as example
    * e.g. XPath: /Goal/extension[@url='http://hl7.org/fhir/StructureDefinition/goal-target']/extension[@url='measure']/valueCodeableConcept
    *     --> Seq(Seq( extension.url -> http://hl7.org/fhir/StructureDefinition/goal-target,
    *                  extension.extension.url -> measure,
    *                  extension.extension.valueCodeableConcept -> ""))
    *
    * e.g. XPath: ProcedureRequest/extension[@url='...']/valueDateTime | ProcedureRequest/extension[@url='...']/valuePeriod
    *     --> Seq(
    *           Seq(extension.url-> ..., extension.valueDateTime->"")
    *           Seq(extension.url-> ..., extension.valuePeriod->"")
    *         )
    * @param searchParameterDef Definition of search parameter
    * @return
    */
  private def handleParametersOnExtensions(searchParameterDef:FHIRSearchParameter): SearchParameterConf = {
    //val extensionXPathReg = """extension\\[@url='(\w+)'\\]""".r
    val extensionXPathReg = """extension\[@url='([^']*)'\]""".r

    //Divide the path if it is a multiple xpaths divided by  or ('|')
    //e.g. ProcedureRequest/extension[@url='...']/valueDateTime | ProcedureRequest/extension[@url='...']/valuePeriod
    val paramereterJsonPaths:Seq[Any] = searchParameterDef.xpath.get.replace(" ", "").split("\\|").toSeq.map(eachPath => {
      //Split each path to its components
      //e.g. /Goal/extension[@url='http://hl7.org/fhir/StructureDefinition/goal-target']/extension[@url='measure']/valueCodeableConcept

      //Extract the extension Urls
      val extensionUrls:Seq[(String, Int)] = extensionXPathReg.findAllMatchIn(eachPath).toList.map(rm => rm.group(1)).zipWithIndex
      // The last component is the value element (e.g. valueCondableConcept)
      val valueElement = eachPath.split("/").last.replaceFirst(".*:", "")
      // Others are extension components (extension[@url='http://hl7.org/fhir/StructureDefinition/goal-target'], extension[@url='measure'])

      //Form the JSON paths for each extension component
      // e.g. extension.url -> http://hl7.org/fhir/StructureDefinition/goal-target
      //      extension.extension.url -> measure
      val paths:Seq[(String, String)] = extensionUrls.map( urlWithIndex => {
        val extensionPrefix:String = "extension[i]." * (urlWithIndex._2 + 1)
        val path:(String, String) = (extensionPrefix + "url") -> urlWithIndex._1
        path
      }) :+ (("extension[i]." * (extensionUrls.last._2 + 1)) + valueElement) -> "" //And add the valueElement (e.g. extension.extension.valueCodeableConcept
      paths
    })
    //Find out the target types for those paths
    val targetTypes =
      paramereterJsonPaths
        .map(rpath =>
          rpath.asInstanceOf[Seq[(String, String)]]
            .last._1 //Last one is the actual value path
            .split("\\.").last //Just get the value* part
            .replace("value", "") //Replace the value to get the type name
        )
        .map(ttype => if(fhirConfig.FHIR_COMPLEX_TYPES.contains(ttype)) ttype else ttype.toLowerCase) // if it is a simple type, convert to lowercase e.g. valueString -> string

    constructConfFromDef(searchParameterDef, paramereterJsonPaths, targetTypes, Nil, Nil,true)
  }*/
}

object SearchParameterConfigurator extends RegexParsers {
  val quote = "'"  ^^ {case x => x}
  val quotedValue:Parser[String] = quote ~  """[^\']*""".r ~ quote ^^ {case _ ~ v ~ _ => v}

  val elemName:Parser[String] = """\w+""".r ^^ {case e => e}
  val prefix:Parser[String] = """\w+:""".r ^^ {case p => p}
  val xpathElementName:Parser[String] = prefix.? ~ elemName ^^ { case _ ~ n => n}

  //a condion check e.g. f:type/@value='composed-of'
  val xpathConditionItem:Parser[(String, String)] = xpathElementName ~ ("/"~ xpathElementName ).* ~ "/@value=" ~ quotedValue ^^ {
    case e ~ te ~ _ ~ v =>
      (e +: te.map(_._2)).mkString(".") -> v
  }

  val xpathCondition:Parser[Seq[(String, String)]] = "[" ~  xpathConditionItem ~ ("and" ~ xpathConditionItem).* ~ "]" ^^ {
    case _ ~ c ~ mc ~ _ =>
      c +: mc.map(_._2)
  }

  val xpathExtensionItem:Parser[(String, Seq[(String, String)])] = prefix.? ~ "extension[@url=" ~ quotedValue ~ "]" ^^ {case  _ ~ _ ~ url ~ _ => "extension[i]" -> Seq("url" -> url)}

  val xpathOtherItem:Parser[(String, Seq[(String, String)])] = xpathElementName ~ xpathCondition.? ^^ { case e ~ c => e -> c.getOrElse(Nil)}

  val xpathItem:Parser[(String, Seq[(String, String)])] = xpathExtensionItem | xpathOtherItem

  val xpathPath:Parser[Seq[(String, Seq[(String, String)])]] = xpathItem ~ ("/" ~ xpathItem).* ^^ {case p ~ op => p +: op.map(_._2)}

  val xpathMultiplePath:Parser[Seq[Seq[(String, Seq[(String, String)])]]] = xpathPath ~ ("|" ~ xpathPath).*  ^^ {case p ~ op => p +: op.map(_._2)}

  /**
   * Parse a path with array index
   * @param path
   * @return
   */
  def parsePathWithArrayIndex(path:String):Seq[(String, Option[Int])] = {
    path.split('.').map(p =>
    if(p.last == ']')
      p.takeWhile(_ != '[') -> Some(p.dropWhile(_ != '[').drop(1).dropRight(1).toInt)
    else
      p -> None
    )
  }

  /**
   * Find out the target type of an extension path e.g. extension[i].extension[i].valueCodeableConcept
   * @param extensionPath
   * @return
   */
  def findOutTargetTypeForExtensionPath(fhirConfig:FhirConfig, extensionPath:String):String = {
    val dataType = extensionPath
      .split('.')
      .last
      .replace("value", "") //Replace the value to get the type name
    // if it is a simple type, convert to lowercase e.g. valueString -> string
    if(fhirConfig.FHIR_COMPLEX_TYPES.contains(dataType)) dataType else dataType.toLowerCase
  }

  /**
   *
   * @param resourceType
   * @param expr
   * @return
   */
  def parseMultiplePathExpression(resourceType:String, expr:String):Seq[(String, Seq[(String, String)])] = {
    expr.trim
      .split('|')
      .map(_.trim)
      .filter(p => p.contains(resourceType + ".") || !p.contains('.'))//Either it starts
      .map(parsePathExpression)
  }

  /**
   * Parse a single path expression that are given in search parameter definitions
   * @param expr  FHIR Path Expression indicating the paths of search parameter
   *              e.g.
   *                - Account.subject.where(resolve() is Patient)  * with a resolve
   *                - ActivityDefinition.useContext.code           * simple example
   *                - ActivityDefinition.relatedArtifact.where(type='composed-of').resource   * with a restriction
   *                - Condition.abatement.as(Age)  * with as
   *                - (ActivityDefinition.useContext.value as CodeableConcept) * with as
   *                - Bundle.entry[0].resource   * with an index
   *                - Patient.deceased.exists() and Patient.deceased != false * special case
   *
   * @return
   */
  def parsePathExpression(expr:String):(String, Seq[(String, String)]) = {
    //Assume that parenthesis cover the expression
    var nexpr =
      if(expr.head == '(') { //If there are parenthesis around remove them
        //Assuming only one as e.g. (Observation.value as CodeableConcept).text", (ActivityDefinition.useContext.value as CodeableConcept)
        val casting = expr.drop(1).split(" as ")
        val pind = casting.last.lastIndexOf(')')

        val typeToCast = casting.last.substring(0, pind)
        //Merge them e.g Observation.valueCodeableConcept.text
        casting.head +
          typeToCast.capitalize +
          (if(pind == casting.last.length - 1) "" else casting.last.substring(pind+1))
      } else
        expr

    //Split the path, and drop the Resource type part
    var paths = nexpr.split('.')
    if(paths.head.head.isUpper) //this is for special cases where they don't use resource type at the beginning
      paths = paths.drop(1)

    val parsedPaths =
      paths
        .zipWithIndex
        .map(p =>
          //restriction or resolve case
          if(p._1.startsWith("where(")){
            val insideExpr = p._1.drop(6).dropRight(1) //remove where( and the last )
            //If it is resolve ignore it, as we have this information in reference targets
            if(insideExpr.startsWith("resolve()"))
              "" -> Nil //No path no restriction
            else if(insideExpr.startsWith("exists()")) {
              throw new Exception(s"Cannot process SearchParameter path expression '$expr'!")
            } else if(insideExpr.startsWith("extension(")){
              val extensionUrl = insideExpr.drop(10).dropRight(1).replace("'","").replace("&#39;", "")
              //we know that extensions are array so we directly add [i]
              "extension[i]" -> Seq("url" -> extensionUrl)
            } else { //restriction case
              val numOfStepsToFindRestrictionElem = paths.length - (p._2 + 1)
              val prefix =
                if(numOfStepsToFindRestrictionElem > 0)
                  (0 until numOfStepsToFindRestrictionElem).map(_ => "@.").mkString("") //prepare a prefix to indicate the position of restriction from right e.g @.type means restriction is on path element one before from the end
                else ""
              val restrictions = insideExpr
                .split(" and ")
                .map(r => {
                  val restrictionArray = insideExpr.split('=')
                  val subProperty = restrictionArray.head.trim()
                  val expectedValue =
                    restrictionArray.last.trim()
                      .replace("'", "").replace("&#39;", "") //remove the ' around the expected value
                  (prefix+ subProperty) -> expectedValue
                }).toSeq

              "" -> restrictions //No path only restriction
            }
          } else if(p._1.startsWith("as(")) {
            //For path we return only the capitalize type which will merge with the one before path
            p._1
              .drop(3).dropRight(1) //drop the as( and last )
              .capitalize -> Nil //Capitalize the first letter
          } else
            ("."+p._1) -> Nil
        )

    val path =
      parsedPaths
        .map(_._1)
        .mkString("") //Merge the paths (we already put . before each path)
        .drop(1) //drop first extra .

    val restrictions = parsedPaths.flatMap(_._2)

    if(!path.forall(c => c.isLetterOrDigit || c=='.' || c=='[' || c==']'))
      throw new Exception(s"Cannot process SearchParameter path expression '$expr'!")

    path -> restrictions
  }

  private def constructRestrictionPrefix(pathLength:Int, i:Int):String = {
    val numOfStepsToFindRestrictionElem = pathLength - (i + 1)
      if(numOfStepsToFindRestrictionElem > 0)
        (0 until numOfStepsToFindRestrictionElem).map(_ => "@.").mkString("") //prepare a prefix to indicate the position of restriction from right e.g @.type means restriction is on path element one before from the end
      else ""
  }

  /**
   * Parses the XPath expression given in the search parameter definitions
   * @param resourceType    Resource type to filter the expressions
   * @param xpath           Xpath expression given in SearchParameter.xpath
   * @return
   */
  def parseXpath(resourceType:String, xpath:String):Seq[(String, Seq[(String, String)])] = {
    parseAll(xpathMultiplePath, xpath.replace("&#39;","'")) match {
      case Success(result, _) =>
        result
          .filter(r => r.head._1 == resourceType) //Only xpaths related to this resource type
          .map(r => {
            val paths =  r.drop(1) //drop the resource type part
            val finalPath = paths.map(_._1).mkString(".")

            val finalRestrictions = paths.zipWithIndex.flatMap {
              case (p,i) =>
                if(p._2.nonEmpty) {
                  val prefix = constructRestrictionPrefix(paths.length, i)
                  p._2
                    .map(r => (prefix + r._1) -> r._2)
                } else
                  Nil
            }
            finalPath -> finalRestrictions
          })
      //Problem in parsing
      case failure : NoSuccess => Nil
    }
  }
/*
  /**
   * Parse the XPath and returns a JSON query string; elem1-name.elem2.name
   * e.g. xpath: "f:Immunization/f:reaction/f:detail" => "reaction.detail"
   * @param xPath defined in the SearchParameter definition for the search parameter
   * @return
   */
  def parseXPathBasic(xPath:String):(String, Seq[(String, String)]) = {
    /*//var normalizedXPath = xPath.replace(" ", "") //Replace all white spaces
    var restrictions:Option[(String, String)] = None
    //e.g. f:OrganizationAffiliation/f:telecom[system/@value=&#39;email&#39;] or f:PlanDefinition/f:relatedArtifact[f:type/@value=&#39;depends-on&#39;]/f:resource
    //TODO currently there is only one restriction defined in some paths so we only handle that case
    if(normalizedXPath.contains('[')) {
      val parts = normalizedXPath.split('[')
      val otherParts = parts.tail.head.split(']')
      //Extract the path without the [ ]
      normalizedXPath = parts.head + otherParts.tail.headOption.getOrElse("")
      //Split to get restriction path name e.g. system/@value=&#39;email&#39; -> system ,
      val rpnameParts = otherParts.head.split('/')
      var rprefix = ""
      //If the restriction is one before the actual path
      if(otherParts.tail.nonEmpty)
        rprefix = "@."
      restrictions = Some(
        rprefix + rpnameParts.head.replaceFirst(".*:", "") ->
          rpnameParts.last.replace("@value=","").replace("'", "")
      )
    }*/

    val paths =
      xPath
        .split('/')
        .tail //Remove the first Element as it is the name of Resource
        .map(_.replaceFirst(".*:", "")) //Remove Xpath prefixes e.g. "f:"

      val parsedPaths =
        paths
        .zipWithIndex
        .map {
          case (p, i) =>
            //if this is an extension
            if (p.startsWith("extension[")) {
              val extensionUrl =
                p.drop(10).dropRight(1) //drop extension[ and last ]
                  .drop(5) //drop  @url=
                  .replace("'", "").replace("&#39;", "") //drop '
              "extension[i]" -> Seq("url" -> extensionUrl)
            }
            //if this is a restriction e.g. f:telecom[system/@value=&#39;email&#39;] or f:relatedArtifact[f:type/@value='composed-of']
            else if (p.contains('[')) {
              val pind = p.indexOf('[')
              val pt = p.substring(0, pind).replaceFirst(".*:", "") //find out the path
              val insideParenthesis = p.substring(pind + 1).dropRight(1)
              val numOfStepsToFindRestrictionElem = paths.length - (i + 1)
              val prefix =
                if(numOfStepsToFindRestrictionElem > 0)
                  (0 until numOfStepsToFindRestrictionElem).map(_ => "@.").mkString("") //prepare a prefix to indicate the position of restriction from right e.g @.type means restriction is on path element one before from the end
                else ""

              val restrictions =
                insideParenthesis
                .split(" and ")
                .map(_.trim)
                .map(r => {
                   val rparts = r.split('=')
                   val property = rparts.head.dropRight(7).replaceFirst(".*:", "") //drop /@value and prefix if exist
                   val exptectedValue = rparts.last.replace("'", "").replace("&#39;", "")
                  (prefix + property) -> exptectedValue
                }).toSeq

              pt -> restrictions
            } else {
              p -> Nil
            }
        }

      val jsonPath = parsedPaths.map(_._1).mkString(".") //Convert '/' in XPATH to . for JSON path

      jsonPath -> parsedPaths.flatMap(_._2)
  }

  /**
   * Parses xPath and creates path list for multi type parameters(i.e. parameters
   * that are referencing multiple paths
   * e.g. effectivetime refers effectiveTimeDateTime and effectiveTimePeriod
   * "f:MedicationAdministration/f:effectiveTimeDateTime | f:MedicationAdministration/f:effectiveTimePeriod"
   * @param resourceType Resource type that we are interested
   * @param xPath Defined in the SearchParameter definition for the search parameter
   * @return
   */
  def parseXPathMultiple(resourceType:String, xPath:String):Seq[(String, Seq[(String, String)])] = {
    xPath
      .replace(" ", "")
      .split('|')
      .filter(_.contains(resourceType+"/"))
      .map(parseXPathBasic)
  }
*/

}



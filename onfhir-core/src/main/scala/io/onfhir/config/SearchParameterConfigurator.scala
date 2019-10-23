package io.onfhir.config

import org.slf4j.{Logger, LoggerFactory}
import io.onfhir.api._
/**
  * Created by ozan before
  * Update by tuncay on 11/16/2016.
  * Supplementary class to help parsing of XPaths defined in FHIR SearchParameter definitions and converting them to
  * some type of JSON paths by creating search parameter configuration objects (SearchParameterConf)
  * @param rtype Resource type that parameters are configured
  * @param baseDefinitionsMap Base FHIR standard definitions for Resourced and Data Types
  * @param profilesMap Resource/DataType profiles provided in the configuration
  * @param allSearchParameters Name of all search parameters supported by the resource type
  */
class SearchParameterConfigurator(
                                   rtype:String,
                                   baseDefinitionsMap:Map[String, Map[String, (String,Boolean)]],
                                   profilesMap:Map[String, Map[String, String]],
                                   allSearchParameters:Set[String]
                                 ) {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Create the search parameter configurations to handle a defined SearchParameter
    * Note: Composite types may need multiple configurations for a single parameter definition
    * @param pname Name of the FHIR search parameter
    * @param ptype Type of the FHIR search parameter
    * @param xpath XPath defined for the FHIR search parameter
    * @param targets If type is reference, the resource names this reference refers (See FHIR SearchParameter definition)
    * @param modifiers Supported modifiers for this parameter
    * @return
    */
  def createSearchParameterConf(pname:String,
                                ptype:String,
                                xpath:String,
                                targets:Seq[String],
                                modifiers:Set[String],
                               ): Option[SearchParameterConf] = {
    ptype match {
      case null => logger.warn(s"Parameter $pname has no type defined in its SearchParameter definition!!! Skipping this parameter, it will not be supported.")
        None
      //We never use special parameter definitions, just create it to set we are supporting it
      case FHIR_PARAMETER_CATEGORIES.SPECIAL =>
        Some(SearchParameterConf(pname, ptype, "", targets, modifiers, Nil))
      //If parameter is composite
      case FHIR_PARAMETER_TYPES.COMPOSITE =>
          handleCompositeParameter(pname, ptype, xpath, targets, modifiers)
      case _ =>
        xpath match {
          case null => //Skip if it has no xpath definition
            logger.warn(s"Parameter $pname has no xpath in its SearchParameter definition!!! Skipping this parameter, it will not be supported.")
            None
          //If parameter has a path including extension
          case pWithExtension if pWithExtension.contains(FHIR_COMMON_FIELDS.EXTENSION) =>
            Some(handleParametersOnExtensions(pname, ptype, pWithExtension, targets, modifiers))
          case pWithOr if pWithOr.contains("|") =>
            // Parameter has xPath for multiple path parameters (e.g. effective -> effectiveDateTime | effectivePeriod)
            val pathsAndRestrictions = parseXPathMultiple(rtype, pWithOr)
            val (finalPaths, finalTargetTypes, restrictions) = transformPathsAndExtractTargetTypes(ptype, pathsAndRestrictions)
            //val (finalPaths, finalTargetTypes) = filterSearchParamConf(pname, ptype, paths, targetTypes)
            Some(SearchParameterConf(pname, ptype, finalPaths, targets, modifiers, finalTargetTypes, restrictions = restrictions))
          case pAny =>
            // Parameter has basic xpath, the parameter corresponds to single path
            val pathsAndRestrictions = Seq(parseXPathBasic(pAny))
            val (finalPaths, finalTargetTypes, restrictions) = transformPathsAndExtractTargetTypes(ptype,pathsAndRestrictions)
            //val (finalPaths, finalTargetTypes) = filterSearchParamConf(pname, ptype, paths, targetTypes)
            Some(SearchParameterConf(pname, ptype, finalPaths, targets, modifiers, finalTargetTypes, restrictions = restrictions))
        }
    }
  }

  /**
    * Supplementary method to extract target types of elements for each path
    * @param ptype Parameter type
    * @param paths Paths to evaluate
    * @param baseDefinitionPathTypeMap Base FHIR standart definition of elements path -> targetType, isArray
    * @param profilePathTypeMap Extra profiles given to onFhir.io path -> targetType
    * @return
    */
  private def transformPathsAndExtractTargetTypes(ptype:String, pathsAndRestrictions:Seq[(String, Option[(String, String)])]):(Seq[String], Seq[String], Seq[Option[(String, String)]]) = {
    //Filter the paths that exist in the given profile
    val filteredPaths:Seq[(String, String, Option[(String, String)])] = pathsAndRestrictions.flatMap { case (path, restriction) =>
      if(path == "") //If this is a composition root path with the resource as root
        Some((path, "Resource", None))
      else {
        //Find the target type for the path
        var targetTypeOption = findTargetTypeOfPath(path)

        //Filter out the paths with type that is not compatible to search parameter type
        if(ptype != FHIR_PARAMETER_TYPES.COMPOSITE)
          targetTypeOption = targetTypeOption.filter(tt => FHIR_PARAMETER_TYPE_TARGETS(ptype).contains(tt))

        //If there is no target type, it means this path is not valid for this profile (cardinality set to 0, or removed from choices like value[x])
        targetTypeOption.map(targetType => (path, targetType, restriction))
      }
    }


    //Transform the paths to our path format (just add [i] to the path parts that are arrays)
    val finalPaths =
      filteredPaths
        .map {
          case("", _, _) => ""
          case(path, _, _) =>
            //Split the path
            val pathParts = path.split('.')
            //Find cardinality of each prefixes of path
            val arrayIndicators =
              pathParts.indices
                .reverse
                .map(i => findPathCardinality(pathParts.dropRight(i).mkString(".")))
            //If it is an array, add [i] indicator
            pathParts.zip(arrayIndicators).map {
              case (pp, false) => pp
              case (pp, true) => pp+"[i]"
            }.mkString(".") //Merge path again
        }
    //Return paths and types seperately
    (finalPaths, filteredPaths.map(_._2), filteredPaths.map(_._3))
  }

  /**
    * Find the Target FHIR type of a search path within the element definitions
    * @param path Search path
    * @return
    */
  private def findTargetTypeOfPath(path:String):Option[String] = {
    //If this is empty, it means no profile restriction on resource type, so check from base standard
    var targetTypeOption = if (profilesMap.get(rtype).isEmpty) baseDefinitionsMap(rtype).get(path).map(_._1) else profilesMap(rtype).get(path)

    //If still it is empty, path may be refering to an inner element of a FHIR Complex DataType so does not exist in Resource
    if(targetTypeOption.isEmpty){
      val pathParts = path.split('.')
      //Split the path; last part is the inner path the DataType, remaining is the path to the data type
      val pathAfterDataType = pathParts.last
      val pathToDataType = pathParts.dropRight(1).mkString(".")
      //Find the complex type
      val complexDataType = if (profilesMap.get(rtype).isEmpty) baseDefinitionsMap(rtype).get(pathToDataType).map(_._1) else profilesMap(rtype).get(pathToDataType)
      //If it is defined, find the path in the Map of complex type definition
      if(complexDataType.isDefined)
        targetTypeOption = if (profilesMap.get(complexDataType.get).isEmpty) baseDefinitionsMap.getOrElse(complexDataType.get, Map.empty).get(pathAfterDataType).map(_._1) else profilesMap(complexDataType.get).get(pathAfterDataType)
    }

    if(targetTypeOption.isEmpty)
      logger.warn(s"Cannot find element definition for search path $path for resource type $rtype, skipping it!")

    targetTypeOption
  }

  /**
    * Check if path targets an array or not
    * @param path
    * @return
    */
  private def findPathCardinality(path:String):Boolean = {
    baseDefinitionsMap(rtype).get(path) match {
      case Some(bt) => bt._2
      case None =>
        val pathParts = path.split('.')
        //Split the path; last part is the inner path the DataType, remaining is the path to the data type
        val pathAfterDataType = pathParts.last
        val pathToDataType = pathParts.dropRight(1).mkString(".")
        val complexDataType = baseDefinitionsMap(rtype)(pathToDataType)._1
        baseDefinitionsMap(complexDataType)(pathAfterDataType)._2
    }
  }

  /**
    * Parse the XPath and returns a JSON query string; elem1-name.elem2.name
    * e.g. xpath: "f:Immunization/f:reaction/f:detail" => "reaction.detail"
    * @param xPath defined in the SearchParameter definition for the search parameter
    * @return
    */
  private def parseXPathBasic(xPath:String):(String, Option[(String, String)]) = {
    var normalizedXPath = xPath.replace(" ", "") //Replace all white spaces
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
    }

    val jsonPath =
      normalizedXPath
      .split('/')
      .tail //Remove the first Element as it is the name of Resource
      .map(_.replaceFirst(".*:", "")) //Remove Xpath prefixes e.g. "f:"
      .mkString(".") //Convert '/' in XPATH to . for JSON path

    jsonPath -> restrictions
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
  private def parseXPathMultiple(resourceType:String, xPath:String):Seq[(String, Option[(String, String)])] = {
      xPath
        .replace(" ", "")
        .split('|')
        .filter(_.contains(resourceType+"/"))
        .map(parseXPathBasic)
  }


  /**
    * Returns a list of search parameter configuration by parsing the Xpath and converting it to JSON path
    * @param pname Name of the FHIR search parameter
    * @param ptype Type of the FHIR search parameter (e.g. composite, token, etc)
    * @param rootPath Common JSON path for composition e.g. MolecularSequence.variant
    * @param targets Definition urls for combined parameters e.g. http://hl7.org/fhir/SearchParameter/MolecularSequence-referenceseqid
    * @param modifiers Supported modifiers for this parameter
    * @param otherParameters Names of other parameters that is supported for the same resource
    * @return
    */
  private def handleCompositeParameter(pname:String,  ptype:String, rootPath:String, targets:Seq[String], modifiers:Set[String]):Option[SearchParameterConf] = {
    //Extract combined parameter names
    val combinedParamNames =
      targets
         .map(_.split('/').last) //Take the last part
         .map(d => d.substring(d.indexOf('-')+1)) //Split it from the first - to get the parameter name

    //Check if all parameters exist
    if(!combinedParamNames.toSet.subsetOf(allSearchParameters)) {
      //throw new InitializationException(s"Some of the parameters ${combinedParamNames.diff(otherParameters)} referred in composite parameter $pname does not exist in defined parameters for the resource type $rtype!")
      logger.warn(s"Some of the parameters ${combinedParamNames.toSet.diff(allSearchParameters)} referred in composite parameter $pname does not exist in defined parameters for the resource type $rtype!")
      None
    } else {
      //
      val compositionPaths =
        rootPath
          .split('|') //Split if there are alternative paths
          .filter(_.startsWith(rtype)) //Filter the related paths
          .map(_.trim().replace(rtype,"")) //Remove the resource type part from the path
          .map(p => if(p!="") p.drop(1) else  p) //Remove the dot after resource type if exist
          .map(p => p -> None)

      val (finalPaths, finalTargetTypes, _) = transformPathsAndExtractTargetTypes(ptype, compositionPaths)
      Some(SearchParameterConf(pname, ptype, finalPaths, combinedParamNames, modifiers, finalTargetTypes))
    }
  }

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
    * @param pname Name of the FHIR search parameter
    * @param ptype Type of the FHIR search parameter (e.g. composite, token, etc)
    * @param xpath Xpath for the search parameter
    * @param targets If type is reference, the resource names this reference refers
    * @param modifiers Supported modifiers for this parameter
    * @return
    */
  private def handleParametersOnExtensions(pname:String, ptype:String, xpath:String, targets:Seq[String], modifiers:Set[String]): SearchParameterConf = {
    //val extensionXPathReg = """extension\\[@url='(\w+)'\\]""".r
    val extensionXPathReg = """extension\[@url='([^']*)'\]""".r

    //Divide the path if it is a multiple xpaths divided by  or ('|')
    //e.g. ProcedureRequest/extension[@url='...']/valueDateTime | ProcedureRequest/extension[@url='...']/valuePeriod
    val paramereterJsonPaths:Seq[Any] = xpath.replace(" ", "").split("\\|").toSeq.map(eachPath => {
      //Split each path to its components
      //e.g. /Goal/extension[@url='http://hl7.org/fhir/StructureDefinition/goal-target']/extension[@url='measure']/valueCodeableConcept

      //Extract the extension Urls
      val extensionUrls:Seq[(String, Int)] = extensionXPathReg.findAllMatchIn(eachPath).toList.map(rm => rm.group(1)).zipWithIndex
      // The last component is the value element (e.g. valueCondableConcept)
      val valueElement = eachPath.split("/").last.replaceFirst(".*:", "")
      // Others are extension components (extension[@url='http://hl7.org/fhir/StructureDefinition/goal-target'], extension[@url='measure'])
      //val extensionComponents = elementList.dropRight(1)

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
            .split(".").last //Just get the value* part
            .replace("value", "") //Replace the value to get the type name
        )
        .map(ttype => if(FHIR_COMPLEX_TYPES.contains(ttype)) ttype else ttype.toLowerCase) // if it is a simple type, convert to lowercase e.g. valueString -> string

    SearchParameterConf(pname, ptype, paramereterJsonPaths, targets, modifiers, targetTypes, true)
  }
}


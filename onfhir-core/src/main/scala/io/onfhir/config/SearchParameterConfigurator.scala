package io.onfhir.config

import org.slf4j.{Logger, LoggerFactory}
import io.onfhir.api._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintKeys, ProfileRestrictions}
import io.onfhir.validation.{ArrayRestriction, TypeRestriction}
/**
  * Created by ozan before
  * Update by tuncay on 11/16/2016
  * Supplementary class to help parsing of XPaths defined in FHIR SearchParameter definitions and converting them to
  * some type of JSON paths by creating search parameter configuration objects (SearchParameterConf)
  * @param rtype                Resource type that parameters are configured
  * @param rtypeBaseProfile     Base profile specified for the resource type in CapabilityStatement
  * @param fhirConfig           FhirConfig with configured profiles
  * @param allSearchParameters  Name of all search parameters supported by the resource type
  */
class SearchParameterConfigurator(
                                   rtype:String,
                                   rtypeBaseProfile:Option[String],
                                   fhirConfig:FhirConfig,
                                   allSearchParameters:Set[String]
                                 ) {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)
  // Find profile definition for base Resource type
  val rtypeProfile = fhirConfig.getBaseProfile(rtype)
  //Find profile chain for base profile specific for resource type
  val profileChain = rtypeBaseProfile.map(url => fhirConfig.findProfileChain(url)).getOrElse(Seq(rtypeProfile))


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
        searchParameterDef.xpath match {
          case Some(_) => handleCompositeParameter(searchParameterDef)
          case _ => None
        }
      //Otherwise check Xpath
      case _ =>
        searchParameterDef.xpath match {
          //Xpath is not given
          case None => None

          //If parameter has a path including extension
          case Some(pWithExtension) if pWithExtension.contains(FHIR_COMMON_FIELDS.EXTENSION) =>
            Some(handleParametersOnExtensions(searchParameterDef))

          // Parameter has xPath for multiple path parameters (e.g. effective -> effectiveDateTime | effectivePeriod)
          case Some(pWithOr) if pWithOr.contains("|") =>
            val pathsAndRestrictions = parseXPathMultiple(rtype, pWithOr)
            val (finalPaths, finalTargetTypes, restrictions) = transformPathsAndExtractTargetTypes(searchParameterDef.ptype, pathsAndRestrictions)
            Some(constructConfFromDef(searchParameterDef, finalPaths, finalTargetTypes, restrictions))

          // Parameter has basic xpath, the parameter corresponds to single path
          case Some(pAny) =>
            val pathsAndRestrictions = Seq(parseXPathBasic(pAny))
            val (finalPaths, finalTargetTypes, restrictions) = transformPathsAndExtractTargetTypes(searchParameterDef.ptype, pathsAndRestrictions)
            Some(constructConfFromDef(searchParameterDef, finalPaths, finalTargetTypes, restrictions))
        }
    }
  }

  private def constructConfFromDef(searchParameterDef:FHIRSearchParameter, finalPaths:Seq[Any], finalTargetTypes:Seq[String], restrictions:Seq[Option[(String, String)]], onExtension:Boolean = false):SearchParameterConf = {
    SearchParameterConf(
      pname = searchParameterDef.name,
      ptype = searchParameterDef.ptype,
      paths = finalPaths,
      targets = searchParameterDef.target.toSeq,
      modifiers = searchParameterDef.modifiers,
      targetTypes = finalTargetTypes,
      restrictions = restrictions,
      multipleOr = searchParameterDef.multipleOr.getOrElse(true),
      multipleAnd = searchParameterDef.multipleAnd.getOrElse(true),
      comparators = searchParameterDef.comparators,
      onExtension = onExtension
    )
  }

  /**
    * Supplementary method to extract target types of elements for each path
    * @param ptype FHIR search parameter type
    * @param pathsAndRestrictions Paths to evaluate (and the restrictions on path)
    * @return
    */
  private def transformPathsAndExtractTargetTypes(ptype:String, pathsAndRestrictions:Seq[(String, Option[(String, String)])]):(Seq[String], Seq[String], Seq[Option[(String, String)]]) = {
    //Filter the paths that exist in the given profile
    val filteredPaths:Seq[(String, String, Option[(String, String)])] = pathsAndRestrictions.flatMap { case (path, restriction) =>
      if(path == "") //If this is a composition root path with the resource as root
        Some((path, "Resource", None))
      else {
        //Find the target type for the path
        var targetTypeOption = findTargetTypeOfPath(path, profileChain).map(_._1)

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
                .map(i => findPathCardinality(pathParts.dropRight(i).mkString("."), rtypeProfile))
            //If it is an array, add [i] indicator
            pathParts.zip(arrayIndicators).map {
              case (pp, false) => pp
              case (pp, true) => pp+"[i]"
            }.mkString(".") //Merge path again
        }
    //Return paths and types separately
    (finalPaths, filteredPaths.map(_._2), filteredPaths.map(_._3))
  }

  /**
   * Check if a search path match with element definition path
   * @param searchPath
   * @param elementPath
   * @return
   */
  private def checkPathsMatch(searchPath:String, elementPath:String):Boolean = {
    elementPath == searchPath || //If we have a path that is equal to search path e.g. Element path:  Observation.code --> Search Path: Observation.code
      (elementPath.endsWith("[x]") && searchPath.startsWith(elementPath.replace("[x]", "")) && //Or the path has multiple values and path starts with given element path e.g. Element path: Observation.value[x] -> Search path Observation.valueQuantity
        (FHIR_COMPLEX_TYPES.contains(searchPath.replace(elementPath.replace("[x]", ""), "")) || //If complex type, type name starts with a capital e.g. CodeableConcept
          FHIR_PRIMITIVE_TYPES.contains(FHIRUtil.decapitilize(searchPath.replace(elementPath.replace("[x]", ""), "")))) //If simple type, type name is lowercase
        )
  }

  /**
   * Find a target FHIR type of path in a profile chain if direct definition exists
   * @param path
   * @param profileChain
   * @return
   */
  private def findTargetTypeOfPathInProfileChain(path:String, profileChain:Seq[ProfileRestrictions]):Option[(String, Seq[String])] = {
    var targetTypeAndProfiles:Option[(String, Seq[String])] = None
    import scala.util.control.Breaks._
    breakable
    {
      //Search in the profile chain starting from the lower
      for(i <- profileChain.indices)
      {
        profileChain.apply(i).elementRestrictions
          .find(p => checkPathsMatch(path, p._1)) match {
          case Some(er) =>
            //Possible data type e.g. valueCodeableConcept -> CodeableConcept
            val possibleDataType =
              if(path == er._1)
                None
              else {
                val temp = path.replace(er._1.replace("[x]", ""), "")
                if(FHIR_COMPLEX_TYPES.contains(temp)) Some(temp) else Some(FHIRUtil.decapitilize(temp))
              }

            er._2.restrictions.get(ConstraintKeys.DATATYPE) match {
              case Some(dtr) =>
                possibleDataType match {
                  case None =>
                    targetTypeAndProfiles = dtr.asInstanceOf[TypeRestriction].dataTypesAndProfiles.headOption
                    break()
                  case Some(pdt) =>  dtr.asInstanceOf[TypeRestriction].dataTypesAndProfiles.find(_._1 == pdt) match {
                    case Some(dtp) =>
                      targetTypeAndProfiles = Some(dtp)
                      break()
                    case None =>
                  }
                }
              case None =>
            }
          case None =>
        }
      }
    }
    targetTypeAndProfiles
  }

  /**
   * Find the Target FHIR type of a search path within the element definitions
   * @param path Search path
   * @return
   */
  private def findTargetTypeOfPath(path:String, profiles:Seq[ProfileRestrictions]):Option[(String, Seq[String])] = {
    var targetTypeAndProfiles:Option[(String, Seq[String])] = findTargetTypeOfPathInProfileChain(path, profiles)

    //If still it is empty, path may be referring to an inner element of a FHIR Complex DataType so does not exist in Resource e.g. Patient.name.given (name refers to HumanName)
    if(targetTypeAndProfiles.isEmpty && path.contains('.')){
      targetTypeAndProfiles = findTargetTypeInSubPaths(path.split('.'), profiles)
    }

    if(targetTypeAndProfiles.isEmpty)
      logger.warn(s"Cannot find element definition for search path $path for resource type $rtype, skipping it!")

    targetTypeAndProfiles
  }

  /**
   * Find target type by trying different division of path between until DataType and within DataType
   * @param pathParts
   * @param i
   * @return
   */
  private def findTargetTypeInSubPaths(pathParts:Seq[String],profiles:Seq[ProfileRestrictions],  i:Int = 1):Option[(String, Seq[String])] = {
    var targetTypeAndProfiles:Option[(String, Seq[String])] = None
    //Split the path accordingly
    val pathToDataType = pathParts.slice(0, pathParts.length-i).mkString(".")
    val pathAfterDataType = pathParts.slice(pathParts.length-i, pathParts.length).mkString(".")
    //Try to find in this setup
    findTargetTypeOfPathInProfileChain(pathToDataType, profiles) match {
      //If we found it and it is base a DataType, then load the base DataType and find within it
      case Some(onlyDataType) if onlyDataType._2.isEmpty =>
        targetTypeAndProfiles = findTargetTypeOfPath(pathAfterDataType, Seq(fhirConfig.getBaseProfile(onlyDataType._1)))
      //If there are multiple profiles
      case Some(multipleProfiles) =>
        import scala.util.control.Breaks._
        breakable {
          for(i <- multipleProfiles._2.indices){
            fhirConfig.findProfileChain(multipleProfiles._2.apply(i)) match {
              case pc =>
                targetTypeAndProfiles = findTargetTypeOfPathInProfileChain(pathAfterDataType,pc)
                break()
              case Nil =>
            }
          }
        }
      case None =>
        if(i < pathParts.length - 1)
          targetTypeAndProfiles =  findTargetTypeInSubPaths(pathParts, profiles, i+1)
    }

    targetTypeAndProfiles
  }


  /**
    * Check if path targets an array or not
    * @param path
    * @return
    */
  private def findPathCardinality(path:String, profile:ProfileRestrictions):Boolean = {
    //Search in the base profile defined in the standard for resource type
    profile.elementRestrictions
      .find(e => checkPathsMatch(path, e._1)) match {
        case Some(er) => er._2.restrictions.get(ConstraintKeys.ARRAY).exists(_.asInstanceOf[ArrayRestriction].isArray)
        //If such a path not exist
        case None =>
          //Split the path; last part is the inner path the DataType, remaining is the path to the data type
          val pathParts = path.split('.')
          val pathAfterDataType = pathParts.last
          val pathToDataType = pathParts.dropRight(1).mkString(".")
          //Find the data type referred
          findTargetTypeOfPathInProfileChain(pathToDataType, Seq(profile)) match {
            case Some(dt) =>
              //find the base standard profile of data type
              val dtProfile = fhirConfig.getBaseProfile(dt._1)
              //find the remaining path cardinality there
              findPathCardinality(pathAfterDataType, dtProfile)
            case None =>
              logger.warn(s"Problem while identifying cardinality of path $path in profile $profile!")
              false
          }
    }
  }

  private def findPathCardinalityInSubpaths(): Unit ={

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
   * @param searchParameterDef Search parameter definition
   * @return
   */
  private def handleCompositeParameter(searchParameterDef:FHIRSearchParameter):Option[SearchParameterConf] = {
    //Extract combined parameter names
    val combinedParamNames =
      searchParameterDef.components
        .map(_.split('/').last) //Take the last part
        .map(d => d.substring(d.indexOf('-')+1)) //Split it from the first - to get the parameter name

    //Check if all parameters exist
    if(!combinedParamNames.subsetOf(allSearchParameters)) {
      //throw new InitializationException(s"Some of the parameters ${combinedParamNames.diff(otherParameters)} referred in composite parameter $pname does not exist in defined parameters for the resource type $rtype!")
      logger.warn(s"Some of the parameters ${combinedParamNames.toSet.diff(allSearchParameters)} referred in composite parameter ${searchParameterDef.name} does not exist in defined parameters for the resource type $rtype!")
      None
    } else {
      //
      val compositionPaths =
        searchParameterDef.xpath.get
          .split('|') //Split if there are alternative paths
          .filter(_.startsWith(rtype)) //Filter the related paths
          .map(_.trim().replace(rtype,"")) //Remove the resource type part from the path
          .map(p => if(p!="") p.drop(1) else  p) //Remove the dot after resource type if exist
          .map(p => p -> None)

      val (finalPaths, finalTargetTypes, _) = transformPathsAndExtractTargetTypes(searchParameterDef.ptype, compositionPaths)
      Some(constructConfFromDef(searchParameterDef, finalPaths, finalTargetTypes, Nil))
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
            .split(".").last //Just get the value* part
            .replace("value", "") //Replace the value to get the type name
        )
        .map(ttype => if(FHIR_COMPLEX_TYPES.contains(ttype)) ttype else ttype.toLowerCase) // if it is a simple type, convert to lowercase e.g. valueString -> string

    constructConfFromDef(searchParameterDef, paramereterJsonPaths, targetTypes, Nil, true)
  }
}


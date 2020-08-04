package io.onfhir.api.util
import io.onfhir.api.FHIR_DATA_TYPES
import io.onfhir.api.validation.{ConstraintKeys, ElementRestrictions, ProfileRestrictions}
import io.onfhir.config.FhirConfig
import io.onfhir.validation.{ArrayRestriction, ReferenceRestrictions, TypeRestriction}
import org.slf4j.Logger

abstract class BaseFhirProfileHandler(fhirConfig: FhirConfig) {
  protected val logger:Logger
  /**
   * Check if a search path match with element definition path
   * @param searchPath    Path given in the search parameter definition (via xpath or expression)
   * @param elementPath   Actual element path given in StructureDefinition of the resource type (profile)
   * @return
   */
  private def checkPathsMatch(searchPath:String, elementPath:String):Boolean = {
    val normalizedElementPath = elementPath.replace("[x]", "")

    elementPath == searchPath || elementPath == (searchPath + "[x]") || //If we have a path that is equal to search path e.g. Element path:  Observation.code --> Search Path: Observation.code
      (elementPath.endsWith("[x]") && searchPath.startsWith(normalizedElementPath) && //Or the path has multiple values and path starts with given element path e.g. Element path: Observation.value[x] -> Search path Observation.valueQuantity
        (fhirConfig.FHIR_COMPLEX_TYPES.contains(searchPath.replace(normalizedElementPath, "")) || //If complex type, type name starts with a capital e.g. CodeableConcept
          fhirConfig.FHIR_PRIMITIVE_TYPES.contains(FHIRUtil.decapitilize(searchPath.replace(normalizedElementPath, "")))) //If simple type, type name is lowercase
        )
  }

  /**
   * Find the element restriction for the given path within the given profile
   * @param path
   * @param elementRestrictions
   * @return
   */
  private def findElementRestrictionForPath(path:String, elementRestrictions: Seq[(String,ElementRestrictions)]):Option[(String, ElementRestrictions)] = {
    elementRestrictions
      .find(p => p._2.contentReference match {
        //If it is not a content reference, check if path matches
        case None => checkPathsMatch(path, p._1)
        //If it is a content reference than
        case Some(cr) if path.startsWith(p._1) =>
          //if common prefix is not empty
          cr.zip(path).takeWhile(c => c._1 == c._2).map(_._1).mkString != ""
        //Otherwise false
        case _ => false
      }) match {
      //Searched path is on a referenced content
      case Some(er) if path != er._1 &&  path.startsWith(er._1) =>
        val rootPrefix =  er._2.contentReference.get.zip(path).takeWhile(c => c._1 == c._2).map(_._1).mkString
        val pathOnReferencedContent = er._2.contentReference.get + "." + path.replace(rootPrefix, "").split('.').tail.mkString(".")
        findElementRestrictionForPath(pathOnReferencedContent, elementRestrictions)
      case oth => oth
    }
  }

  /**
   * Find target FHIR types of path in a profile chain if direct definition exists
   * @param path            Path defined in SearchParameter definition
   * @param profileChain    Profile chain for the resource type as configured for this onFhir instance
   * @return (Path itself, Target data type for FHIR path, profiles for this path, if reference target resource profiles otherwise empty)
   */
  private def findTargetTypeOfPathInProfileChain(path:String, profileChain:Seq[ProfileRestrictions]):Seq[(String, String, Seq[String], Set[String])] = {
    var targetTypeAndProfiles:Seq[(String, String, Seq[String])] = Nil
    var targetResourceProfilesForReference:Set[String] = Set.empty[String]
    import scala.util.control.Breaks._
    breakable
    {
      //Search in the profile chain starting from the lower
      for(i <- profileChain.indices)
      {
        findElementRestrictionForPath(path, profileChain.apply(i).elementRestrictions) match {
          case Some(er) =>

            //Try to find data type restriction (type definition)
            er._2.restrictions.get(ConstraintKeys.DATATYPE) match {
              case Some(dtr) =>
                //If this element is not a multi type element or search path is equal to the element path
                if(!er._1.contains("[x]") || path == er._1) {
                  targetTypeAndProfiles = dtr.asInstanceOf[TypeRestriction].dataTypesAndProfiles.map(t => (path, t._1, t._2))
                  if(targetTypeAndProfiles.exists(_._2 == FHIR_DATA_TYPES.REFERENCE))
                    targetResourceProfilesForReference = er._2.restrictions.get(ConstraintKeys.REFERENCE_TARGET).map(_.asInstanceOf[ReferenceRestrictions]).map(_.targetProfiles).getOrElse(Nil).toSet
                  break()
                } else {
                  //Otherwise it is a multi type element
                  val temp = path.replace(er._1.replace("[x]", ""), "")
                  // If path indicates all possible data types e.g. Observation.effective
                  if(temp == ""){
                    //Then return all possible target types while updating the path e.g. effectiveDateTime -> DateTime, effectiveInstant -> Instant, etc
                    targetTypeAndProfiles = dtr.asInstanceOf[TypeRestriction].dataTypesAndProfiles.map(t => (path + t._1.capitalize, t._1, t._2))
                    break()
                  } else {
                    //Otherwise find out possible date type
                    val possibleDataType = if(fhirConfig.FHIR_COMPLEX_TYPES.contains(temp)) temp else FHIRUtil.decapitilize(temp)
                    dtr.asInstanceOf[TypeRestriction]
                      .dataTypesAndProfiles.find(_._1 == possibleDataType) match {
                      case Some(dtp) =>
                        targetTypeAndProfiles = Seq((path, dtp._1, dtp._2))
                        targetResourceProfilesForReference = er._2.restrictions.get(ConstraintKeys.REFERENCE_TARGET).map(_.asInstanceOf[ReferenceRestrictions]).map(_.targetProfiles).getOrElse(Nil).toSet
                        break()
                      //If this data type not exist in the list, just return nil list
                      case None =>
                        break()
                    }
                  }
                }

              //If no type definition skip it, try parent profile in next iteration
              case None =>
            }
          //If no restriction in this profile, go try parent profile in next iteration
          case None =>
        }
      }
    }
    targetTypeAndProfiles.map(t=> (t._1, t._2, t._3, targetResourceProfilesForReference))
  }

  /**
   * Find the Target FHIR type of a search path within the element definitions
   * @param path Search path
   * @return
   */
  def findTargetTypeOfPath(path:String, profiles:Seq[ProfileRestrictions]):Seq[(String, String, Seq[String], Set[String])] = {
    var targetTypeAndProfiles:Seq[(String, String, Seq[String], Set[String])] = findTargetTypeOfPathInProfileChain(path, profiles)

    //If still it is empty, path may be referring to an inner element of a FHIR Complex DataType so does not exist in Resource e.g. Patient.name.given (name refers to HumanName)
    if(targetTypeAndProfiles.isEmpty && path.contains('.')){
      targetTypeAndProfiles = findTargetTypeInSubPaths(path.split('.'), profiles)
    }

    if(targetTypeAndProfiles.isEmpty)
      logger.warn(s"Cannot find element definition for search path $path within profiles ${profiles.map(_.url).mkString(", ")}, skipping it!")

    targetTypeAndProfiles
  }

  /**
   * Find target type by trying different division of path between until DataType and within DataType
   * @param pathParts
   * @param i
   * @return
   */
  private def findTargetTypeInSubPaths(pathParts:Seq[String],profiles:Seq[ProfileRestrictions],  i:Int = 1):Seq[(String, String, Seq[String], Set[String])] = {
    var targetTypeAndProfiles:Seq[(String, String, Seq[String], Set[String])] = Nil

    //Split the path accordingly
    val pathToDataType = pathParts.slice(0, pathParts.length-i).mkString(".")
    val pathAfterDataType = pathParts.slice(pathParts.length-i, pathParts.length).mkString(".")
    //Try to find in this setup
    findTargetTypeOfPathInProfileChain(pathToDataType, profiles) match {
      //If there is no matching, try sub paths
      case Nil =>
        if(i < pathParts.length - 1)
          targetTypeAndProfiles =
            findTargetTypeInSubPaths(pathParts, profiles, i+1)

      case possibleTypes =>
        possibleTypes.foreach(possibleType => possibleType match {
          //If we found it and it is base a DataType, then load the base DataType and find within it
          case onlyDataType if onlyDataType._3.isEmpty =>
            targetTypeAndProfiles = targetTypeAndProfiles ++
              findTargetTypeOfPath(pathAfterDataType, Seq(fhirConfig.getBaseProfile(onlyDataType._2)))
                .map(r => (s"$pathToDataType.${r._1}", r._2, r._3, r._4))
          //If there are multiple profiles
          case multipleProfiles =>
            import scala.util.control.Breaks._
            breakable {
              for (i <- multipleProfiles._3.indices) {
                fhirConfig.findProfileChain(multipleProfiles._3.apply(i)) match {
                  case Nil =>
                  //Nothing
                  case pc: Seq[ProfileRestrictions] if pc.nonEmpty =>
                    targetTypeAndProfiles = targetTypeAndProfiles ++
                      findTargetTypeOfPathInProfileChain(pathAfterDataType, pc)
                        .map(r => (s"$pathToDataType.${r._1}", r._2, r._3, r._4))
                    break()
                }
              }
            }
        })
    }

    targetTypeAndProfiles
  }


  /**
   * Check if path targets an array or not
   * @param path      JSON path to the element
   * @param profiles  Chain of profiles
   * @return
   */
  def findPathCardinality(path:String, profiles:Seq[ProfileRestrictions]):Boolean = {
    //Search in the base profile defined in the standard for resource type
    findElementRestrictionForPath(path, profiles.flatMap(_.elementRestrictions))
    match {
      case Some(er) => er._2.restrictions.get(ConstraintKeys.ARRAY).exists(_.asInstanceOf[ArrayRestriction].isArray)
      //If such a path not exist
      case None =>
        if(path.contains('.'))
          findPathCardinalityInSubpaths(path.split('.'), profiles)
        else
          false
    }
  }

  /**
   * Check recursively if path targets an array or not if path goes along a profile and then continues within a FHIR data type e.g. valueQuantity.value
   * @param pathParts Parts of path splitted by '.'
   * @param profiles  Profile chain that defines the content
   * @param i         Index to split for this try
   * @return
   */
  private def findPathCardinalityInSubpaths(pathParts:Seq[String], profiles:Seq[ProfileRestrictions], i:Int =1):Boolean ={

    //Split the path accordingly
    val pathToDataType = pathParts.slice(0, pathParts.length-i).mkString(".")
    val pathAfterDataType = pathParts.slice(pathParts.length-i, pathParts.length).mkString(".")

    findTargetTypeOfPath(pathToDataType, profiles)
      .map(_._2) match {
        case Nil if i < pathParts.length - 1 => findPathCardinalityInSubpaths(pathParts, profiles, i+1)
        case foundTypes if foundTypes.nonEmpty=>
          val baseProfileChainForDataType = fhirConfig.getBaseProfileChain(foundTypes.head)
          findPathCardinality(pathAfterDataType, baseProfileChainForDataType)
        case _ =>
          logger.warn(s"Problem while identifying cardinality of path ${pathParts.mkString(".")} in profiles ${profiles.map(_.url).mkString(", ")}!")
          false
    }
  }
}

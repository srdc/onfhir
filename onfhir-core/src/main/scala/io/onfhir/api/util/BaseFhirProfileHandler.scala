package io.onfhir.api.util

import io.onfhir.api.validation.{ConstraintKeys, ElementRestrictions, ProfileRestrictions}
import io.onfhir.config.FhirConfig
import io.onfhir.validation.{ArrayRestriction, TypeRestriction}
import org.slf4j.Logger

abstract class BaseFhirProfileHandler(fhirConfig: FhirConfig) {
  protected val logger:Logger
  /**
   * Check if a search path match with element definition path
   * @param searchPath
   * @param elementPath
   * @return
   */
  private def checkPathsMatch(searchPath:String, elementPath:String):Boolean = {
    elementPath == searchPath || //If we have a path that is equal to search path e.g. Element path:  Observation.code --> Search Path: Observation.code
      (elementPath.endsWith("[x]") && searchPath.startsWith(elementPath.replace("[x]", "")) && //Or the path has multiple values and path starts with given element path e.g. Element path: Observation.value[x] -> Search path Observation.valueQuantity
        (fhirConfig.FHIR_COMPLEX_TYPES.contains(searchPath.replace(elementPath.replace("[x]", ""), "")) || //If complex type, type name starts with a capital e.g. CodeableConcept
          fhirConfig.FHIR_PRIMITIVE_TYPES.contains(FHIRUtil.decapitilize(searchPath.replace(elementPath.replace("[x]", ""), "")))) //If simple type, type name is lowercase
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
        findElementRestrictionForPath(path, profileChain.apply(i).elementRestrictions)
        match {
          case Some(er) =>
            //Possible data type e.g. valueCodeableConcept -> CodeableConcept
            val possibleDataType =
              if(!er._1.contains("[x]") || path == er._1)
                None
              else {
                val temp = path.replace(er._1.replace("[x]", ""), "")
                if(fhirConfig.FHIR_COMPLEX_TYPES.contains(temp)) Some(temp) else Some(FHIRUtil.decapitilize(temp))
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
  def findTargetTypeOfPath(path:String, profiles:Seq[ProfileRestrictions]):Option[(String, Seq[String])] = {
    var targetTypeAndProfiles:Option[(String, Seq[String])] = findTargetTypeOfPathInProfileChain(path, profiles)

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

    findTargetTypeOfPath(pathToDataType, profiles).map(_._1) match {
      case None if i < pathParts.length - 1 => findPathCardinalityInSubpaths(pathParts, profiles, i+1)
      case Some(foundType) =>
        val baseProfileChainForDataType = fhirConfig.getBaseProfileChain(foundType)
        findPathCardinality(pathAfterDataType, baseProfileChainForDataType)
      case _ =>
        logger.warn(s"Problem while identifying cardinality of path ${pathParts.mkString(".")} in profiles ${profiles.map(_.url).mkString(", ")}!")
        false
    }
  }
}

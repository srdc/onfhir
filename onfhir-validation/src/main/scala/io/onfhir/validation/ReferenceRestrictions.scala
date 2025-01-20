package io.onfhir.validation

import io.onfhir.api.model.{FhirCanonicalReference, FhirInternalReference, FhirLiteralReference, FhirLogicalReference, FhirReference}
import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_DATA_TYPES}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{AbstractFhirContentValidator, ConstraintFailure, FhirRestriction}
import io.onfhir.config.{FhirServerConfig, OnfhirConfig, ResourceConf}
import org.json4s
import org.json4s.JsonAST.{JObject, JString, JValue}

import scala.util.Try

/**
 * Restriction on target profile and versioning if this is a reference type
 *
 * @param referenceDataTypes Possible reference data types, either Reference, canonical or CodeableReference
 * @param targetProfiles    Profiles for targeted references
 * @param versioning        Whether this reference needs to be version specific or version independent, or whether either can be used.
 *                          either | independent | specific
 * @param aggregationMode   If the type is a reference to another resource, how the resource is or can be aggregated - is it a contained resource, or a reference, and if the context is a bundle, is it included in the bundle.
 *                          contained | referenced | bundled
 */
case class ReferenceRestrictions(referenceDataTypes:Set[String],
                                 targetProfiles:Set[String],
                                 versioning:Option[String],
                                 aggregationMode:Set[String]) extends  FhirRestriction {

  /**
   * Evaluate the restriction
   * @param value Json value
   * @param fhirContentValidator  FHIR content validator
   * @return
   */
  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    findOutReferenceType(value) match {
      case Some(FHIR_DATA_TYPES.REFERENCE) =>
        Try(FHIRUtil.parseReference(value))
          .toOption
          .map(validateFhirReference(_, fhirContentValidator))
          .getOrElse(Nil) //If the value is not parsable, we already report this in type validation so don't report anything further here

      case Some(FHIR_DATA_TYPES.CANONICAL) =>
        Try(FHIRUtil.parseCanonicalRef(value))
          .toOption
          .map(validateCanonicalReference(_, fhirContentValidator))
          .getOrElse(Nil) //If the value is not parsable, we already report this in type validation so don't report anything further here

      case Some(FHIR_DATA_TYPES.CODEABLE_REFERENCE) =>
        (value \ "reference") match {
          //If reference is given
          case fhirRef:JObject =>
            Try(FHIRUtil.parseReference(fhirRef))
              .toOption
              .map(validateFhirReference(_, fhirContentValidator))
              .getOrElse(Nil)
          //If not given, return nil
          case _ => Nil
        }
      case None =>
        Nil
    }
  }

  /**
   * Validate the parsed FHIR reference based on given restrictions
   * @param fhirReference           Parsed FHIR reference
   * @param fhirContentValidator    Content validator
   * @return
   */
  private def validateFhirReference(fhirReference:FhirReference, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    var issues:Seq[ConstraintFailure] = Nil
    fhirReference match {
      case FhirLiteralReference(url, rtype, _, version) =>
        //Check aggregation mode constraint
        if (aggregationMode == Set("contained"))
          issues = issues :+ ConstraintFailure("Invalid reference, only reference to contained resources are allowed!")

        //Issue for versioning restriction
        versioning.foreach {
          //If reference should be independent of version but a version is given
          case "independent" if version.isDefined =>
            issues = issues :+ ConstraintFailure(s"Given reference should be version independent!")
          case "specific" if version.isEmpty =>
            issues = issues :+ ConstraintFailure(s"Given reference should be version specific e.g. $rtype/.../_history/1!")
          case _ =>
        }

        //Try to find out resource type that each target profile is based on
        val targetDataTypeAndProfiles = findResourceTypesForProfiles(fhirContentValidator)
        //Target referenced resource type checking
        checkReferencedResourceType(rtype, targetDataTypeAndProfiles)
          .foreach(cf => issues = issues :+ cf)

        if(issues.exists(!_.isWarning))
          issues
        else {
          //Resource type that we are currently validating
          val resourceType = fhirContentValidator.getResourceOrDataType
          //Find the Resource Type that we are working on
          getResourceConfiguration(resourceType, fhirContentValidator)
            .foreach(resourceConf => {
                //If literal policy is not given and but literal policy is used
                if (resourceConf.referencePolicies.nonEmpty && !resourceConf.referencePolicies.contains("literal"))
                  issues = issues :+ ConstraintFailure(s"Element uses literal referencing (with Reference.reference) while it is not allowed for resource '$resourceType'! ")

                //If reference policy is local, all references should be local
                if (resourceConf.referencePolicies.contains("local") && url.exists(!_.startsWith(OnfhirConfig.fhirRootUrl)))
                  issues = issues :+ ConstraintFailure(s"Element uses referencing to a resource in a remote repository (with Reference.reference) while it is not allowed for resource '$resourceType'! ")
                //If enforced, add it to list to check together with related target profiles
                if (resourceConf.referencePolicies.contains("enforced")) {
                  //Find target profiles specified for given referenced resource type
                  val targetProfiles = targetDataTypeAndProfiles.filter(_._2.contains(rtype)).map(_._1)
                  fhirContentValidator
                    .referencesToCheck
                    .append(fhirReference -> targetProfiles)
                }
            })
          //Return the final issues
          issues
        }

      //If reference is a logical reference
      case FhirLogicalReference(rtype:Option[String], _, _) =>

        //Check aggregation mode constraint
        if (aggregationMode == Set("contained"))
          issues = issues :+ ConstraintFailure("Invalid reference, only reference to contained resources are allowed!")

        //Resource type that we are currently validating
        val resourceType = fhirContentValidator.getResourceOrDataType
        //Find the Resource Type that we are working on
        getResourceConfiguration(resourceType, fhirContentValidator)
          .foreach(resourceConf => {
            //Try to find out resource type that each target profile is based on
            val targetDataTypeAndProfiles = findResourceTypesForProfiles(fhirContentValidator)

            //If logical or resolves reference policy is not given and but used
            if (!resourceConf.referencePolicies.contains("logical") && !resourceConf.referencePolicies.contains("resolves"))
              issues = issues :+ ConstraintFailure(s"Element uses logical referencing (with Reference.identifier) while it is not allowed for resource '$resourceType'! ")

            //If resource type is not given in logical reference, policy is resolves or enforced
            // and if the intended target resource type is not single, we cannot resolve the logical reference
            if((resourceConf.referencePolicies.contains("resolves") || resourceConf.referencePolicies.contains("enforced")) &&
                rtype.isEmpty &&
                  (targetDataTypeAndProfiles.flatMap(_._2).toSeq match {
                    case Seq(single) if single != "Resource" => false
                    case _ => true
                  })
            )
              issues = issues :+ ConstraintFailure(s"Logical reference cannot be resolved although policy requires this as the resource type is not indicated in logical reference!")

            //If policy is resolves or enforced and resource type is given, check if refers to one of the expected type
            if(resourceConf.referencePolicies.contains("resolves") || resourceConf.referencePolicies.contains("enforced"))
              rtype
                .flatMap(checkReferencedResourceType(_, targetDataTypeAndProfiles))
                .foreach(cf => issues = issues :+ cf)

            //If enforced, add it to list to check together with related target profiles
            if (rtype.isDefined &&
                  resourceConf.referencePolicies.contains("enforced") &&
                    !resourceConf.referencePolicies.contains("resolves")) {
              //Find target profiles specified for given referenced resource type
              val targetProfiles = targetDataTypeAndProfiles.filter(_._2.contains(rtype.get)).map(_._1)
              fhirContentValidator
                .referencesToCheck
                .append(fhirReference -> targetProfiles)
            }
          })
      issues
      //Internal reference to a contained resource
      case FhirInternalReference(_) =>
        if(aggregationMode.nonEmpty && !aggregationMode.contains("contained"))
          issues = issues :+ ConstraintFailure("Invalid reference, references to contained resources are not allowed!")
        //Internal references should exist
        fhirContentValidator
          .referencesToCheck
          .append(fhirReference -> targetProfiles)

        issues
    }
  }

  /**
   * Validate for given canonical reference
   *
   * @param canonicalReference   Parsed canonical reference
   * @param fhirContentValidator Content
   */
  private def validateCanonicalReference(canonicalReference: FhirCanonicalReference, fhirContentValidator: AbstractFhirContentValidator): Seq[ConstraintFailure] = {
    var issues: Seq[ConstraintFailure] = Nil
    //Check aggregation mode constraint
    if (aggregationMode.size == 1 && aggregationMode.contains("contained"))
      issues = issues :+ ConstraintFailure("Invalid canonical reference, only reference to contained resources are allowed!")
    //Check versioning
    versioning.foreach {
      //If reference should be independent of version but a version is given
      case "independent" if canonicalReference.version.isDefined =>
        issues = issues :+ ConstraintFailure(s"Given reference should be version independent!")
      case "specific" if canonicalReference.version.isEmpty =>
        issues = issues :+ ConstraintFailure(s"Given reference should be version specific!")
      case _ =>
    }

    //Try to find out resource type that each target profile is based on
    val targetDataTypeAndProfiles = findResourceTypesForProfiles(fhirContentValidator)
    //Target referenced resource type checking
    checkReferencedResourceType(canonicalReference.rtype, targetDataTypeAndProfiles)
      .foreach(cf => issues = issues :+ cf)

    //Resource type that we are currently validating
    val resourceType = fhirContentValidator.getResourceOrDataType
    getResourceConfiguration(resourceType, fhirContentValidator)
      .foreach(resourceConf =>
        //If enforcement is required, add it to references to check
        if (resourceConf.referencePolicies.contains("enforced")) {
          val targetProfiles = targetDataTypeAndProfiles.filter(_._2.contains(canonicalReference.rtype)).map(_._1)
          fhirContentValidator
            .referencesToCheck
            .append(canonicalReference -> targetProfiles)
        }
      )
    issues
  }


  /**
   * Check if referenced resource type is allowed in this reference
   * @param rtype                         Referenced resource type
   * @param targetDataTypeAndProfiles     Allowed profiles and types resolved
   * @return
   */
  private def checkReferencedResourceType(rtype:String, targetDataTypeAndProfiles:Set[(String, Option[String])]) = {
    //Target referenced resource type checking
    if (!targetDataTypeAndProfiles.exists(_._2.contains("Resource")) && //If reference to any resource is not allowed
      !targetDataTypeAndProfiles.flatMap(_._2).contains(rtype)) //And referenced resource type is not one of the resource type that allowed profiles are based on
    //If there is a profile that we cannot resolve, return warning to indicate that we cannot evaluate referenced resource type restriction
      if (targetDataTypeAndProfiles.exists(_._2.isEmpty))
        Some(ConstraintFailure(s"As the profiles with urls '${targetDataTypeAndProfiles.filter(_._2.isEmpty).map(_._1).mkString(", ")}' cannot be resolved, we cannot check if the referenced resource type matches one of the intended target profile", isWarning = true))
      else //Otherwise we resolve all profiles and given referenced resource type does not match any of them
        Some(ConstraintFailure(s"Referenced type '$rtype' does not match one of the expected target types '${targetDataTypeAndProfiles.flatMap(_._2).mkString(", ")}'!"))
    else
      None
  }

  /**
   * Try to resolve resource type per given target profile
   * @param fhirContentValidator Content validator module
   * @return
   */
  private def findResourceTypesForProfiles(fhirContentValidator: AbstractFhirContentValidator): Set[(String, Option[String])] = {
    targetProfiles
      .map(profileUrl =>
        profileUrl ->
          (profileUrl match {
            case "http://hl7.org/fhir/StructureDefinition/Resource" => Some("Resource")
            case _ =>
              fhirContentValidator
                .findResourceType(fhirContentValidator.fhirConfig.findProfileChainByCanonical(profileUrl))
          })
      )
  }


  /**
   * Get base resource configuration in capability statement
   * @param fhirContentValidator
   * @return
   */
  private def getResourceConfiguration(resourceType:String, fhirContentValidator: AbstractFhirContentValidator): Option[ResourceConf] = {

    fhirContentValidator.fhirConfig match {
      //If we are validating for onFHIR server also check referencing is OK according to capability statement (literal, logical, etc)
      case serverConfig: FhirServerConfig => serverConfig.resourceConfigurations.get(resourceType)
      case _ => None
    }
  }


  /**
   * Find out the reference type from given value
   * @param value
   * @return
   */
  private def findOutReferenceType(value:JValue):Option[String] = {
    value match {
      //If it is canonical and expected
      case _:JString if referenceDataTypes.contains(FHIR_DATA_TYPES.CANONICAL) =>
        Some(FHIR_DATA_TYPES.CANONICAL)

      case reference:JObject if
        referenceDataTypes.contains(FHIR_DATA_TYPES.REFERENCE) &&
          reference.obj
            .exists(f =>
              (f._1 == "reference" && f._2.isInstanceOf[JString]) || f._1 == "identifier"
            )  => Some(FHIR_DATA_TYPES.REFERENCE)

      case codeableReference:JObject if
        referenceDataTypes.contains(FHIR_DATA_TYPES.CODEABLE_REFERENCE) &&
          codeableReference.obj.exists(f => f._1 == "concept" || (f._1 == "reference" && f._2.isInstanceOf[JObject]))  =>
        Some(FHIR_DATA_TYPES.CODEABLE_REFERENCE)

      case _ =>
        None
    }
  }
}

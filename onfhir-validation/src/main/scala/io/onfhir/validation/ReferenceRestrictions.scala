package io.onfhir.validation

import io.onfhir.api.model.{FhirLiteralReference, FhirLogicalReference}
import io.onfhir.api.{FHIR_COMMON_FIELDS}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{AbstractFhirContentValidator, ConstraintFailure, FhirRestriction}
import io.onfhir.config.OnfhirConfig
import org.json4s.JsonAST.{JObject, JValue}

import scala.util.Try

/**
 * Restriction on target profile and versioning if this is a reference type
 *
 * @param targetProfiles   Profiles for targeted references
 * @param versioning       true -> version specific reference, false -> non-version specific reference, none-> either
 */
case class ReferenceRestrictions(targetProfiles:Seq[String], versioning:Option[Boolean], aggregationMode:Seq[String]) extends  FhirRestriction {

  /**
   *
   * @param value Json value
   * @param fhirContentValidator
   * @return
   */
  override def evaluate(value:JValue, fhirContentValidator: AbstractFhirContentValidator):Seq[ConstraintFailure] = {
    value match {
      case obj: JObject =>
        val fhirReferenceUrl = FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.REFERENCE)
        val parsedFhirReference = fhirReferenceUrl.flatMap(r => Try(FHIRUtil.parseReferenceValue(r)).toOption)
        val referencedResourceType = FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.TYPE)


        val invalidReferenceIssues =
          fhirReferenceUrl match {
            case Some(rurl) if (!rurl.startsWith("#") && (parsedFhirReference.isEmpty || !fhirContentValidator.fhirConfig.FHIR_RESOURCE_TYPES.contains(parsedFhirReference.get._2))) =>
              Seq(ConstraintFailure(s"Invalid reference format '$rurl', cannot parse reference!"))
            case _ => Nil
          }

        //Find the target profiles that are base FHIR specs, and get data types
        val expectedDataTypeAndProfiles =
          targetProfiles
            .map(tp => {
              if(tp == "http://hl7.org/fhir/StructureDefinition/Resource")
                "Resource" -> tp
              else {
                val dt = fhirContentValidator.findResourceType(fhirContentValidator.fhirConfig.findProfileChain(tp)).get
                dt -> tp
              }
            })
            .groupBy(_._1)
            .map(g => g._1 -> g._2.map(_._2))

        val targetProfileIssue =
          if (expectedDataTypeAndProfiles.isEmpty ||
            fhirReferenceUrl
              .flatMap(ref =>
                parsedFhirReference.map(_._2) match { //Try to get the target type from the reference element e.g. Patient/65161565 -> Patient
                  case None => referencedResourceType //If not exist, try to get it from the 'type' element
                  case oth => oth
                }
              ) //Extract data type from the reference
              .forall(dt => expectedDataTypeAndProfiles.contains("Resource") || expectedDataTypeAndProfiles.contains(dt))) //target type is an expected FHIR Resource
            Nil
          else
            Seq(ConstraintFailure(s"Referenced type does not match one of the expected target types '${expectedDataTypeAndProfiles.keys.mkString(", ")}'!"))


        val referenceFormatIssue =
          if (!versioning.forall(v => parsedFhirReference.forall(fref => fref._4.isDefined == v)))
            Seq(ConstraintFailure(s"Reference should be ${if (versioning.get) "version specific" else "version independent"}!"))
          else Nil

        var allIssues = invalidReferenceIssues ++ targetProfileIssue ++ referenceFormatIssue

        if (allIssues.nonEmpty)
          allIssues
        else {
            //Find the Resource Type that we are working on
            val resourceType = fhirContentValidator.getResourceOrDataType()
            fhirContentValidator.fhirConfig.resourceConfigurations.get(resourceType) match {
              //If we can't access profile configuration, skip other validations
              case None =>
                Nil
              case Some(resourceConf) =>
                val refIdentifier = FHIRUtil.extractValueOption[JObject](obj, FHIR_COMMON_FIELDS.IDENTIFIER)

                //If logical reference policy is not given and but used
                if (refIdentifier.isDefined && !resourceConf.referencePolicies.contains("logical"))
                  allIssues = allIssues :+ ConstraintFailure(s"Element uses logical referencing (with Reference.identifier) while it is not allowed for resource  '$resourceType'! ")

                //If literal policy is not given and but literal policy is used
                if(resourceConf.referencePolicies.nonEmpty && !resourceConf.referencePolicies.contains("literal") && parsedFhirReference.isDefined)
                  allIssues = allIssues :+ ConstraintFailure(s"Element uses literal referencing (with Reference.reference) while it is not allowed for resource  '$resourceType'! ")

                //If reference policy is local, all references should be local
                if(resourceConf.referencePolicies.contains("local") && parsedFhirReference.flatMap(_._1).exists(! _.startsWith(OnfhirConfig.fhirRootUrl)))
                  allIssues = allIssues :+ ConstraintFailure(s"Element uses referencing to a resource in a remote repository (with Reference.reference) while it is not allowed for resource  '$resourceType'! ")

                if(resourceConf.referencePolicies.contains("enforced")){
                  if(parsedFhirReference.isDefined) {
                    fhirContentValidator
                        .referencesToCheck.append(
                          FhirLiteralReference(parsedFhirReference.get._1, parsedFhirReference.get._2, parsedFhirReference.get._3, parsedFhirReference.get._4) ->
                            expectedDataTypeAndProfiles.getOrElse(parsedFhirReference.get._2, Nil).toSet)
                  } else if(refIdentifier.isDefined)
                    fhirContentValidator
                      .referencesToCheck.append(
                        FhirLogicalReference(
                          referencedResourceType,
                          FHIRUtil.extractValueOption[String](refIdentifier.get, FHIR_COMMON_FIELDS.SYSTEM),
                          FHIRUtil.extractValue[String](refIdentifier.get, FHIR_COMMON_FIELDS.VALUE))
                          -> Set.empty[String])
                }
            }
          //TODO handle other target profiles (not base FHIR)
          //TODO handle aggregation mode restrictions
          //If this is not a reference element, just skip validation
          allIssues
        }
      case _ => Nil

    }
  }
}

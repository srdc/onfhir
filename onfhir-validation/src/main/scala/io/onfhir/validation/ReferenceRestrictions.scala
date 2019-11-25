package io.onfhir.validation

import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_ROOT_URL_FOR_DEFINITIONS}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction}
import org.json4s.JsonAST.{JObject, JValue}

import scala.util.Try

/**
 * Restriction on target profile and versioning if this is a reference type
 *
 * @param targetProfiles   Profiles for targeted references
 * @param versioning       true -> version specific reference, false -> non-version specific reference, none-> either
 */
case class ReferenceRestrictions(targetProfiles:Seq[String], versioning:Option[Boolean], aggregationMode:Seq[String]) extends  FhirRestriction {
  override def evaluate(value:JValue):Seq[ConstraintFailure] = {
    value match {
      case obj:JObject =>
        val fhirReferenceUrl = FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.REFERENCE)
        val parsedFhirReference = fhirReferenceUrl.flatMap(r => Try(FHIRUtil.parseReferenceValue(r)).toOption)

        //Find the target profiles that are base FHIR specs, and get data types
        val expectedDataTypes =
            targetProfiles
              .filter(_.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS + "/StructureDefinition/"))
              .map(_.replace(FHIR_ROOT_URL_FOR_DEFINITIONS + "/StructureDefinition/", ""))

        val targetProfileIssue =
            if (fhirReferenceUrl
              .flatMap(ref =>
                parsedFhirReference.map(_._2) match { //Try to get the target type from the reference element e.g. Patient/65161565 -> Patient
                  case None => FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.TYPE) //If not exist, try to get it from the 'type' element
                  case oth => oth
                }
              ) //Extract data type from the reference
              .forall(dt => expectedDataTypes.contains(dt) || //Either target type is an expected FHIR Resource
                expectedDataTypes.length != targetProfiles.length)) //Or there is some other expected profile
              Nil
            else
              Seq(ConstraintFailure(s"Referenced type does not match one of the expected profiles!"))

          val referenceFormatIssue =
            if (!versioning.forall(v => parsedFhirReference.forall(fref => fref._4.isDefined == v)))
              Seq(ConstraintFailure(s"Reference should be ${if (versioning.get) "version specific" else "non-version specific"}!"))
            else Nil

          //TODO handle other target profiles (not base FHIR)
          //TODO handle aggregation mode restrictions
          targetProfileIssue ++ referenceFormatIssue
          //If this is not a reference element, just skip validation

      case _ => Nil

    }
  }
}

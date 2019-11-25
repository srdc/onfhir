package io.onfhir.validation

import io.onfhir.api.validation.{ConstraintFailure, FhirRestriction}
import org.json4s.JsonAST.JValue

/**
 * Restriction on a FHIR part to conform to a set of profiles
 * @param dataType  Expected data type
 * @param profiles  Expected profiles to conform
 */
case class ProfileRestriction(dataType:String, profiles:Set[String]) extends  FhirRestriction {
  override def evaluate(value:JValue):Seq[ConstraintFailure] = {
    Nil
  }
}

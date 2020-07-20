package io.onfhir.api.validation

import io.onfhir.api._
import io.onfhir.Onfhir
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.{FhirConfig, OnfhirConfig}
import io.onfhir.exception.BadRequestException
import io.onfhir.validation.FhirContentValidator
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future



/**
  * FHIR content validator
  */
class FHIRResourceValidator(fhirConfig:FhirConfig) extends IFhirResourceValidator {
  private val logger:Logger = LoggerFactory.getLogger(this.getClass)
  //Validations can block so we are using the blocking dispatcher
  implicit val executionContext = Onfhir.actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")

  /**
   * Validates resource based on the related FHIR profiles (StructureDefinition), check the CapabilityStatement.rest.resource.baseProfile
   * @param resource  JSON content of the resource
   * @param rtype     Resource type expected
   * @param silent
   * @return
   */
  def validateResource(resource: Resource, rtype:String, silent:Boolean = false): Future[Seq[OutcomeIssue]] = {
    validateResourceAgainstProfile(resource, rtype, fhirConfig.resourceConfigurations.get(rtype).flatMap(_.profile), silent)
  }
  /**
   * Validates resource based on the related FHIR profiles (StructureDefinition)
   * @param resource   Parsed JSON content of the resource
   * @param rtype      Resource type expected
   * @param profile    Profile that resource is expected to conform (if not exist just validated for base resource type)
   * @param silent     If true, does not throw exception but return issues
   * @return
   */
  def validateResourceAgainstProfile(resource: Resource, rtype:String, profile:Option[String], silent:Boolean = false): Future[Seq[OutcomeIssue]] = {
    Future.apply {
      OnfhirConfig.fhirValidation match {
        case FHIR_VALIDATION_ALTERNATIVES.NONE => Nil // Ignore validation, no issues
        case _ =>
          //Find the leaf profile to validate against
          val baseProfile = profile.getOrElse(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype")

          //profiles listed in Resource.meta.profile
          val profilesClaimedToConform = FHIRUtil.extractProfilesFromBson(resource)
          //Supported profiles for resource type in CapabilityStatement.rest.resource.supportedProfiles
          val supportedProfiles = fhirConfig.resourceConfigurations.get(rtype).map(_.supportedProfiles).getOrElse(Set.empty[String])
          //Unknow profiles among them
          val unknownProfiles = profilesClaimedToConform.diff(supportedProfiles ++ Set(baseProfile))
          //Known profiles among them
          val knownProfiles = profilesClaimedToConform.intersect(supportedProfiles)
          //We only validate agains base profile and known profiles
          val profileChains = (Set(baseProfile) ++ knownProfiles).map(p => p -> fhirConfig.findProfileChain(p))
          //Find inner profile urls in all profile chain
          val allInnerProfiles = profileChains.flatMap(_._2.drop(1).map(_.url))
          //Only validate agains the one that does not exist in inner profiles (because this chain already includes others)
          val profileChainsToValidate = profileChains.filter(pc => !allInnerProfiles.contains(pc._1)).map(_._2)

          val issues = profileChainsToValidate.flatMap(pc => {
            val contentValidator = FhirContentValidator.apply(fhirConfig, pc.head.url, new ReferenceResolver(fhirConfig, resource, None))
            contentValidator.validateComplexContentAgainstProfile(pc, resource, None)
              .map(oi => oi.copy(diagnostics = Some(s"[Validating against '${pc.head.url}'] => " + oi.diagnostics.getOrElse(""))))
          })

          val unknownProfileWarnings = unknownProfiles.map(up => OutcomeIssue(FHIRResponse.SEVERITY_CODES.WARNING, FHIRResponse.OUTCOME_CODES.INFORMATIONAL, None, Some(s"Profile with url '$up' is not known to this server! Therefore, validation is skipped for this profile!"), Seq("meta.profile")))

          if(!silent && issues.exists(_.isError))
            throw new BadRequestException((issues ++ unknownProfileWarnings).toSeq)

          (issues ++ unknownProfileWarnings).toSeq
      }
    }

  }
}


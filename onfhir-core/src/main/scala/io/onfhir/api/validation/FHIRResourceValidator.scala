package io.onfhir.api.validation

import akka.dispatch.MessageDispatcher
import io.onfhir.api._
import io.onfhir.Onfhir
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.{IFhirConfigurationManager, OnfhirConfig}
import io.onfhir.exception.BadRequestException
import io.onfhir.validation.FhirContentValidator

import scala.concurrent.Future



/**
  * FHIR content validator
  */
class FHIRResourceValidator(fhirConfigurationManager: IFhirConfigurationManager) extends IFhirResourceValidator {
  //private val logger:Logger = LoggerFactory.getLogger(this.getClass)
  //Validations can block so we are using the blocking dispatcher
  implicit val executionContext: MessageDispatcher = Onfhir.actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")

  /**
   * Validates resource based on the related FHIR profiles (StructureDefinition), check the CapabilityStatement.rest.resource.baseProfile
   * @param resource    JSON content of the resource
   * @param rtype       Resource type expected
   * @param parentPath  Parent path for the resource (if resource is within a Bundle or contained)
   * @param silent      If true, does not throw exception but return issues
   * @return
   */
  def validateResource(resource: Resource, rtype:String, parentPath:Option[String] =None, bundle:Option[(Option[String],Resource)] = None, silent:Boolean = false): Future[Seq[OutcomeIssue]] = {
    validateResourceAgainstProfile(resource, rtype, fhirConfigurationManager.fhirConfig.resourceConfigurations.get(rtype).flatMap(_.profile), parentPath, bundle, silent)
  }
  /**
   * Validates resource based on the related FHIR profiles (StructureDefinition)
   * @param resource   Parsed JSON content of the resource
   * @param rtype      Resource type expected
   * @param profile    Profile that resource is expected to conform (if not exist just validated for base resource type)
   * @param parentPath Parent path for the resource (if resource is within a Bundle or contained)
   * @param silent     If true, does not throw exception but return issues
   * @return
   */
  def validateResourceAgainstProfile(resource: Resource, rtype:String, profile:Option[String], parentPath:Option[String] =None,  bundle:Option[(Option[String],Resource)] = None, silent:Boolean = false): Future[Seq[OutcomeIssue]] = {
      OnfhirConfig.fhirValidation match {
        case FHIR_VALIDATION_ALTERNATIVES.NONE => Future.apply(Nil) // Ignore validation, no issues
        case _ =>
          //Find the leaf profile to validate against
          val baseProfile = profile.getOrElse(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype")

          //profiles listed in Resource.meta.profile
          val profilesClaimedToConform = FHIRUtil.extractProfilesFromBson(resource)
            .map(profile => {
              // Check if the profile matches the current FHIR version-specific profile URL
              if(profile.contentEquals(s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype|${fhirConfigurationManager.fhirConfig.fhirVersion}")) {
                // If it matches, return the profile URL without the version suffix
                s"$FHIR_ROOT_URL_FOR_DEFINITIONS/StructureDefinition/$rtype"
              } else
                profile
            })

          //Supported profiles for resource type in CapabilityStatement.rest.resource.supportedProfiles
          val supportedProfiles = fhirConfigurationManager.fhirConfig.resourceConfigurations.get(rtype).map(_.supportedProfiles).getOrElse(Set.empty[String])
          //Unknow profiles among them
          val unknownProfiles = profilesClaimedToConform.diff(supportedProfiles ++ Set(baseProfile))
          //Known profiles among them
          val knownProfiles = profilesClaimedToConform.intersect(supportedProfiles)
          //We only validate against the base profile and known profiles
          val profileChains = (Set(baseProfile) ++ knownProfiles).map(p => p -> fhirConfigurationManager.fhirConfig.findProfileChainByCanonical(p))
          //Find inner profile urls in all profile chain
          val allInnerProfiles = profileChains.flatMap(_._2.drop(1).map(_.url))
          //Only validate against the one that does not exist in inner profiles (because this chain already includes others)
          val profileChainsToValidate = profileChains.filter(pc => !allInnerProfiles.contains(pc._1)).map(_._2)

          Future
            .sequence(
              profileChainsToValidate
                .map(pc => {
                  val contentValidator = FhirContentValidator.apply(fhirConfigurationManager.fhirConfig, pc.head.url, new ReferenceResolver(fhirConfigurationManager, resource, bundle), this)
                  contentValidator
                    .validateComplexContentAgainstProfile(pc, resource, parentPath)
                    .map(
                      _.map(oi => oi.copy(diagnostics = Some(s"[Validating against '${pc.head.url}'] => " + oi.diagnostics.getOrElse(""))))
                    )
                })
            )
           .map(_.flatten)
           .map(issues => {
             val unknownProfileWarnings = unknownProfiles.map(up => OutcomeIssue(FHIRResponse.SEVERITY_CODES.WARNING, FHIRResponse.OUTCOME_CODES.INFORMATIONAL, None, Some(s"Profile with url '$up' is not known to this server! Therefore, validation is skipped for this profile!"), Seq("meta.profile")))

             if (!silent && issues.exists(_.isError)) {
               throw new BadRequestException((issues ++ unknownProfileWarnings).toSeq)
             }

             (issues ++ unknownProfileWarnings).toSeq
          })
      }
  }

  /**
   * Return terminology validator
   *
   * @return
   */
  override def getTerminologyValidator(): Option[IFhirTerminologyValidator] = Option(fhirConfigurationManager.fhirTerminologyValidator)
}


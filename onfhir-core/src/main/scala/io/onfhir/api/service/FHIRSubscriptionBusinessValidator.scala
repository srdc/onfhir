package io.onfhir.api.service

import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.{FHIRUtil, SubscriptionUtil}
import io.onfhir.api.validation.IResourceSpecificValidator
import io.onfhir.exception.BadRequestException
import io.onfhir.config.FhirConfigurationManager.fhirConfig


/**
 * Business Rule Validation for FHIR Subscription
 *
 */
class FHIRSubscriptionBusinessValidator extends IResourceSpecificValidator {

  /**
   * Validate extra business rules for the operation
   *
   * @param fhirRequest
   */
  override def validateRequest(fhirRequest: FHIRRequest): Unit = {
    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.CREATE | FHIR_INTERACTIONS.UPDATE =>

        //Validate criteria
        val criteriaStr = FHIRUtil.extractValue[String](fhirRequest.resource.get, "criteria")
        new SubscriptionUtil(fhirConfig).parseAndValidateFhirSubscriptionCriteria(criteriaStr)

        //Validate if channel type is supported
        val channelType = FHIRUtil.extractValueOptionByPath[String](fhirRequest.resource.get, "channel.type").get
        if (!io.onfhir.api.SUPPORTED_SUBSCRIPTION_CHANNELS.contains(channelType))
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR, //fatal
              FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
              None,
              Some(s"FHIR Subscription channel type $channelType not supported by onFhir.io yet!"),
              Seq("Subscription.channel.type")
            )))

        //Validate status
        val status = FHIRUtil.extractValue[String](fhirRequest.resource.get, "status")
        if (status != SubscriptionStatusCodes.requested && status != SubscriptionStatusCodes.off)
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Clients are not allowed to set FHIR Subscription status to values other than '${SubscriptionStatusCodes.requested}' or ${SubscriptionStatusCodes.off}!"),
              Seq("Subscription.status")
            )))

        //If given mime type is not supported
        if(!FHIRUtil.extractValueOptionByPath[String](fhirRequest.resource.get, "channel.payload")
          .forall(mimeType => fhirConfig.FHIR_SUPPORTED_RESULT_MEDIA_TYPES.map(m => m.toString()).contains(mimeType)))
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
              None,
              Some(s"Given mime type is not supported for subscription mechanism!"),
              Seq("Subscription.channel.payload")
            )))

      case FHIR_INTERACTIONS.PATCH =>
        //TODO validate patch requests
    }
  }

  /**
   * Validate rules about changes in the content
   *
   * @param oldContent
   * @param newContent
   */
  override def validateChanges(oldContent: Resource, newContent: Resource): Unit = {
    if(checkIfChange[String](oldContent, newContent, "criteria"))
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Changing criteria for FHIR Subscription is forbidden in onFhir.io! You should delete the subscription and create a new one if you need different criteria!"),
          Seq("Subscription.criteria")
        )))
  }

  private def checkIfChange[T](oldContent: Resource, newContent: Resource, path:String)(implicit manifest:Manifest[T]):Boolean = {
    FHIRUtil.extractValueOptionByPath[T](oldContent, path) != FHIRUtil.extractValueOptionByPath[T](newContent, path)
  }
}

package io.onfhir.api.util

import akka.http.scaladsl.model.Uri.Query
import io.onfhir.api.model._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.{FHIR_PARAMETER_CATEGORIES, Resource}
import io.onfhir.config.{FhirConfig, OnfhirConfig}
import io.onfhir.exception.BadRequestException
import org.json4s.JsonAST.JObject

import scala.util.{Failure, Success, Try}

class SubscriptionUtil(fhirConfig: FhirConfig) {
  /**
   * Parse a FHIR subscription content into our internal model
   * @param subscription
   * @return
   */
  def parseFhirSubscription(subscription:Resource):FhirSubscription = {
    val criteriaStr = FHIRUtil.extractValue[String](subscription, "criteria")
    val (srtype, pcriteria) = parseAndValidateFhirSubscriptionCriteria(criteriaStr)
    FhirSubscription(
      id = FHIRUtil.extractValue[String](subscription, "id"),
      rtype = srtype,
      channel = parseFhirSubscriptionChannel((subscription \ "channel").asInstanceOf[JObject]),
      criteria = pcriteria,
      status = FHIRUtil.extractValue[String](subscription, "status"),
      expiration = FHIRUtil.extractValueOption[String](subscription, "end")
    )
  }

  /**
   * Parse subscriptin channel content
   * @param channel
   * @return
   */
  def parseFhirSubscriptionChannel(channel:Resource):FhirSubscriptionChannel  ={
    FhirSubscriptionChannel(
      channelType = FHIRUtil.extractValue[String](channel, "type"),
      endpoint =  FHIRUtil.extractValueOption[String](channel, "endpoint"),
      payload = FHIRUtil.extractValueOption[String](channel, "payload"),
      headers = FHIRUtil.extractValueOption[Seq[String]](channel, "header").getOrElse(Nil)
    )
  }

  /**
   * Parse and validate the Subscription criteria given in the FHIR Subscription resource
   * @param criteriaStr FHIR Query statement given as string
   * @return            Resource type subscribed and parsed query parameters
   */
  def parseAndValidateFhirSubscriptionCriteria(criteriaStr:String):(String, Seq[Parameter]) = {
    val parts = criteriaStr.split('?')
    val rtype = parts.head
    if(parts.length > 2)
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"Invalid FHIR Subscription criteria '$criteriaStr', it is not a valid FHIR Query statement!"),
          Seq("Subscription.criteria")
        )
      ))

    //Check if the subscribed resource type is supported
    if(fhirConfig.resourceConfigurations.get(rtype).isEmpty)
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"Resource type '$rtype' mentioned in given FHIR Subscription content is not supported in this onFhir instance! Please check the conformance statement of the server..."),
          Seq("Subscription.criteria")
        )
      ))

    if(OnfhirConfig.fhirSubscriptionActive && !OnfhirConfig.fhirSubscriptionAllowedResources.forall(_.contains(rtype)))
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.SECURITY, //not supported
          None,
          Some(s"Resource type '$rtype' mentioned in given FHIR Subscription content is not allowed for subscription in this onFhir instance! Please contact with administrator..."),
          Seq("Subscription.criteria")
        )
      ))

    val parsedParameters:Seq[Parameter] =
      Try(
        parts.drop(1).headOption
          .map(queryStr => {
            val parsedRawParams = Query.apply(queryStr).toMultiMap
            FHIRSearchParameterValueParser.parseSearchParameters(rtype, parsedRawParams)
          })
          .getOrElse(Nil)
      ) match {
        case Success(params) => params
        case Failure(exception) =>
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
              None,
              Some(s"Invalid FHIR Subscription criteria '$criteriaStr'! One or more parameters are not supported or given query statement is not valid." + exception.getMessage),
              Seq("Subscription.criteria")
            )))
      }

    //Only normal parameters are supported for subscription for now (chained or revchained are not supported yet)
    if(!parsedParameters.forall(p => p.paramCategory == FHIR_PARAMETER_CATEGORIES.NORMAL || p.name == "_id"))
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"Invalid FHIR Subscription criteria '$criteriaStr'! Only normal search parameters (chaining, reverse chaining, or special parameters are not supported) are supported for subscription in onFhir.io."),
          Seq("Subscription.criteria")
        )))

    rtype -> parsedParameters
  }

}

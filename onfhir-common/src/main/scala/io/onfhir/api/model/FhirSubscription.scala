package io.onfhir.api.model

import io.onfhir.event.FhirEvent


/**
 * FHIR Subscription channel details
 *
 * @param channelType   Channel type see model.ChannelTypes
 * @param endpoint      Endpoint for channel
 * @param payload       Mime type for notification to return if not exits comminicate with empty content
 * @param headers       Headers to append to the channel
 */
case class FhirSubscriptionChannel(channelType:String, endpoint: Option[String], payload:Option[String], headers:Seq[String] = Nil)

/**
 * FHIR Subscription
 * @param id        Id of subscription in OnFhir
 * @param channel   Channel details
 * @param criteria  Criteria for the subscription
 * @param status    Status of the subscription
 *                  0: Not active (Used for web sockets to indicate web socket connection is not established yet)
 *                  1: Active
 *                  negative integers: Number of successive connection failures for the channel
 * @param expiration If exists expiration time of subscription
 */
case class FhirSubscription(id:String, rtype:String, channel:FhirSubscriptionChannel, criteria:Seq[Parameter] = Nil, status:String, expiration:Option[String]) extends InternalEntity
package io.onfhir.subscription.model

/**
 * A notification to send for a FHIR susbscription
 * @param sid           Subscription id
 * @param endpoint      Endpoint to send (for rest, email, etc channels)
 * @param payload       Payload to send if requested
 * @param headers       Headers to append
 * @param latestStatus  Latest notification status; successfull or not
 */
case class FhirNotification(sid:String, endpoint: Option[String]=None, payload:Option[String] = None, headers:Seq[String] = Nil, latestStatus:Int = 1) extends CborSerializable

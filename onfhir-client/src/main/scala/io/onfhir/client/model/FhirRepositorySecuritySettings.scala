package io.onfhir.client.model

/**
 * Interface for security settings
 */
trait IFhirRepositorySecuritySettings

/**
 * Security settings for FHIR API access via bearer token
 *
 * @param clientId                   OpenID Client identifier assigned to toFhir
 * @param clientSecret               OpenID Client secret given to toFhir
 * @param requiredScopes             List of required scores to write the resources
 * @param authzServerTokenEndpoint   Authorization servers token endpoint
 * @param clientAuthenticationMethod Client authentication method
 */
case class BearerTokenAuthorizationSettings(clientId: String,
                                            clientSecret: String,
                                            requiredScopes: Seq[String],
                                            authzServerTokenEndpoint: String,
                                            clientAuthenticationMethod: String = "client_secret_basic") extends IFhirRepositorySecuritySettings

/**
 * Security settings for FHIR API access via basic authentication
 *
 * @param username Username for basic authentication
 * @param password Password for basic authentication
 */
case class BasicAuthenticationSettings(username: String, password: String) extends IFhirRepositorySecuritySettings

/**
 * Security settings for FHIR API access via fixed token
 *
 * @param token The fixed token
 */
case class FixedTokenAuthenticationSettings(token: String) extends IFhirRepositorySecuritySettings
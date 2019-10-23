package io.onfhir.authz

import java.net.URI

import com.nimbusds.jose.JWSAlgorithm
import com.nimbusds.oauth2.sdk.auth.ClientAuthenticationMethod

/**
  * Created by tuncay on 3/2/2017.
  * Metadata for Authorization Server (OpenId Connect + OAuth2.0)
  * @param issuer Identifier assigned to authorization server
  * @param jwks_uri Uri to access JWKS
  * @param registration_endpoint  Client registration endpoint
  * @param token_endpoint Token endpoint for authorization server
  * @param token_endpoint_auth_methods_supported  Authentication methods supported by the authorization server for token endpoint
  * @param token_endpoint_auth_signing_alg_values_supported JWS algorithms supported by the authorization server for token endpoint
  * @param introspection_endpoint Token Introspoection endpoint
  * @param introspection_endpoint_auth_methods_supported  Authentication methods supported by the authorization server for introspection
  * @param introspection_endpoint_auth_signing_alg_values_supported  JWS algorithms supported by the authorization server for token endpoint
  */
case class AuthorizationServerMetadata(issuer:String,
                                       var jwks_uri:URI,
                                       var registration_endpoint:Option[URI],
                                       var token_endpoint:Option[URI],
                                       token_endpoint_auth_methods_supported:Set[ClientAuthenticationMethod] =Set.empty,
                                       token_endpoint_auth_signing_alg_values_supported:Set[JWSAlgorithm] =Set.empty,
                                       var introspection_endpoint:Option[URI],
                                       introspection_endpoint_auth_methods_supported:Set[ClientAuthenticationMethod] = Set.empty,
                                       introspection_endpoint_auth_signing_alg_values_supported:Set[JWSAlgorithm] = Set.empty)


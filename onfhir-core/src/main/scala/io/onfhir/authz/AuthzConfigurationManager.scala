package io.onfhir.authz

import java.io.{BufferedWriter, File, FileWriter}
import java.net.{URI, URISyntaxException, URL}
import java.security.KeyPairGenerator
import java.security.interfaces.{RSAPrivateKey, RSAPublicKey}
import java.util.UUID

import com.nimbusds.jose.JWSAlgorithm
import com.nimbusds.jose.JWSAlgorithm.Family
import com.nimbusds.jose.jwk._
import com.nimbusds.oauth2.sdk.auth.ClientAuthenticationMethod
import com.nimbusds.oauth2.sdk.client._
import com.nimbusds.oauth2.sdk.http.HTTPRequest
import com.nimbusds.oauth2.sdk.util.JSONObjectUtils
import com.nimbusds.oauth2.sdk.{GrantType, Scope}
import com.nimbusds.openid.connect.sdk.op.OIDCProviderMetadata
import io.onfhir.config.OnfhirConfig
import io.onfhir.config.OnfhirConfig.authzConfig
import io.onfhir.api._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.InitializationException
import io.onfhir.util.JsonFormatter._

import net.minidev.json.JSONObject
import org.json4s.JsonAST.JObject
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._
import scala.io.Source

/**
  * Created by tuncay on 3/1/2017.
  */
object AuthzConfigurationManager {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Defaults
    */
  private final val DEFAULT_PROTECTED_RESOURCE_METADATA_PATH :String = DEFAULT_ROOT_FOLDER + "/protected-resource-server-metadata.json"
  private final val DEFAULT_PROTECTED_RESOURCE_JWKS_FILE_NAME = "fhir-server.jwks"
  private final val DEFAULT_PROTECTED_RESOURCE_DYNAMIC_REGISTRATION_METADATA_FILE_NAME = "protected-resource-server-metadata-dynamic.json"
  /**
    * Parameter names
    */
  private final val PARAM_ISSUER = "issuer"
  private final val PARAM_JWKS_URI = "jwks_uri"
  private final val PARAM_REGISTRATION_ENDPOINT = "registration_endpoint"
  private final val PARAM_TOKEN_ENDPOINT = "token_endpoint"
  private final val PARAM_TOKEN_AUTH_METHODS = "token_endpoint_auth_methods_supported"
  private final val PARAM_TOKEN_AUTH_SIGNING_ALGS = "token_endpoint_auth_signing_alg_values_supported"
  private final val PARAM_INTROSPECTION_ENDPOINT = "introspection_endpoint"
  private final val PARAM_INTROSPECTION_AUTH_METHODS = "introspection_endpoint_auth_methods_supported"
  private final val PARAM_INTROSPECTION_AUTH_SIGNING_ALGS = "introspection_endpoint_auth_signing_alg_values_supported"

  /**
    * Registered Authorization handler
    */
  var authorizationHandler:IAuthorizer = _

  /**
    * Registered token resolver
    */
  var tokenResolver:ITokenResolver = _

  /**
    * Initialize authorization mechanism for a secure OnFhir server
    * @param authorizationHandler Authorization handler
    * @param tokenResolver Token resolver
    */
  def initialize(customAuthorizer:Option[IAuthorizer], customtTokenResolver:Option[ITokenResolver]):Unit = {
    if(authzConfig.isSecure()){
      //Order is important!!
      //First read the authorization server's configurations
      configureForAuthorizationServer()
      //Then our own configuration for a protected resource server
      configureProtectedResourceServer()
      //Set authorization handler
      this.authorizationHandler = customAuthorizer.getOrElse(
        authzConfig.authorizationMethod match {
          case AUTHZ_METHOD_FHIR_ON_SMART => new SmartAuthorizer()
          case _ => throw new InitializationException(s"Unknown default authorization method ${authzConfig.authorizationMethod}!")
        }
      )
      //Set token resolver
      this.tokenResolver = customtTokenResolver.getOrElse(
        authzConfig.tokenResolutionMethod match {
          case TOKEN_RESOLUTION_JWT => new JWTResolver(authzConfig)
          case TOKEN_RESOLUTION_INTROSPECTION => new ResolverWithTokenIntrospection(authzConfig)
          case _ =>   throw new InitializationException(s"Unknown default access token resolution method ${authzConfig.tokenResolutionMethod}!")
        }
      )
    }
  }

  /**
    * Configure runtime environment (authzConfig) with the Authorization Server configurations
    */
  private def configureForAuthorizationServer():Unit = {
    logger.info("Configuring for authorization server...")
    try {
      //Obtain the Authorization Server metadata and cache it
      authzConfig.authzServerMetadata =  authzConfig.authorizationServerDiscovery match {
        //No discovery, so we should read the metadata from file
        case AUTHZ_DISCOVERY_NONE =>
          logger.debug(s"Configuring from existing Authorization Server's metadata file ${authzConfig.authorizationServerMetadataPath} ...")
          // Ok read the authorization server metadata from configuration file
          var metadata = Source.fromFile(authzConfig.authorizationServerMetadataPath.get).mkString //Read the metadata file
          //Replace the Authorization Server Root URL placeholder with actual value
          if(authzConfig.authorizationServerUrl.isDefined) {
            metadata = """\$\{AUTHZ_SERVER_ROOT_URL\}""".r.replaceAllIn(metadata, authzConfig.authorizationServerUrl.get)
            logger.debug(s"Authorization server root url configured: ${authzConfig.authorizationServerUrl.get} ...")
          }
          parseAuthorizationServerMetadata(metadata)
        //OAuth2.0 Discovery, we need an discovery endpoint
        case AUTHZ_DISCOVERY_OAUTH =>
          logger.debug(s"Configuring by OAuth discovery protocol from ${authzConfig.authorizationServerDiscoveryURL} ...")
          val metadata = new HTTPRequest(HTTPRequest.Method.GET, new URL(authzConfig.authorizationServerDiscoveryURL)).send().getContent
          parseAuthorizationServerMetadata(metadata)
        //OpenId connect discovery, we need an discovery endpoint
        case AUTHZ_DISCOVERY_OPENID_CONNECT =>
          logger.debug(s"Configuring by OID discovery protocol from ${authzConfig.authorizationServerDiscoveryURL} ...")
          //Call the endpoint and parse convert the OIDC metadata to our own minimized model
          val metadata = new HTTPRequest(HTTPRequest.Method.GET, new URL(authzConfig.authorizationServerDiscoveryURL)).send().getContentAsJSONObject
          val oidcMetadata = OIDCProviderMetadata.parse(metadata)
          AuthorizationServerMetadata(
            oidcMetadata.getIssuer.getValue,
            Option(oidcMetadata.getJWKSetURI),
            Option(oidcMetadata.getRegistrationEndpointURI),
            Option(oidcMetadata.getTokenEndpointURI),
            oidcMetadata.getTokenEndpointAuthMethods.asScala.toSet,
            oidcMetadata.getTokenEndpointJWSAlgs.asScala.toSet,
            Option(oidcMetadata.getIntrospectionEndpointURI),
            oidcMetadata.getTokenEndpointAuthMethods.asScala.toSet,
            oidcMetadata.getTokenEndpointJWSAlgs.asScala.toSet
          )
      }

      //If token resolution will be done via introspection, we need introspection configurations
      if(authzConfig.tokenResolutionMethod.equals(TOKEN_RESOLUTION_INTROSPECTION) &&
          (authzConfig.authzServerMetadata.introspection_endpoint.isEmpty ||
             authzConfig.authzServerMetadata.introspection_endpoint_auth_methods_supported.isEmpty ||
              authzConfig.authzServerMetadata.introspection_endpoint_auth_signing_alg_values_supported.isEmpty))
          throw new InitializationException(s"Introspection endpoint configuration(s) are invalid! Please check if all the $PARAM_INTROSPECTION_ENDPOINT, $PARAM_INTROSPECTION_AUTH_METHODS, $PARAM_INTROSPECTION_AUTH_SIGNING_ALGS exists... ")

      if(authzConfig.tokenResolutionMethod.equals(TOKEN_RESOLUTION_JWT) &&
        (authzConfig.jwtSignatureAlgorithm.isEmpty || //We need the algorithm
          !(Family.RSA.contains(authzConfig.jwtSignatureAlgorithm.get) ||  Family.HMAC_SHA.contains(authzConfig.jwtSignatureAlgorithm.get)) || //we only support RSA or HMAC
          (Family.RSA.contains(authzConfig.jwtSignatureAlgorithm.get) && (authzConfig.authzServerMetadata == null || authzConfig.authzServerMetadata.jwks_uri == null)) || //If signature algorithm is RSA we need JWKS uri
          (Family.HMAC_SHA.contains(authzConfig.jwtSignatureAlgorithm.get) && authzConfig.jwtSignatureSecretKey.isEmpty)
          ))
        throw new InitializationException(s"JWT token resolutionconfiguration(s) are invalid!")

      logger.info("Configured for the authorization server...")
      }catch{
        case e:Exception =>
          logger.error("Error while configuring for authorization server!")
          throw new InitializationException(e.getMessage)
      }
  }

  /**
    * Is FHIR repository dynamically registered as resource server
    * @return
    */
  private def isResourceServerDynamicallyRegistered:Boolean = {
      authzConfig.isAuthorizationServerRegistrationDynamic &&
        new File(s"./$DEFAULT_PROTECTED_RESOURCE_DYNAMIC_REGISTRATION_METADATA_FILE_NAME").exists()
  }

  /**
    * Configure onfhir as protected resource server
    */
  private def configureProtectedResourceServer():Unit = {
    logger.info("Configuring fhir-server as protected resource server...")
    try {
      val metadata =
        //If we use dynamic registration and already registered read from the dynamic metadata file
        if(isResourceServerDynamicallyRegistered)
          Source.fromFile(s"./$DEFAULT_PROTECTED_RESOURCE_DYNAMIC_REGISTRATION_METADATA_FILE_NAME")
        else
          authzConfig.protectedResourceMetadataPath match {
            case None => Source.fromInputStream(this.getClass.getResourceAsStream(DEFAULT_PROTECTED_RESOURCE_METADATA_PATH))
            case Some(path) => Source.fromFile(path)
          }

      //Parse the metadata
      val prInformation = JSONObjectUtils.parse(metadata.mkString)

      //Handle the JWKSet creation or loading, if we will need it
      val jwksFile = new File(authzConfig.protectedResourceJWKSPath.getOrElse("./" + DEFAULT_PROTECTED_RESOURCE_JWKS_FILE_NAME))
      if (jwksFile.exists()) {
        authzConfig.protectedResourceJWKSet = JWKSet.load(jwksFile) //if the file exists load it
      }
      if(needJWKSetCreation(prInformation)) {
        if (!jwksFile.exists())
          authzConfig.protectedResourceJWKSet = createAndStoreJWKSet(jwksFile.getPath) //Or create it

        //Check if we can find a signing key
        val signingKeys = new JWKSelector(new JWKMatcher.Builder()
          .keyType(KeyType.RSA)
          .keyUse(KeyUse.SIGNATURE)
          .build())
          .select(authzConfig.protectedResourceJWKSet)
        if (signingKeys.size() == 0)
          throw new InitializationException("Signing key not found in JWKSet for resource provider!")
        authzConfig.protectedResourceCurrentSignerKeyId = signingKeys.asScala.head.getKeyID
      }

      //Set the JWKS URI where we will be serving it
      prInformation.put(PARAM_JWKS_URI, OnfhirConfig.fhirRootUrl + "/jwks")

      //If dynamic registration and not registered yet, register ourselves and retrieve the parameters like clientId, etc
      val clientInformation = if (authzConfig.isAuthorizationServerRegistrationDynamic && !isResourceServerDynamicallyRegistered)
        registerResourceServerToAuthzServer(
          authzConfig.authzServerMetadata.registration_endpoint.get,
          prInformation
        )
       else
          ClientInformation.parse(prInformation) //or parse the data as client information (assuming all data is there)

      //Finally parse and store the client information
      authzConfig.protectedResourceInformation = clientInformation
      //Check if everything matches between the metadata
      checkEverythingOk()
    }
    catch {
      case i:InitializationException => throw i
      case e:Exception =>
        logger.error("Error while configuring fhir-server as protected resource server!", e)
        throw new InitializationException(e.getMessage)
    }
  }

  /**
    * OAuth2 Dynamic registration of client - registering resource server to authz server
    * @param registrationUri Registration URL
    * @param prInformation  Resource Server metadata
    * @return
    */
  private def registerResourceServerToAuthzServer(registrationUri:URI, prInformation: JSONObject):ClientInformation = {
    val clientMetadata = ClientMetadata.parse(prInformation)
    clientMetadata.setRedirectionURI(new URI(OnfhirConfig.fhirRootUrl))
    clientMetadata.setGrantTypes(Set(GrantType.CLIENT_CREDENTIALS).asJava)
    val scope = new Scope()
    scope.add("patient")
    scope.add("user")
    scope.add("openid")
    clientMetadata.setScope(scope)
    val registrationResponse = ClientRegistrationResponse.parse(new ClientRegistrationRequest(registrationUri, clientMetadata, null).toHTTPRequest.send())
    registrationResponse match {
      case s:ClientInformationResponse =>
        saveClientInformationFile(s"./$DEFAULT_PROTECTED_RESOURCE_DYNAMIC_REGISTRATION_METADATA_FILE_NAME",s.getClientInformation)
        s.getClientInformation

      case e:ClientRegistrationErrorResponse => throw new InitializationException("Error while registering resource-server to authorization server: "+e.getErrorObject.getDescription)
    }

  }

  /**
    * Check configurations are OK
    */
  private def checkEverythingOk():Unit = {
    authzConfig.tokenResolutionMethod match {
      case TOKEN_RESOLUTION_INTROSPECTION =>
        if(Option(authzConfig.protectedResourceInformation.getID).isEmpty)
          throw new InitializationException(s"Problem with registration to Authz Server, client id is missing!")

        val authMethod = Option(authzConfig.protectedResourceInformation.getMetadata.getTokenEndpointAuthMethod)
        //Check if Authentication method is not empty
        if(authMethod.isEmpty)
          throw new InitializationException(s"The configuration param 'token_endpoint_auth_method' can not be empty if token resolution is with introspection!")
        //and match with the Authorization Server's supported methods
        if(!authzConfig.authzServerMetadata.introspection_endpoint_auth_methods_supported.contains(authMethod.get))
          throw new InitializationException(s"Token introspection client authentication method does not match between Authorization Server (${authzConfig.authzServerMetadata.introspection_endpoint_auth_methods_supported}) and Proteced Resource Server (${authMethod.get.getValue})")

        authMethod.get match {
          case ClientAuthenticationMethod.CLIENT_SECRET_JWT | ClientAuthenticationMethod.PRIVATE_KEY_JWT =>
            val jWSAlgorithm = Option(authzConfig.protectedResourceInformation.getMetadata.getTokenEndpointAuthJWSAlg)
            //Check if authentication signing algorithm is not empty
            if(jWSAlgorithm.isEmpty)
              throw new InitializationException(s"The configuration param 'token_endpoint_auth_signing_alg' can not be empty if token resolution is with introspection and client authentication method is ${ClientAuthenticationMethod.CLIENT_SECRET_JWT.getValue} or ${ClientAuthenticationMethod.PRIVATE_KEY_JWT}  !")
            //and matches with Authorization Server's supported
            if(!authzConfig.authzServerMetadata.introspection_endpoint_auth_signing_alg_values_supported.contains(jWSAlgorithm.get))
              throw new InitializationException(s"Token introspection client authentication  signing algorithm does not match between Authorization Server (${authzConfig.authzServerMetadata.introspection_endpoint_auth_signing_alg_values_supported}) and Proteced Resource Server (${jWSAlgorithm.get.getName})")
          case ClientAuthenticationMethod.CLIENT_SECRET_BASIC =>
            if(Option(authzConfig.protectedResourceInformation.getSecret).isEmpty)
              throw new InitializationException(s"Problem with registration to Authz Server, client secret is missing!")
          case _ => //TODO Nothing
        }
      case TOKEN_RESOLUTION_JWT => //TODO
      case TOKEN_RESOLUTION_JWT_WITH_INTROSPECTION => //TODO
    }
  }

  /**
    * Check if we need a JWKSet for our resource server
    * @param clientInformation ResourceServer client metadata
    * @return
    */
  private def needJWKSetCreation(clientInformation:JSONObject):Boolean = {
    new ClientAuthenticationMethod(JSONObjectUtils.getString(clientInformation, "token_endpoint_auth_method")) match {
      case ClientAuthenticationMethod.CLIENT_SECRET_BASIC  | ClientAuthenticationMethod.NONE => false
      case _ => true
    }
  }

  /**
    * Get the parameter from the map if it does not exist throw Initialization exception
    * @param resource Resource map
    * @param pname Parameter name
    * @return
    */
  private def getRequiredParam(resource:Resource, pname:String):String = {
    FHIRUtil.extractValueOption[String](resource, pname) match {
      case None => throw new InitializationException(s"Param $pname is required!")
      case Some(v) => v
    }
  }

  /**
    * Parse the AuthorizationServer metadata
    * @param metadata Authorization Server metadata in JSON
    * @return
    */
  private def parseAuthorizationServerMetadata(metadata:String):AuthorizationServerMetadata = {
    val metadataJson:JObject = metadata.parseJson
    try {
      AuthorizationServerMetadata(
        getRequiredParam(metadataJson, PARAM_ISSUER),
        (metadataJson \ PARAM_JWKS_URI).extractOpt[String].map(new URI(_)),
        (metadataJson \ PARAM_REGISTRATION_ENDPOINT).extractOpt[String].map(new URI(_)),
        (metadataJson \ PARAM_TOKEN_ENDPOINT).extractOpt[String].map(new URI(_)),
        (metadataJson \ PARAM_TOKEN_AUTH_METHODS).extractOpt[List[String]].map(_.toSet.map((m:String) => ClientAuthenticationMethod.parse(m))).getOrElse(Set.empty),
        (metadataJson \ PARAM_TOKEN_AUTH_SIGNING_ALGS).extractOpt[List[String]].map(_.toSet.map((a:String) => JWSAlgorithm.parse(a))).getOrElse(Set.empty),
        (metadataJson \ PARAM_INTROSPECTION_ENDPOINT).extractOpt[String].map(new URI(_)),
        (metadataJson \ PARAM_INTROSPECTION_AUTH_METHODS).extractOpt[List[String]].map(_.toSet.map((m:String) => ClientAuthenticationMethod.parse(m))).getOrElse(Set.empty),
        (metadataJson \ PARAM_INTROSPECTION_AUTH_SIGNING_ALGS).extractOpt[List[String]].map(_.toSet.map((a:String) => JWSAlgorithm.parse(a))).getOrElse(Set.empty)
      )
    }catch{
      case u:URISyntaxException => throw new InitializationException("Invalid uri! "+u.getMessage)
    }
  }

  /**
    * Create a RSA public-private key pair and build a JWKSet from it and save it to file and return it
    * @param path File path to create the JWKSet
    * @return
    */
  private def createAndStoreJWKSet(path:String): JWKSet ={
    // Generate the RSA key pair
    val gen = KeyPairGenerator.getInstance("RSA")
    gen.initialize(2048); // Set the desired key length
    val keyPair = gen.generateKeyPair()

    // Convert to JWK format
    val jwk = new RSAKey.Builder(
      keyPair.getPublic.asInstanceOf[RSAPublicKey])
      .privateKey(keyPair.getPrivate.asInstanceOf[RSAPrivateKey])
      .keyID(UUID.randomUUID().toString) // Give the key some ID (optional)
      .keyUse(KeyUse.SIGNATURE)
      .build()

    val jwkSet = new JWKSet(jwk)

    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(jwkSet.toString)
    bw.close()
    jwkSet
  }

  /**
    * Save the client metadata to file
    * @param path File path to save the client metadata
    * @param clientInformation Client metadata
    */
  private def saveClientInformationFile(path:String, clientInformation: ClientInformation): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(clientInformation.toJSONObject.toJSONString)
    bw.close()
  }

}

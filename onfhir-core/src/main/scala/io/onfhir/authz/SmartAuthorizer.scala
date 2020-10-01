package io.onfhir.authz


import io.onfhir.api.FHIR_INTERACTIONS
import io.onfhir.config.OnfhirConfig
import io.onfhir.config.FhirConfigurationManager.fhirConfig
/**
  * Created by tuncay on 2/27/2017.
  */
class SmartAuthorizer extends IAuthorizer {
  final val PERMISSION_TYPE_PATIENT = "patient"
  final val PERMISSION_TYPE_USER = "user"
  final val PERMISSION_TYPE_CONFIDENTIALITY = "conf"
  final val PERMISSION_TYPE_SENSITIVITY = "sens"
  //Parameter name for patientId parameter defined in Smart on FHIR specs
  final val PARAM_PATIENT_ID = "patient"
  //Scope scheme defined by WG Heart specification (extended for FHIR operations e.g. patient/Patient.$everything)
  //See http://openid.net/specs/openid-heart-fhir-oauth2-1_0-2017-05-31.html
  case class WGHeartScope(
                           ptype:String, //Scope type patient|user|conf|sens
                           rtype:String, //Resource type or Confidentiality/Sensitivity
                           read:Boolean = false, // Permission for read operation
                           write: Boolean = false, //Permission for write operation
                           operations:Set[String] = Set.empty[String] //Permissions for operations on the resource type
                         )

  //Further Authorization Context parameters for tokens (JWT Token, Introspection response)
  override def furtherParamsInAuthzContext = List(PARAM_PATIENT_ID)

  //Nothing is public
  def authorizeForPublic(interaction:String, resourceType:Option[String], resourceId:Option[String]):AuthzResult = AuthzResult.failureInvalidRequest("")
  /**
    * Decide on authorization of interaction on resource type based on Smart Heart WG specifications
    * @param authzContext resolved Authorization Context
    * @param interaction Name of FHIR  interaction (all fhir interactions + custom or fhir operations as $...
    * @param resourceType Type of the resource to be accessed for FHIR type and instance base interactions
    * @param resourceId Id of the resource to be accessed for FHIR instance base interactions
    * @return Either rejection or success with a list of restrictions in the format of FHIR search query
    */
  def authorize(authzContext: AuthzContext, interaction:String, resourceType:Option[String], resourceId:Option[String]):AuthzResult = {
    //Parse the scopes, if scope is not understood just skip it
    val scopes:Seq[WGHeartScope] = authzContext.scopes.map(parseScope).filter(_.isDefined).map(_.get)
    //Check resourceType is given as Smart only deals with resource specific interactions
    if(resourceType.isEmpty) {
      AuthzResult.undecided("Smart Authorization only authorizes FHIR type and instance interactions!")
    } else {
      //Get only the related scopes with the resource type
      val relatedScopes:Seq[WGHeartScope] = scopes.filter(s => s.rtype == PERMISSION_TYPE_CONFIDENTIALITY || s.rtype == PERMISSION_TYPE_SENSITIVITY || s.rtype.equals(resourceType.get) || s.rtype.equals("*"))
      //Validate the context according to requirements of Smart on FHIR
      if(!isAuthzContextValid(authzContext, relatedScopes))
        AuthzResult.failureInsufficientScope("Information is missing in token related with the given scopes")
      else {
        relatedScopes match {
          //If no matching scope, return failure
          case Nil => AuthzResult.failureInsufficientScope(s"Not authorized for the FHIR interaction '$interaction' on resource type '${resourceType.get}'!")
          //If there is some related scope
          case _ =>
            val (patientOrUserRightsOpt, confOrSensRightsOpt) = resolveTargetAccessRight(relatedScopes, resourceType.get)
            val baseResourceRestrictions: List[(String, String)] =
              patientOrUserRightsOpt
                .filter(accessRight => isAuthorizedForInteraction(accessRight, interaction)) //Go on if it is authorized for the interaction
                .map(accessRight => accessRight.ptype match { //Resolve the resource restriction queries
                  case PERMISSION_TYPE_PATIENT => resolveResourceRestrictionsForPatientTypeScope(authzContext, resourceType.get)
                  case PERMISSION_TYPE_USER => resolveResourceRestrictionsForUserTypeScope(authzContext, resourceType.get)
                }).getOrElse(List.empty)


            val otherResourceRestrictions =
              confOrSensRightsOpt
                .map(resolveResourceRestrictionsForConfSensTypeScope)
                .getOrElse(List.empty)

            val resourceRestrictions = baseResourceRestrictions ++ otherResourceRestrictions
            if (resourceRestrictions.isEmpty)
              AuthzResult.failureInsufficientScope(s"Not authorized for the FHIR interaction '$interaction' on resource type '${resourceType.get}'!")
            else
              AuthzResult.filtering(resourceType.get, baseResourceRestrictions, otherResourceRestrictions)
        }
      }
    }
  }

  /**
    * Resolve the resource restriction queries for access with patient type scope e.g. patient/Observation.read
    * @param authzContext Authorization context
    * @param resourceType resource type
    * @return search parameter -> parameter value
    */
  private def resolveResourceRestrictionsForPatientTypeScope(authzContext: AuthzContext, resourceType:String):List[(String,String)] = {
    //Extract patient ids from authz context
    val authorizedPatients = getPatientId(authzContext)
    List("Patient" -> authorizedPatients.mkString(","))
  }

  /**
    * Resolve the resource restriction queries for access with user type scope e.g. user/Observation.read
    * @param authzContext Authorization context
    * @param resourceType resource type
    * @return search parameter -> parameter value
    */
  private def resolveResourceRestrictionsForUserTypeScope(authzContext: AuthzContext, resourceType:String):List[(String,String)] = {
    //Extract user id from authz context
    val authorizedUser= authzContext.sub.get

    List(
      "Practitioner" -> authorizedUser,
      "RelatedPerson" -> authorizedUser
    )
  }

  /**
    *
    * @param scope
    * @param resourceType
    * @return
    */
  private def resolveResourceRestrictionsForConfSensTypeScope(scope:WGHeartScope):List[(String, String)] = {
    val expectedSecurityTagCodes = scope.rtype.split(",")
     val systemUrl = scope.ptype match {
       case PERMISSION_TYPE_CONFIDENTIALITY => "http://hl7.org/fhir/v3/Confidentiality"
       case PERMISSION_TYPE_SENSITIVITY => "http://hl7.org/fhir/v3/ActCode"
     }

    val queryValue = expectedSecurityTagCodes.map(c => systemUrl + "|" + c).mkString(",")
    List("_security" -> queryValue)
  }


  /**
    * Parse the scope string, and extract the scope definition according to WG Heart Specification See https://openid.bitbucket.io/HEART/openid-heart-fhir-oauth2.html
    * If scope can not be parsed, return None
    * @param scope scope to parse
    * @return
    */
  private def parseScope(scope:String):Option[WGHeartScope] = {
    val scopeElems = scope.split('/')

    //Check if permission is one of the Smart permissions
    if(!Set(PERMISSION_TYPE_PATIENT, PERMISSION_TYPE_USER, PERMISSION_TYPE_SENSITIVITY, PERMISSION_TYPE_CONFIDENTIALITY).contains(scopeElems(0)))
      None
    else {
      val remElems = scopeElems(1).split('.')
      remElems(1) match {
        //Permission for read interactions
        case "read" =>  Some(WGHeartScope(scopeElems.apply(0), remElems.apply(0), read=true, write=false))
        //Permission for write interactions
        case "write" =>   Some(WGHeartScope(scopeElems.apply(0), remElems.apply(0), read=true, write=true))
        //Permission for all interactions
        case "*" =>  Some(WGHeartScope(scopeElems.apply(0), remElems.apply(0), read=true, write=true))
        //Permission for FHIR operation
        case op if op.startsWith("$") => Some(WGHeartScope(scopeElems.apply(0),  remElems.apply(0), read=false, write=false, operations = Set(op)))
      }
    }
  }

  /**
    * Resolve the target scopes related with the requested resourceType
    * @param scopes scopes
    * @param resourceType resource type
    * @return
    */
  private def resolveTargetAccessRight(scopes:Seq[WGHeartScope], resourceType:String):(Option[WGHeartScope], Option[WGHeartScope]) = {
    //Group the scopes according to the type
    val groupedScopes = scopes.groupBy[String](_.ptype)

    //Extract User or patient scope if exists
    val mainScope =
      //If there is a related patient scope, that we can resolve from Patient compartment (Patient compartment should be enabled in orde to use patient scopes)
      if(groupedScopes.isDefinedAt(PERMISSION_TYPE_PATIENT) && fhirConfig.compartmentRelations.get("Patient").exists(_.isDefinedAt(resourceType))){
        Some(groupedScopes(PERMISSION_TYPE_PATIENT).fold(WGHeartScope(PERMISSION_TYPE_PATIENT, resourceType, read=false, write=false))((s1, s2) => WGHeartScope(PERMISSION_TYPE_PATIENT, resourceType, s1.read || s2.read, s1.write || s2.write, s1.operations ++ s2.operations)))
      }
      //Othwerwise search if there is a related user scope, that we can resolve from Practitioner or RelatedPerson compartments
      else if(groupedScopes.isDefinedAt(PERMISSION_TYPE_USER) && fhirConfig.compartmentRelations.filter(c => c._1 == "Practitioner" || c._1 == "RelatedPerson").exists(_._2.isDefinedAt(resourceType))){
        Some(groupedScopes(PERMISSION_TYPE_USER).fold(WGHeartScope(PERMISSION_TYPE_USER, resourceType, read=false, write=false))((s1, s2) => WGHeartScope(PERMISSION_TYPE_USER, resourceType, s1.read || s2.read, s1.write || s2.write, s1.operations ++ s2.operations)))
      } else
        None

    //Extract confidentiality or sensitivity scope if exists
    val supportiveScope =
      //If there is a confidentiality scope
      if(groupedScopes.isDefinedAt(PERMISSION_TYPE_CONFIDENTIALITY))
        Some(WGHeartScope(PERMISSION_TYPE_CONFIDENTIALITY, groupedScopes(PERMISSION_TYPE_CONFIDENTIALITY).map(_.rtype).mkString(",")))
      else if(groupedScopes.isDefinedAt(PERMISSION_TYPE_SENSITIVITY))
        Some(WGHeartScope(PERMISSION_TYPE_SENSITIVITY, groupedScopes(PERMISSION_TYPE_SENSITIVITY).map(_.rtype).mkString(",")))
      else
        None

    mainScope -> supportiveScope
  }

  /**
    * Check if the authzation context is as expected
    * @param authzContext authorization context
    * @param includePatientScope flag that indicates if patient scope is included
    * @return
    */
  private def isAuthzContextValid(authzContext: AuthzContext, scopes:Seq[WGHeartScope]):Boolean = {
    //If it includes patient scope and does not include patientId
    !(scopes.exists(s => s.ptype.equals(PERMISSION_TYPE_PATIENT)) && authzContext.furtherParams.get(PARAM_PATIENT_ID).isEmpty) &&
    //If it includes user scope and does not include user id
    !(scopes.exists(s => s.ptype.equals(PERMISSION_TYPE_USER)) && authzContext.sub.isEmpty) &&
    //If our fhir-server is not within audience
    authzContext.aud.contains(OnfhirConfig.fhirRootUrl)
  }

  /**
    * Retrieve the patient id parameter from AuthzContext
    * @param authzContext authorization context
    * @return
    */
  private def getPatientId(authzContext:AuthzContext):Set[String] = {
    authzContext.getListParam[String](PARAM_PATIENT_ID).get.toSet
  }

  /**
    * Check if the scope indicates an authorization for the interaction
    * @param accessRight Patient or User access right resolved
    * @param interaction FHIR interaction type
    * @return
    */
  private def isAuthorizedForInteraction(accessRight:WGHeartScope, interaction: String):Boolean = {
    interaction match {
      case FHIR_INTERACTIONS.CREATE
           | FHIR_INTERACTIONS.UPDATE
           | FHIR_INTERACTIONS.DELETE
           | FHIR_INTERACTIONS.PATCH => accessRight.write
      case FHIR_INTERACTIONS.READ
           | FHIR_INTERACTIONS.VREAD
           | FHIR_INTERACTIONS.HISTORY_INSTANCE
           | FHIR_INTERACTIONS.SEARCH
           | FHIR_INTERACTIONS.HISTORY_TYPE => accessRight.read
      //FHIR Operations
      case operation if operation.startsWith("$") => accessRight.operations.contains(operation)
      //Otherwise false
      case _ => false
    }
  }
}

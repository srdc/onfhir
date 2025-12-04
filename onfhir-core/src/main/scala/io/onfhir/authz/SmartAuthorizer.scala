package io.onfhir.authz


import akka.http.scaladsl.model.Uri
import com.typesafe.config.Config
import io.onfhir.api.{FHIR_INTERACTIONS, FHIR_PARAMETER_CATEGORIES, FHIR_PARAMETER_TYPES, Resource}
import io.onfhir.api.model.{FHIRRequest, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.authz.SmartAuthorizer._
import io.onfhir.config.IFhirConfigurationManager
import io.onfhir.exception.InitializationException
import org.json4s.{JObject, JString, JValue}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.{Failure, Success, Try}

/**
  * Created by tuncay on 2/27/2017.
  */
class SmartAuthorizer(smartAuthzConfig:Option[Config], fhirConfigurationManager: IFhirConfigurationManager)
  extends BaseAuthorizer(smartAuthzConfig.flatMap(c => Try(c.getString("further-constraints-path")).toOption)) {
  /**
   * The methods to handle user based scopes per resource type or all remaining "*"
   */
  private val userScopeHandling:Map[String, String] =
    smartAuthzConfig
      .flatMap(c => Try(c.getConfig("user-scope-handling")).toOption)
      .map(cnf =>
        cnf.entrySet().asScala.toSeq.map(e => e.getKey -> (cnf.getString(e.getKey) match {
          case "compartment" => "compartment"
          case "careteam" => "careteam"
          case _ => throw new InitializationException("Invalid configuration parameter value for 'user-scope-handling' for Smart-On-Fhir authorization! Use one of the followings; compartment | careteam ")
        }))
      ).map(_.toMap)
      .getOrElse(Map.empty)
  /**
   * The name of the claim to get the FHIR user (e.g. Practitioner/465131) that the end user corresponds
   * e.g. fhirUser, sub
   */
  private val fhirUserClaimName:String = smartAuthzConfig.flatMap(c => Try(c.getString("fhir-user-claim")).toOption).getOrElse("sub")
  /**
   * FHIR search expression parser to parse Smart-on-Fhir complex scopes like patient/Observation?category=...
   */
  private val fhirSearchParameterValueParser = new FHIRSearchParameterValueParser(fhirConfigurationManager.fhirConfig)

  /**
   * Default scopes assigned to any authenticated user
   */
  private val defaultScopes: Seq[WGHeartScope] =
    smartAuthzConfig
      .flatMap(c => Try(c.getStringList("default-scopes-for-any-user")).toOption.map(_.asScala.toSeq))
      .getOrElse(Nil)
      .map(s => SmartAuthorizer.parse(s, fhirSearchParameterValueParser).getOrElse(throw new InitializationException(s"Given default scope ${s} for Smart-on-Fhir authorization configuration is not valid!")))

  /**
   * Further Authorization Context parameters for tokens (JWT Token, Introspection response)
   *  @return
   */
  override def furtherParamsInAuthzContext:List[String] = List(PARAM_PATIENT_ID, PARAM_FHIR_USER)

  /**
   * Nothing is public
   * @param request FHIR request details
   * @return
   */
  def authorizeForPublic(request: FHIRRequest):AuthzResult =
    AuthzResult.failureInvalidRequest("")

  /**
    * Decide on authorization of interaction on resource type based on Smart Heart WG specifications
    * @param authzContext resolved Authorization Context
    * @param fhirRequest  FHIR request
    * @return Either rejection or success with a list of restrictions in the format of FHIR search query
    */
  override def authorize(authzContext: AuthzContext, fhirRequest:FHIRRequest):AuthzResult = {
    //Parse the scopes, if scope is not understood just skip it
    val scopes:Seq[WGHeartScope] =
      authzContext
        .scopes
        .map(s => SmartAuthorizer.parse(s, fhirSearchParameterValueParser))
        .filter(_.isDefined)
        .map(_.get) ++ defaultScopes //Add the default scopes

    //Validate the context according to requirements of Smart on FHIR
    if (!isAuthzContextValid(authzContext, scopes))
      AuthzResult.failureInsufficientScope("Invalid Smart-on-Fhir token: Information (e.g. patient parameter) is missing in token related with the given scopes!")

    fhirRequest.resourceType match {
      //If this is a system level FHIR interaction
      case None =>
        AuthzResult.undecided("Smart Authorization only authorizes FHIR type and instance interactions!")
      //Binary resources are handled specially
      case Some("Binary") if fhirRequest.getResolvedSecurityContext.isDefined =>
        val rtype = FHIRUtil.extractResourceType(fhirRequest.getResolvedSecurityContext.get)
        val fhirPathContext = AuthzManager.getFhirPathContext(fhirRequest, authzContext) - "request"
        //Authorize for that resource type e.g. DocumentReference
        authorizeForResourceType(rtype, fhirRequest.interaction, fhirRequest.resource, fhirPathContext, authzContext, scopes)
      //Type level interaction
      case Some(rtype) =>
        val fhirPathContext = AuthzManager.getFhirPathContext(fhirRequest, authzContext)
        authorizeForResourceType(rtype, fhirRequest.interaction, fhirRequest.resource, fhirPathContext, authzContext, scopes)
    }
  }

  /**
   *
   * @param rtype
   * @param interaction
   * @param resourceContent
   * @param fhirPathContext
   * @param authzContext
   * @param scopes
   */
  protected def authorizeForResourceType(rtype:String,
                                         interaction:String,
                                         resourceContent:Option[Resource],
                                         fhirPathContext:Map[String, JValue],
                                         authzContext: AuthzContext,
                                         scopes:Seq[WGHeartScope]):AuthzResult = {
    // Get only the related scopes with the resource type
    val relatedScopes: Seq[WGHeartScope] =
      scopes
        .filter(s => s.checkForResourceType(rtype))
    //Resolve effective scopes for the given FHIR interaction and resource type (combining scopes if possible)
    resolveEffectiveScopes(relatedScopes, rtype, interaction) match {
      //If there is no effective scopes
      case (Nil, Nil) =>
        AuthzResult.failureInsufficientScope(s"Not authorized for the FHIR interaction '${interaction}' on resource type '${rtype}'!")
      //If only main scopes
      case (mainEffectiveScopes, Nil) =>
        getConstraintsForMainScopes(rtype, interaction, resourceContent, fhirPathContext, mainEffectiveScopes, authzContext)
          .map(AuthzResult.filtering) //If there is any constraint, partial authorization
          .getOrElse(AuthzResult.success()) //Otherwise success
      //If only supportive scopes
      case (Nil, supportiveScopes) =>
        supportiveScopes
          .flatMap(s => resolveRestrictionParamForConfSensTypeScope(s)) match {
          case Nil => AuthzResult.success()
          case oth => AuthzResult.filtering(AuthzConstraints(Seq(oth.toList)))
        }
      //If both exists
      case (mainEffectiveScopes, supportiveScopes) =>
        val extraParams = supportiveScopes.flatMap(s => resolveRestrictionParamForConfSensTypeScope(s))
        getConstraintsForMainScopes(rtype, interaction, resourceContent, fhirPathContext, mainEffectiveScopes, authzContext)
          .map(ac =>
            AuthzResult.filtering(ac.copy(filters = ac.filters.map(query => query ++ extraParams)))
          )
          .getOrElse(
            extraParams match {
              case Nil => AuthzResult.success()
              case oth => AuthzResult.filtering(AuthzConstraints(Seq(oth.toList)))
            })

    }
  }


  /**
   * Get further content constraints defined
   * @param resourceType        Resource type that interaction is about e.g. Observation
   * @param interaction         FHIR interaction e.g. read, create, update, etc
   * @param resourceContent     The given resource content, or resolved
   * @param context             Context parameters for request
   * @return
   */
  private def getFurtherContentConstraints(resourceType:String,
                                           interaction:String,
                                           resourceContent:Option[Resource],
                                           context:Map[String, JValue]
                                          ):Seq[String] = {
    if(interaction == FHIR_INTERACTIONS.SEARCH)
      Nil
    else {
      val rules = getRelatedRules(resourceType, interaction, resourceContent, context)
      rules.flatMap(_.contentConstraint.getOrElse(Nil))
    }
  }

  /**
   * Return authorization constraints if the given scopes causes partial authorization, None if complete authorization (no constraints)
   * @param scopes          All effective main scopes
   * @param authzContext    Authorization context resolved
   * @return
   */
  private def getConstraintsForMainScopes(rtype:String,
                                          interaction:String,
                                          resourceContent:Option[Resource],
                                          fhirPathContext:Map[String, JValue],
                                          scopes:Seq[WGHeartScope],
                                          authzContext: AuthzContext):Option[AuthzConstraints] = {
    //As all the scopes are same type
    val filteringConstraints =
      scopes.head.ptype match {
        case PERMISSION_TYPE_SYSTEM => getConstraintsForSystemLevelScopes(scopes, authzContext)
        case PERMISSION_TYPE_USER => getConstraintsForUserLevelScopes(rtype, scopes, authzContext)
        case PERMISSION_TYPE_PATIENT => getConstraintsForPatientLevelScopes(scopes, authzContext)
      }

    //Get content constraints
    //e.g. for create or update --> check if Observation.practitioner is same with user
    val contentConstraints = getFurtherContentConstraints(rtype, interaction, resourceContent, fhirPathContext)

    if (filteringConstraints.isEmpty && contentConstraints.isEmpty)
      None
    else
      Some(AuthzConstraints(filteringConstraints, contentConstraints))
  }

  /**
   * Construct constraints for patient level scopes
   *
   * @param scopes       All effective patient level scopes related with resource type and interaction
   * @param authzContext Authorization context resolved
   * @return
   */
  private def getConstraintsForPatientLevelScopes(scopes: Seq[WGHeartScope], authzContext: AuthzContext): Seq[List[Parameter]] = {
    //Get the patient identifier from patient parameter in the given token
    val pid = getPatientId(authzContext)
    val filteringConstraints =
      scopes map {
        //If there is a single effective scope with no further query part, it is authorized so no further restriction
        //e.g. patient/Observation
        case WGHeartScope(_, rtype, _, None) =>
          List(fhirSearchParameterValueParser.constructCompartmentSearchParameter("Patient", pid, rtype))
        case WGHeartScope(_, rtype, _, Some(queryParams)) =>
          fhirSearchParameterValueParser.constructCompartmentSearchParameter("Patient", pid, rtype) +: queryParams
      }

    filteringConstraints
  }

  /**
   * Get constraints for user level scopes
   * @param scopes           All effective user level scopes related with resource type and interaction
   * @param authzContext     Authorization context resolved
   * @return
   */
  private def getConstraintsForUserLevelScopes(rtype:String, scopes:Seq[WGHeartScope], authzContext: AuthzContext) : Seq[List[Parameter]] = {
    //Currently user level scopes are implemented to access all patients (like system level scopes)
    val fhirUser = fhirUserClaimName match {
      case "sub" => authzContext.sub.get
      case oth => authzContext.getSimpleParam[String](oth).get
    }
    //Get the type and id of fhir user e.g. Practitioner, 23254354
    val (fhirUserType, fhirUserId) =
      Try(FHIRUtil
        .parseReferenceValue(fhirUser)).toOption.map(r => r._2 -> r._3)
        .getOrElse("Practitioner" -> fhirUser)

    def getCompartmentConstraints:Seq[List[Parameter]] = {
      fhirConfigurationManager
        .fhirConfig
        .compartmentRelations
        .get(fhirUserType)
        .flatMap(_.get(rtype)) match {
          case Some(_) =>
            scopes map {
              //If there is a single effective scope with no further query part, it is authorized so no further restriction
              //e.g. patient/Observation
              case WGHeartScope(_, rtype, _, None) =>
                List(fhirSearchParameterValueParser.constructCompartmentSearchParameter(fhirUserType, fhirUserId, rtype))
              case WGHeartScope(_, rtype, _, Some(queryParams)) =>
                fhirSearchParameterValueParser.constructCompartmentSearchParameter(fhirUserType, fhirUserId, rtype) +: queryParams
            }
          case None => throw new IllegalStateException(s"Compartment $fhirUserType is not defined for resource type $rtype although user level smart authorization requests compartment based authorization, check onfhir compartment configurations!")
      }
    }

    val filteringConstraints =
      userScopeHandling
        .get(rtype) match {
          // Compartment based authorization --> User will be restricted to Practitioner or RelatedPerson compartments
          case Some("compartment") => getCompartmentConstraints
          case Some("careteam") =>
            //TODO CareTeam based authorization --> User will be restricted with patients that user is in the care team of
            // Observation?patient:Patient._has:CareTeam:patient:participant=$fhirUserType/$fhirUserId
            throw new NotImplementedError()
          //If there is a compartment defined for that resource type for the user type and compartment based method is applied to all remaining
          case None if userScopeHandling.get("*").contains("compartment") &&
                            fhirConfigurationManager.fhirConfig.compartmentRelations.get (fhirUserType).exists(_.contains(rtype)) =>
            getCompartmentConstraints
          //If careteam is selected all the remaining resource types
          case None if userScopeHandling.get("*").contains("careteam") =>
            //TODO CareTeam based authorization --> User will be restricted with patients that user is in the care team of
            // Observation?patient:Patient._has:CareTeam:patient:participant=$fhirUserType/$fhirUserId
            throw new NotImplementedError()
          //No constraint apart from what is specified in query part
          case None =>
            scopes map {
              //If there is a single effective scope with no further query part, it is authorized so no further restriction
              //e.g. patient/Observation
              case WGHeartScope(_, _, _, None) => Nil
              case WGHeartScope(_, _, _, Some(queryParams)) => queryParams
            }
        }
    filteringConstraints
  }

  /**
   * Get constraints for system level scopes
   * @param scopes        All effective system level scopes related with resource type and interaction
   * @param authzContext  Authorization context resolved
   * @return
   */
  private def getConstraintsForSystemLevelScopes(scopes: Seq[WGHeartScope], authzContext: AuthzContext): Seq[List[Parameter]] = {
    val filteringConstraints =
      scopes flatMap {
        //If there is a single effective scope with no further query part, it is authorized so no further restriction
        //e.g. patient/Observation
        case WGHeartScope(_, _, _, None) => None
        case WGHeartScope(_, _, _, Some(queryParams)) => Some(queryParams)
      }

    filteringConstraints
  }


  /**
    * Resolve the search parameter to restrict the access to certain resources with tagged confidentiality or sensitivity
    * @param scope  Parsed scope
    * @return
    */
  private def resolveRestrictionParamForConfSensTypeScope(scope:WGHeartScope):Option[Parameter] = {
    scope match {
      //If all confidentiality or sensitivity are accessible return None (no constraint)
      case WGHeartScope(_, "*", _, _) => None
      //Otherwise restrict the access to resources tagged with given confidentiality codes
      case WGHeartScope(PERMISSION_TYPE_CONFIDENTIALITY, confidentiality, _, _) =>
        Some(Parameter(
          paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL,
          paramType = FHIR_PARAMETER_TYPES.TOKEN,
          name = "_security",
          valuePrefixList = confidentiality.split(",").map(c => "http://hl7.org/fhir/v3/Confidentiality|" + c).map(v => "" -> v)
        ))
      //Otherwise restrict the access to resources tagged with given sensitivity codes
      case WGHeartScope(PERMISSION_TYPE_SENSITIVITY, sensitivities, _, _) =>
        Some(Parameter(
          paramCategory = FHIR_PARAMETER_CATEGORIES.NORMAL,
          paramType = FHIR_PARAMETER_TYPES.TOKEN,
          name = "_security",
          valuePrefixList = sensitivities.split(",").map(c => "http://hl7.org/fhir/v3/ActCode|" + c).map(v => "" -> v)
        ))
    }
  }



  /**
    * Resolve the target main (patient | user | system) and supportive (conf | sens) Smart scopes related with the requested resourceType
    *
    * Scopes are resolved against the given resource type in order of patient, user and system
    * e.g. if patient has scope patient/Observation.* then we get that for effective scope although there may be other user or system scopes related with Observation
    *
    *
    * @param scopes           All scopes from the authorization context
    * @param resourceType     FHIR resource type interaction is on
    * @param fhirInteraction  FHIR interaction (create | upated | ...) See [[FHIR_INTERACTIONS]]
    * @return
    */
  private def resolveEffectiveScopes(scopes:Seq[WGHeartScope], resourceType:String, fhirInteraction:String):(Seq[WGHeartScope], Seq[WGHeartScope]) = {
    //Filter the scopes related with the given FHIR interaction
    val interactionRelatedScopes =
      scopes
        .filter(_.checkForInteraction(fhirInteraction))
        .map(s =>
          s.copy(permissions = Set(getPermissionForInteraction(fhirInteraction)))
        )
    //Extract effective patient based scopes
    val effectivePatientBasedScopes:Seq[WGHeartScope] =
      getEffectiveScopes(
        interactionRelatedScopes
          .filter(_.ptype == PERMISSION_TYPE_PATIENT)
          .map(s => s.copy(rtype = resourceType))
      )

    //If there is at least one patient based scope but FHIR Patient compartment is not configured for resource type
    if(effectivePatientBasedScopes.nonEmpty &&
        !fhirConfigurationManager.fhirConfig.compartmentRelations.get("Patient").exists(_.isDefinedAt(resourceType)))
      throw new InitializationException(s"FHIR Patient compartment for resource type $resourceType should be configured in order to authorize based on Smart patient based scopes")

    var mainScopes = effectivePatientBasedScopes
    //If there is no patient based scopes, check user level scopes
    if(mainScopes.isEmpty) {
      mainScopes =
        getEffectiveScopes(
          interactionRelatedScopes
            .filter(_.ptype == PERMISSION_TYPE_USER)
            .map(s => s.copy(rtype = resourceType))
        )
    }

    //If there is no user level scope, check system level scopes
    if(mainScopes.isEmpty){
      mainScopes =
        getEffectiveScopes(
          interactionRelatedScopes
            .filter(_.ptype == PERMISSION_TYPE_SYSTEM)
            .map(s => s.copy(rtype = resourceType))
        )
    }

    //Extract confidentiality or sensitivity scope if exists
    val supportiveScopes =
      (interactionRelatedScopes
        .filter(s => s.ptype == PERMISSION_TYPE_CONFIDENTIALITY) match {
          case Nil => Nil
          case Seq(single) => Seq(single)
          case multiple if multiple.exists(_.rtype == "*") => multiple.find(_.rtype == "*").toSeq
          case multiple => Seq(multiple.head.copy(rtype = multiple.map(_.rtype).mkString(",")))
        }) ++
      (interactionRelatedScopes
        .filter(s => s.ptype == PERMISSION_TYPE_SENSITIVITY) match {
          case Nil => Nil
          case Seq(single) => Seq(single)
          case multiple if multiple.exists(_.rtype == "*") => multiple.find(_.rtype == "*").toSeq
          case multiple => Seq(multiple.head.copy(rtype = multiple.map(_.rtype).mkString(",")))
      })
    mainScopes -> supportiveScopes
  }

  /**
   * Get the effective scopes by trying to combine the scopes
   * @param scopes  Smart scopes
   * @return
   */
  private def getEffectiveScopes(scopes: Seq[WGHeartScope]): Seq[WGHeartScope] = {
    scopes match {
      case Nil => Nil
      //If there is a single related scope regarding the resource type and interaction, use it
      case Seq(single) => Seq(single)
      //If there are multiple ones, try to combine them if possible
      case multiple =>
        multiple.foldLeft[Seq[WGHeartScope]](Nil) {
          //First query
          case (Nil, s) => Seq(s)
          case (acc, s) =>
            //Try to find a combination with one of the already combined set
            val (combined, rest) =
              acc.foldLeft[(Option[WGHeartScope], Seq[WGHeartScope])](None -> Nil) {
                case ((None, others), ms) =>
                  tryCombiningScopes(ms, s) match {
                    case Some(comb) => Some(comb) -> others
                    case None => (None, ms +: others)
                  }
                case ((some, others), ms) => (some, ms +: others)
              }
            combined match {
              //If a combination found
              case Some(newQ) => newQ +: rest // replace with merged query
              //Otherwise extend the acc
              case None => s +: acc // could not merge, keep as is
            }
        }
    }
  }

  /**
    * Check if the authorization context is as expected
    * @param authzContext   Authorization context
    * @param scopes         Current scopes
    * @return
    */
  private def isAuthzContextValid(authzContext: AuthzContext, scopes:Seq[WGHeartScope]):Boolean = {
    //If it includes patient scope and does not include patientId
    !(scopes.exists(s => s.ptype.equals(PERMISSION_TYPE_PATIENT)) && !authzContext.furtherParams.contains(PARAM_PATIENT_ID)) &&
    //If it includes user scope and does not include user id
    !(scopes.exists(s => s.ptype.equals(PERMISSION_TYPE_USER)) && authzContext.sub.isEmpty)
  }

  /**
    * Retrieve the patient id parameter from AuthzContext
    * @param authzContext authorization context
    * @return
    */
  private def getPatientId(authzContext:AuthzContext):String = {
    authzContext.getSimpleParam[String](PARAM_PATIENT_ID).get
  }
}


/**
 * Scope scheme defined by WG Heart specification (extended for FHIR operations e.g. patient/Patient.$everything)
 * See
 *   - Smart v1 : https://openid.net/specs/openid-heart-fhir-oauth2-1_0-2017-05-31.html
 *   - Smart v2:  https://openid.net/specs/openid-heart-fhir-oauth2-1_0.html or
 *     https://build.fhir.org/ig/HL7/smart-app-launch/scopes-and-launch-context.html#scopes-for-requesting-fhir-resources
 *
 * @param ptype       Scope type patient|user|conf|sens
 * @param rtype       Resource type or Confidentiality/Sensitivity
 * @param permissions Set of permissions on resource
 *                    e.g. .cruds -> [c, r, u, d, s]
 *                    e.g. .read -> [r, s]
 *                    e.g. .write -> [c, u, d]
 * @param query       For fine-grained Smart scopes, the query part
 *                    e.g. patient/Observation.rs?category=http://terminology.hl7.org/CodeSystem/observation-category|laboratory ->
 *                    category=http://terminology.hl7.org/CodeSystem/observation-category|laboratory
 */
case class WGHeartScope(
                         ptype: String, //Scope type patient|user|conf|sens
                         rtype: String, //Resource type or Confidentiality/Sensitivity
                         permissions: Set[String], //Permissions on resource e.g. .cruds ->
                         query: Option[List[Parameter]] = None
                       ) {
  /**
   * Check if scope is related with FHIR resource type
   * @param resourceType  FHIR resource type
   * @return
   */
  def checkForResourceType(resourceType:String):Boolean = {
    Set("*", PERMISSION_TYPE_CONFIDENTIALITY, PERMISSION_TYPE_SENSITIVITY).contains(rtype) ||
      rtype.equalsIgnoreCase(resourceType)
  }

  /**
   * Check if given FHIR interaction is related with the scope
   * @param fhirInteraction FHIR interaction
   * @return
   */
  def checkForInteraction(fhirInteraction:String):Boolean = {
    permissions.contains("*") ||
      (fhirInteraction match {
        case FHIR_INTERACTIONS.CREATE => permissions.contains("c")
        case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.PATCH  => permissions.contains("u")
        case FHIR_INTERACTIONS.DELETE => permissions.contains("d")
        case FHIR_INTERACTIONS.READ | FHIR_INTERACTIONS.VREAD => permissions.contains("r")
        case FHIR_INTERACTIONS.HISTORY_INSTANCE
             | FHIR_INTERACTIONS.SEARCH
             | FHIR_INTERACTIONS.HISTORY_TYPE => permissions.contains("s")
        //FHIR Operations
        case operation if operation.startsWith("$") => permissions.contains(operation)
        //Otherwise false
        case _ => false
      })
  }
}

object SmartAuthorizer {
  private val logger = LoggerFactory.getLogger("SmartAuthorizer")
  private val ScopeRegex = """^(patient|user|system|sens|conf)\/(\w+|\*)\.(read|write|\*|[cruds]+|\$[a-zA-Z0-9_.-]+)(\?.+)?$""".r

  //Permission types
  final val PERMISSION_TYPE_PATIENT = "patient"
  final val PERMISSION_TYPE_USER = "user"
  final val PERMISSION_TYPE_SYSTEM = "system"
  final val PERMISSION_TYPE_CONFIDENTIALITY = "conf"
  final val PERMISSION_TYPE_SENSITIVITY = "sens"
  //Claim name for patientId parameter defined in Smart on FHIR specs
  final val PARAM_PATIENT_ID = "patient"
  //FHIR user claim name
  final val PARAM_FHIR_USER = "fhirUser"
  /**
   * Parse a Smart-on-FHIR v1 or v2 scope string
   * e.g. patient/Observation.rs
   * e.g. user/Condition.read
   * @param scope Scope string
   * @param fhirSearchParameterValueParser
   * @return
   */
  def parse(scope: String, fhirSearchParameterValueParser:FHIRSearchParameterValueParser): Option[WGHeartScope] = scope match {
    //If query part exists, try to parse it
    case ScopeRegex(ptype, rtype, perms, q) if Option(q).isDefined =>
      Try(parseAndValidateFhirQueryPartForSmartScope(rtype, q, fhirSearchParameterValueParser)) match {
        case Success(parsedParams) =>
          Some(WGHeartScope(
                ptype = ptype,
                rtype = rtype,
                permissions = normalizePermissions(perms),
                query = Some(parsedParams)
              ))
        case Failure(ex) =>
          logger.warn(s"The query part of Smart scope $scope cannot be parsed, ignoring the scope!", ex)
          None
       }
    case ScopeRegex(ptype, rtype, perms, q) =>
      Some(WGHeartScope(
        ptype = ptype,
        rtype = rtype,
        permissions = normalizePermissions(perms),
        query = None
      ))
    case _ =>
      logger.debug("Ignoring scope as it is not a smart compliant scope!")
      None
  }

  def parse(scope: String): Option[WGHeartScope] = scope match {
    case ScopeRegex(ptype, rtype, perms, q) =>
      Some(WGHeartScope(
        ptype = ptype,
        rtype = rtype,
        permissions = normalizePermissions(perms),
        query =
          Option(q)
            .map(q =>
              throw new IllegalArgumentException("Smart scopes with query parts should be parsed with supplied search statement parser!")
            )
      ))
    case _ =>
      logger.debug("Ignoring scope as it is not a smart compliant scope!")
      None
  }

  /**
   *
   * @param queryPart
   * @return
   */
  private def parseAndValidateFhirQueryPartForSmartScope(rtype:String, queryPart:String, fhirSearchParameterValueParser:FHIRSearchParameterValueParser):List[Parameter] = {
    val filters = fhirSearchParameterValueParser.parseSearchParameters(rtype, Uri.Query(queryPart.drop(1)).toMultiMap)
    if(!filters
        .forall(p =>
          p.paramCategory == FHIR_PARAMETER_CATEGORIES.NORMAL &&
            Set(FHIR_PARAMETER_TYPES.TOKEN, FHIR_PARAMETER_TYPES.REFERENCE, FHIR_PARAMETER_TYPES.URI, FHIR_PARAMETER_TYPES.STRING).contains(p.paramType)
        )
    )
      throw new IllegalArgumentException(s"Currently only normal search parameters with reference,token,uri and string are supported within Smart scopes!")
    //If a parameter is used more than once with or without different suffixes/prefixes/values, throw exception
    if(filters.map(_.name).toSet.size != filters.map(_.name).length)
      throw new IllegalArgumentException(s"The query part should include a search parameter only once in Smart scopes!")

    filters
  }

  /**
   *
   * @param perms
   * @return
   */
  private def normalizePermissions(perms: String): Set[String] = perms match {
    case "*" => Set("*")
    case "read" => Set("r", "s")
    case "write" => Set("c", "u", "d")
    case op if op.head == '$' => Set(op)
    case letters => letters.toSet.map(c =>  "" + c)
  }

  def getPermissionForInteraction(fhirInteraction:String):String = {
        fhirInteraction match {
          case FHIR_INTERACTIONS.CREATE => "c"
          case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.PATCH => "u"
          case FHIR_INTERACTIONS.DELETE => "d"
          case FHIR_INTERACTIONS.READ | FHIR_INTERACTIONS.VREAD => "r"
          case FHIR_INTERACTIONS.HISTORY_INSTANCE
               | FHIR_INTERACTIONS.SEARCH
               | FHIR_INTERACTIONS.HISTORY_TYPE => "s"
          //FHIR Operations
          case operation if operation.startsWith("$") => operation
        }
  }

  /**
   * Try combining Smart scopes (only reference,token,uri,string type parameters are assumed to exist in Smart scopes)
   * This is only called for scopes that are targeting the same resource type and permission
   * If merge is possible, return the combined parameters
   * Otherwise return None
   * @param scope1  Smart scope 1 (on the same resource type and permission)
   * @param scope2  Smart Scope 2 (on the same resource type and permission)
   * @return
   */
  def tryCombiningScopes(scope1:WGHeartScope, scope2:WGHeartScope):Option[WGHeartScope] = {
    if (scope1.query.isEmpty)
      Some(scope1)
    else if (scope2.query.isEmpty)
      Some(scope2)
    else {
      val q1 = scope1.query.getOrElse(List.empty).map(p => (p.name -> p.suffix) -> p.valuePrefixList.map(_._2).toSet).toMap
      val q2 = scope2.query.getOrElse(List.empty).map(p => (p.name -> p.suffix) -> p.valuePrefixList.map(_._2).toSet).toMap
      //If both queries are on the same parameters
      if (q1.keySet == q2.keySet) {
        val paramSetsForQ1 =
          q1
            .keySet
            .map(k =>
              k -> (q1(k).diff(q2(k)), q1(k).intersect(q2(k)), q2(k).diff(q1(k)))
            )

        //Result set 1 includes the result set 2
        //e.g. ?category=lab,vitalsign vs ?category=lab
        if (paramSetsForQ1.forall(_._2._3.isEmpty))
          Some(scope1)
        //If result set 2 includes the result set 1
        //e.g. ?category=lab vs ?category=lab,vitalsign
        else if (paramSetsForQ1.forall(_._2._1.isEmpty))
          Some(scope2)
        //Otherwise
        else {
          paramSetsForQ1.filterNot(ps => ps._2._1.isEmpty && ps._2._3.isEmpty).toSeq match {
            //If they are completely exclusive in terms of values only on a single parameter, merge them
            //e.g.  ?category=lab vs ?category=vitalsign
            //e.g. ?category=lab,symptom vs ?category=vitalsign,symptom
            case Seq(k -> _) =>
              Some(
                scope1.copy(query = Some(
                  scope1.query.get.filterNot(p => p.name == k._1 && p.suffix == k._2) :+
                    scope1.query.get.find(p => p.name == k._1 && p.suffix == k._2).get.copy(valuePrefixList = q1(k).union(q2(k)).map(v => "" -> v).toSeq)
                ))
              )
            //Otherwise contradictory
            //e.g. ?category=lab,symptom&status=final  vs ?category=lab,vitalsign&status=final,draft
            case _ => None
          }
        }
      }
      //If query1 is subset of query2 in terms of parameters and suffixes
      //e.g. ?category=lab vs ?category=lab&status=final    --> ?category=lab
      else if (q1.keySet.subsetOf(q2.keySet)) {
        val paramSetsForQ1 =
          q1
            .keySet
            .map(k =>
              k -> (q1(k).diff(q2(k)), q1(k).intersect(q2(k)), q2(k).diff(q1(k)))
            )

        //Result set 1 includes the result set 2
        //e.g. ?category=lab vs ?category=lab&status=final
        //e.g. ?category=lab,vitalsign vs ?category=lab&status=final
        if (paramSetsForQ1.forall(ps => ps._2._3.isEmpty))
          Some(scope1)
        else
          None
      } else if (q2.keySet.subsetOf(q1.keySet)) {
        val paramSetsForQ2 =
          q2
            .keySet
            .map(k =>
              k -> (q1(k).diff(q2(k)), q1(k).intersect(q2(k)), q2(k).diff(q1(k)))
            )

        //Result set 1 includes the result set 2
        //e.g. ?category=lab vs ?category=lab&status=final
        //e.g. ?category=lab,vitalsign vs ?category=lab&status=final
        if (paramSetsForQ2.forall(ps => ps._2._1.isEmpty))
          Some(scope2)
        else
          None
      }
      //If queries are on different set of parameters, no merge
      //e.g. ?category=lab vs code=...
      else {
        None
      }
    }
  }
}
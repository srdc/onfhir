package io.onfhir.audit

import akka.http.scaladsl.model.StatusCode
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.{FHIR_INTERACTIONS, Resource}
import io.onfhir.authz.{AuthContext, AuthzContext}
import io.onfhir.config.{FhirConfigurationManager, OnfhirConfig}
import org.json4s.JObject
import org.json4s.JsonDSL._
/**
  * Created by tuncay on 5/15/2017.
  * Interface to create a FHIR AuditEvent for onFhir.io interactions (as a FHIR repository)
  */
trait IFhirAuditCreator {
  /**
    * Create base AuditEvent resource
    * @return
    */
  protected def createBaseAuditEventRecord():JObject = "resourceType" -> "AuditEvent"
  /**
    * Create FHIR Coding element
    * @param system Code system
    * @param code code
    * @return
    */
  protected def createCodingElement(system:String, code:String):JObject = ("system" -> system) ~ ("code" -> code)

  /**
    * Create FHIR Identifier element
    * @param system Code system
    * @param value identifier value
    * @return
    */
  protected def createIdentifierElement(system:String, value:String):JObject = ("system" -> system) ~ ("value" -> value)


  /**
    * Resolve Audit Event Action Code
    * @param fhirRequest FHIR Request
    * @return
    */
  protected def resolveAuditEventActionCode(fhirRequest: FHIRRequest):String = {
    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.CREATE => "C"
      case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.PATCH => "U"
      case FHIR_INTERACTIONS.DELETE => "D"
      case op if op.startsWith("$") => "E"
      case FHIR_INTERACTIONS.BATCH | FHIR_INTERACTIONS.TRANSACTION => "E"
      case _ => "R"
    }
  }

  /**
    * Resolve outcome description from Status code
    * @param sc HTTP Status Code
    * @return
    */
  protected def resolveAuditEventOutcomeDescription(sc:StatusCode):String = {
    s"${sc.intValue} ${sc.reason}: ${sc.defaultMessage}"
  }

  /**
    * Resolve query if exist
    * @param fhirRequest FHIR Request
    * @return
    */
  protected def resolveQueryPart(fhirRequest: FHIRRequest):Option[String] = {
    import java.util.Base64

    val queryPart = fhirRequest.ifNoneExist match {
      case None =>
        if(fhirRequest.interaction == FHIR_INTERACTIONS.SEARCH || fhirRequest.interaction == FHIR_INTERACTIONS.SEARCH_SYSTEM || fhirRequest.queryParams.nonEmpty) {
          fhirRequest.requestUri.replaceFirst(OnfhirConfig.fhirRootUrl, "") match {
            case "" => Some("/")
            case other => Some(other)
          }
        }
        else
          None
      case other => other
    }
    //Encode query
    queryPart.map(q => Base64.getEncoder.encodeToString(q.getBytes("UTF-8")))
  }

  /**
    * Resolve Outcome code from StatusCode
    * @param statusCode HTTP Status code
    * @return
    */
  protected def resolveAuditEventOutcomeCode(statusCode: StatusCode):String = {
    statusCode.intValue match {
      case succ if succ <300 && succ >= 200 => "0" //SUCCESS
      case mf if mf <500 && mf >= 400 => "4" //Minor Failure
      case sf if sf <600 && sf >= 500 => "8" //Serious Failure
      case _ =>  "12"
    }
  }

  /**
    * Extract agent info from Authentication and Authorization context
    * @param authContext Authentication Context
    * @param authzContext Authorization Context
    * @return AgentInfo object that describes the requestor (client + user)
    */
  protected def extractAgentInfoFromAuthzContext(authContext: AuthContext, authzContext: Option[AuthzContext]):AgentsInfo = {
    //Resolve roles of user
    val roles =
      OnfhirConfig.authzConfig
        .authorizationExtraClaimsRole
        .flatMap(roleClaimName => authzContext.flatMap(_.getListParam[String](roleClaimName))) //Get roles from the specified claim
        .getOrElse(Nil)
        .map(r => r.split("#")) //Try to resolve role system and code from a format like system#code
        .flatMap {
          case Array(system, code) => Some(Some(system) -> code)
          case Array(code) => Some(None -> code)
          case _ => None
        }

    //Resolve reference to FHIR Identity resource of user
    val subRef =
      OnfhirConfig.authzConfig
        .authorizationExtraClaimsFhirSubjectReference
          .flatMap(refClaimName => authzContext.flatMap(_.getSimpleParam[String](refClaimName)))

    //Resolve client's name from extra claims if exist
    val clientName =
      OnfhirConfig.authzConfig
        .authorizationExtraClaimsClientName
        .flatMap(clientNameClaim =>authzContext.flatMap(_.getSimpleParam[String](clientNameClaim)))
    //Return Agents Info
    AgentsInfo(
      userId = authzContext.flatMap(_.sub),
      refToIdentityResource = subRef,
      roles = roles,
      userName = authzContext.flatMap(_.username),
      clientId = authzContext.flatMap(_.clientId),
      clientName = clientName,
      networkAddress = authContext.networkAddress
    )
  }

  /**
    * Extract referecences to related resources for FHIRRequest
    * @param fhirRequest FHIR Request
    * @return
    */
  protected def extractRelatedResources(fhirRequest: FHIRRequest):Seq[String] = {
    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.READ | FHIR_INTERACTIONS.HISTORY_INSTANCE => Seq(fhirRequest.resourceType.get + "/" + fhirRequest.resourceId.get)
      case FHIR_INTERACTIONS.VREAD => Seq(fhirRequest.resourceType.get + "/" + fhirRequest.resourceId.get + "/_history/"+fhirRequest.versionId.get)
      case FHIR_INTERACTIONS.CREATE => fhirRequest.response.flatMap(r => r.location.map(_.toString().replace(OnfhirConfig.fhirRootUrl+"/", ""))).toSeq
      case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.DELETE | FHIR_INTERACTIONS.PATCH =>
        if(fhirRequest.resourceId.isDefined)
          Seq(fhirRequest.resourceType.get + "/" + fhirRequest.resourceId.get)
        else
          fhirRequest.response.flatMap(r => r.location.map(_.toString().replace(OnfhirConfig.fhirRootUrl+"/", ""))).toSeq
      case op if op.startsWith("$") =>
        if(fhirRequest.resourceId.isDefined)
          Seq(fhirRequest.resourceType.get + "/" + fhirRequest.resourceId.get)
        else
          Nil

      case FHIR_INTERACTIONS.TRANSACTION | FHIR_INTERACTIONS.BATCH =>
        Seq("Bundle/"+fhirRequest.id)
      case _ => Nil
    }
  }

  /**
    * Extract related patient references
    * TODO ??
    * @param fhirRequest FHIR Request
    * @param authzContext Authorization Request
    * @return
    */
  protected def extractRelatedPatientReferences(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext]):Seq[String] = {
    fhirRequest.compartmentType match {
      //If compartment search, we know the related patient
      case Some("Patient") => Seq("Patient" + "/" + fhirRequest.compartmentId.get)
      //Otherwise
      case _ =>
        if(fhirRequest.resourceType.isEmpty)
          Nil
        else {
          val patientIndicatedInAuthzContext:Option[String] = authzContext.flatMap(_.getSimpleParam[String]("patient"))
          patientIndicatedInAuthzContext match {
            case Some(pid) => Seq("Patient" + "/" +pid)
            case None => Nil
          }
        }
    }
  }

  /**
    * Create Audit resources for FHIR batch or transaction
    * @param fhirRequest    FHIR Request
    * @param authContext    Authentication Context
    * @param authzContext   Authorization Context
    * @param statusCode     HTTP Status Code for the result of operation
    * @return
    */
  def createAuditResourcesForBatchTransaction(fhirRequest: FHIRRequest,
                                             authContext: AuthContext,
                                             authzContext: Option[AuthzContext],
                                             statusCode: StatusCode):Seq[Resource] = {
    val batchTransactionAudit = createAuditResource(fhirRequest, authContext, authzContext, statusCode)
    //If it is failure only return this audit
    if(statusCode.isFailure())
      Seq(batchTransactionAudit)
    else {
      batchTransactionAudit +: fhirRequest.childRequests.filter(_.response.isDefined).map(crequest =>
        createAuditResource(crequest, authContext, authzContext, crequest.response.get.httpStatus, Some(fhirRequest.id))
      )
    }
  }

  /**
    * Create FHIR Audit resource for the interaction
    * @param fhirRequest  FHIRRequest
    * @param authContext Authentication Context
    * @param authzContext Authorization Context
    * @param statusCode HTTP Response Status Code
    * @param batchTransactionId If exists, the transaction/batch identifier that this is child request is bound to
    * @return
    */
  def createAuditResource(fhirRequest: FHIRRequest,
                          authContext: AuthContext,
                          authzContext: Option[AuthzContext],
                          statusCode: StatusCode,
                          batchTransactionId:Option[String] = None):Resource

}

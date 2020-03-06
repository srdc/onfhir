package io.onfhir.r4.audit

import java.time.Instant

import akka.http.scaladsl.model.StatusCode
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRRequest
import io.onfhir.audit.{AgentsInfo, IFhirAuditCreator}
import io.onfhir.authz.{AuthContext, AuthzContext}
import io.onfhir.config.OnfhirConfig
import io.onfhir.util.DateTimeUtil

import org.json4s.JsonAST.{JArray, JObject, JString}
import org.json4s.JsonDSL._

class R4AuditCreator extends IFhirAuditCreator {
  /**
    * Create FHIR Audit resource for the interaction
    *
    * @param fhirRequest        FHIRRequest
    * @param authContext        Authentication Context
    * @param authzContext       Authorization Context
    * @param statusCode         HTTP Response Status Code
    * @param batchTransactionId If exists, the transaction/batch identifier that this is child request is bound to
    * @return
    */
  override def createAuditResource(fhirRequest: FHIRRequest, authContext: AuthContext, authzContext: Option[AuthzContext], statusCode: StatusCode, batchTransactionId: Option[String]): Resource = {
    //Resolve agents
    val agentsInfo = extractAgentInfoFromAuthzContext(authContext, authzContext)
    val userAgent = createUserAgent(agentsInfo)
    val clientAgent = createClientAgent(agentsInfo)
    val anonymousAgent = createAnonymousAgent(agentsInfo)
    val receiverAgent =createReceiverAgent(agentsInfo)
    //Resolve entities
    val relatedResourceEntities = createRelatedResourceEntitities(fhirRequest)
    val relatedPatientEntities = createRelatedPatientEntitities(fhirRequest, authzContext)
    val queryEntity = createQueryEntity(fhirRequest)

    val allEntities =
      if(batchTransactionId.isDefined)
        relatedResourceEntities ++ relatedPatientEntities ++ queryEntity.toSeq :+ createRelatedBatchTransactionEntity(batchTransactionId.get)
      else
        relatedResourceEntities ++ relatedPatientEntities ++ queryEntity.toSeq

    //Construct audit record
    var auditRecord =
        createBaseAuditEventRecord() ~
        ("type" ->  createCodingElement("http://terminology.hl7.org/CodeSystem/audit-event-type", "rest")) ~
        ("subtype" -> Seq(createCodingElement( "http://hl7.org/fhir/restful-interaction", fhirRequest.interaction))) ~
        ("action" -> resolveAuditEventActionCode(fhirRequest)) ~
        ("recorded" -> DateTimeUtil.serializeInstant(Instant.now())) ~
        ("period" -> ("start" -> DateTimeUtil.serializeInstant(fhirRequest.requestTime)) ~ ("end" -> DateTimeUtil.serializeInstant(fhirRequest.responseTime.getOrElse(Instant.now)))) ~
        ("outcome" -> resolveAuditEventOutcomeCode(statusCode)) ~
        ("outcomeDesc" -> resolveAuditEventOutcomeDescription(statusCode)) ~
        ("agent" -> Seq(Some(receiverAgent), anonymousAgent, userAgent, clientAgent).flatten) ~
        ("source" ->  ("site" -> OnfhirConfig.fhirRootUrl ) ~ ("observer" -> ("display" -> "onFhir.io")))

    if(allEntities.nonEmpty)
      auditRecord = auditRecord ~ ("entity" -> allEntities)

    auditRecord



  }

  /**
    *
    * @param agentsInfo
    * @return
    */
  private def createUserAgent(agentsInfo:AgentsInfo): Option[JObject] = {
    agentsInfo.userId.map(uid => {
      var temp =
          ("altId" -> uid) ~
          ("requestor" -> true) ~
          ("network" ->
            ("address" -> agentsInfo.networkAddress) ~
              ("type" -> "2")
            )

      if(agentsInfo.userName.isDefined)
        temp = temp ~ ("name" -> agentsInfo.userName.get)

      if(agentsInfo.refToIdentityResource.isDefined)
        temp = temp ~ ("who" -> ("reference" -> agentsInfo.refToIdentityResource.get))

      if(agentsInfo.roles.nonEmpty)
        temp = temp ~ ("role" -> agentsInfo.roles.map {
          case (None, code) => "text" -> JString(code)
          case (Some(system), code) => "coding" -> JArray(List(createCodingElement(system, code)))
        })

      temp
    })
  }

  private def createClientAgent(agentsInfo:AgentsInfo): Option[JObject] = {
    agentsInfo.clientId.map(cid => {
      var temp =
        ("altId" -> cid) ~
          ("requestor" -> agentsInfo.userId.isEmpty) ~
          ("role" -> Seq("coding" -> Seq(createCodingElement("http://nema.org/dicom/dicm", "110153"))))

      if(agentsInfo.clientName.isDefined)
        temp = temp ~ ("name" -> agentsInfo.clientName.get)

      if(agentsInfo.userId.isEmpty)
        temp = temp ~ ("network" -> ("address" -> agentsInfo.networkAddress) ~ ("type" -> "2"))

      temp
    })
  }

  private def createAnonymousAgent(agentsInfo:AgentsInfo):Option[JObject] = {
    if(agentsInfo.userId.isEmpty && agentsInfo.clientId.isEmpty)
      Some(
        ("requestor" -> agentsInfo.userId.isEmpty) ~
        ("network" -> ("address" -> agentsInfo.networkAddress) ~ ("type" -> "2")) ~
        ("role" -> Seq("coding" -> Seq(createCodingElement("http://nema.org/dicom/dicm", "110153"))))
      )
    else None
  }

  private def createReceiverAgent(agentsInfo:AgentsInfo):JObject = {
      ("name" -> OnfhirConfig.serverName) ~
      ("requestor" -> false) ~
      ("network" -> ("address" -> OnfhirConfig.fhirRootUrl) ~ ("type" -> "2")) ~
      ("role" -> Seq("coding" -> Seq(createCodingElement("http://nema.org/dicom/dicm", "110152"))))
  }

  private def createQueryEntity(fhirRequest:FHIRRequest):Option[JObject] = {
    resolveQueryPart(fhirRequest).map(query =>
        ("query" -> query) ~
        ("type" ->
          fhirRequest.resourceType
            .map(rt =>  createCodingElement("http://hl7.org/fhir/resource-types", rt))
            .getOrElse(createCodingElement("http://terminology.hl7.org/CodeSystem/audit-entity-type", "2"))) ~
        ("role" -> createCodingElement("http://terminology.hl7.org/CodeSystem/object-role", "24"))
    )
  }

  private def createRelatedResourceEntitities(fhirRequest:FHIRRequest):Seq[JObject] ={
    extractRelatedResources(fhirRequest).map(rref => {
      ("what" -> ("reference" -> rref)) ~
        ("type" ->
          fhirRequest.resourceType
            .map(rt =>  createCodingElement("http://hl7.org/fhir/resource-types", rt))
            .getOrElse(createCodingElement("http://terminology.hl7.org/CodeSystem/audit-entity-type", "2"))) ~
        ("role" -> createCodingElement("http://terminology.hl7.org/CodeSystem/object-role", "4"))
    })
  }

  private def createRelatedPatientEntitities(fhirRequest:FHIRRequest, authzContext: Option[AuthzContext]):Seq[JObject] = {
    extractRelatedPatientReferences(fhirRequest, authzContext).map(rref =>
      ("what" -> ("reference" -> rref)) ~
        ("type" -> createCodingElement("http://terminology.hl7.org/CodeSystem/audit-entity-type", "1")) ~
        ("role" -> createCodingElement("http://terminology.hl7.org/CodeSystem/object-role", "1"))
    )
  }

  private def createRelatedBatchTransactionEntity(batchTransactionId:String):JObject = {
    ("what" -> ("reference" -> ("Bundle/"+batchTransactionId) )) ~
      ("type" -> createCodingElement("http://terminology.hl7.org/CodeSystem/audit-entity-type", "4")) ~
      ("role" -> createCodingElement("http://terminology.hl7.org/CodeSystem/object-role", "21"))
  }
}

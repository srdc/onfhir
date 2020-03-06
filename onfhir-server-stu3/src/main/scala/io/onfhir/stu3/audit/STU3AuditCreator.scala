package io.onfhir.stu3.audit

import java.time.Instant

import akka.http.scaladsl.model.StatusCode
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRRequest
import io.onfhir.audit.{AgentsInfo, IFhirAuditCreator}
import io.onfhir.authz.{AuthContext, AuthzContext}
import io.onfhir.config.OnfhirConfig
import io.onfhir.util.DateTimeUtil
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

/**
  * Created by tuncay on 5/15/2017.
  */
class STU3AuditCreator extends IFhirAuditCreator {
  /**
    * Create STU3 AuditEvent for FHIR interactions on FHIR repository
    * @param fhirRequest FHIRRequest object indicating the request and response
    * @param authContext Authentication Context
    * @param authzContext Authorization Context
    * @param statusCode Resulting HTTP StatusCode
    * @return AuditEvent in JSON4s format
    */
  def createAuditResource(fhirRequest: FHIRRequest,
                          authContext: AuthContext,
                          authzContext: Option[AuthzContext],
                          statusCode: StatusCode,
                          batchTransactionId:Option[String] = None):Resource = {

    //Resolve agents
    val agentsInfo = extractAgentInfoFromAuthzContext(authContext, authzContext)
    val userAgent = createUserAgent(agentsInfo)
    val clientAgent = createClientAgent(agentsInfo)
    val anonymousAgent = createAnonymousAgent(agentsInfo)
    val receiverAgent =createReceiverAgent(agentsInfo)
    //Resolve entities
    val relatedResourceEntities = createRelatedResourceEntitities(fhirRequest)
    val relatedPatientEntities = createRelatedPatientEntitities(fhirRequest, authzContext)
    val allEntities =
      if(batchTransactionId.isDefined)
        relatedResourceEntities ++ relatedPatientEntities :+ createRelatedBatchTransactionEntity(batchTransactionId.get)
      else
        relatedResourceEntities ++ relatedPatientEntities

    //Construct audit record
    var auditRecord = createBaseAuditEventRecord() ~
      ("type" ->  createCodingElement("http://hl7.org/fhir/audit-event-type","rest")) ~
      ("subtype" -> createCodingElement( "http://hl7.org/fhir/restful-interaction", fhirRequest.interaction)) ~
      ("action" -> resolveAuditEventActionCode(fhirRequest)) ~
      ("recorded" -> DateTimeUtil.serializeInstant(Instant.now())) ~
      ("outcome" -> resolveAuditEventOutcomeCode(statusCode)) ~
      ("outcomeDesc" -> resolveAuditEventOutcomeDescription(statusCode)) ~
      ("agent" -> Seq(Some(receiverAgent), anonymousAgent, userAgent, clientAgent).flatten) ~
      ("source" -> ("site" -> OnfhirConfig.fhirRootUrl))

    if(allEntities.nonEmpty)
      auditRecord ~ ("entity" -> allEntities)

    auditRecord
  }

  private def createUserAgent(agentsInfo:AgentsInfo): Option[JObject] = {
    agentsInfo.userId.map(uid => {
      var temp =
        ("userId" -> createIdentifierElement(OnfhirConfig.authzConfig.authzServerMetadata.issuer, uid)) ~
          ("requestor" -> true) ~
          ("network" ->
            ("address" -> agentsInfo.networkAddress) ~
              ("type" -> "2")
            )

      if(agentsInfo.userName.isDefined)
        temp = temp ~ ("name" -> agentsInfo.userName.get)

      if(agentsInfo.refToIdentityResource.isDefined)
        temp = temp ~ ("reference" -> ("reference" -> agentsInfo.refToIdentityResource.get))

      if(agentsInfo.roles.nonEmpty)
        temp = temp ~ ("role" -> agentsInfo.roles.map {
          case (None, code) => JObject("text" -> JString(code))
          case (Some(system), code) => createCodingElement(system, code)
        })

      temp
    })
  }

  private def createClientAgent(agentsInfo:AgentsInfo): Option[JObject] = {
    agentsInfo.clientId.map(cid => {
      var temp =
        ("userId" -> createIdentifierElement(OnfhirConfig.authzConfig.authzServerMetadata.issuer, cid)) ~
          ("requestor" -> agentsInfo.userId.isEmpty) ~
          ("role" -> Seq(createCodingElement("http://nema.org/dicom/dicm", "110153")))

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
          ("role" -> Seq(createCodingElement("http://nema.org/dicom/dicm", "110153")))
      )
    else None
  }

  private def createReceiverAgent(agentsInfo:AgentsInfo):JObject = {
    ("name" -> OnfhirConfig.serverName) ~
      ("requestor" -> false) ~
      ("network" -> ("address" -> OnfhirConfig.fhirRootUrl) ~ ("type" -> "2")) ~
      ("role" -> Seq(createCodingElement("http://nema.org/dicom/dicm", "110152")))
  }


  private def createQueryEntity(fhirRequest:FHIRRequest):Option[JObject] = {
    resolveQueryPart(fhirRequest).map(query =>
      ("query" -> query) ~
        ("type" ->
          fhirRequest.resourceType
            .map(rt =>  createCodingElement("http://hl7.org/fhir/resource-types", rt))
            .getOrElse(createCodingElement("http://hl7.org/fhir/audit-entity-type", "2"))) ~
        ("role" -> createCodingElement("http://hl7.org/fhir/object-role", "24"))
    )
  }

  private def createRelatedResourceEntitities(fhirRequest:FHIRRequest):Seq[JObject] ={
    extractRelatedResources(fhirRequest).map(rref => {
      ("reference" -> ("reference" -> rref)) ~
        ("type" ->
          fhirRequest.resourceType
            .map(rt =>  createCodingElement("http://hl7.org/fhir/resource-types", rt))
            .getOrElse(createCodingElement("http://hl7.org/fhir/audit-entity-type", "2"))) ~
        ("role" -> createCodingElement("http://hl7.org/fhir/object-role", "4"))
    })
  }

  private def createRelatedPatientEntitities(fhirRequest:FHIRRequest, authzContext: Option[AuthzContext]):Seq[JObject] = {
    extractRelatedPatientReferences(fhirRequest, authzContext).map(rref =>
      ("reference" -> ("reference" -> rref)) ~
        ("type" -> createCodingElement("http://hl7.org/fhir/audit-entity-type", "1")) ~
        ("role" -> createCodingElement("http://hl7.org/fhir/object-role", "1"))
    )
  }

  private def createRelatedBatchTransactionEntity(batchTransactionId:String):JObject = {
    ("reference" -> ("reference" -> ("Bundle/"+batchTransactionId) )) ~
      ("type" -> createCodingElement("http://hl7.org/fhir/audit-entity-type", "4")) ~
      ("role" -> createCodingElement("http://hl7.org/fhir/object-role", "21"))
  }

  /*/**
    * Create a DSTU3 AuditEvent
    * @param interaction Name of the FHIR interaction
    * @param auditEventAction Action e.g. C, R, U, D
    * @param auditEventOutcome The outcome of event
    * @param patientIds Ids of patients that are related with this event (Use * for all)
    * @param agentsInfo Information about the accessor (user and client system)
    * @param query The fhir query if this event is about search
    * @param resourceType Type of resource that this event is about
    * @param resourceReferences References to resources that this event is about
    * @return
    */
  def createAuditResource(
                          interaction:String,
                          auditEventAction:AuditEvent.AuditEventAction,
                          auditEventOutcome:AuditEvent.AuditEventOutcome,
                          auditEventOutcomeDescription:Option[String] = None,
                          patientIds:Set[String] = Set.empty,
                          agentsInfo: AgentsInfo,
                          query:Option[String] = None,
                          resourceType:String,
                          resourceReferences:Seq[String] = Nil):IBaseResource = {
    val auditEvent = new AuditEvent
    //It is a Rest interaction, fixed
    auditEvent.setType(AUDIT_EVENT_TYPE_REST)
    //Sub type; the FHIR interaction
    auditEvent.addSubtype(new Coding().setSystem("http://hl7.org/fhir/restful-interaction").setCode(interaction))
    auditEvent.setAction(auditEventAction)
    //Time of event, now
    auditEvent.setRecorded(new Date())
    //Outcome of event
    auditEvent.setOutcome(auditEventOutcome)
    if(auditEventOutcomeDescription.isDefined)
      auditEvent.setOutcomeDesc(auditEventOutcomeDescription.get)
    //TODO Purpose of event

    if(agentsInfo.userId.isDefined) {
      //Agent - User
      val userAgent = new AuditEventAgentComponent()
      //If user is an outside user (Not have a identity resource in fhir repository)
      userAgent.setUserId(new Identifier().setSystem(AuthzConfig.authzServerMetadata.issuer).setValue(agentsInfo.userId.get))
      if (agentsInfo.refToIdentityResource.isDefined)
        userAgent.getReference.setReference(agentsInfo.refToIdentityResource.get)
      //Name of user
      if (agentsInfo.userName.isDefined)
        userAgent.setName(agentsInfo.userName.get)
      //User is the initiator
      userAgent.setRequestor(true)
      //Network address IP
      userAgent.getNetwork.setAddress(agentsInfo.networkAddress)
      userAgent.getNetwork.setType(AuditEventAgentNetworkType._2)
      agentsInfo.roles.foreach(role => {
        userAgent.getRole().add(new CodeableConcept().addCoding(new Coding().setSystem(role._1).setCode(role._2)))
      })
      auditEvent.addAgent(userAgent)
    }

    if(agentsInfo.clientId.isDefined) {
      //Agent - Client
      val clientAgent = new AuditEventAgentComponent()
      clientAgent.setUserId(new Identifier().setSystem(AuthzConfig.authzServerMetadata.issuer).setValue(agentsInfo.clientId.get))
      if (agentsInfo.clientName.isDefined)
        clientAgent.setName(agentsInfo.clientName.get)

      if(agentsInfo.userId.isDefined) {
        clientAgent.setRequestor(false)
      }else{
        clientAgent.setRequestor(true)
        //Network address IP
        clientAgent.getNetwork.setAddress(agentsInfo.networkAddress)
        clientAgent.getNetwork.setType(AuditEventAgentNetworkType._2)
      }
      clientAgent.getRole.add(new CodeableConcept().addCoding(new Coding().setCode("110153").setSystem("http://nema.org/dicom/dicm")))
      auditEvent.addAgent(clientAgent)
    }
    //If we have no information
    if(agentsInfo.userId.isEmpty && agentsInfo.clientId.isEmpty){
      val anonymousAgent =  new AuditEventAgentComponent()
      anonymousAgent.setRequestor(true)
      anonymousAgent.getNetwork.setAddress(agentsInfo.networkAddress)
      anonymousAgent.getNetwork.setType(AuditEventAgentNetworkType._2)
      anonymousAgent.getRole.add(new CodeableConcept().addCoding(new Coding().setCode("110153").setSystem("http://nema.org/dicom/dicm")))
      auditEvent.addAgent(anonymousAgent)
    }
    // FHIR Repository itself as the default receiver agent
    val receiverAgent = new AuditEventAgentComponent()
    receiverAgent.setRequestor(false)
    receiverAgent.getRole.add(new CodeableConcept().addCoding(new Coding().setCode("110152").setSystem("http://nema.org/dicom/dicm")))
    receiverAgent.setName(Config.serverName)
    receiverAgent.getNetwork.setAddress(Config.fhirRootUrl)
    receiverAgent.getNetwork.setType(AuditEventAgentNetworkType._2)
    auditEvent.addAgent(receiverAgent)

    //Source of event is our URL
    auditEvent.getSource.getIdentifier.setValue(Config.fhirRootUrl)

    //Patient Entities if exist
    patientIds.foreach(pid => {
      val patientEntity = new AuditEventEntityComponent()
      patientEntity.getReference.setReference(s"${Config.fhirRootUrl}/Patient/${pid}")
      patientEntity.setType(new Coding().setCode("1").setSystem("http://hl7.org/fhir/audit-entity-type"))
      patientEntity.setRole(new Coding().setCode("1").setSystem("http://hl7.org/fhir/object-role"))
      auditEvent.addEntity(patientEntity)
    })

    //Resource entities if exist
    resourceReferences.foreach(ref => {
      val resourceEntity = new AuditEventEntityComponent()
      resourceEntity.getReference.setReference(ref)
      resourceEntity.setType(new Coding().setCode(resourceType).setSystem("http://hl7.org/fhir/resource-types"))
      resourceEntity.setRole(new Coding().setCode("4").setSystem("http://hl7.org/fhir/object-role"))
      auditEvent.addEntity(resourceEntity)
    })
    //If this is a search
    query.foreach(q => {
      val resourceEntity = new AuditEventEntityComponent()
     // val r = new String(Base64.encodeBase64(q.getBytes("UTF-8")), "UTF-8")
      resourceEntity.setQuery(q.getBytes("UTF-8"))
      resourceEntity.setType(new Coding().setCode(resourceType).setSystem("http://hl7.org/fhir/resource-types"))
      resourceEntity.setRole(new Coding().setCode("24").setSystem("http://hl7.org/fhir/object-role"))
      auditEvent.addEntity(resourceEntity)
    })

    auditEvent
  }*/
}

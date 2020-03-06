package io.onfhir.audit

import java.util.concurrent.{ConcurrentLinkedQueue, TimeUnit}

import akka.actor.{Actor, ActorSystem, Cancellable, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import akka.http.scaladsl.server.{Directive0, Directives}
import io.onfhir.Onfhir
import io.onfhir.api.{FHIR_INTERACTIONS, Resource}
import io.onfhir.api.model.{FHIRRequest, FHIRResponse}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.audit.AuditManager.{AuditEventLog, FlushAudits}
import io.onfhir.authz._
import io.onfhir.config.OnfhirConfig
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.FhirConfigurationManager.fhirAuditCreator
import io.onfhir.db.ResourceManager
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonDSL._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

/**
  * Created by tuncay on 5/6/2017.
  */
class AuditManager(customAuditHandler:Option[ICustomAuditHandler]) extends Actor {
  //Actor system
  implicit val actorSystem:ActorSystem = Onfhir.actorSystem
  implicit val executionContext = context.dispatcher // actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")

  //Logger for actor
  private val logger: Logger = LoggerFactory.getLogger("AuditManager")

  //For remote audit repositories, thr queue of audits in AuditEvent format
  val remoteAudits = new ConcurrentLinkedQueue[Resource]()

  // Periodic schedule for sending audits as batch
  var scheduledRemoteAuditSender:Option[Cancellable] = None

  /**
    * Before starting this actor
    */
  override def preStart() {
    //If it is remote auditing, schedule batch auditing with given interval
    if(OnfhirConfig.fhirAuditingRepository == "remote" && OnfhirConfig.fhirAuditingRepositoryUrl.isDefined) {
      logger.info(s"Scheduling remote batch auditing service with interval; '${OnfhirConfig.fhirAuditingRemoteBatchInterval}' minutes ... ")
      scheduledRemoteAuditSender = Some(actorSystem.scheduler.schedule(FiniteDuration.apply(OnfhirConfig.fhirAuditingRemoteBatchInterval, TimeUnit.MINUTES), FiniteDuration.apply(OnfhirConfig.fhirAuditingRemoteBatchInterval, TimeUnit.MINUTES), self, FlushAudits()))
    }
  }

  /**
    * After starting this actor
    */
  override def postStop() {
    //Cancel the scheduler, if audit manager is stopped
    logger.info("Stopping AuditManager and related scheduler...")
    scheduledRemoteAuditSender.foreach(_.cancel())
  }

  /**
    * Events for the actor
    * @return
    */
  override def receive: Receive = {
    //Single audit
    case ael: AuditEventLog =>
      createAndStoreAudit(ael.fhirRequest, ael.authContext, ael.authzContext, ael.httpResponse)
    //Command to send all accumulated audits
    case fa:FlushAudits =>
      sendBatchAudits()
  }


  /**
    * For remote auditing, we need access token to reach audit server
    */
  lazy val accessTokenManager =
    new TokenClient(
      clientId = OnfhirConfig.authzConfig.protectedResourceInformation.getID.getValue,
      clientSecret = OnfhirConfig.authzConfig.protectedResourceInformation.getSecret.getValue,
      requiredScopes = Seq("user/AuditEvent.write"),
      authzServerMetadata = OnfhirConfig.authzConfig.authzServerMetadata
    )


  /**
    * Batch Audit sender
    * Collects the current content of syhnronized queue and send the audits in a single batch request
    */
  def sendBatchAudits():Unit = {
    val numOfAudits = remoteAudits.size()
    logger.info(s"Executing batch auditing for '$numOfAudits' audit records ...")
    if(numOfAudits != 0) {
      val audits = (0 until numOfAudits).map(_ => remoteAudits.poll())
      //Create a batch request for all audits
      val auditBundle = FHIRUtil.createTransactionBatchBundle("batch", audits.map(audit =>
        ("resource" -> audit) ~
          ("request" -> ("method" -> "POST") ~ ("url" -> "AuditEvent"))
      ))

      val response = Http().singleRequest(createAuditBatchCreationRequest(OnfhirConfig.fhirAuditingRepositoryUrl.get, auditBundle))

      response.onComplete {
        case Success(res) => logger.debug(s"$numOfAudits audits successfully delivered")
        case Failure(fres) =>  logger.error(s"Problem in audit sending; ${fres.toString}")
      }
    } else {
      logger.info("No audits to send!...")
    }
    Unit
  }

  /**
    * Create a HTTP Request for a batch audit creation for a remote audit repository
    * @param auditRepositoryUrl FHIR URL for AuditRepository
    * @param bundle FHIR Bundle of audits
    * @return
    */
  def createAuditBatchCreationRequest(auditRepositoryUrl:String, bundle:Resource):HttpRequest = {
    var request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$auditRepositoryUrl",
      entity =  HttpEntity(ContentTypes.`application/json`, bundle.toJson)//.serializeResourceToOriginalJson(bundle, true))
    )

    if(OnfhirConfig.fhirAuditingRepositoryIsSecure) {
      //Try to get the access token from Authorization Manager
      request = accessTokenManager.getToken match {
        case Some(token) => //request ~> addCredentials(new OAuth2BearerToken(token))
          request.withHeaders(Authorization.apply(OAuth2BearerToken(token)))
        case None => request
      }
    }
    request
  }

  /**
    * Create a HTTP Request for individual audit creation for a remote audit repository
    * @param auditRepositoryUrl FHIR URL for AuditRepository
    * @param auditRecord FHIR Audit record
    * @return
    */
  def createAuditCreationRequest(auditRepositoryUrl:String, auditRecord:Resource):HttpRequest = {
    var request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$auditRepositoryUrl/AuditEvent",
      entity =  HttpEntity(ContentTypes.`application/json`, auditRecord.toJson)
    )

    if(OnfhirConfig.fhirAuditingRepositoryIsSecure) {
      //Try to get the access token from Authorization Manager
      request = accessTokenManager.getToken match {
        case Some(token) => //request ~> addCredentials(new OAuth2BearerToken(token))
          request.withHeaders(Authorization.apply(OAuth2BearerToken(token)))
        case None => request
      }
    }
    request
  }

  /**
    * Create an Audit record and store locally and sent to configured Audit Record Server according to configuration
    * @param fhirRequest FHIRRequest to be audited
    * @param authContext Authentication context
    * @param authzContext Authorization context
    * @param httpResponse HTTP Response returned
    * @return
    */
  def createAndStoreAudit(fhirRequest: FHIRRequest, authContext: AuthContext, authzContext: Option[AuthzContext], httpResponse: HttpResponse):Future[Unit] = {
    Future.apply {
      try {
        //logger.debug(s"Creating an audit record for request $fhirRequest ...")

        val auditCreationJobs = customAuditHandler match {
          case Some(cah) =>
            Seq(cah.createAndSendAudit(fhirRequest, authContext, authzContext, httpResponse))
          case None =>
            OnfhirConfig.fhirAuditingRepository match {
              case AuditManager.AUDITING_METHOD_LOCAL | AuditManager.AUDITING_METHOD_REMOTE =>
                val auditRecords =
                  fhirRequest.interaction match {
                    case FHIR_INTERACTIONS.BATCH | FHIR_INTERACTIONS.TRANSACTION =>
                      fhirAuditCreator.createAuditResourcesForBatchTransaction(fhirRequest, authContext, authzContext, httpResponse.status)
                    case _ if fhirRequest.interaction != FHIR_INTERACTIONS.CREATE || !fhirRequest.resourceType.contains("AuditEvent") =>
                      Seq(fhirAuditCreator.createAuditResource(fhirRequest, authContext, authzContext, httpResponse.status))
                    case  _ =>
                      Nil
                  }
                //Saving the audits to local FHIR repo
                if(OnfhirConfig.fhirAuditingRepository == "local")
                  auditRecords.map(auditRecord => {
                    ResourceManager.createResource("AuditEvent", auditRecord)
                    //new FHIRCreateService().performCreate(auditRecord, fhirConfig.FHIR_AUDIT_EVENT)
                  })
                else //Sending the audits to a remote FHIR repo
                  auditRecords.map(auditRecord => {
                    remoteAudits.add(auditRecord)
                    //Send the audits if it exceeds the batch size
                    if (remoteAudits.size() > OnfhirConfig.fhirAuditingRemoteBatchSize)
                      sendBatchAudits()
                    Future(Unit)
                  })
              case any =>
                logger.error(s"Unknown default audit mode '$any'")
                Seq(Future(Unit))
            }
        }

        Future.sequence(auditCreationJobs).map(responses => {
          responses.foreach {
            case fr: FHIRResponse => if (!fr.httpStatus.isSuccess) logger.error(s"Problem in audit storage!")
            case hr: HttpResponse => if (!hr.status.isSuccess) logger.error(s"Problem in audit sending; ${hr.toString}")
            case br:Boolean => if(!br) logger.error(s"Problem in custom audit sending!")
            case _ =>
          }
          Future(Unit)
        })
      } catch {
        case e:Exception =>
          logger.error("Unexpected error while creating/sending audit", e)
          Future(Unit)
      }
    }
  }
}

/**
  * Companion manager for audits
  */
object AuditManager {
  final val AUDITING_METHOD_NONE = "none"     //No auditing
  final val AUDITING_METHOD_LOCAL = "local"   //Audits (FHIR AuditEvent) are stored to local repository
  final val AUDITING_METHOD_REMOTE = "remote" //Audits (FHIR AuditEvent) are sent to a remote FHIR repository
  //Name for the actor
  final val ACTOR_NAME = "audit-manager"

  /**
    * Internal model for Audit Agents (parties that take role)
    * @param userId                 User identifier accessing the user
    * @param refToIdentityResource  Reference to the Identity resource corresponding to user e.g. Practitioner/....
    * @param roles                  Roles of the user (system,code)
    * @param userName               Name of the user
    * @param clientId               Identifier for the client system
    * @param clientName             Name of the client system
    * @param networkAddress         Network IP address where the user is accessing to the resources
    */
  case class AgentsInfo(
                         userId:Option[String], //
                         refToIdentityResource:Option[String], //
                         roles:Seq[(Option[String], String)], //
                         userName:Option[String], //
                         clientId:Option[String], //
                         clientName:Option[String], //
                         networkAddress:String) //

  /**
    * Event class for AuditManager actor to create audit records for request
    * @param fhirRequest  FHIR Request to be audited
    * @param authContext  Authentication context
    * @param authzContext Authorization Context
    * @param httpResponse HTTPResponse returned
    */
  case class AuditEventLog(fhirRequest: FHIRRequest, authContext: AuthContext, authzContext: Option[AuthzContext], httpResponse:HttpResponse)


  /**
    * Event class for AuditManager actor to flush all audits cached until now for batch processing
    */
  case class FlushAudits()
  /**
    * Audit the interaction result
    * @param fhirRequest  FHIRRequest to be audited
    * @param authContext  Authentication Context
    * @param authzContext AuthorizationContext
    * @return
    */
  def audit(fhirRequest: FHIRRequest, authContext: AuthContext, authzContext: Option[AuthzContext]): Directive0 =
    Directives.mapResponse { httpResponse ⇒
      //Create and Store audit
      if(!OnfhirConfig.fhirAuditingRepository.equalsIgnoreCase(AUDITING_METHOD_NONE))
        Onfhir.actorSystem.actorSelection(s"/user/$ACTOR_NAME") ! AuditEventLog(fhirRequest, authContext, authzContext, httpResponse)
      //Return HttpResponse as it is
      httpResponse
    }

  /**
    * Props for Actor
    * @return
    */
  def props(customAuditHandler:Option[ICustomAuditHandler] = None) = Props(new AuditManager(customAuditHandler))

}

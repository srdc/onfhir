package io.onfhir.db

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, Props}
import io.onfhir.Onfhir
import io.onfhir.api.FHIR_COMMON_FIELDS
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.DBConflictManager.RealizeConflictCheck
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.equal
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.{Duration, FiniteDuration}

class DBConflictManager extends Actor {
  implicit val executionContext = context.dispatcher

  //Logger for actor
  private val logger: Logger = LoggerFactory.getLogger("DBConflictManager")

  /**
    * Actor events
    * @return
    */
  override def receive: Receive = {
    case r: RealizeConflictCheck =>
      checkupAndCleanHistory(r.rtype, r.rid, r.version)
  }

  /**
    * Checkup if there is a problem in persistency (same version is in current and history due to a crash)
    *
    * @param rtype   Resource type
    * @param rid     Resource id
    * @param version Old document version to check
    */
  private def checkupAndCleanHistory(rtype: String, rid: String, version: String): Future[Unit] = {
    DocumentManager.getDocument(rtype, rid, excludeExtraFields = false).flatMap {
      case None => Future.unit
      case Some(doc) =>
        if (version == doc.get(FHIR_COMMON_FIELDS.META).get.asDocument().get(FHIR_COMMON_FIELDS.VERSION_ID).asString().getValue) {
          MongoDB.getCollection(rtype, history = true)
            .deleteMany(
                Filters.and(
                  Filters.equal(FHIR_COMMON_FIELDS.ID, rid),
                  Filters.equal(FHIR_COMMON_FIELDS.META + "." + FHIR_COMMON_FIELDS.VERSION_ID, version),
                )
              ).head().map(dr => {
              if(dr.getDeletedCount == 1)
                logger.info(s"Inconsistent history document with id $rid and version $version is deleted from history collection $rtype!")
              else
                logger.error(s"Problem while deleting inconsistent history document with id $rid and version $version from history collection $rtype!")
          })
        } else {
          Future.unit
        }
    }
  }
}

object DBConflictManager {
  final val ACTOR_NAME = "db-conflict-manager"
  /**
    * Schedule a conflict check on collections for resource type with the given id and version
    * @param rtype    Resource type to check
    * @param rid      Resource id to check
    * @param version  Version to check
    */
  case class RealizeConflictCheck(rtype:String, rid:String, version:String)

  def props() = Props(new DBConflictManager())

  def scheduleCheckAndCleanupForHistory(rtype:String, rid:String, version:String)(implicit executionContext: ExecutionContext):Unit = {
    val duration = FiniteDuration.apply(OnfhirConfig.fhirRequestTimeout.getSeconds + 10, TimeUnit.SECONDS)
    val actorRef:ActorRef = Onfhir.apply().dbConflictManager.get
    Onfhir.actorSystem.scheduler.scheduleOnce(duration,  actorRef, RealizeConflictCheck(rtype, rid, version))
  }
}

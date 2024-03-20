package io.onfhir.async

import akka.actor.{Actor, PoisonPill, Props, ReceiveTimeout}
import akka.pattern.pipe
import io.onfhir.api.Resource
import io.onfhir.api.client.OnFhirLocalClient
import io.onfhir.api.model.FHIRResponse
import io.onfhir.api.util.FHIRUtil
import io.onfhir.async.BulkImportJobHandler._
import io.onfhir.config.OnfhirConfig
import io.onfhir.util.JsonFormatter
import org.slf4j.{Logger, LoggerFactory}

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.{DAYS, Duration, HOURS}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source


class BulkImportJobHandler(job:BulkImportJob) extends Actor {
  //Logger for actor
  private val logger: Logger = LoggerFactory.getLogger("BulkImportJobHandler")
  var sourceIndex = -1
  var sourceItr:Iterator[Resource]#GroupedIterator[Resource] = _
  var groupCount:Int = _
  var lineIndex = 0

  var isCompleted:Boolean = false
  var resourcesImported:ListBuffer[Long] = new ListBuffer[Long]();
  var errorResponses:Seq[FHIRResponse] = _

  implicit val ec:ExecutionContext = context.system.dispatcher
  /**
   * Actor behaviour
   * @return
   */
  override def receive: Receive = {
    case StartImportJob if sourceIndex == -1 =>
      logger.debug(s"Start bulk import job ${job.jobId} from source ${job.sourceDetails.rootUri} for ${job.sources.length} ndjson files ...")
      self ! StartImportForSource(0)
    case StartImportJob =>
      //Do nothing
    case StartImportForSource(i) =>
      sourceIndex = i
      val (rtype, url) = job.sources.apply(sourceIndex)
      logger.debug(s"Starting bulk import from source ${url} for job ${job.jobId} ...")
      val finalUri =
          //Absolute path
          if(Paths.get(url).isAbsolute)
            new URI(url)
          else  //Relative path
            Path.of(job.sourceDetails.rootUri.getPath, url).toUri

      val bufferedSource = Source.fromFile(finalUri, "UTF-8")
      groupCount = if(rtype == "Bundle") 10 else OnfhirConfig.bulkNumResourcesPerGroup
      sourceItr =
        bufferedSource
          .getLines() //Read the lines one by one
          .map(json => JsonFormatter.parseFromJson(json).parseJson) // Parse them
          .grouped(groupCount)
      if(sourceItr.hasNext)
        self ! HandleNextGroup

    /**
     * Start importing next group of resources in parallel
     */
    case HandleNextGroup =>
      val resources = sourceItr.next()
      val jobForGroup =
        Future.sequence(
          job.sources.apply(sourceIndex)._1 match {
            case "Bundle" =>
                resources
                  .map(r =>
                    FHIRUtil.extractValue[String](r, "type") match {
                      case "transaction" => OnFhirLocalClient.transaction().entriesFromBundle(r).execute()
                      case "batch" => OnFhirLocalClient.batch().entriesFromBundle(r).execute()
                    }
                  )
            case rtype =>
              //Special upsert without checking previous versions (for performance)
              if(OnfhirConfig.bulkUpsertMode)
                Seq(resources.foldLeft(OnFhirLocalClient.bulkUpsert(rtype))((rb, r) => rb.upsert(r)).execute())
              else
                Seq(resources.foldLeft(OnFhirLocalClient.batch())((rb, r) => rb.entry(_.update(r))).execute())
          }
        ).map(HandleGroupResponse)
      jobForGroup.pipeTo(self)
    //If all of them is successfull
    case HandleGroupResponse(responses) if responses.forall(!_.isError) =>
      val (rtype, url) = job.sources.apply(sourceIndex)
      lineIndex += groupCount
      logger.debug(s"Bulk import completed for source ${url} for ${lineIndex} $rtype resources in total for job ${job.jobId} ...")
      //If there are more resources, continue with next group
      if(sourceItr.hasNext)
        self ! HandleNextGroup
      else {
        resourcesImported.append(lineIndex)
        //If all sources are completed
        if(sourceIndex - 1 == job.sources.length) {
          isCompleted = true
          context.setReceiveTimeout(Duration.apply(1, HOURS))
        } else {
          lineIndex = 0
          self ! StartImportForSource(sourceIndex+1)
        }
      }
    //If some of them is problematic
    case HandleGroupResponse(responses) =>
      val (rtype, url) = job.sources.apply(sourceIndex)
      errorResponses = responses.filter(_.isError)
      logger.debug(s"Bulk import completed with error for source ${url} for ${errorResponses.length} $rtype resources for job ${job.jobId} at lines between $lineIndex - ${lineIndex + responses.length}!")
      lineIndex += responses.count(!_.isError)
      resourcesImported.append(lineIndex)
      isCompleted = true
      context.setReceiveTimeout(Duration.apply(1, HOURS))
    // Stop itself
    case ReceiveTimeout =>
      context.setReceiveTimeout(Duration.Undefined)
      self ! PoisonPill
  }
}

object BulkImportJobHandler {
  trait Command
  trait Response

  case object StartImportJob extends Command

  private case class StartImportForSource(sourceIndex:Int) extends Command
  private case object HandleNextGroup
  private case class HandleGroupResponse(responses:Seq[FHIRResponse]) extends Command

  case class BulkImportJob(jobId:String, sourceDetails:UriSource, sources:Seq[(String, String)])

  case class UriSource(rootUri: URI, basicHttpCredential: Option[String] = None)

  /**
   *
   * @param job
   * @return
   */
  def props(job:BulkImportJob) = Props(new BulkImportJobHandler(job))
}
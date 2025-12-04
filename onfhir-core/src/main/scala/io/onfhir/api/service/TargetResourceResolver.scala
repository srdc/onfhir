package io.onfhir.api.service

import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.directives.{BasicDirectives, RouteDirectives}
import akka.http.scaladsl.server.directives.FutureDirectives.onComplete
import io.onfhir.Onfhir
import io.onfhir.api.{FHIR_INTERACTIONS, Resource}
import io.onfhir.api.model.{FHIRRequest, FhirLiteralReference, FhirUUIDReference}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.TransientRejection
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Success, Try}
import io.onfhir.config.IFhirConfigurationManager

/**
 * Handles the resolution of target FHIR resources mentioned in FHIR requests
 * e.g. FHIR Update --> previous version of the resource
 */
class TargetResourceResolver(fhirConfigurationManager: IFhirConfigurationManager) {
  //Execution context
  implicit val executionContext: ExecutionContextExecutor = Onfhir.actorSystem.dispatcher

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Directive to resolve target FHIR resources that request is referring if applicable and set the resolved resource
   * within FHIR request. If not resolved, just skip
   * @param fhirRequest FHIR request
   * @return
   */
  def resolveTargetResource(fhirRequest:FHIRRequest):Directive0 = {
    if(Set(FHIR_INTERACTIONS.BATCH, FHIR_INTERACTIONS.TRANSACTION).contains(fhirRequest.interaction)){
      onComplete(resolveTargetResourcesForBatchOrTransaction(fhirRequest))
        .flatMap {
          case Success(chidRequests) =>
            fhirRequest.childRequests = chidRequests
            BasicDirectives.pass
          case scala.util.Failure(ex) =>
            logger.error("Exception while resolving target resources!", ex)
            RouteDirectives.reject(TransientRejection(s"Problem while resolving target resources in batch/transaction request!", ex))
        }
    } else if(!isResolvable(fhirRequest))
      BasicDirectives.pass
    else {
      onComplete(resolveTargetResourceOrVersion(fhirRequest)).flatMap {
        //If it is resolved, set it to request
        case Success(Some(resource -> sc)) =>
          fhirRequest.setResolvedTargetResource(resource, sc)
          BasicDirectives.pass
        //If the mentioned resource is not resolved, ignore it
        case Success(None) => BasicDirectives.pass
        //Any failure
        case scala.util.Failure(ex) =>
          logger.error("Exception while resolving target resource!", ex)
          RouteDirectives.reject(TransientRejection(s"Problem while resolving target resource ${fhirRequest.resourceType.get}/${fhirRequest.resourceId.get}!", ex))
      }
    }
  }

  /**
   * Resolve target resource for FHIR request and update the request
   * @param fhirRequest FHIR request details
   * @return
   */
  def resolveTargetResourceUpdateRequest(fhirRequest: FHIRRequest): Future[Unit] = {
    if (Set(FHIR_INTERACTIONS.BATCH, FHIR_INTERACTIONS.TRANSACTION).contains(fhirRequest.interaction)) {
      resolveTargetResourcesForBatchOrTransaction(fhirRequest)
        .map(childRequests => fhirRequest.childRequests = childRequests)
    } else if (!isResolvable(fhirRequest))
      Future.apply(())
    else {
      resolveTargetResourceOrVersion(fhirRequest)
        .map(rOpt => rOpt.foreach(r => fhirRequest.setResolvedTargetResource(r._1, r._2)))
    }
  }

  /**
   *
   * @param fhirRequest
   * @return
   */
  private def isResolvable(fhirRequest:FHIRRequest):Boolean =
    fhirRequest.resourceType.isDefined &&
      fhirRequest.resourceId.isDefined &&
        fhirConfigurationManager.fhirConfig.resourceConfigurations.contains(fhirRequest.resourceType.get) //If resource type is supported


  /**
   *
   * @param fhirRequest
   * @return
   */
  private def resolveTargetResourcesForBatchOrTransaction(fhirRequest:FHIRRequest):Future[Seq[FHIRRequest]] = {
    val parentRequest = if(fhirRequest.interaction == FHIR_INTERACTIONS.TRANSACTION) Some(fhirRequest) else None
    Future.sequence(fhirRequest
      .childRequests
      .map(cr =>
        if (isResolvable(cr))
          resolveTargetResourceOrVersion(cr, parentRequest)
            .recover(_ => None) //do not throw any exception
            .map(resolved => {
              resolved
                .foreach(r => cr.setResolvedTargetResource(r._1, r._2))
              cr
            })
        else
          Future.apply(cr)
      ))
  }


  /**
   * Resolve target FHIR resource mentioned in request
   * @param fhirRequest FHIR request
   * @return  Target resource resolved (and for special resources like Binary, the security context resource related with it)
   */
  private def resolveTargetResourceOrVersion(fhirRequest:FHIRRequest, parentTransactionRequest:Option[FHIRRequest] = None):Future[Option[(Resource, Option[Resource])]] = {
    (fhirRequest.versionId match {
      case None => fhirConfigurationManager.resourceManager.getResource(fhirRequest.resourceType.get, fhirRequest.resourceId.get)
      case Some(v) => fhirConfigurationManager.resourceManager.getResource(fhirRequest.resourceType.get, fhirRequest.resourceId.get, Some(v))
    })
      .flatMap {
        case Some(resolvedResource) =>
          fhirRequest.resourceType.get match {
            //For Binary resources we get security context from content
            case "Binary" =>
              Try(FHIRUtil.parseReference(resolvedResource \ "securityContext"))
                .toOption
                .map {
                  //Resolve it from the resource manager
                  case FhirLiteralReference(_, sctype, scid, v) => fhirConfigurationManager.resourceManager.getResource(sctype, scid, v)
                  //Resolve it from the bundle
                  case FhirUUIDReference(urnUuid) =>
                    Future.apply(
                      parentTransactionRequest
                        .flatMap(ptr => ptr.childRequests.find(_.id == urnUuid))
                        .flatMap(r => r.resource)
                    )
                  case _ => Future.apply(None)
                }
                .getOrElse(Future.apply(None))
                .map(securityContext => Some(resolvedResource -> securityContext))
            case _ =>
              Future.apply(Some(resolvedResource -> None))
          }
        case _ =>   Future.apply(None)
      }
  }
}

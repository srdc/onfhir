package io.onfhir.api.service

import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.directives.{BasicDirectives, RouteDirectives}
import akka.http.scaladsl.server.directives.FutureDirectives.onComplete
import io.onfhir.Onfhir
import io.onfhir.api.{FHIR_INTERACTIONS, Resource}
import io.onfhir.api.model.FHIRRequest
import io.onfhir.exception.TransientRejection
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Success
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
        case Success(Some(resource)) =>
          fhirRequest.setResolvedTargetResource(resource)
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
   *
   * @param fhirRequest
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
        .map(rOpt => rOpt.foreach(r => fhirRequest.setResolvedTargetResource(r)))
    }
  }

  private def isResolvable(fhirRequest:FHIRRequest):Boolean =
    fhirRequest.resourceType.isDefined &&
      fhirRequest.resourceId.isDefined &&
        fhirConfigurationManager.fhirConfig.resourceConfigurations.contains(fhirRequest.resourceType.get) //If resource type is supported


  private def resolveTargetResourcesForBatchOrTransaction(fhirRequest:FHIRRequest):Future[Seq[FHIRRequest]] = {
    Future.sequence(fhirRequest
      .childRequests
      .map(cr =>
        if (isResolvable(cr))
          resolveTargetResourceOrVersion(cr)
            .recover(_ => None) //do not throw any exception
            .map(resolved => {
              resolved
                .foreach(r => cr.setResolvedTargetResource(r))
              cr
            })
        else
          Future.apply(cr)
      ))
  }


  /**
   * Resolve target FHIR resource mentioned in request
   * @param fhirRequest FHIR request
   * @return
   */
  private def resolveTargetResourceOrVersion(fhirRequest:FHIRRequest):Future[Option[Resource]] = {
      fhirRequest.versionId match {
        case None => fhirConfigurationManager.resourceManager.getResource(fhirRequest.resourceType.get, fhirRequest.resourceId.get)
        case Some(v) =>
          fhirConfigurationManager.resourceManager.getResource(fhirRequest.resourceType.get, fhirRequest.resourceId.get, Some(v))
      }
  }
}

package io.onfhir.subscription.cache

import akka.cluster.ddata.{ORMap, ORMapKey, ORSet, ORSetKey, SelfUniqueAddress}
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator, ReplicatorMessageAdapter}
import io.onfhir.config.SearchParameterConf
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.cluster.ddata.Replicator.WriteMajority

import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}
import io.onfhir.subscription.OnFhirClient
import akka.http.scaladsl.unmarshalling.Unmarshal
import io.onfhir.api.model.InternalEntity
import io.onfhir.subscription.model.{CborSerializable, SearchParameterConfWrapper}
import io.onfhir.util.InternalJsonMarshallers._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}

object DistributedSearchParameterConfCache {

  sealed trait Command extends CborSerializable
  sealed trait Response extends CborSerializable
  //Commands
  case class SyncSearchParameterConfs(rtype:String, spc:Set[String], replyTo:ActorRef[Response]) extends Command
  case class GetSearchParameterConfs(rtype:String, sp:Set[String], replyTo:ActorRef[Response]) extends Command
  //Responses
  case class SyncSearchParameterConfsResponse(result:Boolean) extends Response
  case class GetSearchParamConfsResponse(sp:Map[String, SearchParameterConf]) extends Response

  trait InternalCommand extends Command
  case class InternalCacheUpdateResponse(rsp: Replicator.UpdateResponse[ORMap[String,SearchParameterConfWrapper]], replyTo:ActorRef[Response]) extends InternalCommand
  case class InternalSubscribeResponseForResourceTypes(chg: Replicator.SubscribeResponse[ORSet[String]]) extends InternalCommand
  case class InternalSubscribeResponseForSearchParams(chg: Replicator.SubscribeResponse[ORMap[String,SearchParameterConfWrapper]]) extends InternalCommand
  case class InternalSaveSearchParameterConfs(rtype:String, spc:Seq[SearchParameterConf], replyTo:ActorRef[Response]) extends InternalCommand
  case class InternalSendSyncFailureResponse(rtype:String, ex:Throwable, replyTo:ActorRef[Response]) extends InternalCommand

  case class InternalGetResponseForSearchParams(rtype:String, rsp:Replicator.GetResponse[ORMap[String, SearchParameterConfWrapper]], sps:Set[String], replyTo:ActorRef[Response], isSync:Boolean) extends InternalCommand

  /**
   *
   * @param onFhirClient
   * @return
   */
  def apply(onFhirClient: OnFhirClient): Behavior[Command] =  running(onFhirClient, Map.empty[String, Map[String, SearchParameterConf]])

  /**
   *
   * @param onFhirClient
   * @param searchParamsUsed
   * @return
   */
  def running(
              onFhirClient: OnFhirClient,
              searchParamsUsed:Map[String, Map[String, SearchParameterConf]],
              ): Behavior[Command] = {
    Behaviors.setup { ctx =>
      implicit val node: SelfUniqueAddress = DistributedData(ctx.system).selfUniqueAddress

        DistributedData.withReplicatorMessageAdapter[Command, ORMap[String, SearchParameterConfWrapper]] { replicatorAdapterForSpc =>

          Behaviors.receiveMessage[Command] {
            /**
             * Synchronize the given search parameter configurations (retrieve from onFHIR write to distributed cache) and return if sync is successful
             */
            case SyncSearchParameterConfs(rtype, spc, replyTo) =>
              //Check local
              val notFoundKeys = spc.diff(searchParamsUsed.get(rtype).map(_.keySet).getOrElse(Set.empty[String]))
              //If all found there, return true
              if (notFoundKeys.isEmpty) {
                replyTo ! SyncSearchParameterConfsResponse(true)
              } else {
                ctx.log.debug(s"Search parameter configurations ${notFoundKeys.mkString(",")} for $rtype not found in local cache for synchronization, looking in akka-distributed cache!")
                //Otherwise check distributed cache
                replicatorAdapterForSpc.askGet(
                  askReplyTo => Replicator.Get(getSearchParametersKey(rtype), Replicator.ReadLocal, askReplyTo),
                  value => InternalGetResponseForSearchParams(rtype, value, notFoundKeys, replyTo, isSync = true)
                )
              }
              Behaviors.same

            /**
             * Retrieve given search parameter configurations for the resource type
             */
            case GetSearchParameterConfs(rtype, sps, replyTo) =>
              //Try to found from local cache
              val foundParams = searchParamsUsed.get(rtype).map(spm => spm.filterKeys(sps.contains)).getOrElse(Map.empty[String, SearchParameterConf])
              if (foundParams.size == sps.size)
                replyTo ! GetSearchParamConfsResponse(foundParams)
              else {
                ctx.log.debug(s"Requested search parameter configurations for $rtype not found in local cache, looking in akka-distributed cache!")
                //ask to distributed cache if not exist
                replicatorAdapterForSpc.askGet(
                  askReplyTo => Replicator.Get(getSearchParametersKey(rtype), Replicator.ReadLocal, askReplyTo),
                  value => InternalGetResponseForSearchParams(rtype, value, sps, replyTo, isSync = false)
                )
              }
              Behaviors.same

            /**
             * Internal commands
             */
            //Called when cache is updated with the new search parameter configurations
            case InternalCacheUpdateResponse(rsp@Replicator.UpdateSuccess(s), replyTo) =>
              ctx.log.debug("Synchronization ok with distributed cache!")
              replyTo ! SyncSearchParameterConfsResponse(true)
              Behaviors.same
            case InternalCacheUpdateResponse(_, replyTo) =>
              ctx.log.warn("Problem while updating distributed cache for search parameter configuration synchronization!")
              replyTo ! SyncSearchParameterConfsResponse(false)
              Behaviors.same

            case InternalSendSyncFailureResponse(rtype, ex: Throwable, replyTo) =>
              replyTo ! SyncSearchParameterConfsResponse(false)
              ctx.log.error(s"Problem while synchronizing search parameter configurations for $rtype from onFHIR.io cluster!", ex)
              Behaviors.same

            //Handling new search parameter configuration coming from distributed cache
            case InternalSubscribeResponseForSearchParams(chg@Replicator.Changed(key)) =>
              val rtype = parseSearchParametersKey(key.id)
              ctx.log.debug(s"New search parameters for $rtype in distributed cache, updating local cache...")
              running(onFhirClient, searchParamsUsed.+(rtype -> chg.dataValue.entries.mapValues(_.sp)))

            //Save the search parameter configurations to cache
            case InternalSaveSearchParameterConfs(rtype, spc, replyTo) =>

              val key = getSearchParametersKey(rtype)
              replicatorAdapterForSpc.askUpdate(
                askReplyTo =>
                  Replicator.Update(key, ORMap.empty[String, SearchParameterConfWrapper], WriteMajority(60 seconds), askReplyTo)
                  (sp => spc.foldLeft(sp)((m, s) => m :+ s.pname -> SearchParameterConfWrapper(s))),
                rsp => InternalCacheUpdateResponse(rsp, replyTo)
              )

              Behaviors.same

            //Retrieved from cache, update the local map
            case InternalGetResponseForSearchParams(rtype, rsp@Replicator.GetSuccess(found), sps, replyTo, isSync) =>
              val allSearchParams = rsp.dataValue.entries.mapValues(_.sp)
              val foundParams = allSearchParams.filterKeys(sps.contains)

              //Subscribe to the resource type, if it is not in local map
              subscribeToResourceTypeIfFirst(ctx, searchParamsUsed, replicatorAdapterForSpc, rtype)

              if (isSync) {
                //Remaining parameters found in the distributed cache
                if (foundParams.size == sps.size) {
                  ctx.log.debug("Remaining search parameter configurations found in distributed cache, synchronization ok...")

                  replyTo ! SyncSearchParameterConfsResponse(true)
                  running(onFhirClient, searchParamsUsed.+(rtype -> allSearchParams))
                } else {
                  //Otherwise try to get from onFHIR.io cluster
                  ctx.pipeToSelf(retrieveSearchParameterConfs(ctx, onFhirClient, rtype, sps)) {
                    case Success(found) => InternalSaveSearchParameterConfs(rtype, found, replyTo)
                    case Failure(exception) => InternalSendSyncFailureResponse(rtype, exception, replyTo)
                  }
                  Behaviors.same
                }
              } else {
                ctx.log.debug(s"${foundParams.size} search parameter configurations found in distributed cache, ${sps.size} was requested!")
                //If this is just for retrieving from cache
                replyTo ! GetSearchParamConfsResponse(foundParams)
                //Update the local map
                running(onFhirClient, searchParamsUsed.+(rtype -> allSearchParams))
              }
            //If there is nothing in cache yet for resource type
            case InternalGetResponseForSearchParams(rtype, rsp@Replicator.NotFound(_), sps, replyTo, isSync) =>
              //Subscribe to the resource type
              subscribeToResourceTypeIfFirst(ctx, searchParamsUsed, replicatorAdapterForSpc, rtype)
              if(isSync){
                //try to get from onFHIR.io cluster
                ctx.pipeToSelf(retrieveSearchParameterConfs(ctx, onFhirClient, rtype, sps)) {
                  case Success(found) => InternalSaveSearchParameterConfs(rtype, found, replyTo)
                  case Failure(exception) => InternalSendSyncFailureResponse(rtype, exception, replyTo)
                }
              }  else {
                ctx.log.error(s"Problem while retrieving search parameter configurations for $rtype from cache!")
                replyTo ! GetSearchParamConfsResponse(Map.empty[String, SearchParameterConf])
              }
              Behaviors.same
            case InternalGetResponseForSearchParams(rtype, _, sps, replyTo, isSync) =>
              ctx.log.error(s"Problem while retrieving search parameter configurations for $rtype from cache!")
              if (isSync)
                replyTo ! SyncSearchParameterConfsResponse(false)
              else {
                val foundParams = searchParamsUsed.get(rtype).map(spm => spm.filterKeys(sps.contains)).getOrElse(Map.empty[String, SearchParameterConf])
                replyTo ! GetSearchParamConfsResponse(foundParams)
              }
              Behaviors.same
          }
      }
    }
  }

  private def subscribeToResourceTypeIfFirst(ctx:ActorContext[Command], searchParamsUsed:Map[String, Map[String, SearchParameterConf]], replicatorMessageAdapter: ReplicatorMessageAdapter[Command, ORMap[String, SearchParameterConfWrapper]], rtype:String) = {
    if(!searchParamsUsed.contains(rtype)) {
      ctx.log.debug(s"Subscribing to $rtype for search parameter configurations ...")
      replicatorMessageAdapter.subscribe(getSearchParametersKey(rtype), InternalSubscribeResponseForSearchParams)
    }
  }

  /**
   * Cache key for search parameter configurations for given resource type
   * @param rtype
   * @return
   */
  private def getSearchParametersKey(rtype:String):ORMapKey[String, SearchParameterConfWrapper] = {
    ORMapKey[String, SearchParameterConfWrapper](s"searchparams:$rtype")
  }

  /**
   * Parse cache key to retrieve resource type
   * @param id
   * @return
   */
  private def parseSearchParametersKey(id:String):String = {
    id.split(':').last
  }


  /**
   * Retrieve search parameter configurations from OnFhir server
   * @param rtype   Resource type
   * @param sps     Name of search parameters to retrieve
   * @return
   */
  private def retrieveSearchParameterConfs(ctx:ActorContext[Command], onFhirClient: OnFhirClient, rtype:String, sps:Set[String]):Future[Seq[SearchParameterConf]] = {
    ctx.log.debug(s"Synchronizing search parameter configurations for resource type $rtype and parameters ${sps.mkString(",")} ...")
    implicit val executionContext = ctx.system.executionContext
    implicit  val materializer = ctx.system
    val request =
      HttpRequest(HttpMethods.GET).withUri(s"/onfhir/internal/searchparameters/$rtype?pname=${sps.mkString(",")}")

    onFhirClient
      .sendRequest(request)
      .flatMap {
        case resp @ HttpResponse(StatusCodes.OK, _, _ , _ ) =>
          Unmarshal(resp.entity.httpEntity).to[Seq[InternalEntity]].map(_.map(_.asInstanceOf[SearchParameterConf]))
        case resp @ HttpResponse(code, _, _, _) =>
          ctx.log.error("OnFhir retrieve search parameter configuration for resource type {} and parameters {} request failed, response code: {} ", rtype, sps.mkString(","), code)
          resp.discardEntityBytes()
          throw new RuntimeException(s"OnFhir retrieve search parameter configuration for resource type $rtype and parameters ${sps.mkString(",")} request failed, response code: $code !")
      }
  }

}

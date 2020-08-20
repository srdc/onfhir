package io.onfhir.subscription.cache
import java.util.concurrent.ConcurrentHashMap

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}

import collection.JavaConverters._
import io.onfhir.config.SearchParameterConf
import io.onfhir.subscription.OnFhirClient
import akka.http.scaladsl.common.EntityStreamingSupport
import akka.http.scaladsl.common.JsonEntityStreamingSupport
import akka.http.scaladsl.unmarshalling.Unmarshal
import io.onfhir.subscription.util.JsonSupport

import scala.concurrent.Future
import scala.util.Try

class FhirSearchParameterCache(onFhirClient: OnFhirClient)(implicit actorSystem:ActorSystem[_]) {
  implicit val executionContext = actorSystem.executionContext

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()

  val searchParamsUsed:ConcurrentHashMap[String, ConcurrentHashMap[String, SearchParameterConf]] = new ConcurrentHashMap[String, ConcurrentHashMap[String, SearchParameterConf]]()

  def getSearchParameterConf(rtype:String, sp:String):Option[SearchParameterConf] = {
    Try(searchParamsUsed.get(rtype).get(sp)).toOption
  }

  /**
   * Retrieve the parameter definitions if not exist
   * @param rtype  Resource type
   * @param sps    Requested search parameters
   * @return
   */
  def syncSearchParameterConfs(rtype:String, sps:Set[String]):Future[Boolean] = {
    var spConfs = searchParamsUsed.getOrDefault(rtype, null)
    if(spConfs == null){
      spConfs = new  ConcurrentHashMap[String, SearchParameterConf]

      retrieveSearchParameterConfs(rtype, sps)
        .map(pconfs => {
          spConfs.putAll(pconfs.map(p => p.pname -> p).toMap.asJava)
          searchParamsUsed.put(rtype, spConfs)
          sps.size == pconfs.size
        })
    } else {
      val missingConfParams = spConfs.keys().asScala.toSet.diff(sps)
      if(missingConfParams.nonEmpty)
        retrieveSearchParameterConfs(rtype, missingConfParams)
          .map(pconfs => {
            spConfs.putAll(pconfs.map(p => p.pname -> p).toMap.asJava)
            missingConfParams.size == pconfs.size
          })
       else
        Future.apply(true)
    }
  }

  /**
   * Retrieve search parameter configurations from OnFhir server
   * @param rtype   Resource type
   * @param sps     Name of search parameters to retrieve
   * @return
   */
  private def retrieveSearchParameterConfs(rtype:String, sps:Set[String]):Future[Seq[SearchParameterConf]] = {
    actorSystem.log.debug("Synchronizing search parameter configurations for resource type {} and parameters {} ...", rtype, sps.mkString(","))
    val request =
      HttpRequest(HttpMethods.GET).withUri(s"/onfhir/internal/conf/searchparameters/$rtype?pname=${sps.mkString(",")}")

    onFhirClient
      .sendRequest(request)
      .flatMap {
        case resp @ HttpResponse(StatusCodes.OK, _, _ , _ ) =>
          Unmarshal(resp).to[Seq[SearchParameterConf]]
        case resp @ HttpResponse(code, _, _, _) =>
          actorSystem.log.error("OnFhir retrieve search parameter configuration for resource type {} and parameters {} request failed, response code: {} ", rtype, sps.mkString(","), code)
          resp.discardEntityBytes()
          Future.apply(Nil)
      }
  }
}

package io.onfhir.subscription.cache
import java.util.concurrent.{ConcurrentHashMap}
import collection.JavaConverters._
import io.onfhir.config.SearchParameterConf

class FhirSearchParameterCache {

  val searchParamsUsed:ConcurrentHashMap[String, ConcurrentHashMap[String, SearchParameterConf]] = new ConcurrentHashMap[String, ConcurrentHashMap[String, SearchParameterConf]]()

  def getSearchParameterConf(rtype:String, sp:String):SearchParameterConf = {
    syncSearchParameterConfs(rtype, Set(sp))
    searchParamsUsed.get(rtype).get(sp)
  }

  /**
   * Retrieve the parameter definitions if not exist
   * @param rtype  Resource type
   * @param sps    Requested search parameters
   * @return
   */
  def syncSearchParameterConfs(rtype:String, sps:Set[String]):Unit = {
    var spConfs = searchParamsUsed.getOrDefault(rtype, null)
    if(spConfs == null){
      spConfs = new  ConcurrentHashMap[String, SearchParameterConf]
      spConfs.putAll(retrieveSearchParameterConfs(rtype, sps).map(p => p.pname -> p).toMap.asJava)
      searchParamsUsed.put(rtype, spConfs)
    } else {
      val missingConfParams = spConfs.keys().asScala.toSet.diff(sps.toSet)
      if(missingConfParams.nonEmpty)
        spConfs.putAll(retrieveSearchParameterConfs(rtype, missingConfParams).map(p => p.pname -> p).toMap.asJava)
    }
  }


  def retrieveSearchParameterConfs(rtype:String, sps:Set[String]):Seq[SearchParameterConf] = {
    Nil
  }
}

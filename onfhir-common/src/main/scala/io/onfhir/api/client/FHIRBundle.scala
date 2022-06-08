package io.onfhir.api.client

import akka.http.scaladsl.model.{DateTime, StatusCode, Uri}
import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRResponse
import io.onfhir.api.util.FHIRUtil
import io.onfhir.util.DateTimeUtil
import org.json4s.JsonAST.{JArray, JObject}

/**
 * FHIR Bundle representation
 * @param bundle
 */
abstract class FHIRBundle(bundle:Resource) {
  /**
   * Entries of the bundle
   */
  val entries:Seq[JObject] = bundle \ "entry" match {
    case JArray(arr) => arr.map(_.asInstanceOf[JObject])
    case _ => Nil
  }

  /**
   * Get the resource from the entry
   * @param entry
   * @return
   */
  def getResourceFromEntry(entry:JObject):Option[JObject] =
    FHIRUtil.extractValueOption[JObject](entry, "resource")
}

abstract class FHIRPaginatedBundle(bundle:Resource, val request:FhirRequestBuilder)  extends FHIRBundle(bundle) {
  val total:Option[Long] = FHIRUtil.extractValueOption[Long](bundle, "total")

  private val links:Map[String, String] = (bundle \ "link") match {
    case JArray(arr) =>
      arr
        .map(_.asInstanceOf[JObject])
        .map(l =>
          FHIRUtil.extractValue[String](l, "relation") ->
            FHIRUtil.extractValue[String](l, "url")
        ).toMap
    case _ => Map.empty
  }

  def getLastPage():Long = links.get("last").flatMap(l => Uri(l).query().get("_page")).map(_.toLong).getOrElse(if(entries.isEmpty) 0L else 1L)

  /**
   * If search set has next page
   * @return
   */
  def hasNext():Boolean = links.isDefinedAt("next")


  /**
   * Get the url for next page
   * @return
   */
  def getNext():String = links("next")
}

/**
 * FHIR Bundle that represents a search set
 * @param bundle
 */
class FHIRSearchSetBundle(bundle:Resource, override val request:FhirSearchRequestBuilder) extends FHIRPaginatedBundle(bundle, request) {

  /**
   * Search results
   */
  var searchResults:Seq[JObject] =
    entries
      .filter(entry => FHIRUtil.extractValueOptionByPath[String](entry, "search.mode").contains("match"))
      .flatMap(getResourceFromEntry)

  /**
   * Included results indexed with reference url e.g. Observation/653351 -> resource
   */
  var includedResults:Map[String, Resource] =
    entries
      .filter(entry => FHIRUtil.extractValueOptionByPath[String](entry, "search.mode").contains("include"))
      .flatMap(getResourceFromEntry)
      .map(r => FHIRUtil.getReference(r) -> r)
      .toMap

  def mergeResults(other:FHIRSearchSetBundle):FHIRSearchSetBundle = {
    searchResults = searchResults ++ other.searchResults
    includedResults = includedResults ++ other.includedResults
    this
  }

  /**
   * Use when there are multi type results in search set
   * @param rtype
   * @return
   */
  def getSearchResultsWithResourceType(rtype:String):Seq[JObject] = {
    searchResults
      .filter(r => FHIRUtil.extractValue[String](r, "resourceType") == rtype)
  }
}

class FHIRHistoryBundle(bundle:Resource, override val request:FhirHistoryRequestBuilder) extends FHIRPaginatedBundle(bundle, request) {
  case class BundleRequestEntry(method:String, url:String)
  case class BundleResponseEntry(status:String, lastUpdateTime:DateTime, version:Option[Long])

  private val historyEntries:Map[String, Seq[(Long, BundleRequestEntry, BundleResponseEntry, Option[Resource])]] = {
        val parsedEntries = entries.map(parseEntry)
        if(request.request.resourceId.isDefined) {
          Map(request.request.resourceId.get -> parsedEntries)
        } else {
          parsedEntries
            .map(e => {
              val rid = e._2.method match {
                case "CREATE" => FHIRUtil.extractIdFromResource(e._4.get)
                case _ => e._2.url.split('/').last
              }
              rid -> e
            }).groupMap(_._1)(_._2)
        }
    }

  /**
   * Get history of the resource
   * @return
   */
  def getHistory():Seq[(Long, Option[Resource], DateTime)] = historyEntries.head._2.map(e => (e._1 , e._4, e._3.lastUpdateTime))



  /**
   * Get history of the resource given by the resource id
   * @param rid
   * @return
   */
  def getHistory(rid:String):Seq[(Long, Option[Resource], DateTime)] =
    historyEntries
      .getOrElse(rid, Nil)
      .map(e => (e._1 , e._4, e._3.lastUpdateTime))

  /**
   * Get histories of all resources in the bundle
   * @return
   */
  def getHistories:Map[String, Seq[(Long, Option[Resource], DateTime)]] =
    historyEntries
      .map(g => g._1 -> g._2.map(e => (e._1 , e._4, e._3.lastUpdateTime)))

  private def parseEntry(entry:JObject):(Long, BundleRequestEntry, BundleResponseEntry, Option[Resource]) = {
    val rq = parseBundleRequest((entry \ "request").asInstanceOf[JObject])
    val rp = parseBundleResponse((entry \ "response").asInstanceOf[JObject])
    val resource = getResourceFromEntry(entry)
    val version = rp.version.getOrElse(resource.map(FHIRUtil.extractVersionFromResource).getOrElse(1L))
    (version, rq, rp, resource)
  }

  private def parseBundleRequest(br:JObject): BundleRequestEntry = {
    BundleRequestEntry(
      FHIRUtil.extractValue[String](br, "method"),
      FHIRUtil.extractValue[String](br, "url")
    )
  }

  private def parseBundleResponse(br:JObject): BundleResponseEntry = {
    BundleResponseEntry(
      FHIRUtil.extractValue[String](br, "status"),
      DateTimeUtil.instantToDateTime(DateTimeUtil.parseFhirDateTimeOrInstant(FHIRUtil.extractValue[String](br, "lastModified"))),
      FHIRUtil.extractValueOption[String](br, "etag").map(e => e.drop(3).dropRight(1).toLong)
    )
  }

}

class FHIRTransactionBatchBundle(val bundle:Resource) extends FHIRBundle(bundle) {
  /**
   * Parsed individual responses
   */
  val responses:Seq[(Option[String], FHIRResponse)] =
    entries.map(e =>
      FHIRUtil.extractValueOption[String](e, "fullUrl") ->
        parseEntryAsResponse(e)
    )

  def hasAnyError():Boolean = {
    responses.exists(_._2.isError)
  }

  /**
   * Get the response of an child request of batch/transaction with given fullUrl
   * @param fullUrl
   * @return
   */
  def getResponse(fullUrl:String):FHIRResponse = {
    responses.find(_._1.contains(fullUrl)).map(_._2).get
  }

  private def parseEntryAsResponse(entry:JObject):FHIRResponse = {

    val resource = getResourceFromEntry(entry)
    val rsp = (entry \ "response").asInstanceOf[JObject]

    new FHIRResponse(
      httpStatus = StatusCode.int2StatusCode(FHIRUtil.extractValue[String](rsp, "status").trim.split(' ').head.toInt),
      responseBody = resource,
      location = FHIRUtil.extractValueOption[String](rsp, "location").map(Uri.apply),
      lastModified = FHIRUtil.extractValueOption[String](rsp, "lastModified").map(l=> DateTimeUtil.instantToDateTime(DateTimeUtil.parseFhirDateTimeOrInstant(l))),
      newVersion = FHIRUtil.extractValueOption[String](rsp, "etag").map(e => e.drop(3).dropRight(1).toLong),
    )
  }

}
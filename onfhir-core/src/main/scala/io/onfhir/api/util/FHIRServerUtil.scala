package io.onfhir.api.util

import akka.http.scaladsl.model.headers.{EntityTag, `Content-Type`}
import akka.http.scaladsl.model.{MediaRange, MediaTypes}
import io.onfhir.api.model.{FHIRSearchResult, Parameter}
import io.onfhir.api.parsers.FHIRResultParameterResolver
import io.onfhir.api.util.FHIRUtil.resourceLocation
import io.onfhir.api._
import io.onfhir.config.{FhirServerConfig, OnfhirConfig}
import io.onfhir.util.JsonFormatter.formats
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.json4s._

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/**
 * Utility functions for a FHIR server
 * @param fhirConfig FHIR configurations
 */
class FHIRServerUtil(fhirConfig:FhirServerConfig) {
  val resultParameterResolver = new FHIRResultParameterResolver(fhirConfig)

  /**
   * Generates and returns the bundle links for search responses
   *
   * @param rtype       type (s) of the resource to search if Nil it means all
   * @param rid         id of the resource (for History searches)
   * @param totalCount  Total number of search results
   * @param bundleCount Number of matched results (matched entries) in bundle
   * @param parameters  query parameters used for query
   * @return List of LinkName and Location
   */
  def generateBundleLinks(
                           rtype: Seq[String],
                           rid: Option[String],
                           totalCount: Long,
                           bundleCount: Int,
                           parameters: List[Parameter],
                           isHistory: Boolean): List[(String, String)] = {

    val (count, pageOrOffset) = resultParameterResolver.resolveCountPageParameters(parameters)
    val page = pageOrOffset match {
      case Left(p) => p
      case _ => throw new IllegalStateException("Illegal state occurred while resolving count page parameters (at FHIRServerUtil.generateBundleLinks).")
    }

    //Extract compartment type and id if exist
    val compartmentTypeAndId = parameters.find(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT).map(p => p.valuePrefixList.head)

    val otherParametersExceptPage = parameters.filterNot(_.name == FHIR_SEARCH_RESULT_PARAMETERS.PAGE)

    // Prepare the Location builder function
    val locationFunc =
      if (isHistory)
        constructHistorySearchLocation(rtype.headOption, rid, _: List[Parameter])
      else
        compartmentTypeAndId
          .map(c => constructCompartmentSearchLocation(c._1, c._2, rtype, _: List[Parameter]))
          .getOrElse(
            rtype match {
              case Seq(s) => constructSearchLocation(Some(s), _: List[Parameter]) //Normal search
              case oth => constructSearchLocation(None, _: List[Parameter], Some(oth)) //System level search with multiple types
            }
          )
    //Get the location (path + query) ready to append _page parameter
    val location = locationFunc(otherParametersExceptPage)

    //If we don't calculate the total count in search, then does not return last link
    val links =
      if (totalCount == -1) {
        val previousPage = page - 1
        List(
          Some(FHIR_BUNDLE_FIELDS.SELF_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$page"),
          Some(FHIR_BUNDLE_FIELDS.FIRST_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=1"),
          if (bundleCount == count) Some(FHIR_BUNDLE_FIELDS.NEXT_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=${page + 1}") else None,
          if (previousPage > 0) Some(FHIR_BUNDLE_FIELDS.PREVIOUS_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$previousPage") else None
        ).filter(_.isDefined).map(_.get)
      }
      else if (totalCount < count) {
        List(
          Some(FHIR_BUNDLE_FIELDS.SELF_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$page"), //Only return self link if there is no previous or next
          if (page != 1) Some(FHIR_BUNDLE_FIELDS.FIRST_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=1") else None
        ).filter(_.isDefined).map(_.get)
      } else {
        //Find next, last and previous pages in FHIR paging
        val lastPage = if (totalCount % count == 0) totalCount / count else totalCount / count + 1
        val nextPage = page + 1
        val previousPage = page - 1

        List(
          Some(FHIR_BUNDLE_FIELDS.SELF_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$page"),
          Some(FHIR_BUNDLE_FIELDS.FIRST_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=1"), //First is always page 1
          if (nextPage <= lastPage) Some(FHIR_BUNDLE_FIELDS.NEXT_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$nextPage") else None, //Put next page if this is not last page
          if (previousPage > 0) Some(FHIR_BUNDLE_FIELDS.PREVIOUS_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$previousPage") else None, //Put previous page if this is not first page
          Some(FHIR_BUNDLE_FIELDS.LAST_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$lastPage") //Set the last page
        ).filter(_.isDefined).map(_.get)
      }
    links
  }

  /**
   * Generates and returns the bundle links for search responses for offset based pagination
   * @param rtype           type of the resource to search
   * @param searchResult    search result
   * @param parameters      search parameters
   * @return
   */
  def generateBundleLinksForOffsetBasedPagination(rtype: Seq[String],
                                                  searchResult:FHIRSearchResult,
                                                  parameters: List[Parameter]
                                                 ):List[(String, String)] = {
    val (count, pageOrOffset) = resultParameterResolver.resolveCountPageParameters(parameters)
    val offsetParam = pageOrOffset match {
      case Right( offsets -> true) => s"${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER}=${offsets.mkString(",")}"
      case Right( offsets -> false) => s"${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE}=${offsets.mkString(",")}"
      case _ => throw new IllegalStateException("Illegal state occurred while resolving count page parameters (at FHIRServerUtil.generateBundleLinksForOffsetBasedPagination).")
    }

    //Extract compartment type and id if exist
    val compartmentTypeAndId = parameters.find(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT).map(p => p.valuePrefixList.head)

    val otherParametersExceptOffset = parameters.filterNot(p => p.name == FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER || p.name == FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE)

    // Prepare the Location builder function
    val locationFunc =
        compartmentTypeAndId
          .map(c => constructCompartmentSearchLocation(c._1, c._2, rtype, _: List[Parameter]))
          .getOrElse(
            rtype match {
              case Seq(s) => constructSearchLocation(Some(s), _: List[Parameter]) //Normal search
              case oth => constructSearchLocation(None, _: List[Parameter], Some(oth)) //System level search with multiple types
            }
          )
    //Get the location (path + query) ready to append _page parameter
    val location = locationFunc(otherParametersExceptOffset)

    //If we don't calculate the total count in search, then does not return last link
    val links =
      List(
        Some(FHIR_BUNDLE_FIELDS.SELF_LINK -> s"$location${offsetParam}"),
        if (searchResult.offsetAfter.nonEmpty && count == searchResult.matches.length)
          Some(FHIR_BUNDLE_FIELDS.NEXT_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER}=${searchResult.offsetAfter.mkString(",")}")
        else None,
        if (searchResult.offsetBefore.nonEmpty)
          Some(FHIR_BUNDLE_FIELDS.PREVIOUS_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE}=${searchResult.offsetAfter.mkString(",")}")
        else None
      ).flatten

    links
  }


  /**
   * Returns the history url with given parameters
   *
   * @param _type      type of the resource
   * @param _id        id of the resource
   * @param parameters query parameters, e.g. _count, _since, _format
   * @return url
   */
  private def constructHistorySearchLocation(_type: Option[String], _id: Option[String], parameters: List[Parameter]): String = {
    val historyParams = Set(
      FHIR_SEARCH_RESULT_PARAMETERS.COUNT,
      FHIR_HTTP_OPTIONS.FORMAT,
      FHIR_SEARCH_RESULT_PARAMETERS.SINCE,
      FHIR_SEARCH_RESULT_PARAMETERS.AT,
      FHIR_SEARCH_RESULT_PARAMETERS.PAGE)
    //allowed parameters for url
    val url = OnfhirConfig.fhirRootUrl +
      _type.map("/" + _).getOrElse("") + //Resource type if exist
      _id.map("/" + _).getOrElse("") //Resource id if exist

    val paramStr =
      parameters
        .filter(p => historyParams.contains(p.name))
        .map(p => p.name + "=" + p.valuePrefixList.head._2)

    val query = "?" + paramStr.map(pstr => pstr + "&").mkString("")

    val location = url + "/" + FHIR_HTTP_OPTIONS.HISTORY + query

    location.replace(' ', '+')
  }


  /**
   * Construct URI for search
   *
   * @param path          Path of the search after root url if exists
   * @param parameters    query parameters, e.g. _id, _lastUpdated, _format, . . .
   * @param typeParameter _type parameter if exist for system level search
   * @return url
   */
  private def constructSearchLocation(path: Option[String], parameters: List[Parameter], typeParameter: Option[Seq[String]] = None): String = {
    val url = OnfhirConfig.fhirRootUrl + path.map(p => "/" + p).getOrElse("")

    val typeParamString = typeParameter match {
      case Some(Nil) | None => Nil
      case Some(oth) => Seq(s"_type=${oth.mkString(",")}")
    }

    val otherParamsStr = parameters.map(p => {
      //Construct name part
      val namePart = p.paramCategory match {
        case FHIR_PARAMETER_CATEGORIES.CHAINED =>
          p.chain.map(c => c._2 + ":" + c._1).mkString(".") + "." + p.name
        case FHIR_PARAMETER_CATEGORIES.REVCHAINED =>
          p.chain.map(c => "_has" + c._1 + ":" + c._2).mkString(":") + ":" + p.name
        case _ =>
          p.name + p.suffix
      }
      //Construct the value part
      val valuePart =
        if (p.paramCategory == FHIR_PARAMETER_CATEGORIES.RESULT && (p.name == FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE || p.name == FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE))
          p.valuePrefixList.map {
            case (typ, prName) => s"$typ:$prName"
          }.head
        else
          p.valuePrefixList.map(vp => vp._1 + vp._2).mkString(",")

      //Return name and value part
      namePart + "=" + URLEncoder.encode(valuePart, StandardCharsets.UTF_8.toString)
    })
    //Merge all
    val allParams = typeParamString ++ otherParamsStr
    //Construct query string
    val query = "?" + allParams.map(p => p + "&").mkString("")

    (url + query).replace(' ', '+')
  }

  /**
   * Returns the search url with given parameters, used for compartment based search
   *
   * @param compartmentType compartment type of the resource
   * @param compartmentId   compartment id of the resoruce
   * @param _type           type of the resource if type level search or types for system level search
   * @param parameters      query parameters, e.g. _id, _lastUpdated, _format, . . .
   * @return url
   */
  private def constructCompartmentSearchLocation(compartmentType: String, compartmentId: String, _type: Seq[String], parameters: List[Parameter]): String = {
    _type match {
      case Seq(s) =>
        val compartmentUrl = compartmentType + "/" + compartmentId + "/" + s
        constructSearchLocation(Some(compartmentUrl), parameters.filterNot(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT))
      case oth =>
        //Compartment search with all resources
        val compartmentUrl = compartmentType + "/" + compartmentId + "/" + "*"
        constructSearchLocation(Some(compartmentUrl), parameters.filterNot(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT), typeParameter = Some(oth))
    }
  }

  /**
   * Create a FHIR Bundle entry for the given resulting resource
   *
   * @param resource     The resource content
   * @param bundleType   Type of bundle operation i.e. history,search
   * @param isSummarized If search results are summarized by FHIR _summary or _elements parameters
   * @param isMatched    If true this is a matched resource otherwise it is included
   * @return
   */
  def createBundleEntry(resource: Resource, bundleType: String, isSummarized: Boolean = true, isMatched: Boolean = true): Resource = {

    var updatedResource = resource
    // Check the search flag and assign search mode accordingly
    val searchMode = if (isMatched) FHIR_BUNDLE_ENTRY_TYPES.MATCH else FHIR_BUNDLE_ENTRY_TYPES.INCLUDE

    val resourceType = (resource \ FHIR_COMMON_FIELDS.RESOURCE_TYPE).extract[String]
    val resourceId = (resource \ FHIR_COMMON_FIELDS.ID).extract[String]
    val currentVersion = (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.VERSION_ID).extract[String]
    val lastUpdated = (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.LAST_UPDATED).extract[String]
    val resourceUrl = resourceLocation(resourceType, resourceId)
    val statusCode = (resource \ FHIR_EXTRA_FIELDS.STATUS_CODE).extract[String]
    val method = (resource \ FHIR_EXTRA_FIELDS.METHOD).extractOpt[String]

    //Update the meta
    if (isSummarized /*(summary.isDefined && summary.get != "false") || _elements.isDefined*/ ) {
      updatedResource = indicateSummarization(updatedResource)
    }

    //Create the Entry
    var entry = JObject(List(JField.apply(FHIR_BUNDLE_FIELDS.FULL_URL, resourceUrl)))

    //Put the resource part if it is not history or deleted version
    if (bundleType != FHIR_BUNDLE_TYPES.HISTORY || !method.contains(FHIR_METHOD_NAMES.METHOD_DELETE))
      entry = entry ~ (FHIR_BUNDLE_FIELDS.RESOURCE -> FHIRUtil.clearExtraFields(updatedResource))


    //bdl-2: entry.search only when a search (expression : entry.search.empty() or (type = 'searchset'))
    if (bundleType.equals(FHIR_BUNDLE_TYPES.SEARCH_SET)) {
      entry = entry ~ (FHIR_BUNDLE_FIELDS.SEARCH -> (FHIR_BUNDLE_FIELDS.MODE -> searchMode))
    }

    //bdl-3: entry.request only for some types of bundles (expression : entry.request.empty() or type = 'batch' or type = 'transaction' or type = 'history')
    if (Seq(FHIR_BUNDLE_TYPES.BATCH, FHIR_BUNDLE_TYPES.TRANSACTION, FHIR_BUNDLE_TYPES.HISTORY).contains(bundleType)) {
      //Construct Bundle.request
      entry = entry ~
        (FHIR_BUNDLE_FIELDS.REQUEST ->
          (FHIR_COMMON_FIELDS.METHOD -> method.get) ~
            (FHIR_COMMON_FIELDS.URL -> (method.get match {
              case FHIR_METHOD_NAMES.METHOD_POST => resourceType
              case FHIR_METHOD_NAMES.METHOD_PUT | FHIR_METHOD_NAMES.METHOD_DELETE => resourceType + "/" + resourceId
            }))
          )
    }

    //bdl-4: entry.response only for some types of bundles (expression : entry.response.empty() or type = 'batch-response' or type = 'transaction-response')
    if (Seq(FHIR_BUNDLE_TYPES.BATCH_RESPONSE, FHIR_BUNDLE_TYPES.TRANSACTION_RESPONSE, FHIR_BUNDLE_TYPES.HISTORY).contains(bundleType)) {
      val etag: String = EntityTag.apply(currentVersion, weak = true).toString()
      //Construct Bundle.response
      entry = entry ~
        (FHIR_BUNDLE_FIELDS.RESPONSE ->
          (FHIR_BUNDLE_FIELDS.STATUS -> statusCode) ~
            (FHIR_BUNDLE_FIELDS.ETAG -> etag) ~
            (FHIR_BUNDLE_FIELDS.LAST_MODIIFED -> lastUpdated)
          )
    }

    entry
  }


  /**
   * Process and add the necessary elements to indicate summarization of the resource
   *
   * @param resource FHIR resource
   * @return
   */
  def indicateSummarization(resource: Resource): Resource = {
    resource.transformField {
      case (FHIR_COMMON_FIELDS.META, meta: JObject) =>
        if ((meta \ FHIR_COMMON_FIELDS.TAG) == JNothing) {
          FHIR_COMMON_FIELDS.META -> meta ~ (FHIR_COMMON_FIELDS.TAG -> JArray(List(
            (FHIR_COMMON_FIELDS.SYSTEM -> fhirConfig.FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM) ~
              (FHIR_COMMON_FIELDS.CODE -> "SUBSETTED"))))
        } else {
          FHIR_COMMON_FIELDS.META ->
            meta.transformField {
              case (FHIR_COMMON_FIELDS.TAG, tags: JArray) =>
                //Add the new tag
                FHIR_COMMON_FIELDS.TAG -> tags.merge(JArray(List(
                  (FHIR_COMMON_FIELDS.SYSTEM -> fhirConfig.FHIR_SUMMARIZATION_INDICATOR_CODE_SYSTEM) ~
                    (FHIR_COMMON_FIELDS.CODE -> "SUBSETTED"))))
            }
        }
    }.asInstanceOf[JObject]
  }

  /**
   * Extract all paths of the search parameter for the given resource type
   *
   * @param _type     Resource type
   * @param parameter Parsed Search Parameter value
   * @return the paths defined for the parameter for the resource  type
   */
  def extractElementPaths(_type: String, parameter: Parameter): Set[String] = {
    parameter.paramCategory match {
      case FHIR_PARAMETER_CATEGORIES.COMPARTMENT =>
        val compartmentRelatedParams = parameter.chain.map(_._2)
        compartmentRelatedParams.map(p =>
          fhirConfig.findSupportedSearchParameter(_type, p).map(_.extractElementPaths()).getOrElse(Nil)
        ).reduce((s1, s2) => s1 ++ s2).toSet
      case FHIR_PARAMETER_CATEGORIES.NORMAL =>
        fhirConfig.findSupportedSearchParameter(_type, parameter.name).map(_.extractElementPaths()).getOrElse(Nil).toSet
      //Other parameters are not important
      case _ => Set.empty
    }
  }

  /**
   * Resolve ContentType to be returned in the Response if possible
   *
   * @param _format     FHIR _format parameter
   * @param contentType Content Type header stated by client
   * @param mediaRanges Media ranges stated by client in Accept Header
   * @return
   */
  def resolveResponseMediaRange(_format: Option[String], contentType: Option[`Content-Type`], mediaRanges: Seq[MediaRange]): Option[MediaRange] = {
    // The URI processing replace + with empty space for formats like application/xml+fhir, so we replace it back
    val pFormat = _format.map(_.replace(" ", "+"))
    // When the client accept anything, remove it to override by format
    //val filteredRanges = mediaRanges.filter(mediaRange => !mediaRange.mainType.equals("*"))
    // If format is defined, it overrides (?)
    pFormat match {
      case Some(format) =>
        fhirConfig.FHIR_FORMAT_MIME_TYPE_MAP
          .get(format)
          .map(MediaRange.apply)
      case None =>
        //If there is no accept header
        if (mediaRanges.isEmpty) {
          if (contentType.nonEmpty && //if there is a content type
            !contentType.get.contentType.mediaType.matches(MediaTypes.`application/x-www-form-urlencoded`)) //which is not equal to form url encoded
            fhirConfig.FHIR_FORMAT_MIME_TYPE_MAP
              .get(contentType.get.contentType.mediaType.value)
              .map(MediaRange.apply) //Use that same in response
          else //Otherwise use default
            Some(fhirConfig.FHIR_DEFAULT_MEDIA_TYPE).map(MediaRange.apply) //Use default
        } else {
          mediaRanges
            .find(mediaRange => fhirConfig.FHIR_SUPPORTED_RESULT_MEDIA_TYPES.exists(supportedMediaType => mediaRange.matches(supportedMediaType)))
            .map {
              case mr if mr.mainType().equals("*") => MediaRange.apply(fhirConfig.FHIR_DEFAULT_MEDIA_TYPE)
              case other => other
            }
          /*fhirConfigurator.FHIR_SUPPORTED_RESULT_MEDIA_TYPES
            .find(mediaType => filteredRanges.map(mediaRange => mediaRange.matches(mediaType)).fold(false)(_ | _))*/
        }
    }
  }

}

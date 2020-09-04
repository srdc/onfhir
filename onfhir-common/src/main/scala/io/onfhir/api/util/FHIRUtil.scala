package io.onfhir.api.util

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.UUID

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.`Content-Type`
import com.nimbusds.jose.util.Base64URL
import io.onfhir.config.{OnfhirConfig, OperationParamDef}
import io.onfhir.api._
import io.onfhir.api.model.{FHIRMultiOperationParam, FHIROperationParam, FHIRResponse, FHIRSimpleOperationParam, FhirCanonicalReference, FhirInternalReference, FhirLiteralReference, FhirLogicalReference, FhirReference, Parameter}
import io.onfhir.api.parsers.FHIRResultParameterResolver
import io.onfhir.util.JsonFormatter.formats
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.exception.InvalidParameterException
import io.onfhir.util.DateTimeUtil
import org.json4s.JsonAST.{JNothing, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s.{JsonAST, _}

object FHIRUtil {

  /**
    * Generates a random identifier according to FHIR id rules with given minimum length
    *
    * @return a new logical identifier
    */
  def generateResourceId(): String = {
    UUID.randomUUID().toString
  }

  /**
    * Returns the location of a resource (should be accessible via read operation)
    *
    * @param _type type of the resource
    * @param _id   id of the resource
    * @return URL of the resource
    */
  def resourceLocation(_type: String, _id: String): String = {
    OnfhirConfig.fhirRootUrl + "/" + _type + "/" + _id
  }

  /**
    * Returns the location of a resource including version identifier (should be accessible via read operation)
    *
    * @param _type type of the resource
    * @param _id   id of the resource
    * @param _vid  version id of the resource
    * @return URL of the resource
    */
  def resourceLocationWithVersion(_type: String, _id: String, _vid: Long): String = {
    OnfhirConfig.fhirRootUrl + "/" + _type + "/" + _id + "/" + FHIR_HTTP_OPTIONS.HISTORY + "/" + _vid
  }

  /**
    * Generates and returns the bundle links for search responses
    *
    * @param rtype           type of the resource
    * @param rid             id of the resource (for History searches)
    * @param totalCount      number of entries in bundle
    * @param parameters      query parameters used for query
    * @return List of LinkName and Location
    */
  def generateBundleLinks(
                           rtype: Option[String],
                           rid: Option[String],
                           totalCount: Long,
                           parameters: List[Parameter],
                           isHistory: Boolean): List[(String, String)] = {

    val (page, count) = FHIRResultParameterResolver.resolveCountPageParameters(parameters)
    //Extract compartment type and id if exist
    val compartmentTypeAndId = parameters.find(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT).map(p => p.valuePrefixList.head)

    val otherParametersExceptPage = parameters.filterNot(_.name == FHIR_SEARCH_RESULT_PARAMETERS.PAGE)

    // Prepare the Location builder function
    val locationFunc =
      if (isHistory)
        constructHistorySearchLocation(rtype, rid, _: List[Parameter])
      else
        compartmentTypeAndId
          .map(c => constructCompartmentSearchLocation(c._1, c._2, rtype.get, _: List[Parameter]))
          .getOrElse(constructSearchLocation(rtype.get, _: List[Parameter]))
    //Get the location
    var location = locationFunc(otherParametersExceptPage)

    location = if(otherParametersExceptPage.isEmpty) location + "?" else location + "&"

    //If we don't calculate the total count in search, then does not return last link
    val links =
      if (totalCount == -1) {
        val previousPage = page - 1
        List(
          Some(FHIR_BUNDLE_FIELDS.SELF_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$page"),
          Some(FHIR_BUNDLE_FIELDS.FIRST_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=1"),
          Some(FHIR_BUNDLE_FIELDS.NEXT_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=${page + 1}"),
          if (previousPage > 0) Some(FHIR_BUNDLE_FIELDS.PREVIOUS_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$previousPage") else None
        ).filter(_.isDefined).map(_.get)
      }
      else if (totalCount < count) {
        List(FHIR_BUNDLE_FIELDS.SELF_LINK -> s"$location${FHIR_SEARCH_RESULT_PARAMETERS.PAGE}=$page") //Only return self link if there is no previous or next
      } else {
        //Find next, last and previous pages in FHIR paging
        val lastPage = if (totalCount % count == 0) totalCount / count else totalCount / count + 1
        val nextPage = page.toInt + 1
        val previousPage = page.toInt - 1

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
      _type.map( "/" + _).getOrElse("") + //Resource type if exist
      _id.map("/" + _ ).getOrElse("") //Resource id if exist

    val query = "?" + parameters.filter(p => historyParams.contains(p.name)).map(p => p.name + "=" + p.valuePrefixList.head._2).mkString("&")
    val location = url + "/" + FHIR_HTTP_OPTIONS.HISTORY + (if (!query.endsWith("?")) query else "")

    location.replace(' ', '+')
  }

  /**
    * Construct URI for search
    *
    * @param _type      type of the resource
    * @param parameters query parameters, e.g. _id, _lastUpdated, _format, . . .
    * @return url
    */
  private def constructSearchLocation(_type: String, parameters: List[Parameter]): String = {
    val url = OnfhirConfig.fhirRootUrl + "/" + _type

    val query =
      if(parameters.nonEmpty)
        "?" + parameters.map ( p => {
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
            if(p.paramCategory == FHIR_PARAMETER_CATEGORIES.RESULT && (p.name == FHIR_SEARCH_RESULT_PARAMETERS.INCLUDE || p.name == FHIR_SEARCH_RESULT_PARAMETERS.REVINCLUDE))
              p.valuePrefixList.mkString(":") + (if(p.paramType != "") ":"+ p.paramType else "")
            else
              p.valuePrefixList.map(vp => vp._1 + vp._2).mkString(",")

          //Return name and value part
          namePart + "=" + URLEncoder.encode(valuePart, StandardCharsets.UTF_8.toString)
        }).mkString("&")
      else
        ""//No parameter

    (url + query).replace(' ', '+')
  }

  /**
    * Returns the search url with given parameters, used for compartment based search
    *
    * @param compartmentType compartment type of the resource
    * @param compartmentId   compartment id of the resoruce
    * @param _type           type of the binded resource if specified
    * @param parameters      query parameters, e.g. _id, _lastUpdated, _format, . . .
    * @return url
    */
  private def constructCompartmentSearchLocation(compartmentType: String, compartmentId: String, _type: String, parameters: List[Parameter]): String = {
    val compartmentUrl = compartmentType + "/" + compartmentId + "/" + _type
    constructSearchLocation(compartmentUrl, parameters.filterNot(_.paramCategory == FHIR_PARAMETER_CATEGORIES.COMPARTMENT))
  }

  /**
    * Create a resource to represent deleted resource
    * @param _type Resource type
    * @param _id Resource identifier
    * @param _vid Version of resource
    * @param statusCode Possible HttpResponse status for this operation
    * @return
    */
  def createEmptyResourceForDelete(_type:String, _id:String, _vid:Long, lastModified:DateTime, statusCode:StatusCode):Resource = {
    val resourceForDeletion =
      (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> _type) ~
        (FHIR_COMMON_FIELDS.ID -> _id) ~
        (FHIR_COMMON_FIELDS.META ->
          (FHIR_COMMON_FIELDS.VERSION_ID -> _vid.toString) ~
            (FHIR_COMMON_FIELDS.LAST_UPDATED -> (lastModified.toIsoDateTimeString + "Z"))
          )

    FHIRUtil.populateResourceWithExtraFields(resourceForDeletion, FHIR_METHOD_NAMES.METHOD_DELETE, statusCode)
  }



  /**
    * Sets a number of META fields to the given resource
    *
    * @param resource     resource to be edited
    * @param id           logical id of the resource
    * @param versionId    version id of the resources
    * @param lastModified last modified information of the resource
    * @return
    */
  def populateResourceWithMeta(resource: Resource, id: Option[String], versionId: Long, lastModified: Instant): Resource = {
    val resourceTypeField = resource.findField(_._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE).get

    val meta:Resource = (resource \ FHIR_COMMON_FIELDS.META)  match {
      case meta:JObject =>
        FHIR_COMMON_FIELDS.META -> (meta merge ((FHIR_COMMON_FIELDS.VERSION_ID -> versionId.toString) ~ (FHIR_COMMON_FIELDS.LAST_UPDATED -> DateTimeUtil.serializeInstant(lastModified))))
      case _  =>
        (FHIR_COMMON_FIELDS.META ->
          (FHIR_COMMON_FIELDS.VERSION_ID -> versionId.toString) ~
            (FHIR_COMMON_FIELDS.LAST_UPDATED -> DateTimeUtil.serializeInstant(lastModified))
          )
    }

    var result:Resource = resource.obj.filterNot(f => f._1 == FHIR_COMMON_FIELDS.ID || f._1 == FHIR_COMMON_FIELDS.META || f._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE)

    //Merge it
    result = meta merge result

    //Put id
    if (id.isDefined)
      result = (JObject() ~ (FHIR_COMMON_FIELDS.ID -> id.get)) merge result  //add id field to resource

    result = resourceTypeField ~ result
    result
  }

  /**
    * Populate the resource with our own fields (required for Bundle.entry construction)
    * @param httpMethod
    * @param httpStatusCode
    */
  def populateResourceWithExtraFields(resource: Resource, httpMethod:String, httpStatusCode:StatusCode): Resource ={
    val extra =
        //(FHIR_EXTRA_FIELDS.CURRENT -> true) ~
        (FHIR_EXTRA_FIELDS.METHOD -> httpMethod) ~
        (FHIR_EXTRA_FIELDS.STATUS_CODE -> httpStatusCode.intValue.toString)

    resource ~ extra
  }

  /**
    * Remove the extra fields (that we use internally) from the resource content and return it
    * @param resource
    * @return
    */
  def clearExtraFields(resource: Resource):Resource = {
    resource.removeField(f => ONFHIR_EXTRA_FIELDS.contains(f._1)).asInstanceOf[JObject]
  }

  /**
    * Set a profile within meate for a resource
    * @param resource
    * @param profile
    */
  def setProfile(resource: Resource, profile:String):Resource = {
    val meta:Resource =
      (FHIR_COMMON_FIELDS.META ->
        (FHIR_COMMON_FIELDS.PROFILE -> JArray(List(JString(profile))))
        )

    resource merge meta
  }

  /**
    * Set id of a resource
    */
  def setId(resource: Resource, id:String):Resource = {
    resource merge JsonAST.JObject.apply(FHIR_COMMON_FIELDS.ID -> JString(id))
    //resource ~ (FHIR_COMMON_FIELDS.ID -> id)
  }

  /**
    * Converts a given resource into json format taking Prefer header retrieved in HTTP Request into account.
    * If the client does not send Prefer header, default value set in the application.conf is used.
    *
    * @param resource resource to be serialized
    * @param prefer   value of the prefer header
    * @return a string in json format
    */
  def getResourceContentByPreference(resource: Resource, prefer: Option[String]): Option[Resource] = {
    prefer.getOrElse(OnfhirConfig.fhirDefaultReturnPreference) match {
      case preferHeader if preferHeader.contains(FHIR_HTTP_OPTIONS.FHIR_RETURN_MINIMAL) =>  None //if return=minimal send empty string
      case preferHeader if preferHeader.contains(FHIR_HTTP_OPTIONS.FHIR_RETURN_REPRESENTATION) => Some(resource)//resource.clone() //if return=representation send whole resource back
      case preferHeader if preferHeader.contains(FHIR_HTTP_OPTIONS.FHIR_RETURN_OPERATION_OUTCOME)=> Some(FHIRResponse.createOperationOutcomeWithSuccess())
      case _ =>
        Some(resource)//resource.clone() // Default representation
    }
  }



  /**
    * Creates a resource of type "Bundle" for search and history results
    *
    * @param bundleType  type of the Bundle (e.g. history, searchset, etc)
    * @param _links links to this Bundle
    * @param entries  Bundle entries
    * @param total  total number of resources which is requested
    * @return
    */
  def createBundle(bundleType: String, _links: List[(String, String)], entries: Seq[Resource], total: Long, summary: Option[String]): Resource = {
    //Prepare links
    val link = FHIR_BUNDLE_FIELDS.LINK ->
        _links.map(l=> (FHIR_BUNDLE_FIELDS.RELATION  -> l._1) ~ (FHIR_COMMON_FIELDS.URL  -> l._2))

    //Create Bundle
    var bundle:JObject =
        (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> FHIR_BUNDLE_TYPES.BUNDLE) ~
        (FHIR_COMMON_FIELDS.ID-> generateResourceId()) ~
        (FHIR_COMMON_FIELDS.TYPE-> bundleType)

    //bdl-1: total only when a search or history (expression : total.empty() or (type = 'searchset') or (type = 'history'))
    if(total != -1 && Seq(FHIR_BUNDLE_TYPES.SEARCH_SET, FHIR_BUNDLE_TYPES.HISTORY).contains(bundleType))
      bundle = bundle ~ (FHIR_BUNDLE_FIELDS.TOTAL -> total)

    //Add links and entries
    if (summary.isEmpty || summary.get != FHIR_SUMMARY_OPTIONS.COUNT) {
      bundle = bundle ~ link
      bundle = bundle ~ (FHIR_BUNDLE_FIELDS.ENTRY -> entries)
    }

    bundle
  }

  /**
    * Creates  a resource of type "Bundle" for batch and transaction
    * @param bundleType
    * @param entries
    * @return
    */
  def createTransactionBatchBundle(bundleType: String, entries: Seq[Resource]) : Resource = {
    val bundle:JObject =
      (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> FHIR_BUNDLE_TYPES.BUNDLE) ~
      (FHIR_COMMON_FIELDS.ID-> generateResourceId()) ~
      (FHIR_COMMON_FIELDS.TYPE-> bundleType) ~
      (FHIR_BUNDLE_FIELDS.ENTRY -> entries)

    bundle
  }

  /**
    * Create a FHIR Bundle entry for the given resulting resource
    * @param resource   The resource content
    * @param bundleType Type of bundle operation i.e. history,search
    * @param isSummarized  If search results are summarized by FHIR _summary or _elements parameters
    * @param isMatched  If true this is a matched resource otherwise it is included
    * @return
    */
  def createBundleEntry(resource:Resource, bundleType: String, isSummarized:Boolean = true, isMatched:Boolean = true): Resource = {

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
      if (isSummarized /*(summary.isDefined && summary.get != "false") || _elements.isDefined*/) {
        updatedResource = indicateSummarization(updatedResource)
      }

      //Create the Entry
      var entry = JObject(List(JField.apply(FHIR_BUNDLE_FIELDS.FULL_URL,resourceUrl)))

      //Put the resource part if it is not history or deleted version
      if(bundleType != FHIR_BUNDLE_TYPES.HISTORY || !method.contains(FHIR_METHOD_NAMES.METHOD_DELETE))
          entry = entry  ~ (FHIR_BUNDLE_FIELDS.RESOURCE -> FHIRUtil.clearExtraFields(updatedResource))


      //bdl-2: entry.search only when a search (expression : entry.search.empty() or (type = 'searchset'))
      if(bundleType.equals(FHIR_BUNDLE_TYPES.SEARCH_SET)) {
        entry = entry ~ (FHIR_BUNDLE_FIELDS.SEARCH -> (FHIR_BUNDLE_FIELDS.MODE -> searchMode))
      }

      //bdl-3: entry.request only for some types of bundles (expression : entry.request.empty() or type = 'batch' or type = 'transaction' or type = 'history')
      if(Seq(FHIR_BUNDLE_TYPES.BATCH,FHIR_BUNDLE_TYPES.TRANSACTION,  FHIR_BUNDLE_TYPES.HISTORY).contains(bundleType)) {
        //Construct Bundle.request
        entry = entry ~
          (FHIR_BUNDLE_FIELDS.REQUEST ->
              (FHIR_COMMON_FIELDS.METHOD -> method.get) ~
              (FHIR_COMMON_FIELDS.URL ->  (method.get match {
                case FHIR_METHOD_NAMES.METHOD_POST => resourceType
                case FHIR_METHOD_NAMES.METHOD_PUT | FHIR_METHOD_NAMES.METHOD_DELETE => resourceType + "/" +  resourceId
              }))
          )
      }

      //bdl-4: entry.response only for some types of bundles (expression : entry.response.empty() or type = 'batch-response' or type = 'transaction-response')
      if(Seq(FHIR_BUNDLE_TYPES.BATCH_RESPONSE, FHIR_BUNDLE_TYPES.TRANSACTION_RESPONSE, FHIR_BUNDLE_TYPES.HISTORY).contains(bundleType)) {
        //Construct Bundle.response
        entry = entry ~
          (FHIR_BUNDLE_FIELDS.RESPONSE ->
            (FHIR_BUNDLE_FIELDS.STATUS -> statusCode) ~
            (FHIR_BUNDLE_FIELDS.ETAG -> currentVersion) ~
            (FHIR_BUNDLE_FIELDS.LAST_MODIIFED -> lastUpdated)
          )
      }

      entry
  }

  /**
    * Process and add the necessary elements to indicate summarization of the resource
    * @param resource
    * @return
    */
  def indicateSummarization(resource:Resource):Resource = {
      resource.transformField {
        case (FHIR_COMMON_FIELDS.META, meta: JObject) =>
          if((meta \ FHIR_COMMON_FIELDS.TAG) == JNothing) {
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
    * Extract Id, Version and Last Update Time
    * @param resource
    * @return
    */
  def extractBaseMetaFields(resource: Resource):(String, Long, DateTime) = {
    (
      extractIdFromResource(resource),
      extractVersionFromResource(resource),
      extractLastUpdatedFromResource(resource)
    )
  }

  /**
    * Extract resource id
    * @param resource
    * @return
    */
  def extractIdFromResource(resource: Resource): String = {
    (resource \ FHIR_COMMON_FIELDS.ID).extract[String]
  }

  /**
    * Extract version of resource
    * @param resource
    * @return
    */
  def extractVersionFromResource(resource: Resource): Long = {
    (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.VERSION_ID).extract[String].toLong
  }

  /**
    * Extract Last Updated time of resource
    * @param resource
    * @return
    */
  def extractLastUpdatedFromResource(resource: Resource): DateTime = {
    val lastUpdatedString = (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.LAST_UPDATED).extract[String]
    DateTimeUtil.parseInstant(lastUpdatedString).get
  }

  /***
    * Extract Resource type and id
    * @param resource
    * @return
    */
  def extractResourceTypeAndId(resource: Resource):(String, String) =
    (resource \ FHIR_COMMON_FIELDS.RESOURCE_TYPE).extract[String] ->
    (resource \ FHIR_COMMON_FIELDS.ID).extract[String]

  def extractValue[T](resource: Resource, elementName:String)(implicit m:Manifest[T]):T = {
    (resource \ elementName).extract[T]
  }

  /**
    * Extract value of an element (not path)
    * @param resource JSON Resource
    * @param elementName Name of the element
    * @param m
    * @tparam T Expected type
    * @return
    */
  def extractValueOption[T](resource: Resource, elementName:String)(implicit m:Manifest[T]):Option[T] = {
    (resource \ elementName).extractOpt[T]
  }

  /**
    * Extract value of a path
    * @param resource JSON Resource
    * @param path Path of the element
    * @param m
    * @tparam T Expected type
    * @return
    */
  def extractValueOptionByPath[T](resource: Resource, path:String)(implicit m:Manifest[T]):Option[T] = {
    val result =
      path
        .split('.')
        .foldLeft(resource.asInstanceOf[JValue])((v, i) =>  v \ i)

    result.extractOpt[T]
  }

  /**
    * Check if resource is deleted
    * @param resource
    * @return
    */
  def isDeleted(resource: Resource):Boolean =
    (resource \ FHIR_EXTRA_FIELDS.METHOD).extract[String] == FHIR_METHOD_NAMES.METHOD_DELETE

  /**
    * Extract the profiles of the resources
    *
    * @param resource
    * @return
    */
  def extractProfilesFromBson(resource: Resource): Set[String] = {
    (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.PROFILE).extract[Seq[String]].toSet
  }

  /**
    * Rebuild string with escaping special characters
    *
    * @param str string to be escaped
    * @return string is escaped
    */
  def escapeCharacters(str: String): String = {
    val escapeChars = List("\\[", "\\]", "\\+", "\\/", "\\*", "\\$", "\\^", "\\?", "\\|", "\\(", "\\)")
    var ret = str
    escapeChars foreach { c =>
      ret = ret.replaceAll(c, "\\" + c)
    }
    ret
  }

  /**
    * Extract all paths of the search parameter for the given resource type
    * @param _type Resource type
    * @param parameter Parsed Search Parameter value
    * @return the paths defined for the parameter for the resource  type
    */
  def extractElementPaths(_type: String, parameter:Parameter):Set[String] = {
    parameter.paramCategory match {
      case FHIR_PARAMETER_CATEGORIES.COMPARTMENT =>
        val compartmentRelatedParams = parameter.chain.map(_._2)
        compartmentRelatedParams.map(p =>
          fhirConfig.findSupportedSearchParameter(_type, p).map(_.extractElementPaths()).getOrElse(Nil)
        ).reduce((s1,s2)=> s1++s2).toSet
      case FHIR_PARAMETER_CATEGORIES.NORMAL =>
        fhirConfig.findSupportedSearchParameter(_type, parameter.name).map(_.extractElementPaths()).getOrElse(Nil).toSet
      //Other parameters are not important
      case _ => Set.empty
    }
  }



  /**
    *
    * @param paramName The parameter name given in Conformance.rest.resource.searchParam.name
    * @return Transform the names of composite parameters that ends with "-x" to "-[x]", so that it is same
    *         in the corresponding Search Parameter definition SearchParamer.code
    *         (This is a anomaly case in base Conformance and SearchParameter definitions given by FHIR and )
    */
  def transformSearchParameterName(paramName: String): String = {
    if (paramName.endsWith("-x"))
      paramName.dropRight(2) + "-[x]"
    else paramName
  }

  /**
    * Resolve ContentType to be returned in the Response if possible
    * @param _format FHIR _format parameter
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
        if(mediaRanges.isEmpty){
          if(contentType.nonEmpty && //if there is a content type
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

 /* /**
    * Run json path (with dots) on the resource
    * @param resource Resource
    * @param jsonPath Path seperated with dots
    * @tparam T return type
    * @return
    */
  def runJsonPath[T](resource: Resource, jsonPath:String):List[T] = {
    val pathElems = jsonPath.split('.')

    def flatten(xs: List[Any]): List[Any] = xs match {
      case Nil => Nil
      case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
      case (head:Seq[_])::tail =>  flatten(head.toList) ++ flatten(tail)
      case head :: tail => head :: flatten(tail)
    }

    val result = pathElems.foldLeft[List[Any]](List(resource))((resources, path) =>{
      val founds:List[Any] =
        resources.flatMap(res => res match {
          case r:mutable.LinkedHashMap[String, Any] @unchecked =>
            val ret = r.get(path)
            ret
          case _ => None
        })
      val flattenFounds = flatten(founds)
      flattenFounds
    })

    result.map(_.asInstanceOf[T])
  }*/

  /**
    * Extraxt the list of JValue s in the given search parameter path e.g. Observaiton.subject
    * @param path
    * @param resource
    * @return
    */
  def applySearchParameterPath(path:String, resource:JValue):Seq[JValue] = {
    val result =
      path
        .split('.')
        .foldLeft(resource)((v, i) =>  v \ i)

    result match {
      case JNothing => Nil
      case org.json4s.JsonAST.JArray(arr) => arr
      case _ => Seq(result)
    }
  }

  /**
    * Extract FHIR reference values from a resource
    * @param refPath Path to the reference element e.g. Observation.subject --> subject
    * @param resource Resource content
    * @return Reference strings e.g. Patient/21554
    */
  def extractReferences(refPath:String, resource: Resource):Seq[String] = {
      applySearchParameterPath(refPath, resource)
        .flatMap(refObj => (refObj \ FHIR_COMMON_FIELDS.REFERENCE).extractOpt[String])
  }

  /**
   * Parse a canononical reference value
   * @param c
   * @return
   */
  def parseCanonicalReference(c:String):FhirCanonicalReference = {
    val urlAndFragment = c.split('#')
    val urlAndVersion = urlAndFragment.head.split('|')
    val urlFragments = urlAndVersion.head.split('/')

    FhirCanonicalReference(
      urlFragments.dropRight(2).mkString("/"),
      urlFragments.dropRight(1).last,
      urlFragments.last,
      urlAndVersion.drop(1).headOption,
      urlAndFragment.drop(1).headOption
    )
  }
  /**
   * Parse a FHIR Reference or Canonical element
   * @param value
   * @return
   */
  def parseReference(value:JValue):FhirReference = {
    value match {
      case obj:JObject =>
        FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.REFERENCE) match {
          case Some(fhirReferenceUrl) if fhirReferenceUrl.startsWith("#") => FhirInternalReference(fhirReferenceUrl.drop(1))
          case Some(fhirReferenceUrl) if !fhirReferenceUrl.startsWith("#") =>
            var r = parseReferenceValue(fhirReferenceUrl)
            FhirLiteralReference(r._1, r._2, r._3, r._4)
          case None =>
            val referencedResourceType = FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.TYPE)
            val refIdentifier = FHIRUtil.extractValueOption[JObject](obj, FHIR_COMMON_FIELDS.IDENTIFIER).get
            FhirLogicalReference(referencedResourceType, FHIRUtil.extractValueOption[String](refIdentifier, FHIR_COMMON_FIELDS.SYSTEM), FHIRUtil.extractValue[String](refIdentifier, FHIR_COMMON_FIELDS.VALUE))
        }
      case _ =>
        throw new Exception("Invalid FHIR reference")
    }
  }

  /**
    * Parse a FHIR reference value
    * e.g. Simple --> Patient/575644,
    * e.g. URL --> http://fhir.hl7.org/svc/StructureDefinition/c8973a22-2b5b-4e76-9c66-00639c99e61b
    * e.g. With URL and history --> http://example.org/fhir/Observation/1x2/_history/2
    * @param reference FHIR Reference value
    * @return Optional URL part, Resource Type, Resource Id, Option version
    */
  def parseReferenceValue(reference:String):(Option[String], String, String, Option[String]) = {
    val parts = reference.split("/")
    parts.length match {
      case 2 => (None, parts.head, parts.last, None) //Resource Type and id
      case l if l >= 4 =>
        if(parts.apply(l - 2) == "_history"){
          val version = parts.last
          val rtype = parts.apply(l-4)
          val rid = parts.apply(l-3)
          val urlParts = parts.dropRight(4)
          val url = if(l > 4) Some(urlParts.mkString("/")) else None

          (url, rtype, rid, Some(version))
        } else {
          val rid = parts.last
          val rtype = parts.apply(l-2)
          val url = parts.dropRight(2).mkString("/")
          (Some(url), rtype, rid, None)
        }
      case _ => throw new Exception("Invalid reference value")
    }
  }

  /**
    * Extract system and code parts from token query value
    * @param tokenValue Searched value for token parameter e.g. http://loinc|500-5 , |500-5, 500-5
    * @return system and code part for thr query
    */
  def parseTokenValue(tokenValue:String):(Option[String], Option[String]) = {
    //Query like [system]|
    if(tokenValue.last == '|')
      Some(tokenValue.dropRight(1)) -> None
    else
      tokenValue.split('|') match {
        //Query like |[code]
        case Array("", c) => Some("") -> Some(c)
        //Query like [code]
        case Array(c) => None -> Some(c)
        //Query like [system]|[code]
        case Array(s,c) => Some(s) -> Some(c)
        case _ => throw new InvalidParameterException(s"Parameter value $tokenValue is not valid token value!")
      }
  }

  /**
    * Resolve query reference value into URL, Resource Type, Resource Id, and version based on modifier
    * @param reference
    * @param modifier
    * @param targetReferenceTypes
    * @return
    */
  def resolveReferenceValue(reference:String, modifier:String, targetReferenceTypes:Seq[String]):(Option[String], String, String, Option[String]) = {
    modifier match {
      case "" | FHIR_PREFIXES_MODIFIERS.TYPE | FHIR_PREFIXES_MODIFIERS.NOT  =>
        if(reference.contains("/"))
          parseReferenceValue(reference)
        else {
          if(targetReferenceTypes.length > 1)
            throw new InvalidParameterException(s"As search paremeter has multiple target reference types, a type should be supplied in the search!")

          (None, targetReferenceTypes.head, reference, None)
        }
      case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
        (None, "", reference, None)
      //If the modifier is the ResourceType to target for the reference
      case m if m.startsWith(":") => // :[type] modifier is used
        (None, modifier.tail, reference, None) // Make the reference value as [type]/[id]
    }
  }

  /**
    * Parse canonical query value and extract URL and version part
    * @param canonical
    * @return
    */
  def parseCanonicalValue(canonical:String):(String, Option[String]) = {
    val canonicalParts = canonical.split('|')
    canonicalParts.head -> canonicalParts.drop(1).lastOption
  }

  /**
    * Parse the quantity search value to retrieve Value, System and Code/Unit
    * @param quantity
    * @return
    */
  def parseQuantityValue(quantity:String):(String, Option[String], Option[String]) = {
    //Parse the given value
    quantity.split('|') match {
        //Query like [number]
        case Array(v) => (v, None, None)
        //Query like [number]||[code]
        case Array(v, "", c) => (v, None, Some(c))
        //Query like [number]|[system]|[code]
        case Array(v, s, c) => (v, Some(s), Some(c))
        case _ =>
          throw new InvalidParameterException(s"Invalid parameter value $quantity for quantity search!" )
    }
  }

  /**
    * Parse token query value for of-type modifier
    * @param tokenValue
    * @return
    */
  def parseTokenOfTypeValue(tokenValue:String):(String, String, String) = {
    tokenValue.split('|') match {
      case Array(typeSystem, typeCode, value) if !typeSystem.isEmpty && !typeCode.isEmpty && !value.isEmpty =>
        (typeSystem, typeCode, value)
      case _ => throw new InvalidParameterException(s"Parameter value $tokenValue is not valid token value for of-type modifier! Modifier of-type needs all three fields in format [system]|[code]|[value]!")
    }
  }

  /**
    * Split the path into two parts;
    *   - First part --> the part to search with elemMatch as it is an array (the last array element in the path) if exist
    *   - Second part --> the query path part
    *   e.g. target[i].dueDate --> target, dueDate
    *   e.g. identifier[i] --> identifier, ""
    *   e.g. code.coding[i] --> code.coding, ""
    * @param path the path to the element
    * @return
    */
  def splitElementPathIntoElemMatchAndQueryPaths(path:String):(Option[String], Option[String]) = {
    if(path=="")
      None -> None
    else {
      //Find the index of last array element in the path
      val ind = path.lastIndexOf("[i]")
      //If there is no array in the path
      if (ind == -1)
        None -> Some(path)
      //If the last one is array
      else if (ind == path.length - 3)
        Some(path.replace("[i]", "")) -> None
      //Otherwise
      else
        Some(
          path
            .substring(0, ind)
            .replace("[i]", "")
        ) -> Some(path.substring(ind + 4, path.length))
    }
  }

  /**
    * Remove the array indicators from the path
    * @param path
    * @return
    */
  def normalizeElementPath(path:String):String = path.replace("[i]", "")

  /**
    * Merge a main path with a subpath as Json path e.g. code , coding.code --> code.coding.code
    * @param mainPath Main path
    * @param subPath sub path
    * @return
    */
  def mergeElementPath(mainPath:Option[String], subPath:String):String = mainPath.map(_ + ".").getOrElse("") + subPath
  def mergeElementPath(mainPath:String, subPath:String):String = if(mainPath == "") subPath else if(subPath == "") mainPath else mainPath + "."+ subPath

  def mergeFilePath(mainPath:Option[String], subPath:String):String = mainPath.map(_ + "/").getOrElse("") + subPath


  /**
   * Decapitilize a string
   * @param s
   * @return
   */
  def decapitilize(s: String): String = {
    s.charAt(0).toLower + s.substring(1)
  }

  /**
   * FHIR index of query restrictions on the path
   * @param pathParts     Splitted path
   * @param restrictions  Restrictions on path e.g. @.type = email
   **/
  def findIndexOfRestrictionsOnPath(pathParts:Seq[String], restrictions:Seq[(String, String)]):Seq[(Int, Seq[(String, String)])]= {
    val indexOfRestrictions  =
      restrictions
        .map(r => (pathParts.length - r._1.count(_ == '@') - 1) -> r)
        .groupBy(_._1)
        .map(g => g._1 ->
          g._2.map(_._2)
            .map(i => i._1.replace("@.", "") -> i._2) //remove paths
        ).toSeq.sortBy(_._1)

    indexOfRestrictions
  }
}

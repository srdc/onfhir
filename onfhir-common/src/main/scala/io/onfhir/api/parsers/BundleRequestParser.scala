package io.onfhir.api.parsers

import akka.http.scaladsl.model.headers.{EntityTag, `If-Match`, `If-Modified-Since`, `If-None-Match`}
import akka.http.scaladsl.model.{StatusCodes, Uri}
import io.onfhir.exception._
import io.onfhir.api.{FHIR_BUNDLE_FIELDS, FHIR_HTTP_OPTIONS, FHIR_INTERACTIONS, FHIR_METHOD_NAMES}
import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.OnfhirConfig
import io.onfhir.util.DateTimeUtil
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JObject}

import scala.util.Try

/**
  * Created by ozan on 23.02.2017.
  * Bundle related supplementary methods
  */
object BundleRequestParser {
  /**
    * Parse the Bundle for batch or transaction and convert them to child FHIRRequest
    * @param bundle
    * @return
    */
  def parseBundleRequest(bundle:Resource, prefer:Option[String] = None):Seq[FHIRRequest] = {
    try {
      //Get the entries
      (bundle \ FHIR_BUNDLE_FIELDS.ENTRY)
        .extractOpt[JArray]
        .map(_.arr.toSeq).getOrElse(Nil)
        .map(entry => {
          Try(
            parseBundleRequestEntry(entry.asInstanceOf[JObject])
              .copy(prefer = prefer)
          ).recover {
            case e: NotFoundException =>
              val requestUrl = (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_BUNDLE_FIELDS.URL).extract[String]
              val request = FHIRRequest(interaction = FHIR_INTERACTIONS.UNKNOWN, requestUri = requestUrl)
              //Set the response
              request.setResponse(FHIRResponse.errorResponse(StatusCodes.BadRequest, e.outcomeIssues))
              request
          }.get
        })
    } catch {
      case e:Exception =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Invalid bundle request, please check the sytax of FHIR Bundle"),
            Nil
          )))
    }
  }

  /**
   * Parse a document bundle to create child requests for creating document entries as a transaction
   * @param bundle    Document bundle content
   * @param prefer
   * @return
   */
  def parseBundleDocumentRequest(bundle:Resource, prefer:Option[String] = None):Seq[FHIRRequest] = {
    //Get the entries
    (bundle \ FHIR_BUNDLE_FIELDS.ENTRY)
      .extractOpt[JArray]
      .map(_.arr.toSeq).getOrElse(Nil)
      .map(entry => {
        parseBundleDocumentRequestEntry(entry.asInstanceOf[JObject])
      })
  }

  /**
   * Parse an entry in a document bundle
   * @param entry
   * @return
   */
  private def parseBundleDocumentRequestEntry(entry:Resource):FHIRRequest = {
    //Parse the entry
    val fullUrl = (entry \  FHIR_BUNDLE_FIELDS.FULL_URL).extractOpt[String].filter(_.startsWith("urn:uuid:"))
    //Get the resource
    val resource = (entry \ FHIR_BUNDLE_FIELDS.RESOURCE).extractOpt[JObject].getOrElse(JObject())
    //Get resource type and resource id
    val resourceType = FHIRUtil.extractValue[String](resource, "resourceType")
    val resourceIdOpt = FHIRUtil.extractValueOption[String](resource, "id")
    resourceIdOpt match {
      case None =>
        val fhirRequest = FHIRRequest(interaction = FHIR_INTERACTIONS.CREATE, requestUri = "/" + resourceType)
        fhirRequest.initializeCreateRequest(resourceType, None, None)
        fhirRequest.resource = Some(resource)
        fhirRequest.setId(fullUrl)
      case Some(rid) =>
        val fhirRequest = FHIRRequest(interaction = FHIR_INTERACTIONS.UPDATE, requestUri = "/" + resourceType + "/" + rid)
        fhirRequest.initializeUpdateRequest(resourceType, Some(rid), None, None)
        fhirRequest.resource = Some(resource)
        fhirRequest.setId(fullUrl)
    }
  }

  /**
    * Parse an entry in Bundle for transaction or batch request and convert it to FHIRRequest
    * @param entry
    * @return
    */
  private def parseBundleRequestEntry(entry:Resource):FHIRRequest = {
    //Parse the entry
    val fullUrl = (entry \  FHIR_BUNDLE_FIELDS.FULL_URL).extractOpt[String].filter(_.startsWith("urn:uuid:"))

    val requestMethod = (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_BUNDLE_FIELDS.METHOD).extract[String]
    val requestUrl = (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_BUNDLE_FIELDS.URL).extract[String]
    val resource = (entry \ FHIR_BUNDLE_FIELDS.RESOURCE).extractOpt[JObject].getOrElse(JObject())
    //Headers
    val ifMatch =
      (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_HTTP_OPTIONS.rIF_MATCH)
        .extractOpt[String]
        .map(h => `If-Match`(
          EntityTag(h.split("\"")(1), weak = h.contains("W/"))
        ))
    val ifNoneExist = (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_HTTP_OPTIONS.rIF_NONE_EXIST).extractOpt[String]
    val ifNoneMatch =
      (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_HTTP_OPTIONS.rIF_NONE_MATCH)
        .extractOpt[String]
        .map(h => `If-None-Match`(
          EntityTag(h.split("\"")(1), weak = h.contains("W/"))
        ))
    val ifModifiedSince =
      (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_HTTP_OPTIONS.rIF_MODIFIED_SINCE)
        .extractOpt[String]
        .flatMap(h =>
          DateTimeUtil
            .parseInstant(h)
            .map(v => `If-Modified-Since`(v))
        )

    //Construct the Spray Uri for request
    val sprayUrl = Uri(requestUrl)

    val fhirRequest = new FHIRRequest(interaction = FHIR_INTERACTIONS.UNKNOWN, requestUri = requestUrl)

    requestMethod match {
      //Delete Interaction
      case FHIR_METHOD_NAMES.METHOD_DELETE => {
        parseUrl(sprayUrl) match {
          case Seq(rtype, rid) =>
            fhirRequest.initializeDeleteRequest(rtype, Some(rid))
          case Seq(rtype) =>
            fhirRequest.initializeDeleteRequest(rtype, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          case _ => throw new NotFoundException(invalidOperation(FHIR_INTERACTIONS.DELETE, requestUrl))
        }
      }
      //Update Interaction
      case FHIR_METHOD_NAMES.METHOD_PUT => {
        parseUrl(sprayUrl) match {
          case Seq(rtype, rid) =>
            fhirRequest.initializeUpdateRequest(rtype, Some(rid), ifMatch, None)
            fhirRequest.resource = Some(resource)
            fhirRequest.setId(fullUrl)
          case Seq(rtype) =>
            fhirRequest.initializeUpdateRequest(rtype, None, ifMatch, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
            fhirRequest.resource = Some(resource)
            fhirRequest.setId(fullUrl)
          case _ => throw new NotFoundException(invalidOperation(FHIR_INTERACTIONS.UPDATE, requestUrl))
        }
      }
      case FHIR_METHOD_NAMES.METHOD_PATCH =>
        parseUrl(sprayUrl) match {
          case Seq(rtype, rid) =>
            fhirRequest.initializePatchRequest(rtype, Some(rid), ifMatch, None)
            fhirRequest.resource = Some(resource)
            fhirRequest.setId(fullUrl)
          case Seq(rtype) =>
            fhirRequest.initializePatchRequest(rtype, None, ifMatch, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
            fhirRequest.resource = Some(resource)
            fhirRequest.setId(fullUrl)
          case _ => throw new NotFoundException(invalidOperation(FHIR_INTERACTIONS.PATCH, requestUrl))
        }
      case  FHIR_METHOD_NAMES.METHOD_POST => {
        parseUrl(sprayUrl) match {
          //System level search
          case Seq(FHIR_HTTP_OPTIONS.SEARCH) =>
            fhirRequest.initializeSearchRequest(None)
          //Search with post
          case Seq(rtype, FHIR_HTTP_OPTIONS.SEARCH) =>
            fhirRequest.initializeSearchRequest(rtype, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          //Compartment search with post
          case Seq(ctype, cid, rtype, FHIR_HTTP_OPTIONS.SEARCH) =>
            fhirRequest.initializeCompartmentSearchRequest(ctype, cid, rtype, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          //Create interaction
          case Seq(rtype) =>
            fhirRequest.initializeCreateRequest(rtype, ifNoneExist, None)
            fhirRequest.resource = Some(resource)
            fhirRequest.setId(fullUrl)
          //Type and instance level Operations
          case Seq(rtype, operation) if operation.startsWith("$") =>
            fhirRequest.initializeOperationRequest(operation, Some(rtype))
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
            fhirRequest.resource = Some(resource)
            fhirRequest.setId(fullUrl)
          case Seq(rtype, rid, operation) if  operation.startsWith("$")=>
            fhirRequest.initializeOperationRequest(operation, Some(rtype), Some(rid))
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
            fhirRequest.resource = Some(resource)
            fhirRequest.setId(fullUrl)
          case _ =>
            throw new NotFoundException(invalidOperation("Create or Search or Operation", requestUrl))
        }
      }
      //ORDER IS IMPORTANT
      case FHIR_METHOD_NAMES.METHOD_GET => {
        parseUrl(sprayUrl) match {
          case Nil =>
            fhirRequest.initializeSearchRequest(None)
          case Seq("metadata") =>
            fhirRequest.initializeCapabilitiesRequest()
          //Search with Get
          case Seq(rtype)=>
            fhirRequest.initializeSearchRequest(rtype, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          //History interaction
          case Seq(rtype, FHIR_HTTP_OPTIONS.HISTORY) =>
            fhirRequest.initializeHistoryRequest(FHIR_INTERACTIONS.HISTORY_TYPE, Some(rtype), None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          //Read interaction
          case Seq(rtype, rid) =>
            fhirRequest.initializeReadRequest(rtype, rid, ifModifiedSince, ifNoneMatch, None, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          case Seq(rtype, rid, FHIR_HTTP_OPTIONS.HISTORY) =>
            fhirRequest.initializeHistoryRequest( FHIR_INTERACTIONS.HISTORY_INSTANCE, Some(rtype), Some(rid))
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          //Compartment search with get
          case Seq(ctype, cid, rtype) =>
            fhirRequest.initializeCompartmentSearchRequest(ctype, cid, rtype, None)
            fhirRequest.queryParams = sprayUrl.query().toMultiMap
          //VRead interaction
          case Seq(rtype, rid, FHIR_HTTP_OPTIONS.HISTORY, vid) =>
            fhirRequest.initializeVReadRequest(rtype,rid, vid)
          case _ =>
            throw new NotFoundException(invalidOperation("Invalid HTTP Get", requestUrl))
        }
      }
    }
    //Return the request
    fhirRequest
  }

  /**
    * Return Outcome issues for invalid interaction
    * @param interaction
    * @param path
    * @return
    */
  def invalidOperation(interaction:String, path:String):Seq[OutcomeIssue] = {
    Seq(
      OutcomeIssue(
        FHIRResponse.SEVERITY_CODES.ERROR, //fatal
        FHIRResponse.OUTCOME_CODES.INVALID,
        None,
        Some(s"Invalid type of path for ${interaction} interaction; path: '$path'"),
        Nil
      )
    )
  }

  /**
    * Parse the url and return Seq of segments
    * @param sprayUrl
    * @return
    */
  def parseUrl(sprayUrl:Uri):Seq[String] = {
    sprayUrl.path.toString.split(OnfhirConfig.fhirRootUrl).last.split("/").filterNot(_.equals(""))
  }

}

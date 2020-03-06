package io.onfhir.api.parsers

import akka.http.scaladsl.model.headers.{EntityTag, `If-Modified-Since`, `If-None-Match`}
import akka.http.scaladsl.model.{StatusCodes, Uri}

import io.onfhir.exception._
import io.onfhir.api.{FHIR_BUNDLE_FIELDS, FHIR_HTTP_OPTIONS, FHIR_INTERACTIONS, FHIR_METHOD_NAMES}
import io.onfhir.api.Resource
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
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
    val ifMatch = (entry \ FHIR_BUNDLE_FIELDS.REQUEST \ FHIR_HTTP_OPTIONS.rIF_MATCH).extractOpt[String]
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

    requestMethod match {
      //Delete Interaction
      case FHIR_METHOD_NAMES.METHOD_DELETE => {
        parseUrl(sprayUrl) match {
          case Seq(rtype, rid) =>
            FHIRRequest(interaction = FHIR_INTERACTIONS.DELETE, requestUri = requestUrl, resourceType = Some(rtype), resourceId = Some(rid))
          case Seq(rtype) =>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.DELETE, requestUri = requestUrl, resourceType = Some(rtype), queryParams = queryParams)
          case _ => throw new NotFoundException(invalidOperation(FHIR_INTERACTIONS.DELETE, requestUrl))
        }
      }
      //Update Interaction
      case FHIR_METHOD_NAMES.METHOD_PUT => {
        parseUrl(sprayUrl) match {
          case Seq(rtype, rid) =>
            FHIRRequest(interaction = FHIR_INTERACTIONS.UPDATE, requestUri = requestUrl, resourceType = Some(rtype), resourceId = Some(rid), ifMatch = ifMatch, resource = Some(resource))
              .setId(fullUrl)
          case Seq(rtype) =>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.DELETE, requestUri = requestUrl, resourceType = Some(rtype), queryParams = queryParams, resource = Some(resource))
              .setId(fullUrl)
          case _ => throw new NotFoundException(invalidOperation(FHIR_INTERACTIONS.UPDATE, requestUrl))
        }
      }
      case  FHIR_METHOD_NAMES.METHOD_POST => {
        parseUrl(sprayUrl) match {
          //Search with post
          case Seq(rtype, FHIR_HTTP_OPTIONS.SEARCH) =>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = requestUrl, resourceType = Some(rtype), queryParams = queryParams)
          //Compartment search with post
          case Seq(ctype, cid, rtype, FHIR_HTTP_OPTIONS.SEARCH) =>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = requestUrl, compartmentType = Some(ctype), compartmentId = Some(cid), resourceType = Some(rtype), queryParams = queryParams)
          //Create interaction
          case Seq(rtype) =>
            FHIRRequest(interaction = FHIR_INTERACTIONS.CREATE, requestUri = requestUrl, resource = Some(resource), resourceType = Some(rtype), ifNoneExist = ifNoneExist)
              .setId(fullUrl)
          //Type and instance level Operations
          case Seq(rtype, operation) if operation.startsWith("$") =>
            FHIRRequest(interaction = operation, requestUri = requestUrl, resourceType = Some(rtype), operationParameters = sprayUrl.query().toMultiMap, resource = Some(resource))
          case Seq(rtype, rid, operation) if  operation.startsWith("$")=>
            FHIRRequest(interaction = operation, requestUri = requestUrl, resourceType = Some(rtype), resourceId = Some(rid), operationParameters = sprayUrl.query().toMultiMap, resource = Some(resource))
          case _ =>
            throw new NotFoundException(invalidOperation("Create or Search or Operation", requestUrl))
        }
      }
      //ORDER IS IMPORTANT
      case FHIR_METHOD_NAMES.METHOD_GET => {
        parseUrl(sprayUrl) match {
          case Seq("metadata") =>
            FHIRRequest(interaction = FHIR_INTERACTIONS.CAPABILITIES, requestUri = requestUrl)
          //Search with Get
          case Seq(rtype)=>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = requestUrl, resourceType = Some(rtype), queryParams = queryParams)
          //History interaction
          case Seq(rtype, FHIR_HTTP_OPTIONS.HISTORY) =>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.HISTORY_TYPE, requestUri = requestUrl, resourceType = Some(rtype), queryParams = queryParams)
          //Read interaction
          case Seq(rtype, rid) =>
            FHIRRequest(interaction = FHIR_INTERACTIONS.READ, requestUri = requestUrl, resourceType = Some(rtype), resourceId = Some(rid), ifModifiedSince = ifModifiedSince, ifNoneMatch = ifNoneMatch)
          case Seq(rtype, rid, FHIR_HTTP_OPTIONS.HISTORY) =>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.HISTORY_INSTANCE, requestUri = requestUrl, resourceType = Some(rtype), resourceId = Some(rid),queryParams = queryParams)
          //Compartment search with get
          case Seq(ctype, cid, rtype) =>
            val queryParams = FHIRSearchParameterValueParser.parseSearchParameters(rtype, sprayUrl.query().toMultiMap)
            FHIRRequest(interaction = FHIR_INTERACTIONS.SEARCH, requestUri = requestUrl, compartmentType = Some(ctype), compartmentId = Some(cid), resourceType = Some(rtype), queryParams = queryParams)
          //VRead interaction
          case Seq(rtype, rid, FHIR_HTTP_OPTIONS.HISTORY, vid) =>
            FHIRRequest(interaction = FHIR_INTERACTIONS.VREAD, requestUri = requestUrl, resourceType = Some(rtype), resourceId = Some(rid), versionId = Some(vid))
          case _ =>
            throw new NotFoundException(invalidOperation("Invalid HTTP Get", requestUrl))
        }
      }
    }
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

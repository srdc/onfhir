package io.onfhir.api.model

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{ETag, EntityTag, Location, `Last-Modified`}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import io.onfhir.api.Resource
import io.onfhir.config.FhirConfigurationManager
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.util.JsonFormatter._
import org.apache.commons.lang3.StringEscapeUtils
import org.json4s.JsonAST.JObject

import scala.collection.immutable.StringOps

/**
  * FHIR Marshallers for FHIR Resources and FHIR response messages
  */
object FHIRMarshallers {

  /**
    * Umarshaller for FHIR request HTTP bodies, umarshall to our Resource (LinkedHashMap) model
    */
  implicit val FHIRResourceUnmarshaller:Unmarshaller[HttpEntity, Resource] =
    Unmarshaller
      .stringUnmarshaller
      .forContentTypes(fhirConfig.FHIR_SUPPORTED_CONTENT_TYPE_RANGES:_*)
      .mapWithInput {
        case (entity: HttpEntity, data: String) =>
          if (entity.isKnownEmpty() || data.isEmpty) {
            JObject()
          } else {
            //We support text/plain for command line testing purposes and assume it is JSON
            if(entity.contentType.mediaType.matches(MediaTypes.`text/plain`)){
              convertToMap(data)
            } else
            //JSON Unmarshalling
            if (fhirConfig.FHIR_JSON_MEDIA_TYPES.exists(supportedJsonMediaType => entity.contentType.mediaType.matches(supportedJsonMediaType))
            ) {
              convertToMap(data)
            } else if ( fhirConfig.FHIR_XML_MEDIA_TYPES.exists(supportedXmlMediaType => entity.contentType.mediaType.matches(supportedXmlMediaType))
            ) {
              //XML Unmarshalling
              new XmlToJsonConvertor(FhirConfigurationManager.fhirConfig).parseFromXml(data).parseXML
            } else if (fhirConfig.FHIR_JSON_PATCH_MEDIA_TYPE.exists(e => entity.contentType.mediaType.matches(e))

            ) {
              //JSON Patch
              val patch = "{ \"patches\":" + s"$data }"
              val patchResource = convertToMapForNonFHIR(patch)
              patchResource
            } else {
              null
            }
          }
      }

  /**
    * Unmarshaller for full FHIR Response
    */
  implicit val FHIRResponseMarshaller:ToResponseMarshaller[FHIRResponse] =
    Marshaller.oneOf(
      fhirConfig.FHIR_SUPPORTED_RESULT_CONTENT_TYPES.map(ctype => ctype.mediaType match {
        case jsontype if fhirConfig.FHIR_JSON_MEDIA_TYPES.contains(jsontype) =>
          Marshaller.withFixedContentType[FHIRResponse, HttpResponse](ctype)(jsonResponseMarshaller(ctype))
        case xmltype if  fhirConfig.FHIR_XML_MEDIA_TYPES.contains(xmltype) =>
          Marshaller.withFixedContentType[FHIRResponse, HttpResponse](ctype)(xmlResponseMarshaller(ctype))
        case htmltype  =>
          Marshaller.withFixedContentType[FHIRResponse, HttpResponse](ctype)(htmlResponseMarshaller(ContentTypes.`text/html(UTF-8)`))
      }):_*)

  /**
    * FHIRResponse to HttpResponse with JSON body marshaller
    * @param contentType
    * @param fhirResponse
    * @return
    */
  private def jsonResponseMarshaller(contentType: ContentType)(fhirResponse:FHIRResponse): HttpResponse = {
    val httpResponse = buildHttpResponseWithoutEntity(fhirResponse) //Build basics of the response
    getResponseBody(fhirResponse) match {
      case Some(body) => httpResponse.withEntity(HttpEntity(contentType, body.toJson.getBytes("UTF-8"))) //add the body
      case None => httpResponse.withEntity(HttpEntity.empty(contentType))
    }
  }

  /**
    * FHIRResponse to HttpResponse with XML body marshaller
    * @param contentType
    * @param fhirResponse
    * @return
    */
  private def xmlResponseMarshaller(contentType: ContentType)(fhirResponse:FHIRResponse): HttpResponse = {
    val httpResponse = buildHttpResponseWithoutEntity(fhirResponse) //Build basics of the response
    getResponseBody(fhirResponse) match {
      case Some(body) => httpResponse.withEntity(HttpEntity(contentType, convertToXml(body).getBytes("UTF-8"))) //add the body
      case None => httpResponse.withEntity(HttpEntity.empty(contentType))
    }
  }

  /**
    * FHIRResponse to HttpResponse with HTML body marshaller
    * @param contentType
    * @param fhirResponse
    * @return
    */
  private def htmlResponseMarshaller(contentType: ContentType.NonBinary)(fhirResponse:FHIRResponse): HttpResponse = {
    val httpResponse = buildHttpResponseWithoutEntity(fhirResponse) //Build basics of the response
    getResponseBody(fhirResponse) match {
      case Some(body) => httpResponse.withEntity(HttpEntity(contentType, prepareHtml(body))) //add the body
      case None => httpResponse.withEntity(HttpEntity.empty(contentType))
    }
  }

  /**
    * Convert a FHIR resource (our internal map model) to XML
    * Currently using HAPI; first serialize to JSON then parse by HAPI and serialize to XML finally
    * @param resource
    * @return
    */
  private def convertToXml(resource: Resource):String = {
    new JsonToXmlConvertor().convertToXml(resource).toXml
  }

  /**
    * Prepare HTML rendering of a FHIR resource
    * @param resource
    * @return
    */
  private def prepareHtml(resource: Resource): String ={
    val prettyJson = resource.toPrettyJson
    val prettyHtmlJson = new StringOps(prettyJson).lines.map(l => {

      val prefixSpace = " " * l.indexOf(l.trim())
      val trimmed = l.trim()
      trimmed.head match {
        case '{' | '}' | ']' | '['  => s"${prefixSpace}<span class='fControl'>${trimmed}</span>"
        case '"' =>
          val sprt = trimmed.indexOf(':')
          if(sprt != -1) {
            val firstPart = trimmed.substring(0, sprt).trim()
            val secondPart = trimmed.substring(sprt + 1, trimmed.length).trim()
            val tagName = s"${prefixSpace}<span class='fTagName'>${StringEscapeUtils.escapeHtml4(firstPart)}</span>"
            val tagValue = secondPart.head match {
              case '[' | '{' => s"<span class='fControl'>${secondPart}</span>"
              case _ => {
                if(secondPart.endsWith(","))
                  s"<span class='fAttr'>${StringEscapeUtils.escapeHtml4(secondPart.substring(0, secondPart.length-1))}</span><span class='fControl'>,</span>"
                else
                  s"<span class='fAttr'>${StringEscapeUtils.escapeHtml4(secondPart)}</span>"
              }
            }
            tagName + ":" + tagValue
          }
          // This is the case of a string array; each string in a separate line
          else {
            if(trimmed.endsWith(","))
              s"${prefixSpace}<span class='fAttr'>${StringEscapeUtils.escapeHtml4(trimmed.substring(0, trimmed.length-1))}</span><span class='fControl'>,</span>"
            else
              s"${prefixSpace}<span class='fAttr'>${StringEscapeUtils.escapeHtml4(trimmed)}</span>"
          }
        case _ => ""
      }
    }).mkString("\n")


    val htmlResponse =
      "<html lang=\"en\">" +
        "<head>"+
        "<meta charset=\"utf-8\">" +
        "<style>"+
        ".fAttr { color: #888;}"+
        ".fTagName { color: #006699;}"+
        ".fControl { color: #660000;}" +
        ".headersDiv { background: #EEE;}.headerName { color: #888;font-family: monospace;}"+
        ".headerValue {color: #88F;font-family: monospace;}"+
        "</style>"+
        "</head>"+
        "<body>"+
        "<p>This result is being rendered in HTML for easy viewing. You may access this content as <a href=\"?_format=application/json\">Raw JSON</a> or <a href=\"?_format=application/xml\">Raw XML</a></p>"+
        s"<pre>${prettyHtmlJson}</pre>"+
        "</body>"+
        "</html>"

    htmlResponse
  }

  /**
    * Helper method to build HttpResponse headers
    * @param fResponse
    * @return
    */
  private def buildHttpResponseWithoutEntity(fResponse:FHIRResponse):HttpResponse = {
    //Construct Http Headers
    val headers = Seq(
      fResponse.lastModified.map(lm => `Last-Modified`(lm)), // Last-Modified header
      fResponse.newVersion.map(nw => ETag(EntityTag(nw.toString, weak = true))), // ETag header
      fResponse.location.map(l => Location(l)),  // Location Header
      fResponse.authenticateHeader //WWW-Authenticate header if any
    ).filter(_.isDefined).map(_.get).toList

    var response = HttpResponse(fResponse.httpStatus)
    if(headers.nonEmpty)
      response = response.withHeaders(headers)

    response
  }

  /**
    * Retrieve the body of response either as success or OperationOutcome
    * @param fResponse
    * @return
    */
  private def getResponseBody(fResponse:FHIRResponse):Option[Resource] = {
    if(fResponse.responseBody.isDefined)
      fResponse.responseBody
    else if(fResponse.outcomeIssues.nonEmpty)
      Some(FHIRResponse.createOperationOutcome(fResponse.outcomeIssues))
    else
      None
  }


  /**
    * Converts a resource in JSON format into mutable Map. While converting, it ignores
    * meta.versionId and meta.lastUpdated values
    *
    * @param json resource in JSON Format
    * @return A mutable map for the given resource
    */
  def convertToMap(json: String): Resource = {
    json.parseJson
  }

  def convertToMapForNonFHIR(json: String): Resource = {
    json.parseJson
  }
}

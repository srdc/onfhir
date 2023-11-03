package io.onfhir.client.parsers

import akka.http.scaladsl.marshalling.{Marshal, Marshaller, ToRequestMarshaller}
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.headers.{Accept, RawHeader}
import akka.http.scaladsl.model._
import io.onfhir.api.client.FhirClientException
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.{FHIR_CONTENT_TYPES, FHIR_INTERACTIONS, Resource}
import io.onfhir.util.DateTimeUtil
import org.json4s.JArray
import org.json4s.JsonAST.{JObject, JString}
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonDSL._
import org.json4s.jackson.Serialization
import org.json4s.{JsonAST, _}

import scala.concurrent.{ExecutionContext, Future}

object FHIRRequestMarshaller {

  def marshallRequest(fhirRequest: FHIRRequest, fhirServerBaseUrl: Uri)(implicit ex: ExecutionContext): Future[HttpRequest] = {
    implicit val marshaller = getFhirRequestMarshaller(fhirRequest, fhirServerBaseUrl)
    Marshal.apply(fhirRequest).to[HttpRequest]
  }

  /**
   * Get marshaller for FHIRRequest
   *
   * @param fhirRequest
   * @param fhirServerBaseUrl
   * @return
   */
  private def getFhirRequestMarshaller(fhirRequest: FHIRRequest, fhirServerBaseUrl: Uri): ToRequestMarshaller[FHIRRequest] = {
    val contentType = fhirRequest.contentType.getOrElse(FHIR_CONTENT_TYPES.FHIR_JSON_CONTENT_TYPE)
    Marshaller.withFixedContentType[FHIRRequest, HttpRequest](contentType)(requestMarshaller(contentType, fhirServerBaseUrl))
  }

  /**
   * FHIR request marshaller method
   *
   * @param ctype
   * @param fhirServerBaseUrl
   * @param fhirRequest
   * @return
   */
  private def requestMarshaller(ctype: ContentType.WithCharset, fhirServerBaseUrl: Uri)(fhirRequest: FHIRRequest): HttpRequest = {
    val method = getHttpMethod(fhirRequest)
    var request =
      HttpRequest()
        .withMethod(method)
        .withUri(getRequestUri(fhirRequest, fhirServerBaseUrl, method))
        .withHeaders(getHeaders(fhirRequest))

    getEntity(fhirRequest, method, fhirServerBaseUrl, ctype).foreach(e =>
      request = request.withEntity(e)
    )
    request
  }

  /**
   * Create the Http Request Entity (body)
   *
   * @param fhirRequest
   * @param method
   * @param fhirServerBaseUrl
   * @param ctype
   * @return
   */
  private def getEntity(fhirRequest: FHIRRequest, method: HttpMethod, fhirServerBaseUrl: Uri, ctype: ContentType.WithCharset): Option[RequestEntity] = {
    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.CREATE | FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.PATCH =>
        val body = fhirRequest.resource.getOrElse(JObject())
        Some(getEntityContent(body, ctype))
      case FHIR_INTERACTIONS.BATCH | FHIR_INTERACTIONS.TRANSACTION =>
        //Construct the bundle
        val body = JObject(
          "resourceType" -> JString("Bundle"),
          "type" -> JString(fhirRequest.interaction),
          "entry" -> JArray(fhirRequest.childRequests.map(createBundleRequestEntry(_, fhirServerBaseUrl)).toList)
        )
        Some(getEntityContent(body, ctype))
      case FHIR_INTERACTIONS.SEARCH if method == HttpMethods.POST =>
        getQuery(fhirRequest.queryParams).map(FormData(_).toEntity)
      case opr if opr.startsWith("$") =>
        fhirRequest
          .resource
          .map(r => getEntityContent(r, ctype))
      case _ =>
        None
    }
  }

  /**
   * Get the entity content
   *
   * @param body
   * @param ctype
   * @return
   */
  private def getEntityContent(body: Resource, ctype: ContentType.WithCharset): HttpEntity.Strict = {
    ctype match {
      case FHIR_CONTENT_TYPES.FHIR_JSON_CONTENT_TYPE  =>
        HttpEntity.apply(ctype, body.toJson)
      case FHIR_CONTENT_TYPES.FHIR_JSON_PATCH_CONTENT_TYPE =>
        //While request building we add the patches element to make the array JObject, now we get rid of it
        HttpEntity.apply(ctype,Serialization.write((body \ "patches" )))
      case FHIR_CONTENT_TYPES.FHIR_XML_CONTENT_TYPE | FHIR_CONTENT_TYPES.FHIR_XML_PATCH_CONTENT_TYPE =>
        throw FhirClientException("Currently, XML based content types are not supported in OnFhirClient!")
      case oth =>
        throw FhirClientException(s"Content type $oth is not supported in OnFhirClient!")
    }
  }

  /**
   * Create Bundle.entry.request object from child FHIRRequest
   *
   * @param childRequest
   * @param fhirServerBaseUrl
   * @return
   */
  private def createBundleRequestEntry(childRequest: FHIRRequest, fhirServerBaseUrl: Uri): JObject = {
    val fullUrlField = if (!childRequest.isIdGenerated) Some("fullUrl" -> JString(childRequest.id)) else None
    val resourceField = childRequest.resource.map(r => "resource" -> r)

    val method = getHttpMethod(childRequest)
    var request = JObject(
      "method" -> JString(method.value),
      "url" ->
        JString(
          getRequestUri(childRequest, fhirServerBaseUrl, method)
            .toString()
            .replace(
              if(fhirServerBaseUrl.toString().endsWith("/")) fhirServerBaseUrl.toString() else fhirServerBaseUrl.toString() + "/",
              "")
        )
    )
    childRequest.ifMatch.foreach(im => request = request ~ ("ifMatch" -> JString(im.m.toString())))
    childRequest.ifModifiedSince.foreach(im => request = request ~ ("ifModifiedSince" -> JString(DateTimeUtil.serializeDateTime(im.date))))
    childRequest.ifNoneMatch.foreach(in => request = request ~ ("ifNoneMatch" -> JString(in.m.toString())))
    childRequest.ifNoneExist.foreach(in => request = request ~ ("ifNoneExist" -> JString(in)))
    JObject(
      (fullUrlField.toSeq ++ resourceField.toSeq ++ Seq("request" -> request)).toList
    )
  }

  /**
   * Decide on HTTP method
   *
   * @param fhirRequest
   * @return
   */
  private def getHttpMethod(fhirRequest: FHIRRequest): HttpMethod = {
    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.CREATE |
           FHIR_INTERACTIONS.TRANSACTION |
           FHIR_INTERACTIONS.BATCH =>
        HttpMethods.POST

      case FHIR_INTERACTIONS.UPDATE => HttpMethods.PUT
      case FHIR_INTERACTIONS.PATCH => HttpMethods.PATCH
      case FHIR_INTERACTIONS.DELETE => HttpMethods.DELETE

      case FHIR_INTERACTIONS.READ |
           FHIR_INTERACTIONS.VREAD |
           FHIR_INTERACTIONS.HISTORY_INSTANCE |
           FHIR_INTERACTIONS.HISTORY_TYPE |
           FHIR_INTERACTIONS.HISTORY_SYSTEM |
           FHIR_INTERACTIONS.SEARCH |
           FHIR_INTERACTIONS.CAPABILITIES =>

        fhirRequest.httpMethod.getOrElse(HttpMethods.GET)

      case opr if opr.startsWith("$") =>
        fhirRequest.httpMethod.getOrElse(HttpMethods.POST)
      case oth =>
        throw FhirClientException(s"Invalid FHIR interaction $oth!")
    }
  }

  /**
   * Construct query string
   *
   * @param queryParams
   * @return
   */
  private def getQuery(queryParams: Map[String, List[String]]): Option[Query] = {
    if (queryParams.nonEmpty)
      Some(Query.apply(queryParams.map(qp => qp._2.map(qpv => qp._1 -> qpv)).toSeq.flatten:_*))
    else
      None
  }

  /**
   * Construct request Uri
   *
   * @param fhirRequest
   * @param method
   * @return
   */
  private def getRequestUri(fhirRequest: FHIRRequest, fhirServerBaseUri: Uri, method: HttpMethod): Uri = {
    val basePath = fhirServerBaseUri.path

    val (path: Uri.Path, query: Option[Uri.Query]) =
      fhirRequest.interaction match {
        case FHIR_INTERACTIONS.TRANSACTION | FHIR_INTERACTIONS.BATCH =>
          basePath -> None

        case FHIR_INTERACTIONS.CREATE =>
          basePath./(fhirRequest.resourceType.get) -> None

        case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.PATCH | FHIR_INTERACTIONS.DELETE =>
          if (fhirRequest.resourceId.isDefined)
            basePath./(fhirRequest.resourceType.get)./(fhirRequest.resourceId.get) -> None
          else
            basePath./(fhirRequest.resourceType.get) -> getQuery(fhirRequest.queryParams)

        case FHIR_INTERACTIONS.READ =>
          basePath./(fhirRequest.resourceType.get)./(fhirRequest.resourceId.get) -> getQuery(fhirRequest.queryParams)

        case FHIR_INTERACTIONS.VREAD =>
          basePath./(fhirRequest.resourceType.get)./(fhirRequest.resourceId.get)./("_history")./(fhirRequest.versionId.get) -> None

        case FHIR_INTERACTIONS.HISTORY_INSTANCE =>
          basePath./(fhirRequest.resourceType.get)./(fhirRequest.resourceId.get)./("_history") -> getQuery(fhirRequest.queryParams)
        case FHIR_INTERACTIONS.HISTORY_TYPE =>
          basePath./(fhirRequest.resourceType.get)./("_history") -> getQuery(fhirRequest.queryParams)
        case FHIR_INTERACTIONS.HISTORY_SYSTEM =>
          basePath./("_history") -> getQuery(fhirRequest.queryParams)
        case FHIR_INTERACTIONS.CAPABILITIES =>
          basePath./("metadata") -> None
        case FHIR_INTERACTIONS.SEARCH =>
          var temp =
            if (fhirRequest.compartmentType.isDefined)
              basePath./(fhirRequest.compartmentType.get)./(fhirRequest.compartmentId.get)
            else
              basePath

          if (fhirRequest.resourceType.isDefined)
            temp = temp./(fhirRequest.resourceType.get)

          if(method == HttpMethods.POST)
            temp = temp./("_search")

          temp -> (if (method == HttpMethods.GET) getQuery(fhirRequest.queryParams) else None)

        case opr if opr.startsWith("$") =>
          var temp = basePath
          fhirRequest.resourceType.foreach(rt => temp = temp./(rt))
          fhirRequest.resourceId.foreach(rid => temp = temp./(rid))
          temp./(opr) -> (if (fhirRequest.queryParams.nonEmpty) getQuery(fhirRequest.queryParams) else None)
      }

    var resultUri = fhirServerBaseUri.withPath(path)
    if (query.isDefined)
      resultUri = resultUri.withQuery(query.get)

    resultUri
  }

  /**
   * Construct HTTP headers
   *
   * @param fhirRequest
   * @return
   */
  private def getHeaders(fhirRequest: FHIRRequest): Seq[HttpHeader] = {
    fhirRequest.prefer.toSeq.map(p => RawHeader("Prefer", p)) ++
      fhirRequest.ifNoneMatch.toSeq ++
      fhirRequest.ifModifiedSince.toSeq ++
      fhirRequest.ifMatch.toSeq ++
      fhirRequest.ifNoneExist.map(q => RawHeader("If-None-Exist", q)).toSeq ++
      Seq(
        Accept.apply(MediaRange.apply(MediaType.applicationWithOpenCharset("fhir+json"))), //
        RawHeader("X-Request-Id", fhirRequest.id) //request id
      )
  }


}

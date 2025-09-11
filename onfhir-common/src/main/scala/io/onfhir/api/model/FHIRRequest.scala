package io.onfhir.api.model

import java.time.Instant
import akka.http.scaladsl.model.{ContentType, HttpMethod, Uri}
import akka.http.scaladsl.model.headers.{`If-Match`, `If-Modified-Since`, `If-None-Match`, `X-Forwarded-For`, `X-Forwarded-Host`}
import io.onfhir.api.Resource
import io.onfhir.api._
import io.onfhir.api.parsers.BundleRequestParser
import io.onfhir.util.JsonFormatter
import io.onfhir.util.JsonFormatter.formats

/**
  * Created by tuncay on 5/11/2017.
  * Object to represent any FHIR request (all FHIR interactions)
  * @param id                     Unique (generated or set) id for the FHIR request
  * @param interaction            Name of FHIR interaction or operation name(with $)
  * @param requestUri             The whole Request URI
  * @param resourceType           FHIR Resource Type given in the request if relevant
  * @param resourceId             Resource identifier given in the request if relevant
  * @param versionId              Version identifier given in the request if relevant
  * @param compartmentType        FHIR Compartment type if this is a compartment search
  * @param compartmentId          Compartment id if this is a compartment search
  * @param resource               The body of request (parsed into Json4s model)
  * @param queryParams            Supplied search parameters if this is a search or conditional operation
  * @param ifNoneExist            FHIR IfNone Header
  * @param prefer                 FHIR Prefer Header
  * @param ifMatch                FHIR IfMatch Header
  * @param ifNoneMatch            FHIR IfNoneMatch Header
  * @param ifModifiedSince        FHIR IfModifiedSince Header
  */
case class FHIRRequest(
                        var id:String = s"urn:uuid:${java.util.UUID.randomUUID.toString}",
                        var interaction:String,
                        requestUri:String,
                        var resourceType:Option[String] = None,
                        var resourceId:Option[String] = None,
                        var versionId:Option[String] = None,
                        var compartmentType:Option[String] = None,
                        var compartmentId:Option[String] = None,
                        var resource:Option[Resource] = None,
                        var queryParams:Map[String, List[String]] =  Map.empty,
                        //var operationParameters:Map[String, List[String]] = Map.empty,
                        //HTTP Headers used in different interactions
                        var ifNoneExist:Option[String] = None,
                        var prefer:Option[String] = None,
                        var ifMatch:Option[`If-Match`] = None,
                        var ifNoneMatch: Option[`If-None-Match`] = None,
                        var ifModifiedSince:Option[`If-Modified-Since`] = None,
                        var xForwardedFor:Option[`X-Forwarded-For`] = None,
                        var xForwardedHost:Option[`X-Forwarded-Host`] = None,
                        var xIntermediary:Option[String] = None,
                        //Other contextual params
                        var contentType: Option[ContentType.WithCharset] = None,
                        var isIdGenerated:Boolean = true, //If we generate the id of request or is it come from the Bundle request
                        var requestTime:Instant = Instant.now(), //Time when request is constructed
                        var response:Option[FHIRResponse] = None, // FHIR response to the request
                        var responseTime:Option[Instant] = None, // Time that response is ready
                        var childRequests:Seq[FHIRRequest] = Nil, // Child requests for batch and transaction
                        //Other client specific params
                        var httpMethod:Option[HttpMethod] = None //Preferred HTTP method when there is alternative
                      ) extends InternalEntity {
  // Parsed query parameters for the resource type
  private var parsedQueryParams:Either[List[Parameter], Map[String, List[Parameter]]] = Left(List.empty)

  def addParsedQueryParams(params:List[Parameter]):Unit = {
    parsedQueryParams = Left(parsedQueryParams.left.getOrElse(List.empty[Parameter]) ++ params)
  }

  def addParsedQueryParams(rtype:String, params:List[Parameter]):Unit = {
    val tempMap =
      parsedQueryParams
        .swap
        .left
        .getOrElse(Map.empty[String, List[Parameter]])

    parsedQueryParams = Right(
      tempMap
        .get(rtype) match {
          case Some(p) => tempMap ++ Map(rtype -> (p ++ params))
          case None =>  tempMap ++ Map(rtype -> params)
        }
    )
  }

  def getParsedQueryParams():List[Parameter] = parsedQueryParams.left.getOrElse(List.empty[Parameter])

  def getAllParsedQueryParams():Map[String, List[Parameter]] = parsedQueryParams.swap.left.getOrElse(Map.empty[String, List[Parameter]])

  //Set id for the request externally
  def setId(id:Option[String]):FHIRRequest = {
    if(id.isDefined) {
      this.id = id.get
      isIdGenerated = false
    }
    this
  }

  //Set FHIR response to this request
  def setResponse(resp:FHIRResponse): Unit ={
    response = Some(resp)
    responseTime = Some(Instant.now())
  }
  /**
    * Initialize a FHIR create request
    * @param resourceType   FHIR Resource Type
    * @param ifNoneExist    FHIR IfNoneExist Header value
    * @param prefer         FHIR Prefer Header value
    */
  def initializeCreateRequest(resourceType:String, ifNoneExist:Option[String], prefer:Option[String]):Unit = {
    this.interaction = FHIR_INTERACTIONS.CREATE
    this.resourceType = Some(resourceType)
    this.ifNoneExist = ifNoneExist
    this.prefer = prefer
  }

  def initializeSearchRequest(prefer:Option[String]):Unit = {
    this.interaction = FHIR_INTERACTIONS.SEARCH_SYSTEM
    this.prefer = prefer
  }

  /**
    * Initialize a FHIR Search request
    * @param resourceType   FHIR Resource Type to search
    * @param prefer         FHIR Prefer Header value
    */
  def initializeSearchRequest(resourceType:String, prefer:Option[String]):Unit = {
    this.interaction = FHIR_INTERACTIONS.SEARCH
    this.resourceType = Some(resourceType)
    this.prefer = prefer
  }

  /**
    * Initialize a FHIR Compartment Search request
    * @param compartmentType  FHIR Compartment Type
    * @param compartmentId    FHIR Compartment Id
    * @param resourceType     FHIR Resource Type
    * @param prefer           FHIR Prefer Header value
    */
  def initializeCompartmentSearchRequest(compartmentType:String, compartmentId:String, resourceType:String, prefer:Option[String]):Unit = {
    this.interaction = if(resourceType == "*") FHIR_INTERACTIONS.SEARCH_SYSTEM else FHIR_INTERACTIONS.SEARCH
    this.compartmentType = Some(compartmentType)
    this.compartmentId = Some(compartmentId)
    this.resourceType = if(resourceType == "*") None else Some(resourceType)
    this.prefer = prefer
  }

  /**
    * Initialize a FHIR update request
    * @param resourceType   FHIR Resource Type
    * @param resourceId     Id of the resource to be updated
    * @param ifMatch        FHIR IfMatch Header value
    * @param prefer         FHIR Prefer Header value
    */
  def initializeUpdateRequest(resourceType:String, resourceId:Option[String], ifMatch:Option[`If-Match`], prefer:Option[String]):Unit = {
    this.interaction = FHIR_INTERACTIONS.UPDATE
    this.resourceType = Some(resourceType)
    this.resourceId  = resourceId
    this.prefer = prefer
    this.ifMatch = ifMatch
  }

  /**
    * Initialize a FHIR delete request
    * @param resourceType   FHIR Resource Type
    * @param resourceId     Id of the resource to be deleted
    * @param prefer         FHIR Prefer Header value
    */
  def initializeDeleteRequest(resourceType:String, resourceId:Option[String], prefer:Option[String] = None):Unit = {
    this.interaction = FHIR_INTERACTIONS.DELETE
    this.resourceType = Some(resourceType)
    this.resourceId  = resourceId
    this.prefer = prefer
  }

  /**
    * Initialize a FHIR capabilities request
    */
  def initializeCapabilitiesRequest():Unit = {
    this.interaction = FHIR_INTERACTIONS.CAPABILITIES
    this.resourceId = Some(SERVER_CONFORMANCE_STATEMENT_ID)
    this.resourceType = Some("CapabilityStatement")
  }

  /**
    * Initialize a FHIR read request
    * @param resourceType     FHIR Resource Type
    * @param resourceId       Id of the resource to be retrieved
    * @param ifModifiedSince  FHIR IfModifiedSince Header value
    * @param ifNoneMatch      FHIR IfNoneMatch Header value
    * @param summary          FHIR _summary parameter value if exists
    * @param elements         FHIR _elements parameter value if exists
    */
  def initializeReadRequest(resourceType:String,
                            resourceId:String,
                            ifModifiedSince:Option[`If-Modified-Since`],
                            ifNoneMatch: Option[`If-None-Match`],
                            summary:Option[String],
                            elements:Option[String]
                           ):Unit = {
    this.interaction = FHIR_INTERACTIONS.READ
    this.resourceType = Some(resourceType)
    this.resourceId  = Some(resourceId)
    this.ifModifiedSince = ifModifiedSince
    this.ifNoneMatch = ifNoneMatch
    this.queryParams =
      (
        summary.map(s => "_summary" -> List(s)).toSeq ++
        elements.map(e => "_elements" -> List(e)).toSeq
      ).toMap
  }

  /**
    * Initialize a FHIR vread request
    * @param resourceType   FHIR Resource Type
    * @param resourceId     Id of the resource to be retrieved
    * @param versionId      Version id of the resource to be retrieved
    */
  def initializeVReadRequest(resourceType:String, resourceId:String, versionId:String):Unit = {
    this.interaction = FHIR_INTERACTIONS.VREAD
    this.resourceType = Some(resourceType)
    this.resourceId  = Some(resourceId)
    this.versionId = Some(versionId)
  }

  /**
    * Initialize a FHIR patch request
    * @param resourceType   FHIR Resource Type
    * @param resourceId     Id of the resource to be patched
    * @param ifMatch        FHIR IfMatch Header value
    * @param prefer         FHIR Prefer Header value
    */
  def initializePatchRequest(resourceType:String, resourceId:Option[String], ifMatch:Option[`If-Match`], prefer:Option[String]):Unit = {
    this.interaction = FHIR_INTERACTIONS.PATCH
    this.resourceType = Some(resourceType)
    this.resourceId  = resourceId
    this.prefer = prefer
    this.ifMatch = ifMatch
  }

  /**
    * Initialize a FHIR history request
    * @param interaction    FHIR Interaction name
    * @param resourceType   FHIR Resource Type
    * @param resourceId     Id of the resource
    */
  def initializeHistoryRequest(interaction:String, resourceType:Option[String], resourceId:Option[String]):Unit = {
    this.interaction = interaction
    this.resourceType = resourceType
    this.resourceId  = resourceId
  }

  /**
    * Initialize a FHIR operation request
    * @param operation      FHIR Operation Name
    * @param resourceType   FHIR Resource Type that operation is performed
    * @param resourceId     If of resource that operation is performed
    * @param versionId      Version id of resource
    */
  def initializeOperationRequest(operation:String, resourceType:Option[String] = None, resourceId:Option[String] = None, versionId:Option[String] = None):Unit = {
    this.interaction = operation
    this.resourceType = resourceType
    this.resourceId = resourceId
    this.versionId = versionId
  }

  /**
    * Initialize FHIR transaction or batch request
    * @param resource Transcation/Batch request body
    */
  def initializeTransactionOrBatchRequest(resource: Resource, prefer:Option[String] = None): Unit = {
    this.interaction = (resource \ FHIR_COMMON_FIELDS.TYPE).extractOpt[String].getOrElse("unknown")
    this.resource = Some(resource)
    this.childRequests =
      if(interaction == FHIR_BUNDLE_TYPES.DOCUMENT) {
        this.interaction = FHIR_INTERACTIONS.TRANSACTION
        BundleRequestParser.parseBundleDocumentRequest(resource, prefer)
      } else
        BundleRequestParser.parseBundleRequest(resource, prefer)
    //Set prefer header
    this.prefer = prefer
  }

  /**
    * Get Location of request in transaction (Used for identifiying erroneous requests within transactions/batchs)
    */
  def getRequestLocation():String = s"Request Id: ${id}, Request Url: ${requestUri}"

  /**
   * Get a log string summarizing the FHIR request
   * @return
   */
  def getSummaryString():String = {
    val attributes =
      Seq(
      "Interaction" -> interaction,
      "Request Id" -> id,
      "Request Url" -> requestUri
      ) ++
        resourceType.map(rtype => "ResourceType" -> rtype).toSeq ++
        resourceId.map(rid => "ResourceId" -> rid).toSeq ++
        resource.map(r => s"Resource" -> JsonFormatter.convertToJson(r).toJson) ++
        (if(queryParams.nonEmpty) Some("Query" -> queryParams.flatMap(qp => qp._2.map(v => s"${qp._1}=${v}")).mkString("&")) else None).toSeq

    attributes.map(a => s"${a._1}: ${a._2}").mkString(", ")
  }

  /**
   * Get level of interaction
   * @return
   */
  def levelOfInteraction =
    resourceType match {
      case Some(_) => resourceId match {
        case Some(_) => "instance"
        case None => "type"
      }
      case None => "system"
    }
}


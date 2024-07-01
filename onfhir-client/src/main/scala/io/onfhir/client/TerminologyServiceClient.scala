package io.onfhir.client

import io.onfhir.api.Resource
import io.onfhir.api.client.{FhirClientException, IOnFhirClient}
import io.onfhir.api.service.IFhirTerminologyService
import io.onfhir.client.TerminologyServiceClient.{EXPAND_OPERATION_NAME, EXPAND_OPERATION_REQUEST_PARAMS, LOOKUP_OPERATION_NAME, LOOKUP_OPERATION_REQUEST_PARAMS, TRANSLATE_OPERATION_NAME, TRANSLATE_OPERATION_REQUEST_PARAMS}
import org.json4s.JObject
import org.slf4j.LoggerFactory

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

/**
 * A simple client for FHIR terminology service
 *
 * @param onFhirClient
 * @param ec
 */
class TerminologyServiceClient(onFhirClient: IOnFhirClient)(implicit ec: ExecutionContext) extends IFhirTerminologyService {
  val logger = LoggerFactory.getLogger(this.getClass)
  /**
   * Return timeout specified for the terminology service calls
   * //TODO
   *
   * @return
   */
  override def getTimeout: Duration = Duration.apply(1, TimeUnit.MINUTES)

  /**
   * Translate the given code + system based on given conceptMapUrl
   *
   * @param code          A FHIR code value
   * @param system        FHIR Code system url
   * @param conceptMapUrl A canonical URL for a concept map
   * @return Resulting Parameters resource
   */
  override def translate(code: String, system: String, conceptMapUrl: String): Future[JObject] = translate(code, system, conceptMapUrl, None, None, false)

  /**
   * Translate the given code + system based on given conceptMapUrl
   *
   * @param code              A FHIR code value
   * @param system            FHIR Code system url
   * @param conceptMapUrl     A canonical URL for a concept map
   * @param version           The version of the system, if one was provided in the source data
   * @param conceptMapVersion The identifier that is used to identify a specific version of the concept map to be used for the translation.
   * @param reverse           If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return
   */
  override def translate(code: String, system: String, conceptMapUrl: String, version: Option[String], conceptMapVersion: Option[String], reverse: Boolean):Future[JObject]  = {
    var request =
      onFhirClient
        .operation(TRANSLATE_OPERATION_NAME)
        .on("ConceptMap")
        .addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CODE, code)
        .addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.SYSTEM, system)
        .addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CONCEPT_MAP_URL, conceptMapUrl)
    //Optional parameters
    version.foreach(v => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.VERSION, v))
    conceptMapVersion.foreach(v => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CONCEPT_MAP_VERSION, v))
    if (reverse)
      request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.REVERSE, "true")

    request
      .executeAndReturnResource()
  }


  /**
   * Translate the given Coding or codeable concept based on given conceptMapUrl
   *
   * @param codingOrCodeableConcept FHIR Coding or CodeableConcept
   * @param conceptMapUrl           A canonical URL for a concept map
   * @return Resulting Parameters resource
   */
  override def translate(codingOrCodeableConcept: JObject, conceptMapUrl: String): Future[JObject] = translate(codingOrCodeableConcept, conceptMapUrl, None, false)

  /**
   * Translate the given Coding or codeable concept based on given conceptMapUrl
   *
   * @param codingOrCodeableConcept FHIR Coding or CodeableConcept
   * @param conceptMapUrl           A canonical URL for a concept map
   * @param conceptMapVersion       The identifier that is used to identify a specific version of the concept map to be used for the translation.
   * @param reverse                 If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return
   */
  override def translate(codingOrCodeableConcept: JObject, conceptMapUrl: String, conceptMapVersion: Option[String], reverse: Boolean): Future[JObject] = {
    val isCodeableConcept = codingOrCodeableConcept.obj.exists(_._1 == "coding")

    var request =
      onFhirClient
        .operation(TRANSLATE_OPERATION_NAME)
        .on("ConceptMap")
        .addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CONCEPT_MAP_URL, conceptMapUrl)
    //Add the coding or codeableconcept
    request =
      if (isCodeableConcept)
        request.addParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CODEABLE_CONCEPT, "CodeableConcept" -> codingOrCodeableConcept)
      else
        request.addParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CODING, "Coding" -> codingOrCodeableConcept)

    //Optional parameters
    conceptMapVersion.foreach(v => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CONCEPT_MAP_VERSION, v))
    if (reverse)
      request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.REVERSE, "true")

    request
      .executeAndReturnResource()
  }

  /**
   * Translate the given code + system based on given source and target value sets
   *
   * @param code   A FHIR code value
   * @param system FHIR Code system url
   * @param source Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target Identifies the value set in which a translation is sought.
   * @return Resulting Parameters resource
   */
  override def translate(code: String, system: String, source: Option[String], target: Option[String]): Future[JObject] = translate(code, system, source, target, None, false)

  /**
   * Translate the given code + system based on given source and target value sets
   *
   * @param code    A FHIR code value
   * @param system  FHIR Code system url
   * @param source  Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target  Identifies the value set in which a translation is sought.
   * @param version The version of the system, if one was provided in the source data
   * @param reverse If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return
   */
  override def translate(code: String, system: String, source: Option[String], target: Option[String], version: Option[String], reverse: Boolean): Future[JObject] = {
    var request =
      onFhirClient
        .operation(TRANSLATE_OPERATION_NAME)
        .on("ConceptMap")
        .addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CODE, code)
        .addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.SYSTEM, system)

    //Optional parameters
    source.foreach(s => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.SOURCE, s))
    target.foreach(t => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.TARGET, t))
    version.foreach(v => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.VERSION, v))

    if (reverse)
      request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.REVERSE, "true")

    request
      .executeAndReturnResource()
  }

  /**
   * Translate the given Coding or codeable concept based on given source and target value sets
   *
   * @param codingOrCodeableConcept FHIR Coding or CodeableConcept
   * @param source                  Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target                  Identifies the value set in which a translation is sought.
   * @return Resulting Parameters resource
   */
  override def translate(codingOrCodeableConcept: JObject, source: Option[String], target: Option[String]): Future[JObject] = translate(codingOrCodeableConcept, source, target, false)

  /**
   * Translate the given Coding or codeable concept based on given source and target value sets
   *
   * @param codingOrCodeableConcept FHIR Coding or CodeableConcept
   * @param source                  Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target                  Identifies the value set in which a translation is sought.
   * @param reverse                 If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return
   */
  override def translate(codingOrCodeableConcept: JObject, source: Option[String], target: Option[String], reverse: Boolean): Future[JObject] = {
    val isCodeableConcept = codingOrCodeableConcept.obj.exists(_._1 == "coding")

    var request =
      onFhirClient
        .operation(TRANSLATE_OPERATION_NAME)
        .on("ConceptMap")

    //Add the coding or codeableconcept
    request =
      if (isCodeableConcept)
        request.addParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CODEABLE_CONCEPT, "CodeableConcept" -> codingOrCodeableConcept)
      else
        request.addParam(TRANSLATE_OPERATION_REQUEST_PARAMS.CODING, "Coding" -> codingOrCodeableConcept)

    //Optional parameters
    source.foreach(s => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.SOURCE, s))
    target.foreach(t => request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.TARGET, t))
    if (reverse)
      request = request.addSimpleParam(TRANSLATE_OPERATION_REQUEST_PARAMS.REVERSE, "true")

    request
      .executeAndReturnResource()
  }

  /**
   * Given a code/system, get additional details about the concept, including definition, status, designations, and properties.
   *
   * @param code   A FHIR code value
   * @param system FHIR Code system url
   * @return Resulting Parameters resource
   */
  override def lookup(code: String, system: String):Future[Option[JObject]] = lookup(code, system, None, None, None, Nil)

  /**
   * Given a code/system, get additional details about the concept, including definition, status, designations, and properties.
   *
   * @param code       A FHIR code value
   * @param system     FHIR Code system url
   * @param version    The version of the system, if one was provided in the source data
   * @param date       The date for which the information should be returned. Normally, this is the current conditions (which is the default value) but under some circumstances, systems need to acccess this information as it would have been in the past
   * @param properties A property that the client wishes to be returned in the output. If no properties are specified, the server chooses what to return.
   * @return
   */
  override def lookup(code: String, system: String, version: Option[String], date: Option[String], displayLanguage:Option[String], properties: Seq[String]): Future[Option[JObject]] = {
    var request =
      onFhirClient
        .operation(LOOKUP_OPERATION_NAME)
        .on("CodeSystem")
        .addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.CODE, code)
        .addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.SYSTEM, system)

    version.foreach(v => request = request.addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.VERSION, v))
    date.foreach(d => request = request.addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.DATE, d))
    displayLanguage.foreach(d => request = request.addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.DISPLAY_LANGUAGE, d))

    if (properties.nonEmpty)
      request = request.addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.PROPERTIES, properties: _*)

    request
      .executeAndReturnResource()
      .map(Some(_))
      .recover {
        case fce:FhirClientException if fce.serverResponse.map(_.httpStatus.intValue()).exists(r => r == 404 || r == 400) =>
          None
        case t:Throwable =>
          throw t
      }
  }

  /**
   * Given a Coding, get additional details about the concept, including definition, status, designations, and properties.
   *
   * @param coding FHIR Coding
   * @return Resulting Parameters resource
   */
  override def lookup(coding: JObject):Future[Option[JObject]]  = lookup(coding, None, None, Nil)

  /**
   * Given a Coding, get additional details about the concept, including definition, status, designations, and properties.
   *
   * @param coding     FHIR Coding
   * @param date       The date for which the information should be returned. Normally, this is the current conditions (which is the default value) but under some circumstances, systems need to acccess this information as it would have been in the past
   * @param properties A property that the client wishes to be returned in the output. If no properties are specified, the server chooses what to return.
   * @return
   */
  override def lookup(coding: JObject, date: Option[String], displayLanguage:Option[String], properties: Seq[String]): Future[Option[JObject]] = {
    var request =
      onFhirClient
        .operation(LOOKUP_OPERATION_NAME)
        .on("CodeSystem")
        .addParam(LOOKUP_OPERATION_REQUEST_PARAMS.CODING, "Coding" -> coding)

    date.foreach(d => request = request.addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.DATE, d))
    displayLanguage.foreach(d => request = request.addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.DISPLAY_LANGUAGE, d))

    if (properties.nonEmpty)
      request = request.addSimpleParam(LOOKUP_OPERATION_REQUEST_PARAMS.PROPERTIES, properties: _*)

    request
      .executeAndReturnResource()
      .map(Some(_))
      .recover {
        case fce:FhirClientException if fce.serverResponse.map(_.httpStatus.intValue()).exists(r => r == 404 || r == 400) =>
          None
        case t:Throwable =>
          throw t
      }
  }

  /**
   * Expand the given ValueSet with identifier (ValueSet.id)
   *
   * @param id     Identifier of the ValueSet
   * @param filter Filter text
   * @param offset Paging support - where to start if a subset is desired (default = 0). Offset is number of records (not number of pages)
   * @param count  Paging support - how many codes should be provided in a partial page view. Paging only applies to flat expansions - servers ignore paging if the expansion is not flat. If count = 0, the client is asking how large the expansion is. Servers SHOULD honor this request for hierarchical expansions as well, and simply return the overall count
   * @return
   */
  override def expandWithId(id: String, filter: Option[String], offset: Option[Long], count: Option[Long]): Future[JObject] = {
    var request =
      onFhirClient
        .operation(EXPAND_OPERATION_NAME)
        .on("ValueSet", Some(id))

    filter.foreach(f => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.FILTER, f))
    offset.foreach(o => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.OFFSET, ""+o))
    count.foreach(c => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.COUNT, ""+c))

    request
      .executeAndReturnResource()
  }

  /**
   *
   * @param url     Canonical url of the valueset
   * @param version The identifier that is used to identify a specific version of the value set to be used when generating the expansion. This is an arbitrary value managed by the value set author and is not expected to be globally unique. For example, it might be a timestamp (e.g. yyyymmdd) if a managed version is not available.
   * @param filter  Filter text
   * @param offset  Paging support - where to start if a subset is desired (default = 0). Offset is number of records (not number of pages)
   * @param count   Paging support - how many codes should be provided in a partial page view. Paging only applies to flat expansions - servers ignore paging if the expansion is not flat. If count = 0, the client is asking how large the expansion is. Servers SHOULD honor this request for hierarchical expansions as well, and simply return the overall count
   * @return
   */
  override def expand(url: String, version: Option[String], filter: Option[String], offset: Option[Long], count: Option[Long]): Future[JObject] = {
    var request =
      onFhirClient
      .operation(EXPAND_OPERATION_NAME)
      .on("ValueSet")

    filter.foreach(f => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.FILTER, f))
    offset.foreach(o => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.OFFSET, "" + o))
    count.foreach(c => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.COUNT, "" + c))

    request
      .executeAndReturnResource()
  }

  /**
   *
   * @param valueSet ValueSet definition
   * @param offset   Paging support - where to start if a subset is desired (default = 0). Offset is number of records (not number of pages)
   * @param count    Paging support - how many codes should be provided in a partial page view. Paging only applies to flat expansions - servers ignore paging if the expansion is not flat. If count = 0, the client is asking how large the expansion is. Servers SHOULD honor this request for hierarchical expansions as well, and simply return the overall count
   *  @return
   */
  override def expand(valueSet:Resource, offset: Option[Long] = None, count: Option[Long] = None):Future[JObject] = {
    var request =
      onFhirClient
        .operation(EXPAND_OPERATION_NAME)
        .on("ValueSet")
        .addResourceParam("valueSet", valueSet)

    offset.foreach(o => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.OFFSET, "" + o))
    count.foreach(c => request = request.addSimpleParam(EXPAND_OPERATION_REQUEST_PARAMS.COUNT, "" + c))

    request
      .executeAndReturnResource()
  }
}

object TerminologyServiceClient {
  final val TRANSLATE_OPERATION_NAME = "translate"
  final val LOOKUP_OPERATION_NAME = "lookup"
  final val EXPAND_OPERATION_NAME = "expand"

  final object TRANSLATE_OPERATION_REQUEST_PARAMS {
    val CONCEPT_MAP_URL = "url"
    val CODE = "code"
    val SYSTEM = "system"
    val VERSION = "version"
    val CONCEPT_MAP_VERSION = "conceptMapVersion"
    val SOURCE = "source"
    val TARGET = "target"
    val REVERSE = "reverse"
    val CODING = "coding"
    val CODEABLE_CONCEPT = "codeableConcept"
  }

  final object LOOKUP_OPERATION_REQUEST_PARAMS {
    val CODE = "code"
    val SYSTEM = "system"
    val VERSION = "version"
    val DATE = "date"
    val PROPERTIES = "property"
    val CODING = "coding"
    val DISPLAY_LANGUAGE = "displayLanguage"
  }

  final object EXPAND_OPERATION_REQUEST_PARAMS {
    val URL = "url"
    val FILTER  = "filter"
    val VERSION = "valueSetVersion"
    val OFFSET = "offset"
    val COUNT = "count"
  }
}

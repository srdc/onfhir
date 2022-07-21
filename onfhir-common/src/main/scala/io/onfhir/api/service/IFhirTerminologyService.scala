package io.onfhir.api.service

import org.json4s.JObject

import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
 * Interface for FHIR terminology service
 */
trait IFhirTerminologyService extends IFhirTerminologyTranslationService with IFhirTerminologyLookupService with Serializable{
  /**
   * Return timeout specified for the terminology service calls
   * @return
   */
  def getTimeout:Duration
}

//TODO Other terminology service interfaces

/**
 * Interface for Terminology service lookup functionality
 */
trait IFhirTerminologyLookupService {
  /**
   * Given a code/system, get additional details about the concept, including definition, status, designations, and properties.
   * @param code            A FHIR code value
   * @param system          FHIR Code system url
   * @param version         The version of the system, if one was provided in the source data
   * @param date            The date for which the information should be returned. Normally, this is the current conditions (which is the default value) but under some circumstances, systems need to acccess this information as it would have been in the past
   * @param displayLanguage The requested language for display (see $expand.displayLanguage)
   * @param properties      A property that the client wishes to be returned in the output. If no properties are specified, the server chooses what to return.
   * @return                Resulting Parameters resource if code is found
   */
  def lookup(code:String, system:String, version:Option[String], date:Option[String], displayLanguage:Option[String], properties:Seq[String]):Future[Option[JObject]]

  /**
   * Given a code/system, get additional details about the concept, including definition, status, designations, and properties.
   * @param code     A FHIR code value
   * @param system   FHIR Code system url
   * @return          Resulting Parameters resource if code is found
   */
  def lookup(code:String, system:String):Future[Option[JObject]]

  /**
   * Given a Coding, get additional details about the concept, including definition, status, designations, and properties.
   * @param coding        FHIR Coding
   * @param date          The date for which the information should be returned. Normally, this is the current conditions (which is the default value) but under some circumstances, systems need to acccess this information as it would have been in the past
   * @param displayLanguage The requested language for display (see $expand.displayLanguage)
   * @param properties    A property that the client wishes to be returned in the output. If no properties are specified, the server chooses what to return.
   * @return               Resulting Parameters resource if code is found
   */
  def lookup(coding:JObject,  date:Option[String], displayLanguage:Option[String], properties:Seq[String]):Future[Option[JObject]]

  /**
   * Given a Coding, get additional details about the concept, including definition, status, designations, and properties.
   * @param coding   FHIR Coding
   * @return          Resulting Parameters resource if code is found
   */
  def lookup(coding:JObject):Future[Option[JObject]]
}

/**
 * Interface for FHIR Terminology Service translate functionality
 */
trait IFhirTerminologyTranslationService {

  /**
   * Translate the given code + system based on given conceptMapUrl
   * @param code                A FHIR code value
   * @param system              FHIR Code system url
   * @param conceptMapUrl       A canonical URL for a concept map
   * @param version             The version of the system, if one was provided in the source data
   * @param conceptMapVersion   The identifier that is used to identify a specific version of the concept map to be used for the translation.
   * @param reverse             If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return                    Resulting Parameters resource
   */
  def translate(code:String, system:String, conceptMapUrl:String, version:Option[String], conceptMapVersion:Option[String], reverse:Boolean):Future[JObject]

  /**
   * Translate the given code + system based on given conceptMapUrl
   * @param code                A FHIR code value
   * @param system              FHIR Code system url
   * @param conceptMapUrl       A canonical URL for a concept map
   * @return                    Resulting Parameters resource
   */
  def translate(code:String, system:String, conceptMapUrl:String):Future[JObject]

  /**
   * Translate the given Coding or codeable concept based on given conceptMapUrl
   * @param codingOrCodeableConcept   FHIR Coding or CodeableConcept
   * @param conceptMapUrl             A canonical URL for a concept map
   * @param conceptMapVersion         The identifier that is used to identify a specific version of the concept map to be used for the translation.
   * @param reverse                   If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return                          Resulting Parameters resource if successful
   */
  def translate(codingOrCodeableConcept:JObject, conceptMapUrl:String, conceptMapVersion:Option[String], reverse:Boolean):Future[JObject]

  /**
   * Translate the given Coding or codeable concept based on given conceptMapUrl
   * @param codingOrCodeableConcept   FHIR Coding or CodeableConcept
   * @param conceptMapUrl             A canonical URL for a concept map
   * @return                          Resulting Parameters resource
   */
  def translate(codingOrCodeableConcept:JObject, conceptMapUrl:String):Future[JObject]

  /**
   * Translate the given code + system based on given source and target value sets
   * @param code        A FHIR code value
   * @param system      FHIR Code system url
   * @param source      Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target      Identifies the value set in which a translation is sought.
   * @param version     The version of the system, if one was provided in the source data
   * @param reverse     If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return            Resulting Parameters resource
   */
  def translate(code:String, system:String, source:Option[String], target:Option[String], version:Option[String], reverse:Boolean):Future[JObject]

  /**
   * Translate the given code + system based on given source and target value sets
   * @param code        A FHIR code value
   * @param system      FHIR Code system url
   * @param source      Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target      Identifies the value set in which a translation is sought.
   * @return            Resulting Parameters resource
   */
  def translate(code:String, system:String, source:Option[String], target:Option[String]):Future[JObject]

  /**
   * Translate the given Coding or codeable concept based on given source and target value sets
   * @param codingOrCodeableConcept    FHIR Coding or CodeableConcept
   * @param source      Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target      Identifies the value set in which a translation is sought.
   * @param reverse     If this is true, then the operation should return all the codes that might be mapped to this code.
   * @return            Resulting Parameters resource
   */
  def translate(codingOrCodeableConcept:JObject, source:Option[String], target:Option[String], reverse:Boolean):Future[JObject]

  /**
   * Translate the given Coding or codeable concept based on given source and target value sets
   * @param codingOrCodeableConcept    FHIR Coding or CodeableConcept
   * @param source                    Identifies the value set used when the concept (system/code pair) was chosen. May be a canonical url for the valueset, or an absolute or relative location.
   * @param target                    Identifies the value set in which a translation is sought.
   * @return                          Resulting Parameters resource if successful
   */
  def translate(codingOrCodeableConcept:JObject, source:Option[String], target:Option[String]):Future[JObject]
}


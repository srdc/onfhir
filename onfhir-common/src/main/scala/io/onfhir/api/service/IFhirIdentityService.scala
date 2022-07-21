package io.onfhir.api.service

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

/**
 * Interface to resolve identities (e.g. Patient identities, Encounter identities)
 */
trait IFhirIdentityService extends Serializable{
  /**
   *
   * @return
   */
  def getTimeout:Duration =  Duration.apply(1, TimeUnit.MINUTES)

  /**
   * Find matching FHIR Resource identifier given business identifier for the entity
   * @param resourceType    FHIR resource type e.g. Patient, Encounter
   * @param identifier      Business identifier of entity (e.g. DomainResource.identifier.value)
   * @param system          System for the identifier     (e.g. DomainResource.identifier.system)
   * @return                FHIR resource identifier (Resource.id) if found
   */
  def findMatching(resourceType:String, identifier:String, system: Option[String] = None):Future[Option[String]]
}

trait IFhirIdentityCache extends IFhirIdentityService {
  /**
   * Store the identity mapping in the cache
   * @param resourceType    FHIR resource type e.g. Patient, Encounter
   * @param identifier      Business identifier of entity (e.g. DomainResource.identifier.value)
   * @param system          System for the identifier     (e.g. DomainResource.identifier.system)
   * @param correspondingId Corresponding FHIR resource identifier (Resource.id)
   * @return
   */
  def storeIdentity(resourceType:String, identifier:String, system: Option[String], correspondingId:String):Future[Unit]
}

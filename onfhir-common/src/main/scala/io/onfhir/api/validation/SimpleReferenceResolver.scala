package io.onfhir.api.validation

import io.onfhir.api.Resource

import scala.concurrent.{ExecutionContext, Future}

class SimpleReferenceResolver(resource: Resource)(implicit ec:ExecutionContext) extends AbstractReferenceResolver(resource)(ec){
  /**
   * Get the resource given with rtype, rid and optionall version id
   *
   * @param serverUrl FHIR server url to retrieve (if none it means local onFhir repository)
   * @param rtype     Resource type
   * @param rid       Resource identifier
   * @param version   Version of the resource
   * @return
   */
  override protected def getResource(serverUrl: Option[String], rtype: String, rid: String, version: Option[String]): Future[Option[Resource]] =
    throw new Exception("Simple reference resolver does not support resolving from a FHIR server!")

  /**
   * Get the resource by given canonical url parts
   *
   * @param url     Root part of canonical url
   * @param rtype   Resource type
   * @param rid     Resource identifier
   * @param version Business version of the resource
   * @return
   */
  override protected def getResourceByCanonicalUrl(url: String, rtype: String, rid: String, version: Option[String]): Future[Option[Resource]] =
    throw new Exception("Simple reference resolver does not support resolving from a FHIR server!")

  /**
   * Get the resource by given FHIR Identifier
   *
   * @param rtype  Resource type
   * @param system System of identifier
   * @param value  Value of identifier
   * @return
   */
override protected def getResourceByIdentifier(rtype: String, system: Option[String], value: String): Future[Option[Resource]] =
  throw new Exception("Simple reference resolver does not support resolving from a FHIR server!")

  /**
   * Check if the resource given with rtype, rid and optionall version id exists
   *
   * @param serverUrl FHIR server url to retrieve (if none it means local onFhir repository)
   * @param rtype     Resource type
   * @param rid       Resource identifier
   * @return
   */
override protected def isResourceExist(serverUrl: Option[String], rtype: String, rid: String): Future[Boolean] =
  throw new Exception("Simple reference resolver does not support resolving from a FHIR server!")

  /**
   * Check if the resource given with canonical url exists
   *
   * @param url     Root part of canonical url
   * @param rtype   Resource type
   * @param rid     Resource id
   * @param version Business version
   * @return
   */
override protected def isResourceExistByCanonicalUrl(url: String, rtype: String, rid: String, version: Option[String]): Future[Boolean] =
  throw new Exception("Simple reference resolver does not support resolving from a FHIR server!")
}

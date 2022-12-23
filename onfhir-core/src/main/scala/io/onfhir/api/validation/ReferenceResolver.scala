package io.onfhir.api.validation

import io.onfhir.api.Resource
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.config.{FhirServerConfig, IFhirConfigurationManager, OnfhirConfig}
import io.onfhir.db.ResourceManager

import scala.concurrent.{ExecutionContext, Future}

/**
 * A resolver for references within a resource
 *
 * @param fhirConfig Fhir configuration of the server
 * @param resource   Resource itself
 * @param bundle     If resource is a part of a bundle (for inter bundle references), the root part of fullUrl of resource validated and the whole Bundle
 */
class ReferenceResolver(fhirConfigurationManager: IFhirConfigurationManager,
                        resource: Resource,
                        bundle: Option[(Option[String], Resource)] = None)(implicit val ec: ExecutionContext)
  extends AbstractReferenceResolver(resource, bundle) {
  val resourceManager = fhirConfigurationManager.resourceManager
  /**
   * Get the resource given with rtype, rid and optionall version id
   *
   * @param serverUrl FHIR server url to retrieve (if none it means local onFhir repository)
   * @param rtype     Resource type
   * @param rid       Resource identifier
   * @param version   Version of the resource
   * @return
   */
  override protected def getResource(serverUrl: Option[String], rtype: String, rid: String, version: Option[String]): Future[Option[Resource]] = {
    serverUrl match {
      case None | Some(OnfhirConfig.fhirRootUrl) => resourceManager.getResource(rtype, rid, version)
      case Some(_) =>
        //TODO Resolving references from a Remote FHIR server is not implemented yet
        Future.apply(None)
    }
  }

  /**
   * Get the resource by given canonical url parts
   *
   * @param url     Root part of canonical url
   * @param rtype   Resource type
   * @param rid     Resource identifier
   * @param version Business version of the resource
   * @return
   */
  override protected def getResourceByCanonicalUrl(url: String, rtype: String, rid: String, version: Option[String]): Future[Option[Resource]] = {
    var query = Map(
      "url" -> List(s"$url/$rtype/$rid"),
      "_sort" -> List("-_lastUpdated"),
      "_count" -> List("1")
    )
    if (version.isDefined)
      query = query ++ Map("version" -> List(version.get))
    val parameters = fhirConfigurationManager.fhirSearchParameterValueParser.parseSearchParameters(rtype, query)

    resourceManager
      .searchResources(rtype, parameters)
      .map(_.matches.headOption)
  }

  /**
   * Get the resource by given FHIR Identifier
   *
   * @param rtype  Resource type
   * @param system System of identifier
   * @param value  Value of identifier
   * @return
   */
  override protected def getResourceByIdentifier(rtype: String, system: Option[String], value: String): Future[Option[Resource]] = {
    val query = Map(
      "identifier" -> List(s"$system|$value"),
      "_sort" -> List("-_lastUpdated"),
      "_count" -> List("1")
    )
    val parameters = fhirConfigurationManager.fhirSearchParameterValueParser.parseSearchParameters(rtype, query)
    resourceManager
      .searchResources(rtype, parameters)
      .map(_.matches.headOption)
  }

  /**
   * Check if the resource given with rtype, rid and optionall version id exists
   *
   * @param serverUrl FHIR server url to retrieve (if none it means local onFhir repository)
   * @param rtype     Resource type
   * @param rid       Resource identifier
   * @return
   */
  override protected def isResourceExist(serverUrl: Option[String], rtype: String, rid: String): Future[Boolean] = {
    serverUrl match {
      case None | Some(OnfhirConfig.fhirRootUrl) =>
        val query = Map(
          "_id" -> List(rid)
        )

        val parameters = fhirConfigurationManager.fhirSearchParameterValueParser.parseSearchParameters(rtype, query)

        resourceManager
          .countResources(rtype, parameters)
          .map(_ > 0)
      case Some(_) =>
        //TODO Resolving references from a Remote FHIR server is not implemented yet
        Future.apply(false)
    }
  }

  /**
   * Check if the resource given with canonical url exists
   *
   * @param url     Root part of canonical url
   * @param rtype   Resource type
   * @param rid     Resource id
   * @param version Business version
   * @return
   */
  override protected def isResourceExistByCanonicalUrl(url: String, rtype: String, rid: String, version: Option[String]): Future[Boolean] = {
    var query = Map(
      "url" -> List(s"$url/$rtype/$rid")
    )
    if (version.isDefined)
      query = query ++ Map("version" -> List(version.get))

    val parameters = fhirConfigurationManager.fhirSearchParameterValueParser.parseSearchParameters(rtype, query)

    resourceManager
      .countResources(rtype, parameters)
      .map(_ > 0)
  }
}

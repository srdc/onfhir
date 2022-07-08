package io.onfhir.client

import io.onfhir.api.client.IOnFhirClient
import io.onfhir.api.service.{IFhirIdentityCache, IFhirIdentityService}
import io.onfhir.api.util.FHIRUtil
import scala.concurrent.{ExecutionContext, Future}

/**
 * An Identity service implementation by using onFhir client interface
 * @param onFhirClient    OnFhir client
 * @param identityCache   Cache for identities if given
 * @param ec              Execution context
 */
class IdentityServiceClient(onFhirClient: IOnFhirClient, identityCache:Option[IFhirIdentityCache] = None)(implicit ec: ExecutionContext) extends IFhirIdentityService {

  /**
   * Find matching FHIR Resource identifier given business identifier for the entity
   *
   * @param resourceType FHIR resource type e.g. Patient, Encounter
   * @param identifier   Business identifier of entity (e.g. DomainResource.identifier.value)
   * @param system       System for the identifier     (e.g. DomainResource.identifier.system)
   * @return FHIR resource identifier (Resource.id) if found
   */
  override def findMatching(resourceType: String, identifier: String, system: Option[String] = None): Future[Option[String]] = {
    identityCache match {
      //If there is cache, try to find from there
      case Some(cache) =>
        //Search cache
        cache
          .findMatching(resourceType, identifier, system)
          .flatMap {
            //If not exist, search from service
            case None =>
              findMatchingViaFhirQuery(resourceType, identifier, system)
                .flatMap {
                  case None => Future.apply(None)
                  //If found, store it to the cache and return
                  case Some(foundId) =>
                    cache
                      .storeIdentity(resourceType, identifier, system, foundId)
                      .map(_ => Some(foundId))
                }
            //Otherwise return
            case Some(foundId) => Future.apply(Some(foundId))
          }
      //If there is no cache configure, go search via service
      case None =>
        findMatchingViaFhirQuery(resourceType, identifier, system)
    }
  }

  /**
   * Find matching by querying the FHIR server
   * @param resourceType FHIR resource type e.g. Patient, Encounter
   * @param identifier   Business identifier of entity (e.g. DomainResource.identifier.value)
   * @param system       System for the identifier     (e.g. DomainResource.identifier.system)
   * @return
   */
  private def findMatchingViaFhirQuery(resourceType: String, identifier: String, system: Option[String]): Future[Option[String]] = {
    //Query over identifier search param
    var query =
      onFhirClient
      .search(resourceType)
      .where("identifier", s"${system.map(s => s"$s|").getOrElse("")}$identifier")

    query =
      resourceType match {
        //For patient we only need the link element to filter apart from base elements (e.g. id)
        case "Patient" =>
          query.where("_elements", "link")
        //For others we only need the base id
        case _ =>
          query.where("_summary", "text")
      }

    query
      .executeAndReturnBundle()
      .map(searchResults =>
        resourceType match {
          //If resource type is Patient, check also the link element to find out latest Patient resource
          case "Patient" =>
            searchResults.searchResults
              .find(p =>
                FHIRUtil
                  .extractValueOptionByPath[Seq[String]](p, "link.type")//Get the link type if exists
                  .getOrElse(Nil)
                  .forall(linkType => linkType == "replaces" || linkType == "seealso") //Find the first one that this link does not exists or it is one of the codes mentioned
              ).map(p => FHIRUtil.extractIdFromResource(p))
          //If this is others, get the first one
          case _ =>
            searchResults.searchResults
              .headOption
              .map(r => FHIRUtil.extractIdFromResource(r))
        }
      )
  }


}

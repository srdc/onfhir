package io.onfhir.api.validation

import java.util.concurrent.TimeUnit

import io.onfhir.api.Resource
import io.onfhir.api.model.{FhirCanonicalReference, FhirInternalReference, FhirLiteralReference, FhirLogicalReference, FhirReference, FhirUUIDReference, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.{FhirConfig, OnfhirConfig}
import io.onfhir.db.ResourceManager

import org.json4s.JsonAST.{JArray, JObject, JValue}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import io.onfhir.util.JsonFormatter._
/**
 * A resolver for references within a resource
 * @param fhirConfig  Fhir configuration of the server
 * @param resource    Resource itself
 * @param bundle      If resource is a part of a bundle (for inter bundle references), the root part of fullUrl of resource validated and the whole Bundle
 */
class ReferenceResolver(fhirConfig:FhirConfig, val resource: Resource, val bundle:Option[(String, Resource)] = None) extends IReferenceResolver {

  /**
   * Extract bundle items
   */
  val bundleItems:Option[Seq[(String, JValue)]] = bundle.flatMap(b => (b._2 \ "entry") match {
    case JArray(arr) =>
      Some(arr
        .map(e =>
          (e \ "fullUrl")
            .extractOpt[String].getOrElse(
              s"${OnfhirConfig.fhirRootUrl}/${(e \ "resource" \ "resourceType").extract[String]}/${(e \ "resource" \ "id").extract[String]}"
            ) -> //Extract fullUrl from Bundle
            (e \ "resource"))
      )
    case _ =>
      None
  })

  /**
   * Resolve a FHIR reference within onFhir
   *
   * @param reference FHIR  reference
   * @return
   */
  override def resolveReference(reference: FhirReference): Option[Resource] = {
    reference match {
      //Normal literal reference
      case FhirLiteralReference(url, rtype, rid, version) =>
        checkWithinBundle(reference) match {
          case None =>
            //A local server reference
            url match {
              case None | Some(OnfhirConfig.fhirRootUrl) =>
                Await.result(ResourceManager.getResource(rtype, rid, version), Duration.apply(1, TimeUnit.MINUTES))
              case Some(remoteUrl) =>
                //Resolving refrences from a Remote FHIR server is not implemented yet
                None
            }
          case Some(foundInBundle) => Some(foundInBundle)
        }
      //Canonical reference
      case FhirCanonicalReference(url, rtype, rid, version, fragment)  =>
        var query = Map(
          "url" -> List(s"$url/$rtype/$rid"),
          "_sort" -> List("-_lastUpdated"),
          "_count" -> List("1")
        )
        if(version.isDefined)
          query = query ++ Map("version" -> List(version.get))
        val parameters = FHIRSearchParameterValueParser.parseSearchParameters(rtype, query)

       val (_, resources, _) = Await.result(ResourceManager.searchResources(rtype, parameters), Duration.apply(1, TimeUnit.MINUTES))
        if(fragment.isDefined)
          resources.headOption.flatMap(r => getContainedResource(r, fragment.get))
        else
          resources.headOption

      //Internal reference to contained resource
      case FhirInternalReference(ref) =>
        getContainedResource(resource, ref)

      //If it is a logical reference and if rtype is supported, try to resolve it with identifier
      case FhirLogicalReference(Some(rtype), system, identifier) if fhirConfig.resourceConfigurations.contains(rtype) =>
        var query = Map(
          "identifier" -> List(s"$system|$identifier"),
          "_sort" -> List("-_lastUpdated"),
          "_count" -> List("1")
        )
        val parameters = FHIRSearchParameterValueParser.parseSearchParameters(rtype, query)
        val (_, resources, _) = Await.result(ResourceManager.searchResources(rtype, parameters), Duration.apply(1, TimeUnit.MINUTES))
        resources.headOption
      //If it is a uuid reference within bundle
      case FhirUUIDReference(uuid) =>
        checkWithinBundle(reference)
      //Otherwise none
      case _ => None
    }
  }

  /**
   * Check a reference within the bundle if this resource is given in a bundle
   * @param reference
   * @return
   */
  private def checkWithinBundle(reference:FhirReference):Option[Resource] = {
    bundleItems.flatMap(bitems =>
      reference match {
        //If literal reference, check if there is a entry with a fullUrl
        case FhirLiteralReference(url, rtype, rid, version) =>
          bitems.find(_._1 == s"${url.getOrElse(bundle.get._1)}/$rtype/$rid").flatMap {
            case (_, r:JObject) =>
              version match {
                case None => Some(r)
                case Some(v) if FHIRUtil.extractVersionFromResource(r).toString == v  =>
                  Some(r)
                case _ => None
              }
            case _ => None
          }
        case FhirUUIDReference(uuid) =>
          bitems.find(_._1 == uuid).flatMap {
            case (_, r:JObject) => Some(r)
            case _ => None
          }
        case _ => None
      }
    )
  }

  /**
   * Find contained resource
   * @param resource
   * @param id
   * @return
   */
  private def getContainedResource(resource:Resource, id:String):Option[Resource] = {
    (resource \ "contained") match {
      case JArray(arr) => arr.map(_.asInstanceOf[JObject]).find(r => FHIRUtil.extractIdFromResource(r) == id)
      case _ => None
    }
  }

  /**
   * Check if a referenced resource exist
   *
   * @param reference FHIR  reference
   * @param profiles  Profiles that resource is expected to conform
   */
  override def isReferencedResourceExist(reference: FhirReference, profiles: Set[String]): Boolean = {
    reference match {
      //Normal literal reference
      case FhirLiteralReference(url, rtype, rid, version) =>
        checkWithinBundle(reference) match {
          case None =>
            version match {
              case None =>
                //A local server reference
                url match {
                  case None | Some(OnfhirConfig.fhirRootUrl) =>
                    var query = Map(
                      "_id" -> List(rid),
                      "_summary" -> List("count")
                    )
                    if(profiles.nonEmpty)
                      query = query ++ Map("_profile" -> List(profiles.mkString(",")))
                    val parameters = FHIRSearchParameterValueParser.parseSearchParameters(rtype, query)

                    var count = Await.result(ResourceManager.countResources(rtype, parameters), Duration.apply(1, TimeUnit.MINUTES))
                    count > 0
                  case Some(remoteUrl) =>
                    //Resolving refrences from a Remote FHIR server is not implemented yet
                    false
                }
              case Some(_) => resolveReference(reference).isDefined
            }

          case Some(foundInBundle) => true
        }
      //Canonical reference
      case FhirCanonicalReference(url, rtype, rid, version, fragment) =>
        fragment match {
          case None =>
            var query = Map(
              "url" -> List(s"$url/$rtype/$rid"),
              "_sort" -> List("-_lastUpdated"),
              "_count" -> List("1")
            )
            if (version.isDefined)
              query = query ++ Map("version" -> List(version.get))
            val parameters = FHIRSearchParameterValueParser.parseSearchParameters(rtype, query)

            val count = Await.result(ResourceManager.countResources(rtype, parameters), Duration.apply(1, TimeUnit.MINUTES))
            count > 0
          case Some(f) =>
            resolveReference(reference).isDefined
        }
    }

  }
}

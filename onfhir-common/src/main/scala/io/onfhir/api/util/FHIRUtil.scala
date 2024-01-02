package io.onfhir.api.util

import akka.http.scaladsl.model._
import io.onfhir.api._
import io.onfhir.api.model._
import io.onfhir.config.OnfhirConfig
import io.onfhir.util.JsonFormatter.formats

import java.time.Instant
import java.util.UUID
import io.onfhir.exception.InvalidParameterException
import io.onfhir.util.DateTimeUtil
import org.json4s.JsonAST.{JNothing, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s.{JsonAST, _}

import scala.util.{Failure, Success, Try}

/**
 * Utility functions for FHIR content processing/generation, etc
 */
object FHIRUtil {
  /**
   * Regular expression for FHIR RESTfull resource url
   */

  private val fhirResourceUrlRegex = """(?<rootUrl>(?<protocol>http|https)://(?<other>[A-Za-z0-9\-\\.:%$]*/)+)?(?<rtype>[A-Za-z]+)/(?<rid>[A-Za-z0-9\-.]{1,64})(?<allHistory>/_history/(?<version>[A-Za-z0-9\-.]{1,64}))?""".r

  /**
   * Generates a random identifier according to FHIR id rules with given minimum length
   *
   * @return a new logical identifier
   */
  def generateResourceId(): String = {
    UUID.randomUUID().toString
  }

  /**
   * Returns the location of a resource (should be accessible via read operation)
   *
   * @param _type type of the resource
   * @param _id   id of the resource
   * @return URL of the resource
   */
  def resourceLocation(_type: String, _id: String): String = {
    OnfhirConfig.fhirRootUrl + "/" + _type + "/" + _id
  }

  /**
   * Returns the location of a resource including version identifier (should be accessible via read operation)
   *
   * @param _type type of the resource
   * @param _id   id of the resource
   * @param _vid  version id of the resource
   * @return URL of the resource
   */
  def resourceLocationWithVersion(_type: String, _id: String, _vid: Long): String = {
    OnfhirConfig.fhirRootUrl + "/" + _type + "/" + _id + "/" + FHIR_HTTP_OPTIONS.HISTORY + "/" + _vid
  }

  /**
   * Try to parse a URL as a Restfull FHIR resource url
   *
   * @param url Given url
   * @return Tuple of  (Optional root url, resource type, resource id, and option version id)
   */
  def parseRestfullFhirResourceUrl(url: String): Option[(Option[String], String, String, Option[String])] = {
    url match {
      case fhirResourceUrlRegex(rootUrl, _, _, rtype, rid, _, version) => Some(Option(rootUrl).map(_.dropRight(1)), rtype, rid, Option(version))
      case _ => None
    }
  }


  /**
   * Create a resource to represent deleted resource
   *
   * @param _type      Resource type
   * @param _id        Resource identifier
   * @param _vid       Version of resource
   * @param statusCode Possible HttpResponse status for this operation
   * @return
   */
  def createEmptyResourceForDelete(_type: String, _id: String, _vid: Long, lastModified: DateTime, statusCode: StatusCode): Resource = {
    val resourceForDeletion =
      (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> _type) ~
        (FHIR_COMMON_FIELDS.ID -> _id) ~
        (FHIR_COMMON_FIELDS.META ->
          (FHIR_COMMON_FIELDS.VERSION_ID -> _vid.toString) ~
            (FHIR_COMMON_FIELDS.LAST_UPDATED -> DateTimeUtil.serializeDateTime(lastModified))
          )

    FHIRUtil.populateResourceWithExtraFields(resourceForDeletion, FHIR_METHOD_NAMES.METHOD_DELETE, statusCode)
  }


  /**
   * Sets a number of META fields to the given resource
   *
   * @param resource     resource to be edited
   * @param id           logical id of the resource
   * @param versionId    version id of the resources
   * @param lastModified last modified information of the resource
   * @return
   */
  def populateResourceWithMeta(resource: Resource, id: Option[String], versionId: Long, lastModified: Instant): Resource = {
    val resourceTypeField = resource.findField(_._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE).get

    val meta: Resource = (resource \ FHIR_COMMON_FIELDS.META) match {
      case meta: JObject =>
        FHIR_COMMON_FIELDS.META -> (meta merge ((FHIR_COMMON_FIELDS.VERSION_ID -> versionId.toString) ~ (FHIR_COMMON_FIELDS.LAST_UPDATED -> DateTimeUtil.serializeInstant(lastModified))))
      case _ =>
        (FHIR_COMMON_FIELDS.META ->
          (FHIR_COMMON_FIELDS.VERSION_ID -> versionId.toString) ~
            (FHIR_COMMON_FIELDS.LAST_UPDATED -> DateTimeUtil.serializeInstant(lastModified))
          )
    }

    var result: Resource = resource.obj.filterNot(f => f._1 == FHIR_COMMON_FIELDS.ID || f._1 == FHIR_COMMON_FIELDS.META || f._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE)

    //Merge it
    result = meta merge result

    //Put id
    if (id.isDefined)
      result = (JObject() ~ (FHIR_COMMON_FIELDS.ID -> id.get)) merge result //add id field to resource

    result = resourceTypeField ~ result
    result
  }

  /**
   * Populate the resource with our own fields (required for Bundle.entry construction)
   *
   * @param httpMethod
   * @param httpStatusCode
   */
  def populateResourceWithExtraFields(resource: Resource, httpMethod: String, httpStatusCode: StatusCode): Resource = {
    val extra =
      //(FHIR_EXTRA_FIELDS.CURRENT -> true) ~
      (FHIR_EXTRA_FIELDS.METHOD -> httpMethod) ~
        (FHIR_EXTRA_FIELDS.STATUS_CODE -> httpStatusCode.intValue.toString)

    resource ~ extra
  }

  /**
   * Remove the extra fields (that we use internally) from the resource content and return it
   *
   * @param resource
   * @return
   */
  def clearExtraFields(resource: Resource): Resource = {
    resource.removeField(f => ONFHIR_EXTRA_FIELDS.contains(f._1)).asInstanceOf[JObject]
  }

  /**
   * Set a profile within meate for a resource
   *
   * @param resource
   * @param profile
   */
  def setProfile(resource: Resource, profile: String): Resource = {
    val meta: Resource =
      (FHIR_COMMON_FIELDS.META ->
        (FHIR_COMMON_FIELDS.PROFILE -> JArray(List(JString(profile))))
        )

    resource merge meta
  }

  /**
   * Set id of a resource
   */
  def setId(resource: Resource, id: String): Resource = {
    resource merge JsonAST.JObject.apply(FHIR_COMMON_FIELDS.ID -> JString(id))
    //resource ~ (FHIR_COMMON_FIELDS.ID -> id)
  }

  /**
   * Converts a given resource into json format taking Prefer header retrieved in HTTP Request into account.
   * If the client does not send Prefer header, default value set in the application.conf is used.
   *
   * @param resource resource to be serialized
   * @param prefer   value of the prefer header
   * @return a string in json format
   */
  def getResourceContentByPreference(resource: Resource, prefer: Option[String]): Option[Resource] = {
    prefer.getOrElse(OnfhirConfig.fhirDefaultReturnPreference) match {
      case preferHeader if preferHeader.contains(FHIR_HTTP_OPTIONS.FHIR_RETURN_MINIMAL) => None //if return=minimal send empty string
      case preferHeader if preferHeader.contains(FHIR_HTTP_OPTIONS.FHIR_RETURN_REPRESENTATION) => Some(resource) //resource.clone() //if return=representation send whole resource back
      case preferHeader if preferHeader.contains(FHIR_HTTP_OPTIONS.FHIR_RETURN_OPERATION_OUTCOME) => Some(FHIRResponse.createOperationOutcomeWithSuccess())
      case _ =>
        Some(resource) //resource.clone() // Default representation
    }
  }


  /**
   * Creates a resource of type "Bundle" for search and history results
   *
   * @param bundleType type of the Bundle (e.g. history, searchset, etc)
   * @param _links     links to this Bundle
   * @param entries    Bundle entries
   * @param total      total number of resources which is requested
   * @return
   */
  def createBundle(bundleType: String, _links: List[(String, String)], entries: Seq[Resource], total: Long, summary: Option[String]): Resource = {
    //Prepare links
    val link = FHIR_BUNDLE_FIELDS.LINK ->
      _links.map(l => (FHIR_BUNDLE_FIELDS.RELATION -> l._1) ~ (FHIR_COMMON_FIELDS.URL -> l._2))

    //Create Bundle
    var bundle: JObject =
      (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> FHIR_BUNDLE_TYPES.BUNDLE) ~
        (FHIR_COMMON_FIELDS.ID -> generateResourceId()) ~
        (FHIR_COMMON_FIELDS.TYPE -> bundleType)

    //bdl-1: total only when a search or history (expression : total.empty() or (type = 'searchset') or (type = 'history'))
    if (total != -1 && Seq(FHIR_BUNDLE_TYPES.SEARCH_SET, FHIR_BUNDLE_TYPES.HISTORY).contains(bundleType))
      bundle = bundle ~ (FHIR_BUNDLE_FIELDS.TOTAL -> total)

    //Add links and entries
    if (summary.isEmpty || summary.get != FHIR_SUMMARY_OPTIONS.COUNT) {
      bundle = bundle ~ link
      bundle = bundle ~ (FHIR_BUNDLE_FIELDS.ENTRY -> entries)
    }

    bundle
  }

  /**
   * Creates  a resource of type "Bundle" for batch and transaction
   *
   * @param bundleType
   * @param entries
   * @return
   */
  def createTransactionBatchBundle(bundleType: String, entries: Seq[Resource]): Resource = {
    val bundle: JObject =
      (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> FHIR_BUNDLE_TYPES.BUNDLE) ~
        (FHIR_COMMON_FIELDS.ID -> generateResourceId()) ~
        (FHIR_COMMON_FIELDS.TYPE -> bundleType) ~
        (FHIR_BUNDLE_FIELDS.ENTRY -> entries)

    bundle
  }


  /**
   * Extract Id, Version and Last Update Time
   *
   * @param resource
   * @return
   */
  def extractBaseMetaFields(resource: Resource): (String, Long, DateTime) = {
    (
      extractIdFromResource(resource),
      extractVersionFromResource(resource),
      extractLastUpdatedFromResource(resource)
    )
  }

  /**
   * Get the reference url to refer this resource e.g. Observation/65625454
   *
   * @param resource
   * @return
   */
  def getReference(resource: Resource): String = {
    val (rtype, rid) = extractResourceTypeAndId(resource)
    s"$rtype/$rid"
  }

  /**
   * Get the reference url with version to refer this resource e.g. Observation/4646/_history/2
   *
   * @param resource
   * @return
   */
  def getReferenceWithVersion(resource: Resource): String = {
    val versionId = extractVersionFromResource(resource)
    s"${getReference(resource)}/_history/$versionId"
  }

  /**
   * Extract resource id
   *
   * @param resource
   * @return
   */
  def extractIdFromResource(resource: Resource): String = {
    (resource \ FHIR_COMMON_FIELDS.ID).extract[String]
  }

  /**
   * Extract version of resource
   *
   * @param resource
   * @return
   */
  def extractVersionFromResource(resource: Resource): Long = {
    (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.VERSION_ID).extract[String].toLong
  }

  def extractVersionOptionFromResource(resource: Resource): Option[Long] = {
    (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.VERSION_ID).extractOpt[String].map(_.toLong)
  }

  /**
   * Extract Last Updated time of resource
   *
   * @param resource
   * @return
   */
  def extractLastUpdatedFromResource(resource: Resource): DateTime = {
    val lastUpdatedString = (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.LAST_UPDATED).extract[String]
    DateTimeUtil.parseInstant(lastUpdatedString).get
  }

  /** *
   * Extract Resource type and id
   *
   * @param resource
   * @return
   */
  def extractResourceTypeAndId(resource: Resource): (String, String) =
    (resource \ FHIR_COMMON_FIELDS.RESOURCE_TYPE).extract[String] ->
      (resource \ FHIR_COMMON_FIELDS.ID).extract[String]

  def extractResourceType(resource: Resource): String = {
    (resource \ FHIR_COMMON_FIELDS.RESOURCE_TYPE).extract[String]
  }

  def extractValue[T](resource: Resource, elementName: String)(implicit m: Manifest[T]): T = {
    (resource \ elementName).extract[T]
  }

  /**
   * Extract value of an element (not path)
   *
   * @param resource    JSON Resource
   * @param elementName Name of the element
   * @param m
   * @tparam T Expected type
   * @return
   */
  def extractValueOption[T](resource: Resource, elementName: String)(implicit m: Manifest[T]): Option[T] = {
    (resource \ elementName).extractOpt[T]
  }

  /**
   * Extract value of a path
   *
   * @param resource JSON Resource
   * @param path     Path of the element
   * @param m
   * @tparam T Expected type
   * @return
   */
  def extractValueOptionByPath[T](resource: Resource, path: String)(implicit m: Manifest[T]): Option[T] = {
    def parsePathItem(pitem: String): (String, Seq[Int]) = {
      if (pitem.last == ']') {
        val parts = pitem.split('[')
        parts.head -> parts.tail.map(_.dropRight(1).toInt).toIndexedSeq
      } else
        pitem -> Nil
    }

    val result =
      path
        .split('.')
        .foldLeft(resource.asInstanceOf[JValue])((v, i) =>
          parsePathItem(i) match {
            case (p, Nil) => (v \ i)
            case (p, Seq(arrInd)) => (v \ i)(arrInd)
            case (p, oth) => oth.foldLeft(v \ i)((c, a) => c(a))
          }
        )

    result.extractOpt[T]
  }

  /**
   * Check if resource is deleted
   *
   * @param resource
   * @return
   */
  def isDeleted(resource: Resource): Boolean =
    (resource \ FHIR_EXTRA_FIELDS.METHOD).extract[String] == FHIR_METHOD_NAMES.METHOD_DELETE

  /**
   * Extract the profiles of the resources
   *
   * @param resource
   * @return
   */
  def extractProfilesFromBson(resource: Resource): Set[String] = {
    (resource \ FHIR_COMMON_FIELDS.META \ FHIR_COMMON_FIELDS.PROFILE).extract[Seq[String]].toSet
  }

  /**
   * Rebuild string with escaping special characters
   *
   * @param str string to be escaped
   * @return string is escaped
   */
  def escapeCharacters(str: String): String = {
    val escapeChars = List("\\[", "\\]", "\\+", "\\/", "\\*", "\\$", "\\^", "\\?", "\\|", "\\(", "\\)")
    var ret = str
    escapeChars foreach { c =>
      ret = ret.replaceAll(c, "\\" + c)
    }
    ret
  }


  /**
   *
   * @param paramName The parameter name given in Conformance.rest.resource.searchParam.name
   * @return Transform the names of composite parameters that ends with "-x" to "-[x]", so that it is same
   *         in the corresponding Search Parameter definition SearchParamer.code
   *         (This is a anomaly case in base Conformance and SearchParameter definitions given by FHIR and )
   */
  def transformSearchParameterName(paramName: String): String = {
    if (paramName.endsWith("-x"))
      paramName.dropRight(2) + "-[x]"
    else paramName
  }



  /* /**
     * Run json path (with dots) on the resource
     * @param resource Resource
     * @param jsonPath Path seperated with dots
     * @tparam T return type
     * @return
     */
   def runJsonPath[T](resource: Resource, jsonPath:String):List[T] = {
     val pathElems = jsonPath.split('.')

     def flatten(xs: List[Any]): List[Any] = xs match {
       case Nil => Nil
       case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
       case (head:Seq[_])::tail =>  flatten(head.toList) ++ flatten(tail)
       case head :: tail => head :: flatten(tail)
     }

     val result = pathElems.foldLeft[List[Any]](List(resource))((resources, path) =>{
       val founds:List[Any] =
         resources.flatMap(res => res match {
           case r:mutable.LinkedHashMap[String, Any] @unchecked =>
             val ret = r.get(path)
             ret
           case _ => None
         })
       val flattenFounds = flatten(founds)
       flattenFounds
     })

     result.map(_.asInstanceOf[T])
   }*/

  /**
   * Extraxt the list of JValue s in the given search parameter path e.g. Observaiton.subject
   *
   * @param path
   * @param resource
   * @return
   */
  def applySearchParameterPath(path: String, resource: JValue): Seq[JValue] = {
    val result =
      path
        .split('.')
        .foldLeft(resource)((v, i) => v \ i)

    result match {
      case JNothing => Nil
      case a: org.json4s.JsonAST.JArray => flattenJArray(a)
      case _ => Seq(result)
    }
  }

  private def flattenJArray(a: JArray): Seq[JValue] = {
    a.arr.flatMap {
      case sa: JArray => flattenJArray(sa)
      case o => Seq(o)
    }
  }

  /**
   * Extract FHIR reference values from a resource
   *
   * @param refPath  Path to the reference element e.g. Observation.subject --> subject
   * @param resource Resource content
   * @return Reference strings e.g. Patient/21554
   */
  def extractReferences(refPath: String, resource: Resource): Seq[String] = {
    applySearchParameterPath(refPath, resource)
      .flatMap {
        case o: JObject => (o \ FHIR_COMMON_FIELDS.REFERENCE).extractOpt[String].toSeq
        case a: JArray => (a \ FHIR_COMMON_FIELDS.REFERENCE).extract[Seq[String]]
        case _ => Nil
      }
  }

  /**
   * Extract FHIR canonical reference values from a resource
   *
   * @param refPath  Path to the reference element e.g. QuestionnaireResponse.questionnaire
   * @param resource Resource content
   * @return Canonical reference e.g. http://example.com/ValueSet/x
   */
  def extractCanonicals(refPath: String, resource: Resource): Seq[String] = {
    applySearchParameterPath(refPath, resource)
      .flatMap {
        case c: JString => Seq(c.s)
        case a: JArray => a.extract[Seq[String]]
        case _ => throw new IllegalArgumentException("The given path does not indicate a FHIR canonical reference element!")
      }
  }

  /**
   * Parse FHIR canonical value and return FhirCanonicalReference if it is full url and canonical or FhirLiteralReference
   * if it is a local reference
   *
   * @param v Value of element
   * @return
   */
  def parseCanonicalRef(v: JValue): FhirCanonicalReference = {
    val canonicalUrl = v.extract[String]
    parseCanonicalReference(canonicalUrl)
    /*.getOrElse({
      val (url, rtype, rid, version) = parseReferenceValue(canonicalUrl)
      FhirLiteralReference(url, rtype, rid, version)
    })*/
  }

  /**
   * Parse a canononical reference value
   *
   * @param c
   * @return
   */
  def parseCanonicalReference(c: String): FhirCanonicalReference = {
    val urlAndFragment = c.split('#')
    val urlAndVersion = urlAndFragment.head.split('|')
    val urlFragments = urlAndVersion.head.split('/')

    FhirCanonicalReference(
      urlFragments.dropRight(2).mkString("/"),
      urlFragments.dropRight(1).last,
      urlFragments.last,
      urlAndVersion.drop(1).headOption,
      urlAndFragment.drop(1).headOption
    )
  }

  /**
   * Parse a FHIR Reference or Canonical element
   *
   * @param value
   * @return
   */
  def parseReference(value: JValue): FhirReference = {
    value match {
      case obj: JObject =>
        FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.REFERENCE) match {
          case Some(fhirReferenceUrl) if fhirReferenceUrl.startsWith("#") => FhirInternalReference(fhirReferenceUrl.drop(1))
          case Some(fhirReferenceUrl) if !fhirReferenceUrl.startsWith("#") =>
            val r = parseReferenceValue(fhirReferenceUrl)
            FhirLiteralReference(r._1, r._2, r._3, r._4)
          case None =>
            val referencedResourceType = FHIRUtil.extractValueOption[String](obj, FHIR_COMMON_FIELDS.TYPE)
            val refIdentifier = FHIRUtil.extractValueOption[JObject](obj, FHIR_COMMON_FIELDS.IDENTIFIER).get
            FhirLogicalReference(referencedResourceType, FHIRUtil.extractValueOption[String](refIdentifier, FHIR_COMMON_FIELDS.SYSTEM), FHIRUtil.extractValue[String](refIdentifier, FHIR_COMMON_FIELDS.VALUE))
          case _ =>
            throw new Exception("Invalid FHIR reference content!")
        }
      case _ =>
        throw new Exception("Invalid FHIR reference")
    }
  }

  /**
   * Parse a FHIR reference value
   * e.g. Simple --> Patient/575644,
   * e.g. URL --> http://fhir.hl7.org/svc/StructureDefinition/c8973a22-2b5b-4e76-9c66-00639c99e61b
   * e.g. With URL and history --> http://example.org/fhir/Observation/1x2/_history/2
   *
   * @param reference FHIR Reference value
   * @return Optional URL part, Resource Type, Resource Id, Option version
   */
  def parseReferenceValue(reference: String): (Option[String], String, String, Option[String]) = {
    val parts = reference.split("/")
    parts.length match {
      case 2 => (None, parts.head, parts.last, None) //Resource Type and id
      case l if l >= 4 =>
        if (parts.apply(l - 2) == "_history") {
          val version = parts.last
          val rtype = parts.apply(l - 4)
          val rid = parts.apply(l - 3)
          val urlParts = parts.dropRight(4)
          val url = if (l > 4) Some(urlParts.mkString("/")) else None

          (url, rtype, rid, Some(version))
        } else {
          val rid = parts.last
          val rtype = parts.apply(l - 2)
          val url = parts.dropRight(2).mkString("/")
          (Some(url), rtype, rid, None)
        }
      case _ =>
        throw new Exception("Invalid reference value")
    }
  }

  /**
   * Extract system and code parts from token query value
   *
   * @param tokenValue Searched value for token parameter e.g. http://loinc|500-5 , |500-5, 500-5
   * @return system and code part for thr query
   */
  def parseTokenValue(tokenValue: String): (Option[String], Option[String]) = {
    //Query like [system]|
    if (tokenValue.last == '|')
      Some(tokenValue.dropRight(1)) -> None
    else
      tokenValue.split('|') match {
        //Query like |[code]
        case Array("", c) => Some("") -> Some(c)
        //Query like [code]
        case Array(c) => None -> Some(c)
        //Query like [system]|[code]
        case Array(s, c) => Some(s) -> Some(c)
        case _ => throw new InvalidParameterException(s"Parameter value $tokenValue is not valid token value!")
      }
  }

  /**
   * Resolve query reference value into URL, Resource Type, Resource Id, and version based on modifier
   *
   * @param reference
   * @param modifier
   * @param targetReferenceTypes
   * @return
   */
  def resolveReferenceValue(reference: String, modifier: String, targetReferenceTypes: Seq[String]): (Option[String], String, String, Option[String]) = {
    modifier match {
      case FHIR_PREFIXES_MODIFIERS.TYPE =>
        (None, reference, reference, None)
      case "" | FHIR_PREFIXES_MODIFIERS.NOT =>
        if (reference.contains("/"))
          parseReferenceValue(reference)
        else {
          if (targetReferenceTypes.length > 1)
            throw new InvalidParameterException(s"As search paremeter has multiple target reference types, a type should be supplied in the search!")

          (None, targetReferenceTypes.head, reference, None)
        }
      case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
        (None, "", reference, None)
      //If the modifier is the ResourceType to target for the reference
      case m if m.startsWith(":") => // :[type] modifier is used
        (None, modifier.tail, reference, None) // Make the reference value as [type]/[id]
    }
  }

  /**
   * Parse canonical query value and extract URL and version part
   *
   * @param canonical
   * @return
   */
  def parseCanonicalValue(canonical: String): (String, Option[String]) = {
    val canonicalParts = canonical.split('|')
    canonicalParts.head -> canonicalParts.drop(1).lastOption
  }

  /**
   * Parse the quantity search value to retrieve Value, System and Code/Unit
   *
   * @param quantity
   * @return
   */
  def parseQuantityValue(quantity: String): (String, Option[String], Option[String]) = {
    //Parse the given value
    quantity.split('|') match {
      //Query like [number]
      case Array(v) => (v, None, None)
      //Query like [number]||[code]
      case Array(v, "", c) => (v, None, Some(c))
      //Query like [number]|[system]|[code]
      case Array(v, s, c) => (v, Some(s), Some(c))
      case _ =>
        throw new InvalidParameterException(s"Invalid parameter value $quantity for quantity search!")
    }
  }

  /**
   * Parse token query value for of-type modifier
   *
   * @param tokenValue
   * @return
   */
  def parseTokenOfTypeValue(tokenValue: String): (String, String, String) = {
    tokenValue.split('|') match {
      case Array(typeSystem, typeCode, value) if !typeSystem.isEmpty && !typeCode.isEmpty && !value.isEmpty =>
        (typeSystem, typeCode, value)
      case _ => throw new InvalidParameterException(s"Parameter value $tokenValue is not valid token value for of-type modifier! Modifier of-type needs all three fields in format [system]|[code]|[value]!")
    }
  }

  /**
   * Split the path into two parts;
   *   - First part --> the part to search with elemMatch as it is an array (the last array element in the path) if exist
   *   - Second part --> the query path part
   *     e.g. target[i].dueDate --> target, dueDate
   *     e.g. identifier[i] --> identifier, ""
   *     e.g. code.coding[i] --> code.coding, ""
   *
   * @param path the path to the element
   * @return
   */
  def splitElementPathIntoElemMatchAndQueryPaths(path: String): (Option[String], Option[String]) = {
    if (path == "")
      None -> None
    else {
      //Find the index of last array element in the path
      val ind = path.lastIndexOf("[i]")
      //If there is no array in the path
      if (ind == -1)
        None -> Some(path)
      //If the last one is array
      else if (ind == path.length - 3)
        Some(path.replace("[i]", "")) -> None
      //Otherwise
      else
        Some(
          path
            .substring(0, ind)
            .replace("[i]", "")
        ) -> Some(path.substring(ind + 4, path.length))
    }
  }

  /**
   * Remove the array indicators from the path
   *
   * @param path
   * @return
   */
  def normalizeElementPath(path: String): String = path.replace("[i]", "")

  /**
   * Merge a main path with a subpath as Json path e.g. code , coding.code --> code.coding.code
   *
   * @param mainPath Main path
   * @param subPath  sub path
   * @return
   */
  def mergeElementPath(mainPath: Option[String], subPath: String): String = mainPath.map(_ + ".").getOrElse("") + subPath

  def mergeElementPath(mainPath: String, subPath: String): String = if (mainPath == "") subPath else if (subPath == "") mainPath else mainPath + "." + subPath

  def mergeFilePath(mainPath: Option[String], subPath: String): String = mainPath.map(_ + "/").getOrElse("") + subPath


  /**
   * Decapitilize a string
   *
   * @param s
   * @return
   */
  def decapitilize(s: String): String = {
    s"${s.charAt(0).toLower}${s.substring(1)}"
  }

  /**
   * FHIR index of query restrictions on the path
   *
   * @param pathParts    Splitted path
   * @param restrictions Restrictions on path e.g. @.type = email
   * */
  def findIndexOfRestrictionsOnPath(pathParts: Seq[String], restrictions: Seq[(String, String)]): Seq[(Int, Seq[(String, String)])] = {
    val indexOfRestrictions =
      restrictions
        .map(r => (pathParts.length - r._1.count(_ == '@') - 1) -> r)
        .groupBy(_._1)
        .map(g => g._1 ->
          g._2.map(_._2)
            .map(i => i._1.replace("@.", "") -> i._2) //remove paths
        ).toSeq.sortBy(_._1)

    indexOfRestrictions
  }

  /**
   * Extract the value from the Parameters.parameter
   *
   * @param paramObj
   * @return
   */
  def getValueFromParameter(paramObj: JObject): Option[JValue] = {
    paramObj.obj
      .find(f => f._1.startsWith(FHIR_COMMON_FIELDS.VALUE))
      .orElse(
        paramObj.obj
          .find(_._1 == FHIR_COMMON_FIELDS.RESOURCE)
      ).orElse(
        paramObj.obj
          .find(_._1 == "part")
      ).map(_._2)
  }

  /**
   * Get a parameter's value (simple or resource or multi) from Parameters resource by its name
   *
   * @param parametersResource FHIR Parameters resource content
   * @param paramName          Parameter name
   * @return
   */
  def getParameterValueByName(parametersResource: Resource, paramName: String): Option[JValue] = {
    (parametersResource \ FHIR_COMMON_FIELDS.PARAMETER) match {
      case JArray(values) =>
        values
          .filter(p => (p \ FHIR_COMMON_FIELDS.NAME).extract[String] == paramName) match {
          case List(obj: JObject) =>
            getValueFromParameter(obj)

          case l =>
            l
              .map(_.asInstanceOf[JObject])
              .map(obj =>
                getValueFromParameter(obj)
              )
              .filter(_.isDefined)
              .map(_.get) match {
              case Nil => None
              case oth => Some(JArray(oth))
            }
        }
      case _ => None
    }
  }

  /**
   * Return the values of the parameter with given path of parameter names within FHIR Parameters resource
   *
   * @param parametersResource FHIR Parameters resource
   * @param paramPath          Parameter path splitted by dots e.g. match.concept
   * @return
   */
  def getParameterValueByPath(parametersResource: Resource, paramPath: String): Seq[JValue] = {
    def getFromParameters(parameters: JArray, path: Seq[String]): Seq[JValue] = {
      parameters
        .arr
        .map(_.asInstanceOf[JObject])
        .map(parseParameter)
        .filter(p => p._1 == path.head && p._2 != JNull)
        .map(_._2)
        .flatMap(value => {
          path.tail match {
            case Nil => Seq(value)
            case remainingPath =>
              value match {
                case a: JArray =>
                  getFromParameters(a, remainingPath)
                case _ => Nil
              }
          }
        })
    }

    val paramNames = paramPath.split('.').toIndexedSeq
    getFromParameters((parametersResource \ FHIR_COMMON_FIELDS.PARAMETER).asInstanceOf[JArray], paramNames)
  }

  /**
   * Parse Parameters.parameter into name -> value
   *
   * @param param
   * @return
   */
  def parseParameter(param: JObject): (String, JValue) = {
    FHIRUtil.extractValue[String](param, "name") ->
      getValueFromParameter(param).getOrElse(JNull)
  }

  /**
   * Load class if possible
   *
   * @param classPath Class path
   * @return
   */
  def loadFhirOperationClass(classPath: String): Option[Class[_]] = {
    Try(this.getClass.getClassLoader.loadClass(classPath)) match {
      case Success(opClass) => Some(opClass)
      case Failure(e) => Try(ClassLoader.getSystemClassLoader.loadClass(classPath)).toOption
    }
  }
}

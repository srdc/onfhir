package io.onfhir.db

import io.onfhir.api.FHIR_PREFIXES_MODIFIERS
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception.InvalidParameterException
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

import java.net.URL
import scala.util.Try

object UriQueryBuilder extends IFhirQueryBuilder {

  /**
   * The uri parameter refers to a URI (RFC 3986) element. Matches are precise (e.g. case, accent, and escape)
   * sensitive, and the entire URI must match.
   * The modifier :above or :below can be used to indicate that
   * partial matching is used.
   *
   * @param values     Values supplied for the uri parameter
   * @param modifier   Modifier to be handled
   * @param path       Path to the target element to be queried
   * @param targetType FHIR Type of the target element
   * @return equivalent BsonDocument for the target query
   */
  def getQuery(values: Seq[String], modifier: String, path: String, targetType: String): Bson = {
    modifier match {
      //No modifier
      case "" => getQueryForUriEquality(values, path)
      case FHIR_PREFIXES_MODIFIERS.ABOVE =>
        //if(values.length > 1)
        //  throw new InvalidParameterException(s"Only single url value should be provided when modifier ${FHIR_PREFIXES_MODIFIERS.ABOVE} is used for FHIR url type parameters!")
        orQueries(values.map(v => getQueryForAboveModifier(v, path)))
      case FHIR_PREFIXES_MODIFIERS.BELOW =>
        //if (values.length > 1)
        //  throw new InvalidParameterException(s"Only single url value should be provided when modifier ${FHIR_PREFIXES_MODIFIERS.BELOW} is used for FHIR url type parameters!")
        orQueries(values.map(v => getQueryForBelowModifier(v, path))
      case oth =>
        throw new InvalidParameterException(s"Modifier ${oth} is not valid or supported by onFhir.io for FHIR url type parameters!")
    }
  }

  /**
   * Construct query for uri query (without modifier)
   * @param values  Supplied urls
   * @param path    Path to the element
   * @return
   */
  private def getQueryForUriEquality(values:Seq[String], path:String):Bson = {
    //If this is a query on  Canonical URLs of the conformance and knowledge resources (e.g. StructureDefinition, ValueSet, PlanDefinition etc) and a version part is given |[version]
      if (path == "url" && values.exists(_.contains("|"))) {
        val canonicalRefs = values.flatMap(value => Try(FHIRUtil.parseCanonicalReference(value)).toOption)
        val queriesForWithVersions =
          canonicalRefs
            .filter(_.version.isDefined)
            .map(cr =>
              Filters.and(Filters.equal(path, cr.getUrl()), Filters.equal("version", cr.version.get))
            )

       val urls = canonicalRefs.filter(_.version.isEmpty).map(_.getUrl())
       val queryForWithoutVersions = urls match {
         case Nil => None
         case Seq(single) => Some(Filters.equal(path, single))
         case multiple => Some(Filters.in(path, multiple:_*))
       }
       orQueries(queriesForWithVersions ++ queryForWithoutVersions.toSeq)
      } else {
        values match {
          case Seq(single) =>
            // Exact match
            Filters.equal(path, single)
          case multiple =>
            Filters.in(path, values:_*)
        }
      }
  }

  /**
   * Construct query for :above on urls
   * @param value Supplied URL
   * @param path  Path to the element
   * @return
   */
  private def getQueryForAboveModifier(value:String, path:String):Bson = {
    val url = Try(new URL(value)).toOption
    if (url.isEmpty || !value.contains('/'))
      throw new InvalidParameterException(s"Modifier ${FHIR_PREFIXES_MODIFIERS.ABOVE} is only supported for URLs not URNs or OIDs!")

    val initialPart = url.get.getProtocol + "://" + url.get.getHost + (if (value.contains(url.get.getHost + ":" + url.get.getPort.toString)) ":" + url.get.getPort else "")
    var urlPath = url.get.getPath
    if (urlPath.isEmpty || urlPath == "/") urlPath = "" else urlPath = urlPath.drop(1)
    val parts = urlPath.split("/")

    def constructRegexForAbove(parts: Seq[String]): String = {
      parts match {
        case Nil => ""
        case oth => "(" + FHIRUtil.escapeCharacters("/" + parts.head) + constructRegexForAbove(parts.drop(1)) + ")?"
      }
    }

    //Constuct the regex to match any url above
    val regularExpressionValue = FHIRUtil.escapeCharacters(initialPart) + constructRegexForAbove(parts.toIndexedSeq)
    Filters.regex(path, "\\A" + regularExpressionValue + "$")
  }

  /**
   * Construct query for :below modifier
   * @param value Supplied URL
   * @param path  Path to the element
   * @return
   */
  private def getQueryForBelowModifier(value: String, path: String):Bson = {
    val url = Try(new URL(value)).toOption
    if (url.isEmpty)
      throw new InvalidParameterException(s"Modifier ${FHIR_PREFIXES_MODIFIERS.BELOW} is only supported for URLs not URNs or OIDs!")
    // Escape characters for to have valid regular expression
    val regularExpressionValue = FHIRUtil.escapeCharacters(value) + "(" + FHIRUtil.escapeCharacters("/") + ".*)*"
    // Match at the beginning of the uri
    Filters.regex(path, "\\A" + regularExpressionValue + "$")
  }
}

package io.onfhir.db

import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_DATA_TYPES, FHIR_PREFIXES_MODIFIERS, FHIR_EXTRA_FIELDS}
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.InvalidParameterException
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

/**
 * Utility class to construct queries for FHIR reference type search parameters for a specific path and target type
 * Over Reference type elements, this supports the following modifiers
 *  - :identifier
 *  - :[type]
 *  - :type Onfhir specific modifier to search on resource type references e.g. subject:type=Patient
 *
 *  TODO :above :below not supported yet
 *
 * Over canonical type elements, supports the following modifiers
 *  - :below
 *
 *  TODO :above is not supported yet
 *
 * @param onlyLocalReferences Whether the FHIR server supports only local references
 */
class ReferenceQueryBuilder(onlyLocalReferences:Boolean) extends IFhirQueryBuilder {

  /**
   * A reference parameter refers to references between resources. The interpretation of a reference
   * parameter is either:
   *
   * [parameter]=[id] the logical [id] of a resource using a local reference (i.e. a relative reference)
   *
   * [parameter]=[type]/[id] the logical [id] of a resource of a specified type using
   * a local reference (i.e. a relative reference), for when the reference can point to different
   * types of resources (e.g. Observation.subject)
   *
   * [parameter]=[url] where the [url] is an absolute URL - a reference to a resource by its absolute location
   *
   * @param values                Supplied parameter values e.g. Patient/513515
   * @param modifier              Modifier used ("" indicates no modifier)
   * @param path                  Path to the target element to run search on
   * @param targetType            Data type of the target element
   * @param targetReferenceTypes  Allowed resource types to give reference for the target element
   * @return                      Equivalent BsonDocument for the target query
   */
  def getQuery(values:Seq[String], modifier:String, path:String, targetType:String, targetReferenceTypes:Seq[String]):Bson = {
    targetType match {
      //If this is a search on a FHIR Reference type element
      case FHIR_DATA_TYPES.REFERENCE =>
        getQueryOnReference(values, modifier, path, targetReferenceTypes)
      //If this is a search on Canonical references
      case FHIR_DATA_TYPES.CANONICAL =>
        ReferenceQueryBuilder.getQueryOnCanonicals(values, modifier, path)
      case _ =>
        throw new InvalidParameterException(s"Invalid usage of parameter. The reference type parameters cannot search on $targetType type elements!!!")
    }
  }


  /**
   * Get query for reference queries over FHIR Reference elements
   *
   * @param values   Supplied parameter values e.g. Patient/513515
   * @param modifier Modifier used ("" indicates no modifier)
   * @param path     Path to the target element to run search on
   * @param targetReferenceTypes  Allowed resource types to give reference for the target element
   * @return
   */
  private def getQueryOnReference(values:Seq[String], modifier:String, path:String, targetReferenceTypes:Seq[String]):Bson = {
    modifier match {
      //No modifier, normal reference search
      //e.g. Observation?subject=Patient/465465,Patient/4654654
      case "" =>
        getQueryForReferences(values, path, targetReferenceTypes)
      //If modifier is identifier, search like a token on identifier element (Reference.identifier)
      //e.g. Observation?subject:identifier=http://example.org/fhir/mrn|12345
      case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
        getQueryForIdentifierModifier(values, path)

      //Handle :text modifier (search on Reference.display)
      //e.g. Observation?subject:display=Tuncay
      case FHIR_PREFIXES_MODIFIERS.TEXT =>
        orQueries(values.map(value => StringQueryBuilder.getQueryOnTargetStringElement(FHIRUtil.normalizeElementPath(FHIRUtil.mergeElementPath(path, FHIR_COMMON_FIELDS.DISPLAY)), value, "")))

      //Handle :type modifier (searching with only type - onFHIR specific)
      //e.g. Observation?subject:type=Patient
      case FHIR_PREFIXES_MODIFIERS.TYPE =>
        ReferenceQueryBuilder.getQueryForTypeModifier(values, path)

      case FHIR_PREFIXES_MODIFIERS.ABOVE | FHIR_PREFIXES_MODIFIERS.BELOW =>
        throw new InvalidParameterException(s"The modifier $modifier is not supported by onFhir.io yet !!!")

      //Handle :[type] modifier
      //e.g. Observation?subject:Patient=321,5464
      case resourceType if resourceType.apply(1).isUpper =>
        getQueryForReferences(values, path, Seq(resourceType.drop(1)))

      case _ =>
        throw new InvalidParameterException(s"The modifier $modifier is not supported by onFhir.io yet !!!")
    }
  }

  /**
   * Construct query for :identifier modifier
   * e.g.  Observation?subject:identifier=http://example.org/fhir/mrn|12345
   * @param values   Supplied parameter values e.g. http://example.org/fhir/mrn|12345,http://example.org/fhir/mrn|12346
   * @param path     Path to the element e.g. subject.reference
   * @return
   */
  private def getQueryForIdentifierModifier(values:Seq[String], path:String):Bson = {
    //Find out the elemMatch and query parts of the path
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
    // Parse the given the parameter value that should be [system]|[value] format
    val parsedSystemAndValues = values.map(FHIRUtil.parseTokenValue)
    if (parsedSystemAndValues.exists(sv => sv._1.isEmpty || sv._2.isEmpty))
      throw new InvalidParameterException(s"Invalid usage of parameter. The parameter value should be provided in [system]|[value] format for ${FHIR_PREFIXES_MODIFIERS.IDENTIFIER} modifier!!!")

    val groupedSystemValues =
      parsedSystemAndValues
        .map(sv => sv._1.get -> sv._2.get)
        .groupBy(_._1).map(g => g._1 -> g._2.map(_._2))
        .toSeq

    val systemPath = FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.IDENTIFIER}.${FHIR_COMMON_FIELDS.SYSTEM}")
    val valuePath = FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.IDENTIFIER}.${FHIR_COMMON_FIELDS.VALUE}")

    val mainQuery =
      orQueries(
        groupedSystemValues
          .map {
            case (system, Seq(single)) => Filters.and(Filters.eq(systemPath, system), Filters.eq(valuePath, single))
            case (system, values) => Filters.and(Filters.eq(systemPath, system), Filters.in(valuePath, values: _*))
          }
      )
    getFinalQuery(elemMatchPath, mainQuery)
  }

  /**
   * Construct query for normal reference search
   * e.g. Observation?subject=Patient/35135,Patient/25423451
   * @param values                  Supplied parameter values e.g. Patient/2413
   * @param path                    Path to the Reference type element
   * @param targetReferenceTypes    Allowed target referenced types
   * @return
   */
  def getQueryForReferences(values:Seq[String], path:String, targetReferenceTypes:Seq[String]):Bson = {
    //Find out the elemMatch and query parts of the path
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
    //Parse reference value (URL part, resource type, resource id, version)
    //(URL part, resource type, resource id, version)
    val parsedReferences: Seq[(Option[String], String, String, Option[String])] =
      values
        .map(reference => FHIRUtil.resolveReferenceValue(reference, "", targetReferenceTypes))
        .map {
          case (Some(OnfhirConfig.fhirRootUrl), rtype, rid, version) => (None, rtype, rid, version) //If root url of server is given just ignore it
          case (Some(url), rtype, rid, version) => (Some(url), rtype, rid, version)
          case oth => oth
        }

    //For the ones where a version is not given, construct queries
    //e.g. Patient/31321
    //e.g. http://onfhir.io/fhir/Patient/151
    val referencesWithoutVersion = parsedReferences.filter(_._4.isEmpty)
    //Group the references with url, rtype
    val groupedReferences = referencesWithoutVersion.groupBy(r => (r._1, r._2)).map(g => g._1 -> g._2.map(_._3)).toSeq
    val queries =
        groupedReferences
          .map {
            //Local reference allowed only and no url given (or local server's url given), check only resource type and identifiers
            case ((None, rtype), references) if onlyLocalReferences=>
              ReferenceQueryBuilder.getQueryForReferenceMatch(queryPath, rtype, references)
            //If url is not given, but server also allowed referencing to remote url resources
            case ((None, rtype), references) =>
              Filters.and(
                ReferenceQueryBuilder.getQueryForReferenceMatch(queryPath, rtype, references),
                Filters.or(
                  Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), OnfhirConfig.fhirRootUrl), //Either it should equal to our root URL
                  Filters.exists(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), exists= false) //Or url part does not exist
                )
              )
            case ((Some(url),rtype), references ) =>
              Filters.and(
                ReferenceQueryBuilder.getQueryForReferenceMatch(queryPath, rtype, references),
                Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), url)
              )
          }
    //For the ones where version is given
    //e.g. Patient/123/_history/1
    val referencesWithVersion = parsedReferences.filter(_._4.isDefined)
    val queriesWithVersions =
      referencesWithVersion
        .map {
          case (None, rtype, rid, Some(version)) if onlyLocalReferences =>
            Filters.and(
              ReferenceQueryBuilder.getQueryForReferenceMatch(queryPath, rtype, Seq(rid)),
              Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_VERSION}"), version)
            )
          case (None, rtype, rid, Some(version)) =>
            Filters.and(
              ReferenceQueryBuilder.getQueryForReferenceMatch(queryPath, rtype, Seq(rid)),
              Filters.or(
                Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), OnfhirConfig.fhirRootUrl), //Either it should equal to our root URL
                Filters.exists(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), exists = false) //Or url part does not exist
              ),
              Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_VERSION}"), version)
            )
          case (Some(url), rtype, rid, Some(version)) =>
            Filters.and(
              ReferenceQueryBuilder.getQueryForReferenceMatch(queryPath, rtype, Seq(rid)),
              Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_URL}"), url),
              Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_VERSION}"), version)
            )
        }
    //Combine all the queries for each group with Logical OR
    val mainQuery = orQueries(queries ++ queriesWithVersions)
    getFinalQuery(elemMatchPath, mainQuery)
  }


}

object ReferenceQueryBuilder extends IFhirQueryBuilder {

  /**
   * Construct the query to search resources with canonical urls and versions
   *
   * @param urlAndVersions List of url and versions
   * @return
   */
  def getQueryOnCanonicalRefs(urlAndVersions: Seq[(String, Option[String])]): Bson = {
    val canonicalWithVersions = urlAndVersions.filter(_._2.isDefined)
    val canonicalWithoutVersions = urlAndVersions.filter(_._2.isEmpty)

    val withoutVersionQueries =
      if(canonicalWithoutVersions.nonEmpty)
        Some(Filters.in(FHIR_COMMON_FIELDS.URL, canonicalWithoutVersions.map(_._1):_*))
      else
        None

    val withVersionQueries =
      canonicalWithVersions.map {
        case (url, None) => Filters.eq(FHIR_COMMON_FIELDS.URL, url)
        case (url, Some(v)) => Filters.and(Filters.eq(FHIR_COMMON_FIELDS.URL, url), Filters.eq(FHIR_COMMON_FIELDS.VERSION, v))
      }
    orQueries(withVersionQueries ++ withoutVersionQueries.toSeq)
  }
  /**
   * Construct query for reference type parameters on canonical references
   *
   * @param values   Supplied parameter values e.g. http://onfhir.io/fhir/ValueSet/myValueSet|1.0
   * @param modifier Modifier used ("" indicates no modifier)
   * @param path     Path to the target element to run search on
   * @return
   */
  def getQueryOnCanonicals(values: Seq[String], modifier: String, path: String): Bson = {
    //As for canonical, we only look at one field we don't need to care arrays in the path
    modifier match {
      //No modifier
      case "" =>
        val parsedCanonicals = values.map(FHIRUtil.parseCanonicalValue)
        val canonicalWithVersions =
          parsedCanonicals
            .filter(_._2.isDefined)
            .map(c => s"${c._1}|${c._2.get}")

        val queryWithVersion =
          canonicalWithVersions match {
            case Nil => None
            case Seq(single) => Some(Filters.eq(FHIRUtil.normalizeElementPath(path), single))
            case multiple => Some(Filters.in(FHIRUtil.normalizeElementPath(path), multiple: _*))
          }
        val queriesWithoutVersion =
          parsedCanonicals.filter(_._2.isEmpty).map(_._1).map(url =>
            Filters.regex(FHIRUtil.normalizeElementPath(path), "\\A" + FHIRUtil.escapeCharacters(url) + "(\\|[0-9]+(\\.[0-9]*)*)?$")
          )
        orQueries(queryWithVersion.toSeq ++ queriesWithoutVersion)

      //Searching with :below modifier
      case FHIR_PREFIXES_MODIFIERS.BELOW =>
        orQueries(
          values
            .map(FHIRUtil.parseCanonicalValue)
            .map {
              case (canonicalUrl, canonicalVersion) =>
                // Escape characters for to have valid regular expression
                val regularExpressionValue = FHIRUtil.escapeCharacters(canonicalUrl) + canonicalVersion.map(v => s"\\|$v(\\.[0-9]*)+").getOrElse("")
                // Match at the beginning of the uri
                Filters.regex(FHIRUtil.normalizeElementPath(path), "\\A" + regularExpressionValue + "$")
            }
        )
      case _ =>
        throw new InvalidParameterException(s"The modifier $modifier is not supported by onFhir.io yet for canonical targets !!!")
    }
  }

  /**
   * Construct query for basic reference match
   *
   * @param queryPath Path to the Reference type element
   * @param rtype     Resource type to match for reference
   * @param rids      Resource id or ids to match
   * @return
   */
  private def getQueryForReferenceMatch(queryPath: Option[String], rtype: String, rids: Seq[String]): Bson = {
    Filters
      .and(
        rids match {
          case Seq(single) =>
            Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}"), single)
          case multiple =>
            Filters.in(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}"), multiple: _*)
        },
        Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE}"), rtype)
      )
  }


  /**
   * Get query for onFhir specific :type modifier
   *
   * @param values Resource type or types e.g. Patient, Observation
   * @param path   Path to the Reference type element
   * @return
   */
  private def getQueryForTypeModifier(values: Seq[String], path: String): Bson = {
    //Find out the elemMatch and query parts of the path
    val (elemMatchPath, queryPath) = FHIRUtil.splitElementPathIntoElemMatchAndQueryPaths(path)
    val mainQuery =
      values match {
        case Seq(single) => Filters.eq(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE}"), single)
        case multiple => Filters.in(FHIRUtil.mergeElementPath(queryPath, s"${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE}"), multiple: _*)
      }
    getFinalQuery(elemMatchPath, mainQuery)
  }
}
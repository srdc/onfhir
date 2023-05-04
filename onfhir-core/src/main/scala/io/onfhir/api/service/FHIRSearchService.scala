package io.onfhir.api.service

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, FHIRSearchResult, Parameter}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.{InvalidParameterException, UnsupportedParameterException}

import scala.concurrent.Future

class FHIRSearchService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession)  {

  /**
    * Validate if interaction is supported for the request
    *
    * @param fhirRequest FHIR Request
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply {
      fhirRequest.interaction match {
        case FHIR_INTERACTIONS.SEARCH => validateTypeLevelSearch(fhirRequest)
        case FHIR_INTERACTIONS.SEARCH_SYSTEM => validateSystemLevelSearch(fhirRequest)
      }
    }
  }

  /**
   * Validations for type level search
   * @param fhirRequest
   */
  private def validateTypeLevelSearch(fhirRequest: FHIRRequest):Unit = {
    //1) Validate the search operation (parameters + type)
    //1.1) Validate if search is allowed
    FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.SEARCH, fhirRequest.resourceType.get)

    //1.2) Validate if compartment is supported and compartment id is ok
    if(fhirRequest.compartmentType.isDefined) {
      FHIRApiValidator.validateCompartment(fhirRequest.compartmentType.get)
      FHIRApiValidator.validateCompartmentSearchOnResourceType(fhirRequest.compartmentType.get, fhirRequest.resourceType.get)
      FHIRApiValidator.validateId(fhirRequest.compartmentId.get)

      fhirRequest.addParsedQueryParams(List(fhirConfigurationManager.fhirSearchParameterValueParser.constructCompartmentSearchParameter(fhirRequest.compartmentType.get, fhirRequest.compartmentId.get, fhirRequest.resourceType.get)))
    }

    fhirRequest.addParsedQueryParams(
      fhirConfigurationManager.fhirSearchParameterValueParser.parseSearchParameters(fhirRequest.resourceType.get, fhirRequest.queryParams, fhirRequest.prefer))
  }

  /**
   * Validate system level search
   * @param fhirRequest
   */
  private def validateSystemLevelSearch(fhirRequest: FHIRRequest):Unit = {
    //Check if system-level search is supported
    FHIRApiValidator.validateSystemLevelInteraction(fhirRequest.interaction)

    //1.2) Validate if compartment is supported and compartment id is ok
    if(fhirRequest.compartmentType.isDefined) {
      FHIRApiValidator.validateCompartment(fhirRequest.compartmentType.get)
      FHIRApiValidator.validateId(fhirRequest.compartmentId.get)
    }

    //Find out the resource types to search on
    val rtypes = FHIRApiValidator.validateAndReturnTypeParameter(fhirRequest)
    // Get rid of the _type param
    val queryParams = fhirRequest.queryParams.-("_type")

    if(fhirRequest.compartmentType.isDefined) {
      rtypes.foreach(rtype => {
        //Validate compartment search on resource type
        FHIRApiValidator.validateCompartmentSearchOnResourceType(fhirRequest.compartmentType.get, rtype)
        //Add the compartment search parameter for each resource type
        fhirRequest
          .addParsedQueryParams(rtype, List(fhirConfigurationManager.fhirSearchParameterValueParser.constructCompartmentSearchParameter(fhirRequest.compartmentType.get, fhirRequest.compartmentId.get, rtype)))
      })
    }

    //Given search parameters should be supported by all of the given resource types
    rtypes.foreach(rtype =>
      fhirRequest
        .addParsedQueryParams(
          rtype,
          try {
            fhirConfigurationManager.fhirSearchParameterValueParser.parseSearchParameters(rtype, queryParams, Some(FHIR_HTTP_OPTIONS.FHIR_SEARCH_STRICT))
          } catch {
            case e:UnsupportedParameterException => throw new InvalidParameterException(e.getMessage) // What is expected from standard
          }
        )
    )
    //Validate usage of system parameters
    FHIRApiValidator.validateResultParametersForSystemSearch(fhirRequest)
  }

  /**
    * Perform the interaction
    * @param fhirRequest    FHIR Request
    * @param authzContext   Authorization context
    * @param isTesting      If this interaction is just for testing
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean = false): Future[FHIRResponse] = {
    if(fhirRequest.compartmentType.isDefined)
      logger.debug(s"${fhirRequest.compartmentType.get} compartment search on ${fhirRequest.resourceType.getOrElse("*")}...")
    else
      logger.debug(s"Processing search request on ${fhirRequest.resourceType}...")

    fhirRequest.interaction match {
      case FHIR_INTERACTIONS.SEARCH =>
        //Perform the search
        searchAndReturnBundle(fhirRequest.resourceType.get, fhirRequest.getParsedQueryParams()) map { bundle =>
          FHIRResponse(StatusCodes.OK, Some(bundle))
        }

      case FHIR_INTERACTIONS.SEARCH_SYSTEM =>
        searchSystemAndReturnBundle(fhirRequest.getAllParsedQueryParams(), !fhirRequest.queryParams.contains("_type"))map { bundle =>
          FHIRResponse(StatusCodes.OK, Some(bundle))
        }
    }
  }

  /**
   * System level search
   * @param parameters  Parsed parameter for each resource type to search
   * @return
   */
  def searchSystemAndReturnBundle(parameters:Map[String, List[Parameter]], isAllResources:Boolean):Future[Resource] = {
    val rtypes = if(isAllResources) Nil else parameters.keys.toSeq
    fhirConfigurationManager.resourceManager
      .searchResourcesFromMultipleResourceTypes(parameters)(transactionSession)
      .map(searchResult =>
        constructBundle(FHIRSearchResult(searchResult._1, searchResult._2), rtypes, parameters.headOption.map(_._2).getOrElse(List.empty[Parameter]))
      )
  }
  /**
    * Implementation of common search mechanism of all types (single resource, compartment, general search).
    * @param rtype FHIR Resource type to search
    * @param parameters Parsed search parameters
    * @return
    */
  def searchAndReturnBundle(rtype:String, parameters:List[Parameter]):Future[Resource] = {
    fhirConfigurationManager.resourceManager
      .searchResources(rtype, parameters)(transactionSession)
      .map(searchResult =>
        constructBundle(searchResult, Seq(rtype), parameters)
      )
  }

  /**
   * Construct bundle from returned results
   * @param totalNumOfMatched   Total number of results
   * @param matchedResources    Matched resources for search
   * @param includedResources   Included resources due to _include or _revinclude
   * @param rtype               Resource type searched if exist
   * @param parameters
   * @return
   */
  private def constructBundle(
                               searchResult:FHIRSearchResult,
                               rtype:Seq[String],
                               parameters:List[Parameter]):Resource = {
    //Construct paging links
    val pagingLinks =
      if(OnfhirConfig.fhirDefaultPagination == "offset" || parameters.exists(p => p.name == FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER || p.name == FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE))
        fhirConfigurationManager.fhirServerUtil.generateBundleLinksForOffsetBasedPagination(rtype, searchResult, parameters)
      else
        fhirConfigurationManager.fhirServerUtil.generateBundleLinks(rtype, None, searchResult.total, searchResult.matches.length, parameters, isHistory = false)


    //Whether search results are summarized or not
    val summaryParamValue = parameters.find(p => p.name == FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY).map(_.valuePrefixList.head._2)
    val isElementsParamExist = parameters.exists(p => p.name == FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS)
    val isSummarized = summaryParamValue.exists(_ != FHIR_SUMMARY_OPTIONS.FALSE) || isElementsParamExist

    //Create FHIR bundle entries
    val bundleEntries =
      searchResult.matches.map(matchedResource => fhirConfigurationManager.fhirServerUtil.createBundleEntry(matchedResource, FHIR_BUNDLE_TYPES.SEARCH_SET, isSummarized)) ++
        searchResult.includes.map(includedResource => fhirConfigurationManager.fhirServerUtil.createBundleEntry(includedResource, FHIR_BUNDLE_TYPES.SEARCH_SET, isSummarized, isMatched = false))

    logger.debug(s"Returning ${searchResult.matches.length} resources from ${searchResult.total} and ${searchResult.includes.length} documents are marked as include...")
    //Create and return bundle
    FHIRUtil.createBundle(FHIR_BUNDLE_TYPES.SEARCH_SET, pagingLinks, bundleEntries, searchResult.total, summaryParamValue)
  }
}
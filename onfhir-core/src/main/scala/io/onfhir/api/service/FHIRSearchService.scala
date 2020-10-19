package io.onfhir.api.service

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.db.{ResourceManager, TransactionSession}

import scala.concurrent.Future

class FHIRSearchService(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession)  {

  /**
    * Validate if interaction is supported for the request
    *
    * @param fhirRequest FHIR Request
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply {
      //1) Validate the search operation (parameters + type)
      //1.1) Validate if search is allowed
      FHIRApiValidator.validateInteractionOnResourceType(FHIR_INTERACTIONS.SEARCH, fhirRequest.resourceType.get)

      //1.2) Validate if compartment is supported and compartment id is ok
      if(fhirRequest.compartmentType.isDefined) {
        FHIRApiValidator.validateCompartment(fhirRequest.compartmentType.get, fhirRequest.resourceType.get)
        FHIRApiValidator.validateId(fhirRequest.compartmentId.get)

        fhirRequest.addParsedQueryParams(List(FHIRSearchParameterValueParser.constructCompartmentSearchParameter(fhirRequest.compartmentType.get, fhirRequest.compartmentId.get, fhirRequest.resourceType.get)))
      }

      fhirRequest.addParsedQueryParams(
          FHIRSearchParameterValueParser.parseSearchParameters(fhirRequest.resourceType.get, fhirRequest.queryParams, fhirRequest.prefer))
    }
  }

  /**
    * Perform the interaction
    * @param fhirRequest    FHIR Request
    * @param authzContext   Authorization context
    * @param isTesting      If this interaction is just for testing
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext] = None, isTesting: Boolean = false): Future[FHIRResponse] = {
    if(fhirRequest.compartmentType.isDefined)
      logger.debug(s"${fhirRequest.compartmentType.get} compartment search on ${fhirRequest.resourceType.get}...")
    else
      logger.debug(s"Processing search request on ${fhirRequest.resourceType}...")
    //Perform the search
    searchAndReturnBundle(fhirRequest.resourceType.get, fhirRequest.getParsedQueryParams()) map { bundle =>
      FHIRResponse(StatusCodes.OK, Some(bundle))
    }
  }

  /**
    * Implementation of common search mechanism of all types (single resource, compartment, general search).
    * @param rtype FHIR Resource type to search
    * @param parameters Parsed search parameters
    * @return
    */
  def searchAndReturnBundle(rtype:String, parameters:List[Parameter]):Future[Resource] = {
    ResourceManager.searchResources(rtype, parameters)(transactionSession).map(searchResult => {
      val totalNumOfMatched = searchResult._1
      val matchedResources = searchResult._2
      val includedResources = searchResult._3
      //Construct paging links
      val pagingLinks = FHIRUtil.generateBundleLinks(Some(rtype), None, totalNumOfMatched, parameters, isHistory = false)

      //Whether search results are summarized or not
      val summaryParamValue = parameters.find(p => p.name == FHIR_SEARCH_RESULT_PARAMETERS.SUMMARY).map(_.valuePrefixList.head._2)
      val isElementsParamExist = parameters.exists(p => p.name == FHIR_SEARCH_RESULT_PARAMETERS.ELEMENTS)
      val isSummarized = summaryParamValue.exists(_ != FHIR_SUMMARY_OPTIONS.FALSE) || isElementsParamExist

      //Create FHIR bundle entries
      val bundleEntries =
        matchedResources.map(matchedResource => FHIRUtil.createBundleEntry(matchedResource, FHIR_BUNDLE_TYPES.SEARCH_SET, isSummarized)) ++
        includedResources.map(includedResource => FHIRUtil.createBundleEntry(includedResource, FHIR_BUNDLE_TYPES.SEARCH_SET, isSummarized, isMatched = false))

      logger.debug(s"Returning ${matchedResources.length} resources from $totalNumOfMatched and ${includedResources.length} documents are marked as include...")
      //Create and return bundle
      FHIRUtil.createBundle(FHIR_BUNDLE_TYPES.SEARCH_SET, pagingLinks, bundleEntries, totalNumOfMatched, summaryParamValue)
    })
  }
}
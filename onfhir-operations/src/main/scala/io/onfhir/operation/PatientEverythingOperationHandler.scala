package io.onfhir.operation

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api.{FHIR_BUNDLE_FIELDS, FHIR_BUNDLE_TYPES, FHIR_DATA_TYPES, FHIR_PARAMETER_CATEGORIES, FHIR_PARAMETER_TYPES, FHIR_SEARCH_RESULT_PARAMETERS, Resource}
import io.onfhir.api.model.{FHIROperationRequest, FHIROperationResponse, FHIRResponse, FHIRSearchResult, OutcomeIssue, Parameter}
import io.onfhir.api.service.FHIROperationHandlerService
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.{IFhirConfigurationManager, OnfhirConfig}
import io.onfhir.exception.{BadRequestException, MethodNotAllowedException}
import io.onfhir.validation.FhirContentValidator
import org.json4s.JsonAST.JString
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

/**
 *
 * @param fhirConfigurationManager
 */
class PatientEverythingOperationHandler (fhirConfigurationManager:IFhirConfigurationManager) extends FHIROperationHandlerService(fhirConfigurationManager) {
  private val logger: Logger = LoggerFactory.getLogger("EverythingOperationHandler")
  /**
   * Allowed/configured resource types to return in everything operation
   */
  private val allowedResourceTypes: Set[String] =
    fhirConfigurationManager.fhirConfig
      .compartmentRelations.get("Patient")
      .map(_.keySet.diff(PatientEverythingOperationHandler.EXCLUDED_RESOURCES).intersect(fhirConfigurationManager.fhirConfig.resourceConfigurations.keySet)) //Only supported resource types
      .getOrElse(Set.empty)

  /**
   * Handles the everything operation
   * @param operationName Operation name as defined after '$' symbol e.g. meta-add
   * @param operationRequest Operation Request including the parameters
   * @param resourceType The resource type that operation is called if exists
   * @param resourceId The resource id that operation is called if exists
   * @return The response containing the Http status code and the output parameters
   */
  override def executeOperation(operationName: String, operationRequest: FHIROperationRequest, resourceType: Option[String], resourceId: Option[String]): Future[FHIROperationResponse] = {
    //If Patient compartment is not defined, this operation cannot be provided
    if(!fhirConfigurationManager.fhirConfig.compartmentRelations.contains("Patient"))
      throw new MethodNotAllowedException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED,
          None,
          Some(s"The operation 'everything' is not supported as Patient compartment is not configured for this server!"),
          Nil
        )
      ))
    //Parse and validate start parameter
    val start =
      operationRequest
        .extractParamValue[String]("start")
    start.foreach(sd =>
      if (!FhirContentValidator.validatePrimitive(JString(sd), FHIR_DATA_TYPES.DATE))
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"The parameter 'start' should be in FHIR date format!"),
            Seq("Parameters.parameter.where(name ='start')")
          )
        ))
    )
    val end = operationRequest.extractParamValue[String]("end")
    end.foreach(sd =>
      if (!FhirContentValidator.validatePrimitive(JString(sd), FHIR_DATA_TYPES.DATE))
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"The parameter 'end' should be in FHIR date format!"),
            Seq("Parameters.parameter.where(name ='end')")
          )
        ))
    )
    //Parse and validate start parameter
    val since = operationRequest.extractParamValue[String]("_since")
    since.foreach(sd =>
      if (!FhirContentValidator.validatePrimitive(JString(sd), FHIR_DATA_TYPES.INSTANT))
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"The parameter 'since' should be in FHIR instant format!"),
            Seq("Parameters.parameter.where(name ='_since')")
          )
        ))
    )
    //Parse and get count parameter
    val count = operationRequest.extractParamValue[Int]("_count")
    //Get type parameter and validate
    val rtypes = operationRequest.extractParamValues[String]("_type").sorted
    val requestedTypes =
      if(rtypes.nonEmpty){
        val invalidTypes = rtypes.toSet.diff(allowedResourceTypes)
        if(invalidTypes.nonEmpty)
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Invalid resource types (${invalidTypes.mkString(", ")}) in the parameter 'type' should provide resource types that are listed in configured Patient compartment and not listed in exclusion list i.e. ${PatientEverythingOperationHandler.EXCLUDED_RESOURCES.mkString(", " )}!"),
              Seq("Parameters.parameter.where(name ='_type')")
            )
          ))
         rtypes
      } else {
        allowedResourceTypes.toSeq.sorted
      }
    //Get pagination parameters (they are in queryParams as they are not defined for operation)
    val searchAfter =
      operationRequest
        .queryParams.find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER).map(_.valuePrefixList.head._2)
        .map(o => o.split('/') match {
          case Array(rtype, offset) if requestedTypes.contains(rtype) => rtype -> offset
          case _ =>
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"The parameter '${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER}' is invalid format, it should provide an offset for a resource type e.g. Observation/5351533!"),
                Seq(s"Parameters.parameter.where(name ='${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER}')")
              )
            ))
        })
    val searchBefore =
      operationRequest
        .queryParams.find(_.name == FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE).map(_.valuePrefixList.head._2)
        .map(o => o.split('/') match {
          case Array(rtype, offset) if requestedTypes.contains(rtype) => rtype -> offset
          case _ =>
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"The parameter '${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE}' is invalid format, it should provide an offset for a resource type e.g. Observation/5351533!"),
                Seq(s"Parameters.parameter.where(name ='${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE}')")
              )
            ))
        })
    //Based on pagination filter and order the remaining resource types to process
    //e.g. for forward pagination in alphabetic order from the offset resource type
    //e.g. for backward pagination in reverse alphabetic order from the offset resource type
    val rtypesRemaining =
      searchAfter match {
        case Some(otype -> _) => requestedTypes.dropWhile(_ != otype)
        case None =>
          searchBefore match {
            case Some(otype -> _) => requestedTypes.reverse.dropWhile(_ != otype)
            case None => requestedTypes
          }
      }

    getRemainingResources(
          resourceId,
          rtypes = rtypesRemaining,
          start =  start,
          end = end,
          since = since,
          count = count.getOrElse(500), //TODO get from configuration
          offset = searchAfter.map(_._2).orElse(searchBefore.map(_._2)) -> searchBefore.isEmpty
        ).map(searchResult => {
          val bundle = constructBundle(resourceId, if(rtypes.isEmpty) None else Some(rtypes), start, end, since, count, searchAfter.orElse(searchBefore) -> searchBefore.isEmpty, searchResult)
          val operationResponse = new FHIROperationResponse(StatusCodes.OK)
          operationResponse.setResponse(bundle)
          operationResponse
        })
  }

  /**
   * Construct the resulting FHIR Bundle (searchset) for the result of $everything
   * @param pid
   * @param start
   * @param end
   * @param since
   * @param count
   * @param offset
   * @param searchResult
   * @return
   */
  private def constructBundle(pid:Option[String],
                              rtypes:Option[Seq[String]],
                              start:Option[String],
                              end:Option[String],
                              since:Option[String],
                              count:Option[Int],
                              offset:(Option[(String, String)], Boolean),
                              searchResult:FHIRSearchResult):Resource = {
    val rootUrl =
      pid match {
        case Some(pid) => s"${OnfhirConfig.fhirRootUrl}/Patient/$pid/" + "$everything"
        case None => s"${OnfhirConfig.fhirRootUrl}/Patient/" + "$everything"
      }

    val params =
      (rtypes.map(rt => s"_type=${rt.mkString(",")}") ++
        start.map(s => s"start=$s") ++
          end.map(e => s"end=$e") ++
            since.map(s => s"_since=$s") ++
              count.map(c => s"_count=$c")
        ).toSeq

    //Construct paging links
    val selfLink =
      offset match {
        case Some(ot ->o) -> true => s"$rootUrl?${(params :+ s"${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER}=$ot/$o").mkString("&")}"
        case Some(ot -> o) -> false => s"$rootUrl?${(params :+ s"${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE}=$ot/$o").mkString("&")}"
        case None -> _ if params.nonEmpty => s"$rootUrl?${params.mkString("&")}"
        case None -> _ => rootUrl
      }

    val nextLink =
      searchResult.offsetAfter.headOption.map(sa =>
        s"$rootUrl?${(params :+ s"${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER}=${FHIRUtil.extractResourceType(searchResult.matches.last)}/$sa").mkString("&")}"
      )

    val previousLink =
      searchResult.offsetBefore.headOption.map(sb =>
        s"$rootUrl?${(params :+ s"${FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE}=${FHIRUtil.extractResourceType(searchResult.matches.head)}/$sb").mkString("&")}"
      )

    val pagingLinks =
      (FHIR_BUNDLE_FIELDS.SELF_LINK -> selfLink) +:
        (nextLink.map(nl => FHIR_BUNDLE_FIELDS.NEXT_LINK -> nl).toList ++
          previousLink.map(pl => FHIR_BUNDLE_FIELDS.PREVIOUS_LINK -> pl).toList)

    //Create FHIR bundle entries
    val bundleEntries =
      searchResult.matches.map(matchedResource => fhirConfigurationManager.fhirServerUtil.createBundleEntry(matchedResource, FHIR_BUNDLE_TYPES.SEARCH_SET, isSummarized = false))

    //Create and return bundle
   FHIRUtil.createBundle(FHIR_BUNDLE_TYPES.SEARCH_SET, pagingLinks, bundleEntries, searchResult.total, None)
  }

  /**
   * Recursive method to get remaining resources for patient for the given resource types and query and pagination details
   *
   * @param pid    Patient identifier
   * @param rtypes  Resource types to search in alphabetical order
   * @param start  The start of date range relates to care dates
   * @param end    The end of date range relates to care dates
   * @param since  Resources updated after this period will be included in the response.
   * @param count  Number of records to include in the response Bundle for pagination
   * @param offset Search after or before offset (true --> searchafter, false --> searchbefore)
   * @return
   */
  private def getRemainingResources(pid: Option[String],
                                              rtypes: Seq[String],
                                              start: Option[String] = None,
                                              end: Option[String] = None,
                                              since: Option[String] = None,
                                              count: Int = 500,
                                              offset:(Option[String], Boolean)
                                             ): Future[FHIRSearchResult] = {
    //Get resources for the first remaining resource type
    getResourcesForType(pid, rtypes.head, start, end, since, count, offset)
      .flatMap(rs =>
        //If we have all resources for this page or this is the last resource type, return it
        if(count == rs.matches.length || rtypes.length == 1)
          Future.apply(rs)
        //Otherwise continue from the next resource type to return enough resources in this page
        else {
          getRemainingResources(pid, rtypes.tail, start, end, since, count - rs.matches.length, None -> offset._2)
            .map(rs2 =>
              //If we are using normal searchafter pagination (going forward), use the next's resource type's offset before
              if(offset._2)
                rs.copy(matches = rs.matches ++ rs2.matches, offsetBefore = rs.offsetBefore, offsetAfter = rs2.offsetAfter)
              else //Vice versa
                rs.copy(matches = rs2.matches ++ rs.matches, offsetBefore = rs2.offsetBefore, offsetAfter = rs.offsetAfter)
            )
        }
      )
  }

  /**
   * Handle the searching for a given resource type
   *
   * @param pid     Patient identifier if patient specific
   * @param rtype   Resource type to search
   * @param start   The start of date range relates to care dates
   * @param end     The end of date range relates to care dates
   * @param since  Resources updated after this period will be included in the response.
   * @param count  Number of records to include in the response Bundle for pagination
   * @param offset Search after or before offset (true --> searchafter, false --> searchbefore)
   * @return
   */
  private def getResourcesForType(pid: Option[String],
                                     rtype:String,
                                     start: Option[String] = None,
                                     end: Option[String] = None,
                                     since: Option[String] = None,
                                     count: Int = 500,
                                     offset:(Option[String], Boolean)):Future[FHIRSearchResult] = {
    //Construct the compartment param to search resources for that patient
    val compartmentParam =
      pid.map(pid =>
        fhirConfigurationManager.fhirSearchParameterValueParser
          .constructCompartmentSearchParameter("Patient", pid, rtype)
      )

    //If date range is given, find the related parameter name and construct the search statements
    val clinicalDateParams =
      if (start.isDefined || end.isDefined) {
        PatientEverythingOperationHandler.getDateParams(fhirConfigurationManager.fhirConfig.fhirVersion)
          .find(p => p._2.contains(rtype))
          .map(_._1)
          .map(p =>
            if(fhirConfigurationManager.fhirConfig.resourceQueryParameters(rtype).contains(p))
              start.map(sd => Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.DATE, p, Seq("ge" -> sd))).toSeq ++
                end.map(ed => Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.DATE, p, Seq("le" -> ed))).toSeq
            else
              Nil
          )
          .getOrElse(Nil) //No filter if resource is not filtered by dates
      } else Nil
    //If since is given, filter resources updated after that given time
    val lastUpdatedParam =
      since.map(s =>
        Parameter(FHIR_PARAMETER_CATEGORIES.NORMAL, FHIR_PARAMETER_TYPES.DATE, "_lastUpdated", Seq("gt" -> s))
      )
    //Pagination params
    val paginationParams =
      Seq(
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, FHIR_PARAMETER_TYPES.TOKEN, FHIR_SEARCH_RESULT_PARAMETERS.TOTAL, Seq("" -> s"none")),
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, FHIR_PARAMETER_TYPES.NUMBER, FHIR_SEARCH_RESULT_PARAMETERS.COUNT, Seq("" -> s"$count")),
        Parameter(FHIR_PARAMETER_CATEGORIES.RESULT, FHIR_PARAMETER_TYPES.NUMBER, if(offset._2) FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_AFTER else FHIR_SEARCH_RESULT_PARAMETERS.SEARCH_BEFORE, Seq("" -> s"${offset._1.getOrElse("")}")),
      )
    val allParams = compartmentParam ++ clinicalDateParams ++ lastUpdatedParam.toSeq ++ paginationParams

    fhirConfigurationManager.resourceManager.searchResources(rtype, allParams.toList)
  }
}

object PatientEverythingOperationHandler {
  //Excluded resources from Patient compartment not to return within this operation
  final val EXCLUDED_RESOURCES =
    Set("AuditEvent", "Group", "Provenance")

  final val R5_DATE_PARAMS =
    Seq(
    "period" -> Set("Account"),
    "created" -> Set("Basic", "Claim", "ClaimResponse", "ExplanationOfBenefit"),
    "entered-date" -> Set("ChargeItem"),
    "sent" -> Set("Communication"),
    "occurrence" -> Set("CommunicationRequest", "ServiceRequest"),
    "onset-date" -> Set("Condition"),
    "issued" -> Set("Contract", "ImagingSelection"),
    "identified" -> Set("DetectedIssue"),
    "timing" -> Set("DeviceAlert"),
    "event-date" -> Set("DeviceRequest"),
    "target-date" -> Set("Goal"),
    "started" -> Set("ImagingStudy"),
    "recorded" -> Set("MedicationDispense"),
    "authoredon" -> Set("MedicationRequest"),
    "effective" -> Set("MedicationStatement"),
    "datetime" -> Set("NutritionOrder"),
    "authored" -> Set("QuestionnaireResponse"),
    "collected" -> Set("Specimen"),
    "period" -> Set("Task"),
    "datewritten" -> Set("VisionPrescription"),
    "date" -> Set(
      "AdverseEvent",
      "AllergyIntolerance",
      "Appointment",
      "CarePlan",
      "CareTeam",
      "ClinicalImpression",
      "Composition",
      "Consent",
      "DiagnosticReport",
      "DocumentReference",
      "Encounter",
      "EpisodeOfCare",
      "FamilyMemberHistory",
      "Flag",
      "Immunization",
      "ImmunizationEvaluation",
      "ImmunizationRecommendation",
      "Invoice",
      "List",
      "MeasureReport",
      "MedicationAdministration",
      "NutritionIntake",
      "Observation",
      "Procedure",
      "ResearchSubject",
      "RiskAssessment",
      "Schedule",
      "SupplyRequest"
    )
  )

  final val R4_DATE_PARAMS:Seq[(String, Set[String])] =
    Seq(
      "period" -> Set("Account"),
      "created" -> Set("Basic", "Claim", "ClaimResponse", "ExplanationOfBenefit"),
      "entered-date" -> Set("ChargeItem"),
      "sent" -> Set("Communication"),
      "occurrence" -> Set("CommunicationRequest", "ServiceRequest"),
      "onset-date" -> Set("Condition"),
      "issued" -> Set("Contract", "ImagingSelection"),
      "identified" -> Set("DetectedIssue"),
      "timing" -> Set("DeviceAlert"),
      "event-date" -> Set("DeviceRequest"),
      "target-date" -> Set("Goal"),
      "started" -> Set("ImagingStudy"),
      "whenhandedover" -> Set("MedicationDispense"),
      "authoredon" -> Set("MedicationRequest"),
      "effective" -> Set("MedicationStatement"),
      "datetime" -> Set("NutritionOrder"),
      "authored" -> Set("QuestionnaireResponse"),
      "collected" -> Set("Specimen"),
      "period" -> Set("Task"),
      "datewritten" -> Set("VisionPrescription"),
      "effective-time" -> Set("MedicationAdministration"),
      "date" -> Set(
        "AdverseEvent",
        "AllergyIntolerance",
        "Appointment",
        "CarePlan",
        "CareTeam",
        "ClinicalImpression",
        "Composition",
        "Consent",
        "DiagnosticReport",
        "DocumentReference",
        "Encounter",
        "EpisodeOfCare",
        "FamilyMemberHistory",
        "Flag",
        "Immunization",
        "ImmunizationEvaluation",
        "ImmunizationRecommendation",
        "Invoice",
        "List",
        "MeasureReport",
        "NutritionIntake",
        "Observation",
        "Procedure",
        "ResearchSubject",
        "RiskAssessment",
        "Schedule",
        "SupplyRequest"
      )
    )

  /**
   * Search parameter names related to Patient compartment FHIR Resource types that search over clinically related date
   * R5 and R4
   */
  def getDateParams(version: String):Seq[(String, Set[String])] = version match {
    case v if v.startsWith("R5") || v.startsWith("5.") => R5_DATE_PARAMS
    case v if v.startsWith("R4") || v.startsWith("4.") => R4_DATE_PARAMS
    case _ => R5_DATE_PARAMS
  }

}
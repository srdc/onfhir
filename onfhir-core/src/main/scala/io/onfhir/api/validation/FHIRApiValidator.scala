package io.onfhir.api.validation

import java.net.{URI, URL}

import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.StatusCodes.ClientError
import akka.http.scaladsl.model.headers.{EntityTag, `If-Modified-Since`, `If-None-Match`}
import io.onfhir.api._
import io.onfhir.api.model.{FHIRRequest, FHIRResponse, OutcomeIssue}
import io.onfhir.api.service.FHIRSubscriptionBusinessValidator
import io.onfhir.api.util.FHIRUtil
import io.onfhir.exception._
import io.onfhir.util.JsonFormatter._
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import org.json4s.{JDouble, JInt, JString}
import org.json4s.JsonAST._
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try
import scala.util.matching.Regex

/**
  * FHIR REST API validator
  */
object FHIRApiValidator {
  private val logger:Logger = LoggerFactory.getLogger(this.getClass)
  //Regular expression for Resource identifier
  private val ID_REGEX:Regex = """\A[A-Za-z0-9\-\.]{1,64}$""".r
  private val CODE_REGEX:Regex = """\A[^\s]+([\s]?[^\s]+)*$""".r
  private val DATE_REGEX:Regex = """\A-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1]))?)?$""".r
  private val DATETIME_REGEX:Regex = """\A-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)))?)?)?$""".r
  private val INSTANT_REGEX:Regex ="""\A([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))$""".r
  private val TIME_REGEX:Regex = """\A([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?$""".r
  private val OID_REGEX :Regex= """\Aurn:oid:[0-2](\.[1-9]\d*)+$""".r
  private val UUID_REGEX:Regex = """\Aurn:uuid:[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$""".r
  private val VERSION_REGEX:Regex = """\A[0-9]+(\.[0-9]+)*$""".r

  //Extra business rule validator for some resources
  //TODO make this dynamic and extendible (via configuration or modules)
  val extraRulesForResources = Map("Subscription" -> new FHIRSubscriptionBusinessValidator)
  /**
    * Validates FHIR identifiers against FHIR specifications
    * @param id id to be validated
    */
  def validateId(id: String):Unit = {
    if(ID_REGEX.findFirstMatchIn(id).isEmpty)
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //error
          FHIRResponse.OUTCOME_CODES.INVALID, //invalid
          None,
          Some("Invalid identifier: id field can be formed by " +
            "any combination of upper or lower case ASCII letters ('A'..'Z', and 'a'..'z', numerals ('0'..'9'), " +
            "'-' and '.', with a length limit of 64 characters."),
          Seq(".id")
        )
      ))
  }


  /**
    * Checks the existence and matching of the "id" field of the resource against the id in the request URL
    * Returns Nil if everything is OK, otherwise throws exception
    * @param resource resource in the request
    * @param _id id in the request URL
    */
  def validateResourceId(resource:Resource, _id:String):Seq[OutcomeIssue] = {
    //validate the id
    validateId(_id)
    val id = (resource \ FHIR_COMMON_FIELDS.ID).extractOpt[String]
    if(id.isEmpty) {
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //error
          FHIRResponse.OUTCOME_CODES.INVALID, //invalid
          None,
          Some("Missing 'id' field in given resource"),
          Seq(".id")
        )
      ))
    }

    id.get match {
      case s:String =>
        if(!s.equals(_id))
          throw new BadRequestException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR, //error
              FHIRResponse.OUTCOME_CODES.INVALID, //invalid
              None,
              Some(s"id in request URL (${_id}) does not match " +
                s"with the id field in request ($id)"),
              Seq(".id")
            )
          ))
      case _ =>
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //error
            FHIRResponse.OUTCOME_CODES.INVALID, //invalid
            None,
            Some(s"id field must have String type"),
            Seq(".id")
          )
        ))
    }
    Nil
  }

  /**
    * Checks the "non"-existence of the id field in a resource
    * @param resource  Resource to be checked
    * @return
    */
  def validateNonExistenceOfResourceId(resource:Resource):Unit = {
    val id = (resource \ FHIR_COMMON_FIELDS.ID).extractOpt[String]
    if(id.isDefined) {
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //error
          FHIRResponse.OUTCOME_CODES.INVALID, //invalid
          None,
          Some("'id' field of resources should not be provided in 'create' operations"),
          Seq(".id")
        )
      ))
    }
  }

  /**
    * Checks if resource type indicated in the URL is matching to the resource content given in the Body
    * @param resource Resource content
    * @param _rtype Resource type given in the URL
    */
  def validateResourceTypeMatching(resource:Resource, _rtype:String): Unit ={
    val rtype = (resource \ FHIR_COMMON_FIELDS.RESOURCE_TYPE).extractOpt[String]
    if(rtype.isEmpty) {
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.FATAL, //error
          FHIRResponse.OUTCOME_CODES.INVALID, //invalid
          None,
          Some("Missing resourceType field for given resource"),
          Seq(".resourceType")
        )
      ))
    }
    if(!rtype.get.toString.equals(_rtype)){
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //error
          FHIRResponse.OUTCOME_CODES.INVALID, //invalid
          None,
          Some("type in request URL (" + _rtype + ") does not match " +
            "with the resourceType field in request (" + rtype + ")"),
          Seq(".resourceType")
        )
      ))
    }
  }

  /**
    * Checks the existence and matching of the "resourceType" field of the resource against the resource
    * type in the request URL
    *
    * @param resource resource in the request
    * @param _rtype resource type in the request URL
    */
  def validateResourceType(resource:Resource, _rtype:String):Seq[OutcomeIssue] = {
    validateResourceTypeMatching(resource, _rtype)

    //Base profile for resource
    val profile = fhirConfig.resourceConfigurations.get(_rtype).flatMap(_.profile)
    //All supported profiles for resource
    val supportedProfiles = fhirConfig.supportedProfiles.getOrElse(_rtype, Set.empty)
    //If a base profile is defined for resource, then we expect resource contains some profile in meta (base profile or sub profiles)
    if(profile.isDefined && !profile.get.startsWith(FHIR_ROOT_URL_FOR_DEFINITIONS)){
      val resourceProfiles = FHIRUtil.extractProfilesFromBson(resource)
      if(resourceProfiles.intersect(supportedProfiles).isEmpty)
        throw new BadRequestException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //error
            FHIRResponse.OUTCOME_CODES.PROCESSING, //processing
            None,
            if(resourceProfiles.isEmpty)
              Some(s"Profile element does not exist. Resource should conform to one of the supported profiles ($supportedProfiles) for resource type ${_rtype}!!! ")
            else
              Some(s"Indicated profiles ($resourceProfiles) does not match with supported profiles ($supportedProfiles) for resource type ${_rtype}!!! ")
            ,
            Seq(".meta.profile")
          )
        ))
    }
    Nil
  }

  /**
    * Validate versioned update
    * @param rtype
    * @param ifmatch
    */
  def validateVersionedUpdate(rtype:String, ifmatch:Option[String]):Unit = {
    //If versioned update is forced and ifMatch header is empty
    if(fhirConfig.resourceConfigurations.apply(rtype).versioning == FHIR_VERSIONING_OPTIONS.VERSIONED_UPDATE && ifmatch.isEmpty)
      throw new BadRequestException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID, //not supported
          None,
          Some(s"Client error, only versioned updates are supported for resource type $rtype! Please use If-Match header to perform version aware updates!"),
          Seq("HTTP Header[If-Match]")
        )
      ))
  }

  /**
    * Check if given FHIR system level interaction is supported or not
    * @param operation Name of the interaction
    */
  def validateSystemLevelInteraction(operation:String):Unit = {
    if(!fhirConfig.supportedInteractions.contains(operation))
      throw new NotFoundException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.FATAL, //fatal
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"System level interaction $operation is not supported !!! Please check the conformance statement of the system..."),
          Nil
        )
      ))
  }


  /**
    * Validate if the FHIR operation is allowed on ResourceType by the Conformance statement
    * Return no issue (Nil) if no error, throw exception if error
    * @param operation Interaction code (create, update, delete, etc)
    * @param rtype Resource Type
    * @param conditional If this interaction is conditional request (conditional create, update or delete)
    */
  def validateInteractionOnResourceType(operation:String,  rtype:String, conditional:Boolean = false):Seq[OutcomeIssue]  = {
    val profileConfiguration = fhirConfig.resourceConfigurations.get(rtype)
    if(profileConfiguration.isEmpty){
      throw new NotFoundException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.FATAL, //fatal
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"Resource type '$rtype' is not supported!"),
          Seq(".resourceType")
        )
      ))
    }

    if(!profileConfiguration.get.interactions.contains(operation))
      throw new NotFoundException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.FATAL, //fatal
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"Interaction $operation is not supported for resource type $rtype"),
          Seq(".resourceType")
        )
      ))

    if(conditional){
      operation match {
        case FHIR_INTERACTIONS.CREATE =>
          if(!profileConfiguration.get.conditionalCreate)
            throw new PreconditionFailedException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.FATAL, //fatal
                FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
                None,
                Some(s"Conditional $operation is not supported for resource type $rtype! Please check the conformance statement of the server..."),
                Nil
              )
            ))
        case FHIR_INTERACTIONS.UPDATE | FHIR_INTERACTIONS.PATCH =>
          if(!profileConfiguration.get.conditionalUpdate)
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR, //fatal
                FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
                None,
                Some(s"Conditional $operation is not supported for resource type $rtype! Please check the conformance statement of the server..."),
                Nil
              )
            ))
        case FHIR_INTERACTIONS.DELETE =>
          if(profileConfiguration.get.conditionalDelete.equals("not-supported"))
            throw new BadRequestException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR, //fatal
                FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
                None,
                Some(s"Conditional $operation is not supported for resource type $rtype! Please check the conformance statement of the server..."),
                Nil
              )
            ))

      }
    }
    Nil
  }

  /**
    * Validate if update operation is allowed to create a new resource
    */
  def validateUpdateCreate(rtype:String):Unit = {
    if(!fhirConfig.resourceConfigurations(rtype).updateCreate)
      throw new MethodNotAllowedException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"Clients are not allowed to create new resources with update operation!!! Please check the conformance statement..."),
          Nil
        )
      ))
  }

  /*
  /**
    * Checks whether given parameters are supported by the server
    *
    * @param parameters parameters to be validated
    */
def validateSearchParameters(_type:String, parameters:Set[String], preferHeader:Option[String]) = {
    val invalidParameters: ListBuffer[String] = ListBuffer()
    val prefer = preferHeader.getOrElse(Config.fhirSearchHandling)
    if(parameters != Set("") && parameters.nonEmpty) {
      parameters foreach { parameter =>
        if (parameter == "") {
          // In case of ?=.. definitons(e.g. ....power2dm.eu?=somevalue or ...power2dm.eu?)
          throw new UnprocessableEntityException(Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.FATAL,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some("Invalid empty parameter in search query"),
              Nil
            )
          ))
        }

        // Check if the parameter is supported by the server
        val isModelParameter = FhirConfig.resourceQueryParameters(_type).head._2.contains(parameter)
        val isCommonParameter = FhirConfig.commonQueryParameters.contains(parameter)
        val isResultParameter = FhirConfig.FHIR_RESULT_PARAMETERS.contains(parameter)
        if (!parameter.startsWith(COMPARTMENT_PARAMETER) && !isModelParameter && !isCommonParameter && !isResultParameter) {
          if (prefer.contains(FHIR_HTTP_OPTIONS.FHIR_SEARCH_STRICT))
            throw new NotImplementedException(Seq(
              OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR,
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Search operation doesn't support parameter ($parameter) for resource (${_type})!!!"),
                Nil
              )
            ))
          invalidParameters.append(parameter)
        }
      }
    }
    invalidParameters
  }*/


  /**
    * Checks the value in ifMatch Option matches with the given current version of a resource
    *
    * @param ifMatch The version (as Option) sent by the client
    * @param currentVersion Current version of a resource to be checked agains
    */
  def validateIfMatch(ifMatch:Option[String], currentVersion:Long):Unit =  {
    if(ifMatch.isDefined){
      val version = ifMatch.get
      if(version.equals(EntityTag(currentVersion.toString, weak = false).toString) ||
         version.equals(EntityTag(currentVersion.toString, weak = true).toString())){
        //allow user to proceed
      }
      //if requested version does not match with the current version send 409 Conflict
      else {
        logger.debug("conflicting version, returning 409 Conflict...")
        throw new ConflictException(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.FATAL, //fatal
            FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
            None,
            Some(s"Conflicting Version: The version ($version) in If-Match header" +
            s" is not the current version ($currentVersion) for the given resource"),
            Seq("Header[If-Match]")
          )
        )
      }
    }
  }


  /**
    * Checks the value in ifNoneMatch Option matches with the given current version of a resource
    *
    * @param ifNoneMatch The version (as Option) sent by the client
    * @param currentVersion Current version of a resource to be checked agains
    */
  def validateIfNoneMatch(ifNoneMatch:Option[`If-None-Match`], currentVersion:Long):Unit = {
    if(ifNoneMatch.isDefined){
      val version = ifNoneMatch.get.m.toString
      if(version.equals(EntityTag(currentVersion.toString, weak = false).toString) ||
         version.equals(EntityTag(currentVersion.toString, weak = true).toString())){
        throw new NotModifiedException()
      }
    }
  }

  /**
    * Checks the value in ifModifiedSince Option is after given time or not
    *
    * @param ifModifiedSince The version (as Option) sent by the client
    * @param lastUpdated Current version of a resource to be checked against
    */
  def validateIfModifiedSince(ifModifiedSince:Option[`If-Modified-Since`], lastUpdated:DateTime):Unit = {
    if(ifModifiedSince.isDefined){
      val since = ifModifiedSince.get.date.compare(lastUpdated)
      if(since > 0) {
        throw new NotModifiedException()
      }
    }
  }

  /**
    * Checks the compartment type based on the resource type to ensure the
    * provided query is valid.
    *
    * @param compartmentType type of the compartment
    * @param _type type of the resource
    */
  def validateCompartment(compartmentType:String, _type:String):Unit = {
    if(fhirConfig.compartmentRelations.isDefinedAt(compartmentType)) {
      if(!fhirConfig.compartmentRelations(compartmentType).isDefinedAt(_type)) {
        throw new NotFoundException(Seq(
          OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.FATAL, //fatal
            FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
            None,
            Some(s"Querying on resource type ${_type} is not supported for compartment $compartmentType !!! Please check the compartment definition..."),
            Nil
          )
        ))

      }
    } else
      throw new NotFoundException(Seq(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.FATAL, //fatal
          FHIRResponse.OUTCOME_CODES.NOT_SUPPORTED, //not supported
          None,
          Some(s"Compartment type '$compartmentType' is not supported!!! Please check the conformance statement of the system..."),
          Nil
        )
      ))
  }

  /**
    * Interdependency validation of resources at batch request. There couldn't
    * be any interdependency between resources.
    *
    * @param entry Sindle batch entry
    * @param listOfUrls List of entry urls
    */
  def validateInterdependencies(entry: Resource, listOfUrls: Seq[String]):Unit = {
    val entryString = entry.toJson
    listOfUrls foreach { eachUrl =>
      if (entryString.contains(eachUrl))
        throw new PreconditionFailedException(
          Seq(
            OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR,
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Interdependency to batch entry with url $eachUrl."),
              Nil
            )
          )
        )
    }
  }

  /**
    * Validate FHIR Primitive Data Types
    * @param value Json value
    * @param ptype FHIR type expected
    * @return
    */
  def validatePrimitive(value:JValue, ptype:String):Boolean = {
    ptype match {
      case FHIR_DATA_TYPES.STRING => true //OK
      case FHIR_DATA_TYPES.INTEGER =>  value.isInstanceOf[JInt]
      case FHIR_DATA_TYPES.URI => Try(new URI(value.extract[String])).isSuccess
      case FHIR_DATA_TYPES.URL => Try(new URL(value.extract[String])).isSuccess
      case FHIR_DATA_TYPES.CANONICAL =>  value.extract[String].split('|') match {
        case Array(url)=>  Try(new URI(url)).isSuccess
        case Array(url, version) =>
          Try(new URI(url)).isSuccess &&  VERSION_REGEX.findFirstMatchIn(version).isDefined
        case _ => false
      }
      case FHIR_DATA_TYPES.BOOLEAN => value.isInstanceOf[JBool]
      case FHIR_DATA_TYPES.DECIMAL => Try(value.extract[Double]).isSuccess
      case FHIR_DATA_TYPES.CODE => CODE_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.DATE => DATE_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.DATETIME => DATETIME_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.INSTANT => INSTANT_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.TIME => TIME_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.ID => ID_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.OID => OID_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.UUID => UUID_REGEX.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.UNSIGNEDINT => value.isInstanceOf[JInt] && value.extract[Int].intValue() >= 0 //"""[0]|([1-9][0-9]*)$""".r.findFirstMatchIn(value.extract[String]).isDefined
      case FHIR_DATA_TYPES.POSITIVEINT => value.isInstanceOf[JInt] && value.extract[Int].intValue() >= 1 //"""+?[1-9][0-9]*$""".r.findFirstMatchIn(value).isDefined
      case FHIR_DATA_TYPES.BASE64BINARY => true
      case _ => true
    }
  }

  /**
    * Parse and validate FHIR primitive type
    * @param value Json value
    * @param ptype FHIR type expected
    * @return
    */
  def parseAndValidatePrimitive(value:String, ptype:String):Option[JValue] = {
    ptype match {
      case FHIR_DATA_TYPES.STRING => Some(JString(value)) //OK
      case FHIR_DATA_TYPES.INTEGER => Try(value.toInt).toOption.map(JInt(_))
      case FHIR_DATA_TYPES.URI => Try(new URI(value)).toOption.map(_ => JString(value))
      case FHIR_DATA_TYPES.URL =>   Try(new URL(value)).toOption.map(u => JString(u.toString))
      case FHIR_DATA_TYPES.CANONICAL => value.split('|') match {
        case Array(url)=>  Try(new URI(url)).toOption.map(_ => JString(value))
        case Array(url, version) =>
            if(Try(new URI(url)).isSuccess &&  VERSION_REGEX.findFirstMatchIn(version).isDefined)
              Some(JString(value))
            else
              None
        case _ => None
      }
      case FHIR_DATA_TYPES.BOOLEAN => Try(value.toBoolean).toOption.map(JBool(_))
      case FHIR_DATA_TYPES.DECIMAL => Try(value.toDouble).toOption.map(JDouble)
      case FHIR_DATA_TYPES.CODE => CODE_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.DATE => DATE_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.DATETIME =>DATETIME_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.INSTANT =>INSTANT_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.TIME => TIME_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.ID => ID_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.OID  => OID_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.UUID => UUID_REGEX.findFirstMatchIn(value).map(_=>JString(value))
      case FHIR_DATA_TYPES.UNSIGNEDINT => Try(value.toInt).toOption.filter(_ > 0).map(JInt(_))
      case FHIR_DATA_TYPES.POSITIVEINT=> Try(value.toInt).toOption.filter(_ >= 1).map(JInt(_))
      case FHIR_DATA_TYPES.BASE64BINARY => Some(JString(value))
      case _ => Some(JString(value))
    }
  }

  /**
    * Validate FHIR Complex Data Types
    * @param value Json Value
    * @param ptype FHIR type expected
    * @return
    */
  def validateComplex(value:JValue, ptype:String):Boolean= {
    //TODO Validate the schema
    value.isInstanceOf[JObject]
  }

  /**
   * Validate extra business rules for FHIR create, update, patch and delete interactions on resource type
   * @param fhirRequest
   */
  def validateExtraRules(fhirRequest: FHIRRequest):Unit = {
    extraRulesForResources.get(fhirRequest.resourceType.get).foreach(_.validateRequest(fhirRequest))
  }

  /**
   * Validate rules on content change if exist, if they are allowed (FHIR update, patch)
   * @param rtype       Resource type
   * @param oldContent  Old resource content
   * @param newContent  New resource content to update
   */
  def validateContentChanges(rtype:String, oldContent:Resource, newContent:Resource):Unit = {
    extraRulesForResources.get(rtype).foreach(_.validateChanges(oldContent, newContent))
  }


}

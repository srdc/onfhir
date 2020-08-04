package io.onfhir.api.service

import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import io.onfhir.api._
import io.onfhir.api.model._
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.FhirConfigurationManager.fhirValidator
import io.onfhir.config.{OperationConf, OperationParamDef}
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.{BadRequestException, InternalServerException, NotFoundException}
import io.onfhir.util.JsonFormatter._
import org.json4s.{JArray, JNull}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
  * Created by tuncay on 10/3/2017.
  */
class FHIROperationHandler(transactionSession: Option[TransactionSession] = None) extends FHIRInteractionService(transactionSession) {
  /**
    * Validate if operation is supported for the given resource type/id, etc
    * @param fhirRequest FHIRRequest object
    */
  override def validateInteraction(fhirRequest: FHIRRequest): Future[Unit] = {
    Future.apply {
      val operation = fhirRequest.interaction
      val operationConf = fhirConfig.supportedOperations.find(_.name == operation.drop(1))
      if (operationConf.isEmpty)
        throw new BadRequestException(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Operation $operation not supported!"),
          Nil)))

      if (fhirRequest.resourceType.isEmpty && fhirRequest.resourceId.isEmpty && !operationConf.get.levels.contains("system"))
        throw new BadRequestException(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Operation $operation is not a system level operation, please see the Operation definition; levels supported; ${operationConf.get.levels}!"),
          Nil)))

      if (fhirRequest.resourceType.isDefined) {
        if (operationConf.get.resources.nonEmpty && !operationConf.get.resources.contains("Resource") && !operationConf.get.resources.contains(fhirRequest.resourceType.get))
          throw new BadRequestException(Seq(OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Operation $operation is not supported for resource type; ${fhirRequest.resourceType.get}!"),
            Nil)))

        if (fhirRequest.resourceId.isEmpty && !operationConf.get.levels.contains("type"))
          throw new BadRequestException(Seq(OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Operation $operation is not a type level operation, please see the Operation definition; levels supported; ${operationConf.get.levels}!"),
            Nil)))

        if (fhirRequest.resourceId.isDefined && !operationConf.get.levels.contains("instance")) {
          throw new BadRequestException(Seq(OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Operation $operation is not a instance level operation, please see the Operation definition; levels supported; ${operationConf.get.levels}!"),
            Nil)))
        }
      }
    }
  }

  /**
    * Perform the interaction
    * @param fhirRequest  FHIR Request
    * @param authzContext Authorization context if needed (only for transaction and batch)
    * @param isTesting    If we are only testing the interaction or realy perform it
    */
  override def completeInteraction(fhirRequest: FHIRRequest, authzContext: Option[AuthzContext], isTesting: Boolean): Future[FHIRResponse] = {
    val operation = fhirRequest.interaction
    //Find operation configuration
    val operationConf = fhirConfig.supportedOperations.find(_.name == operation.drop(1)).get
    //Validate and complete operation
    validateAndCompleteOperation(fhirRequest, operationConf)
  }

  /**
    * Validate if the Operation Request body is Ok, in general
    * @param fhirRequest FHIR Request
    */
  private def validateRequestBody(fhirRequest: FHIRRequest):Future[Unit] = {
    if(fhirRequest.resource.exists(body => FHIRUtil.extractValueOption[String](body, FHIR_COMMON_FIELDS.RESOURCE_TYPE).contains("Parameters")))
      fhirValidator.validateResource(fhirRequest.resource.get, "Parameters").map(_ => Unit)
    else
      Future.apply(Unit)
  }


  /**
   * Return the FHIR Parameter object (within FHIR Parameters Resource) given with the name from Parameters resource
   * @param name
   * @return
   */
  private def getOperationParametersByName(parametersResource:Resource, name:String):Seq[Resource] = {
    parametersResource \ FHIR_COMMON_FIELDS.PARAMETER match {
      case JArray(values) => values.filter(p => (p \ FHIR_COMMON_FIELDS.NAME).extract[String] == name).map(_.asInstanceOf[JObject])
      case _ => Nil
    }
  }

  /**
   * Return the parameter value from the parameter object (BackboneElement in Parameters definition) given parameter type as JValue
   * @param parameter      Parameter obj (Parameters.parameter or Parameters.parameter.part ...)
   * @param paramDef
   * @return
   */
  private def getOperationParameterValueByDef(parameter:Resource, paramDef:OperationParamDef):Option[FHIROperationParam] = {

    paramDef.pType match {
      case None =>
        (parameter \ "part") match {
          case JArray(arr) =>
            val valueMap =
              arr.map(cp => (cp \ "name").extract[String] -> cp)
                .map(cp =>
                  cp._1 ->
                    (
                      paramDef.parts.find(_.name == cp._1) match {
                        case None => None
                        case Some(cpDef) => getOperationParameterValueByDef(cp._2.asInstanceOf[JObject], cpDef)
                      }
                      )
                ).filter(_._2.isDefined).map(cp => cp._1 -> cp._2.get)
            if(valueMap.isEmpty)
              None
            else
              Some(FHIRMultiOperationParam(valueMap))
          case _ => None
        }
      case Some(dt) if fhirConfig.FHIR_PRIMITIVE_TYPES.contains(dt) || fhirConfig.FHIR_COMPLEX_TYPES.contains(dt) =>
        parameter \ s"value${dt.capitalize}" match {
          case JNothing | JNull => None
          case oth => Some(FHIRSimpleOperationParam(oth))
        }
      //Resource types
      case  Some(_) =>
        parameter \ FHIR_COMMON_FIELDS.RESOURCE match {
          case obj:JObject => Some(FHIRSimpleOperationParam(obj))
          case _ => None
        }
    }
  }

  /**
   * Return the value of parameter from the Parameters resource given name and type of parameter
   * @param parametersResource Parameter obj (Parameters.parameter or Parameters.parameter.part ...)
   * @param parameterDef       Definition for Operation parameter
   * @return
   */
  def getOperationParameterValue(parametersResource:Resource, parameterDef:OperationParamDef): Seq[FHIROperationParam] = {
    getOperationParametersByName(parametersResource, parameterDef.name)
      .flatMap(getOperationParameterValueByDef(_, parameterDef))
  }

  /**
    * Extract parameter value from the OperationRequest
    * @param operationConf  Operation definition
    * @param paramDef       Operation parameter definition
    * @param requestBody    Request body for operation
    * @return
    */
  private def validateAndGetParamFromBody(operationConf: OperationConf, paramDef:OperationParamDef, requestBody:Option[Resource]):Future[Either[Seq[FHIROperationParam], Seq[OutcomeIssue]]] = {
    //If the body is empty (empty Parameters resource or completely empty)
    if (requestBody.isEmpty || requestBody.get.obj.size < 2)
      if (paramDef.min > 0) //If parameter is required
        Future.apply(Right(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Parameter '${paramDef.name}' not found, although it is required for the operation '${operationConf.name}'!"),
          Nil))))
      else Future.apply(Right(Nil))
    else
      FHIRUtil.extractValueOption[String](requestBody.get, FHIR_COMMON_FIELDS.RESOURCE_TYPE) match {
        //If the Request Body is given by Parameters resource. See https://www.hl7.org/fhir/operations.html.
        case Some(FHIR_DATA_TYPES.PARAMETERS) =>
          //Retrieve the parameter values
          val paramValue: Seq[FHIROperationParam] = getOperationParameterValue(requestBody.get, paramDef)
          validateOperationParameter(operationConf.name, paramValue, paramDef) map (issues =>
            if(issues.exists(_.isError))
              Right(issues)
            else
              Left(paramValue)
          )
        //If body is the whole resource and it matches the expected Resource Type, or expected type is a resource
        case Some(resourceType) if paramDef.pType.contains(resourceType) || paramDef.pType.contains(FHIR_DATA_TYPES.RESOURCE) =>
            var resource = requestBody.get
            if(paramDef.pProfile.nonEmpty)
              resource =  paramDef.pProfile.foldLeft(resource)((r, p) => FHIRUtil.setProfile(r, p))
            fhirValidator
              .validateResource(resource, paramDef.pType.get, silent = true) // By setting silent, this does not throw exception but return issues
              .map(issues => {
                  if(!issues.exists(i => i.isError))
                    Left(Seq(FHIRSimpleOperationParam(requestBody.get)))
                  else
                    Right(issues) //Return the issues or the parameter value
              })
        case _ =>
          Future.apply(
            if (paramDef.min > 0) //If parameter is required
              Right(Seq(OutcomeIssue(
                FHIRResponse.SEVERITY_CODES.ERROR, //fatal
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Parameter ${paramDef.name} not found in request body (Parameters), although it is required for the operation ${operationConf.name}!"),
                Nil)))
            else Right(Nil)
          )
          Future.apply(Right(Nil)) //No such case
      }
  }

  /**
   * Validate if a Operation parameter value conforms to the definition given in OperationDefinition for the param
   * @param opName
   * @param paramValue
   * @param paramDef
   * @return
   */
  private def validateOperationParameter(opName:String, paramValue:Seq[FHIROperationParam], paramDef:OperationParamDef):Future[Seq[OutcomeIssue]] = {
    if (paramValue.isEmpty) //If parameter is missing
      if (paramDef.min > 0) //If parameter is required return the issue
        Future.apply(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Parameter ${paramDef.name} not found in request body (Parameters), although it is required for the operation ${opName}!"),
          Nil)))
      else Future.apply(Nil)
    else {
      //Check if max cardinality matches
      if (paramDef.max != "*" && paramValue.size > paramDef.max.toInt)
        Future.apply(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Cardinality of Parameter ${paramDef.name} (${paramValue.size}) does not match with the required cardinality (${paramDef.max}) for the operation ${opName}!"),
          Nil)))
      else {
        val fissues: Future[Seq[OutcomeIssue]] =
          paramDef.pType match {
            //Paremeter is primitive FHIR type e.g. string, int, etc
            case Some(primitive) if fhirConfig.FHIR_PRIMITIVE_TYPES.contains(primitive) =>
              Future.apply(paramValue.flatMap(pv => validatePrimitiveForOperations(paramDef.name, pv.asInstanceOf[FHIRSimpleOperationParam].value, primitive)))
            //Paremeter is complex FHIR type e.g. CodeableConcept
            case Some(complex) if fhirConfig.FHIR_COMPLEX_TYPES.contains(complex) =>
              Future.apply(paramValue.flatMap(pv => validateComplexForOperations(paramDef.name, pv.asInstanceOf[FHIRSimpleOperationParam].value, complex)))

            //Parameter is multi parameter
            case None =>
              val childIssuesFuture =
                Future.sequence(
                  paramValue.map(pv => {
                    Future.sequence(paramDef.parts.map(childParamDef =>
                      validateOperationParameter(
                        opName,
                        pv.asInstanceOf[FHIRMultiOperationParam].getParams(childParamDef.name),
                        childParamDef
                      )
                    ))
                  }))

              childIssuesFuture.map(childIssues => {
               childIssues.flatten.flatten
              })

            //Parameter is Resource type
            case _ =>
              //Check if it is a JObject (json resource)
              if(!paramValue.forall(pv => pv.asInstanceOf[FHIRSimpleOperationParam].value.isInstanceOf[JObject]))
                Future.apply(Seq(OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.ERROR, //fatal
                  FHIRResponse.OUTCOME_CODES.INVALID,
                  None,
                  Some(s"Parameter '${paramDef.name}' is not a valid FHIR Resource!"),
                  Nil)))
              else  {
                var resources = paramValue.map(pv => pv.asInstanceOf[FHIRSimpleOperationParam].value.asInstanceOf[Resource])
                if (paramDef.pProfile.nonEmpty)
                  resources = resources.map(r => paramDef.pProfile.foldLeft(r)((r, p) => FHIRUtil.setProfile(r, p)))
                //Validate the resources given in the parameter and return all issues
                Future
                  .sequence(resources.map(resource => fhirValidator.validateResource(resource, paramDef.pType.get, silent = true))) // By setting silent, this does not throw exception but return issues
                  .map(_.flatten) //Flatten the sequence of OutcomeIssues
              }
          }

        fissues
      }
    }
  }

  /**
    * Extract parameters from URL, or return issues if there is problem
    * @param operationConf  Operation definition
    * @param paramDef       Operation parameter definition
    * @param values         Parameter values supplied
    * @return
    */
  private def validateAndGetParamFromURL(operationConf: OperationConf, paramDef:OperationParamDef, values:List[String]):Future[Either[Seq[FHIROperationParam], Seq[OutcomeIssue]]] = {
    Future.apply {
      //If parameter type is not primitive, then you cannot give it within the URL
      if (paramDef.pType.isEmpty || !fhirConfig.FHIR_PRIMITIVE_TYPES.contains(paramDef.pType.get))
        Right(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Parameter '${paramDef.name}' is not a primitive type for the operation '${operationConf.name}', it cannot be given as a part of URL!"),
          Nil)))
      else {
        if(paramDef.max != "*" && values.size > paramDef.max.toInt)
          Right(Seq(OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Cardinality of Parameter '${paramDef.name}' (${values.size}) is not matching with expected (${paramDef.max}) for the operation '${operationConf.name}', it cannot be given as a part of URL!"),
            Nil)))
        else {
          val paramValue = values.map(FHIRApiValidator.parseAndValidatePrimitive(_, paramDef.pType.get).map(FHIRSimpleOperationParam))
          if(paramValue.exists(_.isEmpty))
            Right(Seq(OutcomeIssue(
              FHIRResponse.SEVERITY_CODES.ERROR, //fatal
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Parameter '${paramDef.name}' is not the required primitive type ${paramDef.pType} for the operation '${operationConf.name}', it cannot be given as a part of URL!"),
              Nil)))
          else {
            Left(paramValue.flatten)
          }
        }
      }
    }
  }

  /**
    * Validate an Operation parameter which is primitive FHIR type
    * @param parameter Name of the parameter
    * @param value Value of parameter in string representation
    * @param ptype primitive FHIR type
    * @return
    */
  private def validatePrimitiveForOperations(parameter:String, value:JValue, ptype:String):Option[OutcomeIssue] = {
    FHIRApiValidator.validatePrimitive(value, ptype) match {
      case false => Some(OutcomeIssue(
        FHIRResponse.SEVERITY_CODES.ERROR, //fatal
        FHIRResponse.OUTCOME_CODES.INVALID,
        None,
        Some(s"Parameter '${parameter}' is not a valid FHIR '$ptype', please see https://www.hl7.org/fhir/datatypes.html!"),
        Nil))
      case _ => None
    }
  }

  /**
    * Validate an Operation parameter which is complex FHIR type
    * @param parameter  Parameter name
    * @param value      Parsed parameter value
    * @param ptype      Parameter type
    * @return
    */
  private def validateComplexForOperations(parameter:String, value:JValue, ptype:String):Option[OutcomeIssue] = {
    FHIRApiValidator.validateComplex(value, ptype) match {
      case false => Some(OutcomeIssue(
        FHIRResponse.SEVERITY_CODES.ERROR, //fatal
        FHIRResponse.OUTCOME_CODES.INVALID,
        None,
        Some(s"Parameter '$parameter' is not a valid FHIR '$ptype', please see https://www.hl7.org/fhir/datatypes.html!"),
        Nil))
      case _ => None
    }
  }


  private def checkResourceExistence(fhirRequest: FHIRRequest):Future[Boolean] = {
    if(fhirRequest.resourceType.isDefined && fhirRequest.resourceId.isDefined)
      ResourceManager.isResourceExist(fhirRequest.resourceType.get, fhirRequest.resourceId.get)
    else
      Future.apply(true)
  }


  /**
    * Handle the operation execution by validating the inputs (operation parameters) and then redirecting execution to actual operation service implementations
    * @param fhirRequest The FHIR request for the operation
    * @param operationConf The operation definition configurations
    * @return FHIR response
    */
  def validateAndCompleteOperation(fhirRequest: FHIRRequest, operationConf:OperationConf):Future[FHIRResponse] = {
    //Validate the Operation request body first
    validateRequestBody(fhirRequest).flatMap( _ =>
      //If the operation is on a resource instance, check existence of that resource
      checkResourceExistence(fhirRequest).flatMap(isOk => {
        //If instance operation and resource does not exist for the given resource id, throw exception
        if(!isOk)
          throw new NotFoundException(Seq(OutcomeIssue(
            FHIRResponse.SEVERITY_CODES.ERROR, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"${fhirRequest.resourceType.get} resource with id '${fhirRequest.resourceId.get}' does not exist!"),
            Nil)))
        else {
              //Check if there is any inconsistency in providing the parameters within the request body (if the resource is given in body, there should be only one parameter of that type to match)
              if(operationConf.inputParams.count(p => p.pType.exists(_.head.isUpper)) > 1 && fhirRequest.resource.exists(requestBody =>
                FHIRUtil.extractValueOption[String](requestBody, FHIR_COMMON_FIELDS.RESOURCE_TYPE) match {
                  case None | Some(FHIR_DATA_TYPES.PARAMETERS) => false
                  case Some(rt) => true
                }
              )){
                throw new  BadRequestException(Seq(OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.ERROR, //fatal
                  FHIRResponse.OUTCOME_CODES.INVALID,
                  None,
                  Some(s"Operation ${operationConf.name} needs more than one parameter which is a FHIR resource type or complex type. Please use ${FHIR_DATA_TYPES.PARAMETERS} for the request body to supply the input parameter values!"),
                  Nil)))
              }
          }

          //Check all defined parameters, if there is error set issues, otherwise set parameter value(s)
          val fresults:Future[Seq[(String, Either[Seq[FHIROperationParam], Seq[OutcomeIssue]])]] =
            Future.sequence(
                operationConf.inputParams.map(paramDef => {
                  (
                    fhirRequest.operationParameters.get(paramDef.name) match {
                      //If parameter is not given in the URL (primitive parameter), given in the body; either Parameters resource or whole resource itself
                      case None => validateAndGetParamFromBody(operationConf, paramDef, fhirRequest.resource)
                      //If parameter is given in the URL
                      case Some(values) => validateAndGetParamFromURL(operationConf, paramDef, values)
                    }) map { valuesOrIssues:Either[Seq[FHIROperationParam], Seq[OutcomeIssue]] =>
                    paramDef.name -> valuesOrIssues
                  }
                })
              )

          fresults flatMap { results =>
            //Check if there is any validation issue for the parameters
            val issues = results.filter(r => r._2.isRight && r._2.right.get.nonEmpty).flatMap(_._2.right.get)
            if (issues.nonEmpty) throw new BadRequestException(issues)

            //Filter the ones with a value
            val parameterValues = results.filter(_._2.isLeft).map(p => p._1 -> p._2.left.get)
            //Load the Operation Service
            val operationService = getOperationServiceImpl(operationConf)

            //If there are query parameters apart from operation parameters, we should parse them and pass to the operation
            val operationParamSet = parameterValues.map(_._1).toSet
            val queryParams = fhirRequest.operationParameters.filterNot(op => operationParamSet.contains(op._1))
            val parsedQueryParams =
              if(fhirRequest.resourceType.isDefined && queryParams.nonEmpty)
                FHIRSearchParameterValueParser.parseSearchParameters(fhirRequest.resourceType.get, queryParams, fhirRequest.prefer)
              else
                List.empty[Parameter]
            //Construct operation request
            val operationRequest =
              new FHIROperationRequest(
                operationParams = parameterValues.flatMap(p => p._2.map(pv => p._1 -> pv)),
                queryParams = parsedQueryParams
              )
            //Execute the operation
            operationService
              .executeOperation(operationConf.name, operationRequest, fhirRequest.resourceType, fhirRequest.resourceId)
              .flatMap(operationResponse => {
                //Validate output
                val foutputIssues = Future.sequence(operationConf.outputParams.map(opParamDef => {
                  val paramValues = operationResponse.getOutputParams.filter(_._1 == opParamDef.name).map(_._2)
                  validateOperationParameter(operationConf.name, paramValues, opParamDef)
                }))

                foutputIssues.map(issues =>
                  if(issues.flatten.exists(_.isError))
                    throw new InternalServerException("Operation implementation generates non-conformant output!", issues.flatten)
                  else
                  //Construct the FHIR response
                    constuctFHIRResponse(operationConf, operationResponse)
                )
              })
          }
        }
      )
    )
  }

  /**
    * Construct the FHIR response for Operation Response
    * @param operationConf      Operation configuration
    * @param operationResponse  FHIR Operation response
    * @return
    */
  private def constuctFHIRResponse(operationConf: OperationConf, operationResponse:FHIROperationResponse):FHIRResponse = {
    if(operationConf.outputParams.length == 1 && operationConf.outputParams.head.name == "return" && !(fhirConfig.FHIR_COMPLEX_TYPES ++ fhirConfig.FHIR_PRIMITIVE_TYPES).contains(operationConf.outputParams.head.pType.getOrElse(""))) {
      operationResponse.copy(responseBody = Some(operationResponse.getOutputParams.head._2.asInstanceOf[FHIRSimpleOperationParam].extractValue[Resource]()))
    } else if(operationConf.outputParams.isEmpty){
      operationResponse
    } else {
      operationResponse.copy(responseBody = Some(constuctParametersResource(operationConf, operationResponse)))
    }
  }

  /**
    * Construct the Parameters resource for the Operation Response body
    * @param operationConf      Operation configuration
    * @param operationResponse  FHIR Operation response
    * @return
    */
  private def constuctParametersResource(operationConf: OperationConf, operationResponse:FHIROperationResponse):Resource = {
    (FHIR_COMMON_FIELDS.RESOURCE_TYPE -> "Parameters") ~
      ("parameter" ->
        operationResponse.getOutputParams.map( param => {
          serializeOperationParameter(param._2, operationConf.outputParams.find(_.name == param._1).get)
        }))
  }

  /**
  * Serialize a Operation output parameter
    * @param value
    * @param parameterDef
    * @return
  */
  private def serializeOperationParameter(value:FHIROperationParam, parameterDef:OperationParamDef):JObject = {
    value match {
      case m:FHIRMultiOperationParam =>
        ("name" ->  parameterDef.name) ~
          ("part" -> JArray(
            m.params.map(cp => serializeOperationParameter(cp._2, parameterDef.parts.find(_.name == cp._1).get)).toList
          ))
      case s:FHIRSimpleOperationParam =>
        ("name" ->  parameterDef.name) ~ (
          parameterDef.pType.get match {
            case dtype if fhirConfig.FHIR_COMPLEX_TYPES.contains(dtype) || fhirConfig.FHIR_PRIMITIVE_TYPES.contains(dtype) =>
              s"value${dtype.capitalize}" -> s.value
            case _ =>
              "resource" -> s.value
          }
          )
    }
  }

  /**
    * Get the operation service implementation from Class path
    * @param operationConf  Operation configuration
    * @return
    */
  private def getOperationServiceImpl(operationConf: OperationConf):FHIROperationHandlerService = {
    val serviceImpl =
      loadOperationClass(operationConf.classPath)
        .map(opClass => opClass.newInstance().asInstanceOf[FHIROperationHandlerService])

    if(serviceImpl.isDefined)
      serviceImpl.get
    else {
      logger.error(s"Operation service not available from class path ${operationConf.classPath} or it is not implementing the FHIROperationService interface !!!")
      throw new InternalServerException("Operation service not available!!!")
    }
  }

  /**
    * Load class if possible
    * @param classPath Class path
    * @return
    */
  def loadOperationClass(classPath:String):Option[Class[_]] = {
    Try(this.getClass.getClassLoader.loadClass(classPath)) match {
      case Success(opClass) => Some(opClass)
      case Failure(e) => Try(ClassLoader.getSystemClassLoader.loadClass(classPath)).toOption
    }
  }
}

package io.onfhir.api.service

import ca.uhn.fhir.validation.ResultSeverityEnum
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import io.onfhir.api._
import io.onfhir.api.model._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.FHIRApiValidator
import io.onfhir.authz.AuthzContext
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.config.FhirConfigurationManager.fhirValidator
import io.onfhir.config.{OperationConf, OperationParamDef}
import io.onfhir.db.{ResourceManager, TransactionSession}
import io.onfhir.exception.{BadRequestException, InternalServerException, NotFoundException}

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
      val operationConf = fhirConfig.supportedOperations.find(_.name == "$" + operation)
      if (operationConf.isEmpty)
        throw new BadRequestException(Seq(OutcomeIssue(
          ResultSeverityEnum.ERROR.getCode, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Operation $operation not supported!"),
          Nil)))

      if (fhirRequest.resourceType.isEmpty && fhirRequest.resourceId.isEmpty && !operationConf.get.levels.contains("system"))
        throw new BadRequestException(Seq(OutcomeIssue(
          ResultSeverityEnum.ERROR.getCode, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Operation $operation is not a system level operation, please see the Operation definition; levels supported; ${operationConf.get.levels}!"),
          Nil)))

      if (fhirRequest.resourceType.isDefined) {
        if (operationConf.get.resources.nonEmpty && !operationConf.get.resources.contains(fhirRequest.resourceType.get))
          throw new BadRequestException(Seq(OutcomeIssue(
            ResultSeverityEnum.ERROR.getCode, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Operation $operation is not supported for resource type; ${fhirRequest.resourceType.get}!"),
            Nil)))

        if (fhirRequest.resourceId.isEmpty && !operationConf.get.levels.contains("type"))
          throw new BadRequestException(Seq(OutcomeIssue(
            ResultSeverityEnum.ERROR.getCode, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Operation $operation is not a type level operation, please see the Operation definition; levels supported; ${operationConf.get.levels}!"),
            Nil)))

        if (fhirRequest.resourceId.isDefined && !operationConf.get.levels.contains("instance")) {
          throw new BadRequestException(Seq(OutcomeIssue(
            ResultSeverityEnum.ERROR.getCode, //fatal
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
    val operationConf = fhirConfig.supportedOperations.find(_.name == "$"+operation).get
    //Validate and complete operation
    validateAndCompleteOperation(fhirRequest, operationConf)
  }

  /**
    * Validate if the Operation Request body is Ok, in general
    * @param fhirRequest FHIR Request
    */
  private def validateRequestBody(fhirRequest: FHIRRequest):Future[Unit] = {
    if(fhirRequest.resource.exists(body => FHIRUtil.extractValueOption[String](body, FHIR_COMMON_FIELDS.RESOURCE_TYPE).contains("Parameters")))
      fhirValidator.validateResource(fhirRequest.resource.get).map(_ => Unit)
    else
      Future.apply(Unit)
  }


  /**
    * Extract parameter value from the OperationRequest
    * @param operationConf  Operation definition
    * @param paramDef       Operation parameter definition
    * @param requestBody    Request body for operation
    * @return
    */
  private def validateAndGetParamFromBody(operationConf: OperationConf, paramDef:OperationParamDef, requestBody:Option[Resource]):Future[Either[JValue, Seq[OutcomeIssue]]] = {
    //If the body is empty (empty Parameters resource or completely empty)
    if (requestBody.isEmpty || requestBody.get.obj.size < 2)
      if (paramDef.min > 0) //If parameter is required
        Future.apply(Right(Seq(OutcomeIssue(
          ResultSeverityEnum.ERROR.getCode, //fatal
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
          val paramValue: Seq[JValue] = FHIRUtil.getParameterValue(requestBody.get, paramDef.name, paramDef.pType)

          if (paramValue.isEmpty) //If parameter is missing
            if (paramDef.min > 0) //If parameter is required return the issue
              Future.apply(Right(Seq(OutcomeIssue(
                ResultSeverityEnum.ERROR.getCode, //fatal
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Parameter ${paramDef.name} not found in request body (Parameters), although it is required for the operation ${operationConf.name}!"),
                Nil))))
            else Future.apply(Right(Nil))
          else {
            //Check if max cardinality matches
            if (paramDef.max != "*" && paramValue.size > paramDef.max.toInt)
              Future.apply(Right(Seq(OutcomeIssue(
                ResultSeverityEnum.ERROR.getCode, //fatal
                FHIRResponse.OUTCOME_CODES.INVALID,
                None,
                Some(s"Cardinality of Parameter ${paramDef.name} (${paramValue.size}) does not match with the required cardinality (${paramDef.max}) for the operation ${operationConf.name}!"),
                Nil))))
            else {
              val fissues: Future[Seq[OutcomeIssue]] =
                paramDef.pType match {
                  //Paremeter is primitive FHIR type e.g. string, int, etc
                  case primitive if FHIR_PRIMITIVE_TYPES.contains(primitive) =>
                    Future.apply(paramValue.flatMap(validatePrimitiveForOperations(paramDef.name, _, paramDef.pType)))
                  //Paremeter is complex FHIR type e.g. CodeableConcept
                  case complex if FHIR_COMPLEX_TYPES.contains(complex) =>
                    Future.apply(paramValue.flatMap(validateComplexForOperations(paramDef.name, _, paramDef.pType)))
                  //Parameter is Resource type
                  case _ =>
                    //Check if it is a JObject (json resource)
                    if(!paramValue.forall(_.isInstanceOf[JObject]))
                      Future.apply(Seq(OutcomeIssue(
                        ResultSeverityEnum.ERROR.getCode, //fatal
                        FHIRResponse.OUTCOME_CODES.INVALID,
                        None,
                        Some(s"Parameter '${paramDef.name}' is not a valid FHIR Resource!"),
                        Nil)))
                    else  {
                      var resources = paramValue.map(_.asInstanceOf[Resource])
                      if (paramDef.pProfile.nonEmpty)
                        resources = resources.map(r => paramDef.pProfile.foldLeft(r)((r, p) => FHIRUtil.setProfile(r, p)))
                      //Validate the resources given in the parameter and return all issues
                      Future
                        .sequence(resources.map(resource => fhirValidator.validateResource(resource, silent = true))) // By setting silent, this does not throw exception but return issues
                        .map(_.flatten) //Flatten the sequence of OutcomeIssues
                    }
                }

              fissues map (issues => {
                if (!issues.exists(i => i.severity == "error" || i.severity == "fatal"))
                  //If parameter is single
                  if(paramDef.max == "1")
                    Left(paramValue.head)
                  else
                    Left(JArray(paramValue.toList)) //Or conver to JArray
                else
                  Right(issues) //Return the issues if there is an error or fatal or the parameter value
              })
            }
          }
        //If body is the whole resource and it matches the expected Resource Type, or expected type is a resource
        case Some(resourceType) if paramDef.pType == resourceType || paramDef.pType == FHIR_DATA_TYPES.RESOURCE =>
            var resource = requestBody.get
            if(paramDef.pProfile.nonEmpty)
              resource =  paramDef.pProfile.foldLeft(resource)((r, p) => FHIRUtil.setProfile(r, p))
            fhirValidator
              .validateResource(resource, silent = true) // By setting silent, this does not throw exception but return issues
              .map(issues => {
                  if(!issues.exists(i => i.severity == "error" || i.severity == "fatal"))
                    Left(requestBody.get)
                  else
                    Right(issues) //Return the issues or the parameter value
              })
        case _ =>
          Future.apply(
            if (paramDef.min > 0) //If parameter is required
              Right(Seq(OutcomeIssue(
                ResultSeverityEnum.ERROR.getCode, //fatal
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
    * Extract parameters from URL, or return issues if there is problem
    * @param operationConf  Operation definition
    * @param paramDef       Operation parameter definition
    * @param values         Parameter values supplied
    * @return
    */
  private def validateAndGetParamFromURL(operationConf: OperationConf, paramDef:OperationParamDef, values:List[String]):Future[Either[JValue, Seq[OutcomeIssue]]] = {
    Future.apply {
      //If parameter type is not primitive, then you cannot give it within the URL
      if (!FHIR_PRIMITIVE_TYPES.contains(paramDef.pType))
        Right(Seq(OutcomeIssue(
          ResultSeverityEnum.ERROR.getCode, //fatal
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Parameter '${paramDef.name}' is not a primitive type for the operation '${operationConf.name}', it cannot be given as a part of URL!"),
          Nil)))
      else {
        if(paramDef.max != "*" && values.size > paramDef.max.toInt)
          Right(Seq(OutcomeIssue(
            ResultSeverityEnum.ERROR.getCode, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Cardinality of Parameter '${paramDef.name}' (${values.size}) is not matching with expected (${paramDef.max}) for the operation '${operationConf.name}', it cannot be given as a part of URL!"),
            Nil)))
        else {
          val paramValue = values.map(FHIRApiValidator.parseAndValidatePrimitive(_, paramDef.pType))
          if(paramValue.exists(_.isEmpty))
            Right(Seq(OutcomeIssue(
              ResultSeverityEnum.ERROR.getCode, //fatal
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Parameter '${paramDef.name}' is not the required primitive type ${paramDef.pType} for the operation '${operationConf.name}', it cannot be given as a part of URL!"),
              Nil)))
          else {
            if (paramDef.max == "1") Left(paramValue.flatten.head) else Left(JArray(paramValue.flatten))
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
        ResultSeverityEnum.ERROR.getCode, //fatal
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
        ResultSeverityEnum.ERROR.getCode, //fatal
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
            ResultSeverityEnum.ERROR.getCode, //fatal
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"${fhirRequest.resourceType.get} resource with id '${fhirRequest.resourceId.get}' does not exist!"),
            Nil)))
        else {
          //Check if there is any inconsistency in providing the parameters within the request body (if the resource is given in body, there should be only one parameter of that type to match)
          if(operationConf.inputParams.count(p => p.pType.head.isUpper) > 1 && fhirRequest.resource.exists(requestBody =>
            FHIRUtil.extractValueOption[String](requestBody, FHIR_COMMON_FIELDS.RESOURCE_TYPE) match {
              case None | Some(FHIR_DATA_TYPES.PARAMETERS) => false
              case Some(rt) => true
            }
          )){
            throw new  BadRequestException(Seq(OutcomeIssue(
              ResultSeverityEnum.ERROR.getCode, //fatal
              FHIRResponse.OUTCOME_CODES.INVALID,
              None,
              Some(s"Operation ${operationConf.name} needs more than one parameter which is a FHIR resource type or complex type. Please use ${FHIR_DATA_TYPES.PARAMETERS} for the request body to supply the input parameter values!"),
              Nil)))
          }

          //Check all defined parameters, if there is error set issues, otherwise set parameter value(s)
          val fresults:Future[Seq[(String, Either[JValue, Seq[OutcomeIssue]])]] =
            Future.sequence(
              operationConf.inputParams.map(paramDef => {
                (
                  fhirRequest.operationParameters.get(paramDef.name) match {
                  //If parameter is not given in the URL (primitive parameter), given in the body; either Parameters resource or whole resource itself
                  case None => validateAndGetParamFromBody(operationConf, paramDef, fhirRequest.resource)
                  //If parameter is given in the URL
                  case Some(values) => validateAndGetParamFromURL(operationConf, paramDef, values)
                }) map { valuesOrIssues:Either[JValue, Seq[OutcomeIssue]] =>
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
            //Execute the operation
            operationService
              .executeOperation(operationConf.name, new FHIROperationRequest(parameterValues.toMap), fhirRequest.resourceType, fhirRequest.resourceId)
              .map(operationResponse => {
                //Construct the FHIR response
                constuctFHIRResponse(operationConf, operationResponse)
              })
          }
        }
      })
    )
  }

  /**
    * Construct the FHIR response for Operation Response
    * @param operationConf      Operation configuration
    * @param operationResponse  FHIR Operation response
    * @return
    */
  private def constuctFHIRResponse(operationConf: OperationConf, operationResponse:FHIROperationResponse):FHIRResponse = {
    if(operationConf.outputParams.length == 1 && operationConf.outputParams.head.name == "return" && !FHIR_ALL_DATA_TYPES.contains(operationConf.outputParams.head.pType)) {
      operationResponse.copy(responseBody = Some(operationResponse.getOutputParams.head._2.asInstanceOf[Resource]))
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
          val paramDef = operationConf.outputParams.find(_.name == param._1)
          if(paramDef.isEmpty) throw new InternalServerException(s"Internal error within operation service, param definition not exist for parameter '${param._1}'!")
          ("name" ->  param._1) ~
            (if(FHIR_ALL_DATA_TYPES.contains(paramDef.get.pType)) s"value${paramDef.get.pType.capitalize}" -> param._2 else "resource" -> param._2)
        }))
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

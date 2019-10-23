package io.onfhir.api.validation

import ca.uhn.fhir.context.FhirContext
import ca.uhn.fhir.validation._
import io.onfhir.api._
import io.onfhir.Onfhir
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.BadRequestException
import io.onfhir.util.JsonFormatter._
import org.apache.commons.lang3.StringUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

/**
  * FHIR content validator
  * @param fhirContext        HAPI FHIR context
  * @param instanceValidator  HAPI instance validator module
  */
class FHIRResourceValidator(fhirContext:FhirContext, instanceValidator:IValidatorModule) extends IFhirResourceValidator {
  private val logger:Logger = LoggerFactory.getLogger(this.getClass)
  //Validations can block so we are using the blocking dispatcher
  implicit val executionContext = Onfhir.actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")
  /**
    * Initialize FHIR validator object based on the Fhir version
    */
  private val validator:FhirValidator = {
    // init FHIRValidator
    val validator = fhirContext.newValidator()
    validator.registerValidatorModule(instanceValidator)
    validator
  }

  /**
    * Validates resource based on business rules
    * Return OutcomeIssue if successful or throw exception
    * @param content JSON content of the resource
    * @param silent If true, does not throw exception but return issues
    */
  def validateResource(resource: Resource, silent:Boolean = false): Future[Seq[OutcomeIssue]] = {
    Future.apply {
      OnfhirConfig.fhirValidation match {
        case FHIR_VALIDATION_ALTERNATIVES.NONE => Nil // Ignore validation, no issues
        case _ =>
          val validationResult = validateAndGetResults(resource.toJson)
          var validationMessages: Array[SingleValidationMessage] = new Array[SingleValidationMessage](validationResult.getMessages.size())
          validationMessages = validationResult.getMessages.toArray[SingleValidationMessage](validationMessages)

          val issues = validationMessages.toSeq.map(msg => {
            //Extract location of error
            val location = (Option(msg.getLocationString), Option(msg.getLocationLine), Option(msg.getLocationCol)) match {
              case (Some(ls), _, _) if StringUtils.isNotBlank(ls) => Some(ls)
              case (_, Some(line), Some(col)) => Some("Line[" + line + "] Col[" + col + "]")
              case _ => None
            }
            //Construct the Issue
            OutcomeIssue(
              msg.getSeverity.getCode, //Severity
              FHIRResponse.OUTCOME_CODES.INVALID, //Issue code: Content invalid against the specification or a profile.
              None,
              Some(msg.getMessage), //diagnostics
              if (location.isDefined) Seq(location.get) else Nil
            )
          })
          //If we are not silent, and validation is not successfull; throw Exception
          if (!validationResult.isSuccessful && !silent)
            throw new BadRequestException(issues)
          issues
      }
    }

  }

  /**
    * Call the validator and get the result
    * @param content FHIR content serialized to string
    * @return
    */
  private def validateAndGetResults(content: String):ValidationResult = {
    try {
      //Run the HAPI Validator (Schema, schematron, instance)
      validator.validateWithResult(content)
    } catch {
      case e: Throwable =>
        logger.error(s"Unexpected validation error: ${e.getMessage}", e)
        createValidationResultForUnexpected(e)
    }
  }
  /**
    * Prepare a ValidationResult for unexpected errors
    * @param exception Exception thrown
    * @return
    */
  private def createValidationResultForUnexpected(exception: Throwable): ValidationResult ={
    import collection.JavaConverters._
    val svm = new SingleValidationMessage()
    svm.setSeverity(ResultSeverityEnum.ERROR)
    svm.setMessage(exception.getMessage)
    new ValidationResult(fhirContext, List(svm).asJava)
  }
}


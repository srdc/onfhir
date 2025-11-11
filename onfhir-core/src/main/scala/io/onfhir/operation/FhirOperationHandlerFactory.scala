package io.onfhir.operation

import io.onfhir.api.service.FHIROperationHandlerService
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.IFhirConfigurationManager
import io.onfhir.exception.InternalServerException
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

/**
 * Default operation handler factory that loads operation implementations from their class paths
 * @param fhirConfigurationManager  FHIR configuration manager
 * @param fhirOperations            Map of FHIR Operation urls and the class path of the implementation class
 */
class FhirOperationHandlerFactory(fhirOperations:Map[String, String]) extends IFhirOperationLibrary {
  private val logger = LoggerFactory.getLogger(classOf[FhirOperationHandlerFactory])

  /**
   * Return the list of URLs for supported FHIR operations
   *
   * @return
   */
  def listSupportedOperations(): Set[String] = fhirOperations.keySet

  /**
   * Return the service implementation that handles the FHIR Operation
   *
   * @param url Url of the FHIR Operation i.e. FHIR OperationDefinition.url
   * @return
   */
  def getOperationHandler(url: String)(implicit fhirConfigurationManager: IFhirConfigurationManager): FHIROperationHandlerService = {
    fhirOperations
      .get(url)
      .map(classPath => getOperationServiceImpl(classPath))
      .getOrElse(
         throw new InternalServerException(s"FHIR operation with url $url is not found!")
      )
  }

  /**
   * Get the operation service implementation from Class path
   *
   * @param classPath Class path for the handler
   *                  e.g. io.onfhir.operation.MyOperationHandler
   * @return
   */
  private def getOperationServiceImpl(classPath:String)(implicit fhirConfigurationManager: IFhirConfigurationManager): FHIROperationHandlerService = {
    val serviceImpl =
      loadFhirOperationClass(classPath)
        .map(opClass => opClass.getConstructor(classOf[IFhirConfigurationManager]).newInstance(fhirConfigurationManager).asInstanceOf[FHIROperationHandlerService])

    if (serviceImpl.isDefined)
      serviceImpl.get
    else {
      logger.error(s"Operation service not available from class path ${classPath} or it is not implementing the FHIROperationService interface !!!")
      throw new InternalServerException("Operation service not available!!!")
    }
  }

  /**
   * Load class if possible
   *
   * @param classPath Class path
   * @return
   */
  private def loadFhirOperationClass(classPath: String): Option[Class[_]] = {
    Try(this.getClass.getClassLoader.loadClass(classPath)) match {
      case Success(opClass) => Some(opClass)
      case Failure(e) => Try(ClassLoader.getSystemClassLoader.loadClass(classPath)).toOption
    }
  }

/*
  val operationsWithInvalidClassPaths =
    fhirOperationImplms
      .view
      .filterKeys(conformance.operationDefUrls.contains)
      .filter(opImpl => FHIRUtil.loadFhirOperationClass(opImpl._2).isEmpty)

  if (operationsWithInvalidClassPaths.nonEmpty)
    throw new InitializationException(s"Invalid class paths ${operationsWithInvalidClassPaths.values.mkString(",")} for FHIR operation implementations!")
*/
}

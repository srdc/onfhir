package io.onfhir.operation

import java.time.Instant

import akka.http.scaladsl.model.{DateTime, StatusCodes}
import io.onfhir.api._
import io.onfhir.api.model.{FHIROperationRequest, FHIROperationResponse}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.{FHIRCreateService, FHIROperationHandlerService, FHIRSearchService}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.db.ResourceManager
import io.onfhir.exception.InternalServerException
import io.onfhir.util.JsonFormatter.formats
import org.json4s.JsonDSL._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

class DocumentOperationHandler extends FHIROperationHandlerService {
  private val logger: Logger = LoggerFactory.getLogger("DocumentOperationHandler")

  final val PARAMETER_PERSIST = "persist"
  final val SEARCHPARAM_ID = "_id"
  final val SEARCHPARAM_INCLUDE = "_include"
  final val RESOURCE_COMPOSITION = "Composition"
  final val RESOURCE_BUNDLE = "Bundle"

  /**
    * Execute the operation and prepare the output parameters for the operation
    *
    * @param operationName    Operation name as defined after '$' symbol e.g. meta-add
    * @param operationRequest Operation Request including the parameters
    * @param resourceType     The resource type that operation is called if exists
    * @param resourceId       The resource id that operation is called if exists
    * @return The response containing the Http status code and the output parameters
    */
  override def executeOperation(operationName: String, operationRequest: FHIROperationRequest, resourceType: Option[String], resourceId: Option[String]): Future[FHIROperationResponse] = {
    if(resourceId.isEmpty)
      throw new InternalServerException(s"Operation $operationName without [id] is not supported yet")

    val searchParams = FHIRSearchParameterValueParser.parseSearchParameters(RESOURCE_COMPOSITION, Map(
      SEARCHPARAM_ID -> List(resourceId.get),
      SEARCHPARAM_INCLUDE -> List("*")
    ))



    new FHIRSearchService().searchAndReturnBundle(RESOURCE_COMPOSITION, searchParams) map { bundle =>

      var result = bundle
      //Type of the bundle
      result = result ~ (FHIR_COMMON_FIELDS.TYPE -> FHIR_BUNDLE_TYPES.DOCUMENT)

      result.transformField {
        case (FHIR_COMMON_FIELDS.ENTRY, entry) =>
          (FHIR_COMMON_FIELDS.ENTRY ->
            entry.removeField {
              case (FHIR_BUNDLE_FIELDS.SEARCH, _) => true
              case _ => false
            }
          )
      }

      /*
      bundle("type") = FHIR_BUNDLE_TYPES.DOCUMENT
      bundle("entry").asInstanceOf[Seq[Resource]] map { entry =>
        entry.remove("search")
      }*/

      val persist = operationRequest.extractParamValue[String]("persist")

      val generatedBundle = persist match {
        case Some("true") =>
          ResourceManager.createResource((bundle \ "resourceType").extract[String], result, generatedId = (result \ "id").extractOpt[String])
          bundle
        case _ => {
          val newVersion = 1L //new version is always 1 for create operation
          val lastModified = Instant.now()
          val newBundle = FHIRUtil.populateResourceWithMeta(result, (result \ "id").extractOpt[String], newVersion, lastModified)
          //newBundle.put(FHIR_EXTRA_FIELDS.STATUS_CODE, "")
          newBundle ~ (FHIR_EXTRA_FIELDS.STATUS_CODE, "")
        }

      }

      val opResponse = new FHIROperationResponse(StatusCodes.OK)
      opResponse.setResponse(generatedBundle)
      opResponse
    }
  }



}

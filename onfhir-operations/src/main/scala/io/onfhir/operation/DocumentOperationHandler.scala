package io.onfhir.operation

import java.time.Instant

import akka.http.scaladsl.model.{DateTime, StatusCodes}
import io.onfhir.api._
import io.onfhir.api.model.{FHIROperationRequest, FHIROperationResponse}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.{FHIRCreateService, FHIROperationHandlerService, FHIRSearchService}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.OnfhirConfig
import io.onfhir.db.ResourceManager
import io.onfhir.exception.InternalServerException
import io.onfhir.util.DateTimeUtil
import io.onfhir.util.JsonFormatter.formats
import org.json4s.JString
import org.json4s.JsonAST.{JField, JObject, JValue}
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

    new FHIRSearchService()
      .searchAndReturnBundle(RESOURCE_COMPOSITION, searchParams) map { bundle =>

      // Convert bundle type to document and eliminate other elements
      var result:JValue = JObject(
        (bundle.obj
          .filter(f => f._1 == FHIR_COMMON_FIELDS.ID || f._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE || f._1 == FHIR_COMMON_FIELDS.ENTRY) :+
            (FHIR_COMMON_FIELDS.TYPE -> JString(FHIR_BUNDLE_TYPES.DOCUMENT)))
          .sortWith((s1, _) => s1._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE || s1._1 == FHIR_COMMON_FIELDS.ID || s1._1 == FHIR_COMMON_FIELDS.TYPE)
      )

      result = result.transformField {
        case (FHIR_COMMON_FIELDS.ENTRY, entry) =>
          (FHIR_COMMON_FIELDS.ENTRY ->
            entry.removeField {
              case (FHIR_BUNDLE_FIELDS.SEARCH, _) => true
              case _ => false
            }
          )
      }
      //Add document time and identifier
      result = result merge JObject(
          "timestamp" -> JString(DateTimeUtil.serializeInstant(Instant.now())),
          "identifier" -> JObject(
            "system" -> JString(OnfhirConfig.fhirRootUrl),
            "value" -> JString(FHIRUtil.generateResourceId())
          )
        )

      val persist = operationRequest.extractParamValue[Boolean]("persist")

      val generatedBundle = persist match {
        case Some(true) =>
          ResourceManager.createResource((bundle \ "resourceType").extract[String], result.asInstanceOf[JObject], generatedId = (result \ "id").extractOpt[String])
          result.asInstanceOf[JObject]
        case _ => {
          val newVersion = 1L //new version is always 1 for create operation
          val lastModified = Instant.now()
          val newBundle =
            FHIRUtil
              .populateResourceWithMeta(result.asInstanceOf[JObject], (result \ "id").extractOpt[String], newVersion, lastModified)
              .removeField(_._1 == FHIR_EXTRA_FIELDS.STATUS_CODE)

         newBundle.asInstanceOf[JObject]
        }
      }

      val opResponse = new FHIROperationResponse(StatusCodes.OK)
      opResponse.setResponse(generatedBundle)
      opResponse
    }
  }



}

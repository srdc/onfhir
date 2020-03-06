package io.onfhir.operation

import akka.http.scaladsl.model.StatusCodes
import io.onfhir.api._
import io.onfhir.api.model.{FHIROperationRequest, FHIROperationResponse, FHIRResponse, FHIRSimpleOperationParam, OutcomeIssue}
import io.onfhir.api.service.FHIROperationHandlerService
import io.onfhir.db.ResourceManager
import io.onfhir.exception._
import io.onfhir.util.JsonFormatter.formats
import org.json4s.Diff
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.json4s.JsonDSL._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

/**
  * Created by tuncay on 10/4/2017.
  * Handles the FHIR Operations;
  *   - $meta-add
  *   - $meta-delete
  *   - $meta
  */
class MetaOperationHandler extends FHIROperationHandlerService {
  private val logger: Logger = LoggerFactory.getLogger("MetaOperationHandler")
  /**
    * Execute the operation and prepare the output parameters for the operation
    * @param operationRequest Operation Request including the parameters
    * @param resourceType     The resource type that operation is called if exists
    * @param resourceId       The resource id that operation is called if exists
    * @return The response containing the Http status code and the output parameters
    */
  override def executeOperation(operationName:String, operationRequest: FHIROperationRequest, resourceType: Option[String], resourceId: Option[String]): Future[FHIROperationResponse] = {
     operationName match {
       //See https://www.hl7.org/fhir/resource-operations.html#meta-add
       case "meta-add" => handleMetaAdd(operationRequest, resourceType.get, resourceId.get)
       //See https://www.hl7.org/fhir/resource-operations.html#meta-delete
       case "meta-delete" => handleMetaDelete(operationRequest, resourceType.get, resourceId.get)
       //See https://www.hl7.org/fhir/resource-operations.html#meta
       case "meta" => throw new InternalServerException(s"Operation ${operationName} not supported yet by the MetaOperationHandler!!!")
       case _ => throw new InternalServerException(s"Operation ${operationName} not supported by the MetaOperationHandler!!!")
     }
   }

  /**
    * Handles $meta-add operation
    * @param operationRequest Operation Request
    * @param resourceType The resource type
    * @param resourceId The resource id
    * @return
    */
   def handleMetaAdd(operationRequest: FHIROperationRequest, resourceType:String, resourceId:String):Future[FHIROperationResponse] = {
     //Get the input parameter "meta:Meta"
     val newMeta = operationRequest.getParam("meta").get.asInstanceOf[FHIRSimpleOperationParam].value

     ResourceManager.getResource(resourceType, resourceId, excludeExtraFields = true).flatMap {
       case None =>
         logger.debug("resource not found, return 404 NotFound...")
         throw new NotFoundException(Seq(
           OutcomeIssue(
             FHIRResponse.SEVERITY_CODES.INFORMATION,
             FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
             None,
             Some(s"Resource with type (${resourceType}), id (${resourceId}) not found..."),
             Nil
           )
         ))
       case Some(resource) =>
         //Update the resource with meta
         val updatedResource = resource merge (JObject() ~ (FHIR_COMMON_FIELDS.META -> newMeta.removeField(f => f._1 == FHIR_COMMON_FIELDS.VERSION_ID || f._1 == FHIR_COMMON_FIELDS.LAST_UPDATED)))

         ResourceManager.replaceResource(resourceType, resourceId, updatedResource).map(isUpdated => {
           if(!isUpdated) throw new InternalServerException("Problem in updating Meta in meta-add operation!!!")
           //Create the response, set the return result
           val response = new FHIROperationResponse(StatusCodes.OK)
           response.setResponse((updatedResource \ FHIR_COMMON_FIELDS.META).asInstanceOf[JObject])
           response
         })
       }
   }


  /**
    * Handles $meta-delete operation
    * @param operationRequest Operation Request
    * @param resourceType The resource type
    * @param resourceId The resource id
    * @return
    */
   def handleMetaDelete(operationRequest: FHIROperationRequest, resourceType:String, resourceId:String):Future[FHIROperationResponse] = {
     //Get the input parameter "meta:Meta"
     val metaToBeDeleted = operationRequest.getParam("meta").get.asInstanceOf[FHIRSimpleOperationParam].value

     ResourceManager.getResource(resourceType, resourceId).flatMap {
       case None =>
         logger.debug("resource not found, return 404 NotFound...")
         throw new NotFoundException(Seq(
           OutcomeIssue(
             FHIRResponse.SEVERITY_CODES.INFORMATION,
             FHIRResponse.OUTCOME_CODES.INFORMATIONAL,
             None,
             Some(s"Resource with type (${resourceType}), id (${resourceId}) not found..."),
             Nil
           )
         ))
       case Some(resource) =>
         //Find the difference
         val Diff(_, _, deleted) = (resource \ FHIR_COMMON_FIELDS.META) diff metaToBeDeleted.removeField(f => f._1 == FHIR_COMMON_FIELDS.VERSION_ID || f._1 == FHIR_COMMON_FIELDS.LAST_UPDATED)

         val profilesToBeDeleted:Seq[String] = (deleted \ FHIR_COMMON_FIELDS.PROFILE).extract[Seq[String]]
         val securityTagsToBeDeleted:Seq[String] =  (deleted \ FHIR_COMMON_FIELDS.SECURITY).asInstanceOf[JArray].arr.map(processCoding(_))
         val tagsToBeDeleted:Seq[String] =  (deleted \ FHIR_COMMON_FIELDS.TAG).asInstanceOf[JArray].arr.map(processCoding(_))
         //Update the resource delete the meta fields
         val updatedResource = resource.transformField {
           case (FHIR_COMMON_FIELDS.META, meta) =>
             FHIR_COMMON_FIELDS.META -> meta.transformField {
               case (FHIR_COMMON_FIELDS.PROFILE, profiles:JArray) => (FHIR_COMMON_FIELDS.PROFILE -> profiles.remove(p => profilesToBeDeleted.contains(p.extract[String])))
               case (FHIR_COMMON_FIELDS.SECURITY, securityTags:JArray) =>  (FHIR_COMMON_FIELDS.SECURITY -> securityTags.remove(st => securityTagsToBeDeleted.contains(processCoding(st.asInstanceOf[JObject]))))
               case (FHIR_COMMON_FIELDS.TAG, tags:JArray) =>  (FHIR_COMMON_FIELDS.TAG -> tags.remove(t => tagsToBeDeleted.contains(processCoding(t.asInstanceOf[JObject]))))
             }
         }.asInstanceOf[JObject]
         //Replace the resource
         ResourceManager.replaceResource(resourceType, resourceId, updatedResource).map(isUpdated => {
           if(!isUpdated) throw new InternalServerException("Problem in updating Meta in meta-add operation!!!")
           //Create the response, set the return result
           val response = new FHIROperationResponse(StatusCodes.OK)
           response.setResponse((updatedResource \ FHIR_COMMON_FIELDS.META).asInstanceOf[JObject])
           response
         })
     }
   }

  /**
    * Create a key for the Coding to understand if two Codings are same
    * @param coding
    * @return
    */
   private def processCoding(coding:JValue):String = {
     val key =
       (coding \ FHIR_COMMON_FIELDS.SYSTEM).extractOpt[String].getOrElse("") + ":" +
         (coding \ "version").extractOpt[String].getOrElse("") +":" +
         (coding \ FHIR_COMMON_FIELDS.CODE).extractOpt[String].getOrElse("")

     key
   }
}

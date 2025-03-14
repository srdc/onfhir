package io.onfhir.operation

import akka.http.scaladsl.model.StatusCodes
import com.nimbusds.jose.crypto.ECDSASigner
import com.nimbusds.jose.jwk.ECKey
import com.nimbusds.jose.{JWSAlgorithm, JWSHeader, JWSObject, Payload}
import io.onfhir.api.{FHIR_BUNDLE_TYPES, Resource}
import io.onfhir.api.model.{FHIROperationRequest, FHIROperationResponse, FHIRResponse, OutcomeIssue, Parameter}
import io.onfhir.api.parsers.FHIRSearchParameterValueParser
import io.onfhir.api.service.FHIROperationHandlerService
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.{FhirConfigurationManager, OnfhirConfig}
import io.onfhir.db.ResourceManager
import io.onfhir.exception.BadRequestException
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonDSL._
import org.json4s._

import java.util.zip.Deflater
import scala.concurrent.Future
import scala.util.Try

class HealthCardIssueOperationHandler extends FHIROperationHandlerService {

  private val HEALTH_CARD_TYPE = "https://smarthealth.cards#health-card"

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
    prepareBundle(operationRequest, resourceType, resourceId) map { bundle =>
      val credentialSubject = ("fhirVersion" -> FhirConfigurationManager.fhirConfig.fhirVersion) ~ ("fhirBundle" -> bundle)
      val vc = ("type" -> JArray(List(HEALTH_CARD_TYPE))) ~ ("credentialSubject" -> credentialSubject)
      val payload = ("iss" -> OnfhirConfig.fhirRootUrl) ~ ("nbf" -> System.currentTimeMillis() / 1000) ~ ("vc" -> vc)
      val deflatedPayload = deflate(payload.toJson)
      val jwk = OnfhirConfig.authzConfig.protectedResourceJWKSet.getKeyByKeyId("Fd_ncCoG4zYPoNit-tYEQ4YLQzraKqQ-kH1r8PN9TW0")
      val signer = new ECDSASigner(jwk.asInstanceOf[ECKey])
      val jwsObject = new JWSObject(
        new JWSHeader.Builder(JWSAlgorithm.ES256).customParam("zip", "DEF").keyID(jwk.computeThumbprint().toString).build(),
        new Payload(deflatedPayload)
      )
      jwsObject.sign(signer)
      val resp = new FHIROperationResponse(StatusCodes.OK)
      resp.setPrimitiveParam("verifiableCredential", jwsObject.serialize())
      resp
    }
  }

  private def prepareBundle(operationRequest: FHIROperationRequest, resourceType: Option[String], resourceId: Option[String]): Future[Resource] = {
    ResourceManager.getResource(resourceType.get, resourceId.get, excludeExtraFields = true) flatMap { patientOpt =>
      if (patientOpt.isEmpty) {
        throw new BadRequestException(Seq(
          OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR,
            FHIRResponse.OUTCOME_CODES.INVALID,
            None,
            Some(s"Patient '${resourceId.get}' not found."),
            Nil)))
      }

      val requestedResourceTypes = operationRequest.extractParamValues[String]("credentialType")
      val defaultElementsToRemove = Set("id", "meta", "extension", "text", "div", "display", "subject", "patient")

      val patient = patientOpt.get
      val patientRef = "Patient/" + resourceId.get

      val requests = getValueSets(operationRequest) flatMap { codes =>
        Future.sequence(
          requestedResourceTypes map {
            case "Patient" =>
              Future.apply(List(cleanJson(patient, defaultElementsToRemove)))
            case resourceType @ ("Condition"|"Observation"|"MedicationStatement") =>
              ResourceManager.searchResources(resourceType, getParams(resourceType, Map[String, List[String]]("subject" -> List(patientRef), "code" -> codes.toList)), excludeExtraParams = true)
                .map(_._2.map(resource => cleanJson(resource, defaultElementsToRemove ++ List("asserter", "performer", "category"))))
            case "FamilyMemberHistory" =>
              ResourceManager.searchResources("FamilyMemberHistory", getParams("FamilyMemberHistory", Map[String, List[String]]("patient" -> List(patientRef), "code" -> codes.toList)), excludeExtraParams = true)
                .map(_._2.map(resource => cleanJson(resource, defaultElementsToRemove)))
            case _ => Future.apply(List.empty[Resource])
          }
        )
      }

      requests map { resources => {
        FHIRUtil.createBundle(FHIR_BUNDLE_TYPES.DOCUMENT, List.empty, resources.flatten.zipWithIndex map {
          case (resource, index) =>
            ("fullUrl" -> s"resource:$index") ~ ("resource" -> resource)
        }, resources.length, None)
      }}
    }
  }

  private def getParams(resourceType: String, params: Map[String, List[String]]) = {
    FHIRSearchParameterValueParser.parseSearchParameters(resourceType, params.filter(_._2.nonEmpty))
  }

  // Function to recursively remove specified fields from JSON
  def cleanJson(json: JValue, fieldsToRemove: Set[String]): JValue = {
    json match {
      case JObject(fields) =>
        JObject(
          fields.collect {
            case (key, value) if !fieldsToRemove.contains(key) =>
              key -> cleanJson(value, fieldsToRemove) // Recursively clean nested objects
          }
        )
      case JArray(elements) =>
        JArray(elements.map(elem => cleanJson(elem, fieldsToRemove))) // Recursively clean arrays
      case other => other // Leave other types (JString, JNumber, etc.) unchanged
    }
  }

//  private def removeElements(obj: JObject, keysToRemove: Set[String]) = {
//    def removeCodingTexts(value: JValue) = {
//      value removeField {
//        case ("text", JString(_)) => true
//        case ("div", JString(_)) => true
//        case _ => false
//      } transformField {
//        case ("coding", JArray(coding)) =>
//          ("coding", JArray(coding.map(_.removeField(_._1 == "display"))))
//      }
//    }
//    (obj removeField {
//      case (key, _) => keysToRemove.contains(key)
//    } transformField {
//      case (key, JArray(values)) => (key, values.map(value => removeCodingTexts(value)))
//      case (key, JObject(value)) => (key, removeCodingTexts(JObject(value)))
//      case (key, value) => (key, value)
//    }).asInstanceOf[Resource]
//  }

  private def getValueSets(operationRequest: FHIROperationRequest) = {
    val vsParams = operationRequest.extractParamValues[String]("credentialValueSet")
    (if (vsParams.nonEmpty) {
      val searchParams:List[Parameter] = FHIRSearchParameterValueParser.parseSearchParameters("ValueSet", Map("url" -> vsParams.toList))
      ResourceManager.searchResources("ValueSet", searchParams, true) map {
        case (_, valueSets, _) =>
          valueSets.flatMap(valueSet => (valueSet \ "compose" \ "include").extract[JArray].values.flatMap(value => {
            val system = (value.asInstanceOf[JObject] \ "system").extract[String]
            (value.asInstanceOf[JObject] \ "concept").extract[JArray].values.map(concept => system + '|' + (concept.asInstanceOf[JObject] \ "code").extract[String])
          }))
      }
    } else {
      Future.apply(Seq.empty)
    })
  }

  private def deflate(input: String): Array[Byte] = {
    val inputBytes = input.getBytes("UTF-8")
    val compressor = new Deflater(Deflater.DEFAULT_COMPRESSION, true)
    compressor.setInput(inputBytes)
    compressor.finish()
    val output = new Array[Byte](inputBytes.size * 2)
    val count = compressor.deflate(output)
    compressor.end()
    output.take(count)
  }

}

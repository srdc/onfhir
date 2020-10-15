package io.onfhir.api.client

import io.onfhir.api.model.{FHIRMultiOperationParam, FHIROperationResponse, FHIRRequest}
import org.json4s.JsonAST.{JArray, JObject, JString, JValue}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

class FhirOperationRequestBuilder(onFhirClient: IOnFhirClient, operation:String) extends FhirRequestBuilder(onFhirClient,
  FHIRRequest(interaction = "$"+operation, requestUri = s"${onFhirClient.getBaseUrl()}")
) {
  type This = FhirOperationRequestBuilder


  val params:ListBuffer[(String, (Option[String],JValue))] = new ListBuffer[(String, (Option[String],JValue))]

  override protected def compile(): Unit = {
    super.compile()
    request.resource =Some(JObject(
      "resourceType" -> JString("Parameters"),
      "parameter" ->
          JArray(
            params.map(p =>
              (p._2 : @unchecked) match {
                case (Some(dt), v) =>
                  JObject(
                    "name" -> JString(p._1),
                    "value"+ dt.capitalize -> v
                  )
                case (None, o:JObject) =>
                  JObject(
                    "name" -> JString(p._1),
                    "resource" -> o
                  )
                case (None, a:JArray) =>
                  JObject(
                    "name" -> JString(p._1),
                    "part" -> a
                  )
              }
            ).toList
          )
    ))
  }

  /**
   * Set resource type and resource id (optional) that operation will be called on
   * @param rtype
   * @param rid
   * @return
   */
  def on(rtype:String, rid:Option[String] = None):FhirOperationRequestBuilder = {
    request.resourceType = Some(rtype)
    request.resourceId = rid
    this
  }

  /**
   * Add operation parameter as search parameter
   * @param name
   * @param value
   * @return
   */
  def addSimpleParam(name:String, value:String*):FhirOperationRequestBuilder = {
    request.queryParams = request.queryParams ++ Map(name -> value.toList)
    this
  }

  /**
   * Add a FHIR operation parameter to the request
   * @param name    Name of the parameter
   * @param value   Data type for the value and the value itself
   * @return
   */
  def addParam(name:String, value:(String,JValue)):FhirOperationRequestBuilder = {
    params.append(name -> (Some(value._1) -> value._2))
    this
  }

  /**
   * Add a FHIR operation resource parameter to the request
   * @param name    Name of the parameter
   * @param value   Resource content
   * @return
   */
  def addResourceParam(name:String, value:JObject):FhirOperationRequestBuilder = {
    params.append(name -> (None -> value))
    this
  }

  /**
   * Add a FHIR operation parameter which is a multi param
   * @param name    Name of the parameter
   * @param parts   Parts of the parameter (Parameters.parameter.part
   * @return
   */
  def addMultiParam(name:String, parts:JArray):FhirOperationRequestBuilder = {
    params.append(name ->  (None -> parts))
    this
  }

  /**
   * Convert the response to Operation outcome
   * @param executionContext
   * @return
   */
  def executeAndReturnOperationOutcome()(implicit executionContext: ExecutionContext):Future[FHIROperationResponse] = {
    execute()
      .map{
          case response: FHIROperationResponse => response
          case oth =>
            try {
              FHIROperationResponse.apply(oth)
            }catch {
              case t:Throwable => throw FhirClientException("Invalid operation response!", Some(oth))
            }
      }
  }

}

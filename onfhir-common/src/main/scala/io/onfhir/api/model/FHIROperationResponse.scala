package io.onfhir.api.model

import akka.http.javadsl.model.headers.WWWAuthenticate
import akka.http.scaladsl.model.{DateTime, StatusCode, Uri}
import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_DATA_TYPES, Resource}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.util.JsonFormatter._
import org.json4s.Extraction
import org.json4s.JsonAST.JObject

import scala.collection.mutable

/**
  * Created by tuncay on 10/3/2017.
  * FHIR Operation response
  */
class FHIROperationResponse(httpStatus:StatusCode, location:Option[Uri] = None, lastModified:Option[DateTime] = None, newVersion:Option[String] = None)
  extends FHIRResponse(httpStatus, location = location, lastModified = lastModified, newVersion = newVersion) {

  //Return parameters for Operation
  private val outputParams:mutable.ListBuffer[(String, FHIROperationParam)]  = new mutable.ListBuffer[(String, FHIROperationParam)]()

  /**
    * Set primitive output param
    * @param pname Parameter name
    * @param value Parameter value
    * @return
    */
  def setPrimitiveParam(pname:String, value:Any):Unit = outputParams.append(pname -> FHIRSimpleOperationParam(Extraction.decompose(value)))

  /**
    * Set a list of primitive output param
    * @param pname  Parameter name
    * @param values Parameter values
    * @return
    */
  def setPrimitiveParamList(pname:String, values:Seq[Any]):Unit = values.foreach(v =>  setPrimitiveParam(pname, v))

  /**
    * Set a FHIR complex type or Resource output param
    * @param pname  Parameter name
    * @param value  Parameter value in JSON object format
    * @return
    */
  def setComplexOrResourceParam(pname:String, value:Resource):Unit = outputParams.append(pname -> FHIRSimpleOperationParam(value))

  /**
    * Set FHIR complex type or Resource output params
    * @param pname    Parameter name
    * @param values   Parameter values in JSON object format
    * @return
    */
  def setComplexOrResourceParams(pname:String, values:List[Resource]):Unit = values.foreach(v => setComplexOrResourceParam(pname, v))

  /**
   * Set a multi param
   * @param pname
   * @param value
   */
  def setMultiParam(pname:String, value:FHIRMultiOperationParam):Unit = outputParams.append(pname ->value)

  /**
    * Set single response with the given resource
    * @param value Set the single FHIR resource response
    * @return
    */
  def setResponse(value:Resource):Unit = {
    setPrimitiveParam("return", value)
  }

  /**
   *
   * @param pname
   * @param value
   */
  def setOutputParam(pname:String, value:FHIROperationParam):Unit = outputParams.append(pname -> value)

  /**
    * Get output parameters returned from the operation
    * @return
    */
  def getOutputParams:Seq[(String,FHIROperationParam)] = outputParams.toSeq

  /**
   * Get single cardinality output param
   * @param pname
   * @return
   */
  def getOutputParam(pname:String):Option[FHIROperationParam] = {
    outputParams.find(_._1 == pname).map(_._2)
  }

  /**
   * Get multi cardinality output param
   * @param pname
   * @return
   */
  def getOutputParams(pname:String):Seq[FHIROperationParam] = {
    outputParams.filter(_._1 == pname).map(_._2).toSeq
  }
}

object FHIROperationResponse {
  //Parse and convert FHIR response to FHIR Operation response
  def apply(response:FHIRResponse):FHIROperationResponse = {
    val operationResponse = new FHIROperationResponse(response.httpStatus, response.location, response.lastModified, response.newVersion)

    response.responseBody
      .foreach(body =>
        FHIRUtil.extractValue[String](body, FHIR_COMMON_FIELDS.RESOURCE_TYPE) match {
          case FHIR_DATA_TYPES.PARAMETERS =>
            val parameters =
              FHIRUtil.extractValue[Seq[JObject]](body, "parameter")
                .map(parseOperationParameter)
            parameters.foreach(p => operationResponse.setOutputParam(p._1, p._2))
          case oth =>
            operationResponse.setResponse(body)
        }
      )
    operationResponse
  }

  private def parseOperationParameter(parameter:JObject):(String, FHIROperationParam) = {
    val pname = FHIRUtil.extractValue[String](parameter, "name")
    FHIRUtil.extractValueOption[JObject](parameter, "resource") match {
      case Some(r) => pname -> FHIROperationParam.createSimpleParam(r)
      case None =>
        parameter.findField(_._1.startsWith("value")).map(_._2) match {
          case Some(v) => pname -> FHIROperationParam.createSimpleParam(v)
          case None =>
            val subparts = FHIRUtil.extractValue[Seq[JObject]](parameter, "part")
            pname -> FHIRMultiOperationParam.apply(subparts.map(parseOperationParameter))
        }
    }
  }

}
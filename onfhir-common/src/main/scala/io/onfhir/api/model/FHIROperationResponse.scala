package io.onfhir.api.model

import akka.http.scaladsl.model.{DateTime, StatusCode, Uri}

import io.onfhir.api.Resource
import io.onfhir.util.JsonFormatter._

import org.json4s.Extraction
import org.json4s.JsonAST.{JArray, JValue}

import scala.collection.mutable

/**
  * Created by tuncay on 10/3/2017.
  * FHIR Operation response
  */
class FHIROperationResponse(statusCode:StatusCode,
                            location:Option[Uri] = None,
                            lastModified:Option[DateTime] = None,
                            newVersion:Option[Long]=None) extends FHIRResponse(statusCode, location = location, lastModified = lastModified, newVersion = newVersion) {
  //Return parameters for Operation
  private val outputParams:mutable.Map[String, JValue]  = new mutable.HashMap[String, JValue]()

  /**
    * Set primitive output param
    * @param pname Parameter name
    * @param value Parameter value
    * @return
    */
  def setPrimitiveParam(pname:String, value:Any):Option[JValue] = outputParams.put(pname, Extraction.decompose(value))

  /**
    * Set a list of primitive output param
    * @param pname  Parameter name
    * @param values Parameter values
    * @return
    */
  def setPrimitiveParamList(pname:String, values:List[Any]):Option[JValue] = outputParams.put(pname, JArray(values.map(Extraction.decompose(_))))

  /**
    * Set a FHIR complex type or Resource output param
    * @param pname  Parameter name
    * @param value  Parameter value in JSON object format
    * @return
    */
  def setComplexOrResourceParam(pname:String, value:Resource):Option[JValue] = outputParams.put(pname, value)

  /**
    * Set FHIR complex type or Resource output params
    * @param pname    Parameter name
    * @param values   Parameter values in JSON object format
    * @return
    */
  def setComplexOrResourceParams(pname:String, values:List[Resource]):Option[JValue] = outputParams.put(pname, JArray(values))

  /**
    * Set single response with the given resource
    * @param value Set the single FHIR resource response
    * @return
    */
  def setResponse(value:Resource):Option[JValue] = {
    outputParams.put("return", value)
  }

  /**
    * Get output parameters returned from the operation
    * @return
    */
  def getOutputParams:Map[String,JValue] = outputParams.toMap

}

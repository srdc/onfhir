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
    * Get output parameters returned from the operation
    * @return
    */
  def getOutputParams:Seq[(String,FHIROperationParam)] = outputParams
}

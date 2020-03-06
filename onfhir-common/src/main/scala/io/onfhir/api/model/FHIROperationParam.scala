package io.onfhir.api.model

import org.json4s.JsonAST.JValue
import io.onfhir.util.JsonFormatter._
import org.json4s.Extraction

class InvalidParamRequest(message:String) extends Exception(message)

abstract class FHIROperationParam

/**
 * Operation parameter value with a specific value
 * @param value Value or values of the param
 *              e.g. JArray for params with cardinality greater than 1
 *              e.g. JObject for FHIR complex data types or resources
 *              e.g. Other JValues for FHIR simple data types
 */
case class FHIRSimpleOperationParam(value:JValue) extends FHIROperationParam {
  /**
   * Get Operation Parameter in the requested type (if cannot convert to this type, throws InvalidParamRequest)
   * @param mf  Scala type for parameter extraction
   * @tparam T Supported
   *                       - primitives (String, Int, Bool, etc)
   *                       - complex FHIR types with case classes
   * @throws
   * @return
   */
  @throws[InvalidParamRequest]
  def extractValue[T]()(implicit mf:Manifest[T]):T = {
    try {
      value.extract[T]
    }catch {
      case e:Exception => throw new InvalidParamRequest(e.getMessage)
    }
  }
}

/**
 * Parameter with child parameters
 * @param params
 */
case class FHIRMultiOperationParam(params:Seq[(String, FHIROperationParam)]) extends FHIROperationParam{

  /**
   * Get a single cardinality parameter
   * @param pname Parameter name
   * @return
   */
  def getParam(pname:String):Option[FHIROperationParam] = params.find(_._1 ==pname).map(_._2)

  /**
   * Get a multi cardinality parameter value
   * @param pname Parameter name
   * @return
   */
  def getParams(pname:String):Seq[FHIROperationParam] = params.filter(_._1 == pname).map(_._2)

  /**
   * Extract value of a single cardinality child param
   *
   * @param pname Parameter name
   * @param mf Scala type for parameter extraction
   * @tparam T Supported
   *            - primitives (String, Int, Bool, etc)
   *           - complex FHIR types with case classes
   * @return
   */
  def extractParamValue[T](pname:String)(implicit mf:Manifest[T]):Option[T] = {
    getParam(pname).map {
      case m:FHIRMultiOperationParam => throw new InvalidParamRequest(s"Parameter $pname should be simple parameter to extract value!")
      case s:FHIRSimpleOperationParam => s.extractValue[T]()
    }
  }

  /**
   * Extract value of a multi cardinality child param
   * @param pname Parameter name
   * @param mf Scala type for parameter extraction
   * @tparam T Supported
   *            - primitives (String, Int, Bool, etc)
   *           - complex FHIR types with case classes
   * @return
   */
  def extractParamValues[T](pname:String)(implicit mf:Manifest[T]):Seq[T] = {
    getParams(pname).map {
      case m:FHIRMultiOperationParam => throw new InvalidParamRequest(s"Parameter $pname should be simple parameter to extract value!")
      case s:FHIRSimpleOperationParam => s.extractValue[T]()
    }
  }

}

object FHIROperationParam {

  def createSimpleParam(v:Any):FHIRSimpleOperationParam = FHIRSimpleOperationParam(Extraction.decompose(v))
}
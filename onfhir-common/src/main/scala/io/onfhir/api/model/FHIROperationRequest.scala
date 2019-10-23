package io.onfhir.api.model

import org.json4s.JsonAST.JValue
import org.json4s.MappingException
import io.onfhir.util.JsonFormatter._

/**
  * Created by tuncay on 10/3/2017.
  * FHIR Operation request
  */
class FHIROperationRequest(operationRequestParams:Map[String, JValue]) {

  /**
    * Get Operation Parameter in the requested type (if cannot convert to this type, throw json4s mapping exception)
    * @param pname  Parameter name
    * @param mf     Scala type for parameter extraction
    * @tparam T Supported
    *             - primitives (String, Int, Bool, etc)
    *             - Seq(), List(), Set(), Map[String, _]
    *             - complex FHIR types with case classes
    * @return
    */
  @throws[MappingException]
  def extractParam[T](pname:String)(implicit mf:Manifest[T]):Option[T] = {
    operationRequestParams
      .get(pname)
      .map(_.extract[T])
  }

  /**
    * Retrieve the operation parameter as Json value
    * @param pname Parameter name
    * @return
    */
  def getParam(pname:String):Option[JValue] = operationRequestParams.get(pname)
}

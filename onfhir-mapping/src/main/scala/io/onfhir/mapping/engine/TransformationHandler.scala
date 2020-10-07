package io.onfhir.mapping.engine

import java.lang.reflect.InvocationTargetException
import java.util.UUID

import io.onfhir.mapping.model.StructureMappingException
import io.onfhir.path.{FhirPathEvaluator, FhirPathException}
import io.onfhir.util.JsonFormatter.formats
import org.json4s.JsonAST.{JArray, JDouble, JInt, JObject, JString, JValue}

object TransformationHandler {

  /**
   * Apply FHIR mapping transformation function on evaluated source values with given parameters
   * @param sources         Evaluate source values
   * @param transformFunc   Transformation function e.g. copy, evaluate
   * @param parameters      Resolved parameter values
   * @return
   */
  def transform(sources:Seq[JValue], transformFunc:String, parameters:Seq[JValue]):Seq[JValue] = {
    try {
      val method = TransformationHandler.getClass.getMethod(transformFunc, classOf[Seq[JValue]], classOf[Seq[JValue]])
      val result = method.invoke(this, sources, parameters).asInstanceOf[Seq[JValue]]
      result
    }catch {
      case n:NoSuchMethodException =>
        throw new StructureMappingException(s"Invalid transformation call, function $transformFunc does not exist!!!")
      case ite:InvocationTargetException =>
        ite.getTargetException match {
          case fpe:FhirPathException => throw fpe
          case e:Throwable => throw new StructureMappingException(s"Invalid function call $transformFunc!")
        }
    }
  }

  /**
   * FHIR mapping create transform operation
   * @param source
   * @param parameters
   * @return
   */
  private def create(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    if(parameters.length != 1 || !parameters.head.isInstanceOf[JString])
      throw new StructureMappingException("Transform operation 'create' takes only a single parameter which should indicate a data type!")
    Seq(JObject())
  }

  /**
   * FHIR mapping copy transform operation
   * @param source
   * @param parameters
   * @return
   */
  private def copy(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue]= {
    if(source.length != 1)
      throw new StructureMappingException("Transform operation 'copy' needs a single source value as input!")
    if(parameters.nonEmpty)
      throw new StructureMappingException("Transform operation 'copy' does not take any argument!")
    source
  }

  /**
   * FHIR mapping truncate transform operation
   * @param source
   * @param parameters
   * @return
   */
  private def truncate(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue]= {
    if(source.length != 1)
      throw new StructureMappingException("Transform operation 'truncate' needs a single source value as input!")
    if(parameters.length != 1 || !parameters.head.isInstanceOf[JInt])
      throw new StructureMappingException("Transform operation 'truncate' should take a single integer argument!")
    val length = parameters.head.extract[Int]
    if(!source.isInstanceOf[JString])
      throw new StructureMappingException("Transform operation 'truncate' only can be used on stringy source values!")

    Seq(JString(source.head.extract[String].slice(0, length)))
  }

  /**
   * FHIR mapping uuid transform function
   * @param source
   * @param parameters
   * @return
   */
  private def uuid(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue]= {
    if(parameters.nonEmpty)
      throw new StructureMappingException("Transform operation 'uuid' does not take any argument!")
    Seq(JString(UUID.randomUUID().toString))
  }

  /**
   * FHIR mapping append transform function
   * @param source
   * @param parameters
   * @return
   */
  private def append(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    if(source.isEmpty)
      throw new StructureMappingException("Invalid source input for transform operation 'append'!")

    if(source.forall(_.isInstanceOf[JString]))
      Seq(JString(source.map(_.extract[String]).reduce((s1,s2) => s1 + s2)))
    else
      source
  }

  /**
   * FHIR mapping evaluate transform operation
   * @param source
   * @param parameters
   * @return
   */
  private def evaluate(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue]= {
    if(source.length != 1)
      throw new StructureMappingException("Transform operation 'evaluate' needs a single source value as input!")

    if(parameters.length != 1 || !parameters.head.isInstanceOf[JString])
      throw new StructureMappingException("Transform operation 'evaluate' should take a single string argument which should be a FHIR Path expression!")

    val fhirPath = parameters.head.extract[String]

    val results = FhirPathEvaluator().evaluate(fhirPath, source.head).map(_.toJson)
    results
  }

  private def translate(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    throw new NotImplementedError()
  }

  /**
   * FHIR mapping cc operation (Create CodeableConcept)
   * @param source
   * @param parameters
   * @return
   */
  private def cc(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
     val result = parameters.length match {
        case 1 if parameters.head.isInstanceOf[JString] =>
          JObject("text" -> parameters.head)
        case 2 if parameters.forall(_.isInstanceOf[JString]) =>
          JObject("coding" -> JArray(List(JObject(
            "system" -> parameters.head,
            "code" -> parameters.last
          ))))
        case 3 if parameters.forall(_.isInstanceOf[JString]) =>
          JObject("coding" -> JArray(List(JObject(
            "system" ->parameters.head,
            "code" -> parameters.apply(1),
            "display" -> parameters.last
          ))))
        case _ =>
          throw new StructureMappingException("Invalid parameters for transform operation 'cc'!")
      }
    Seq(result)
  }

  /**
   * FHIR mapping c operation (Create Coding)
   * @param source
   * @param parameters
   * @return
   */
  private def c(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    val result = parameters.length match {
        case 2 if parameters.forall(_.isInstanceOf[JString]) =>
          JObject(
            "system" -> parameters.head,
            "code" -> parameters.last
          )
        case 3 if parameters.forall(_.isInstanceOf[JString]) =>
          JObject(
            "system" -> parameters.head,
            "code" -> parameters.apply(1),
            "display" -> parameters.last
          )
        case _ =>
          throw new StructureMappingException("Invalid parameters for transform operation 'c'!")
      }
    Seq(result)
  }

  /**
   * FHIR mapping qty operation (Create Quantity)
   * @param source
   * @param parameters
   * @return
   */
  private def qty(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
      val result = parameters.length match {
        case 2 if parameters.head.isInstanceOf[JDouble] && parameters.last.isInstanceOf[JString] =>
          JObject(
            "value" -> parameters.head,
            "unit" -> parameters.last
          )
        case 4 if parameters.head.isInstanceOf[JDouble] && parameters.tail.forall(_.isInstanceOf[JString]) =>
          JObject(
            "value" -> parameters.head,
            "unit" -> parameters.apply(1),
            "system" -> parameters.apply(2),
            "code" -> parameters.last
          )
        case _ =>
          throw new StructureMappingException("Invalid parameters for transform operation 'qty'!")
      }
    Seq(result)
  }

  /**
   *
   * @param source
   * @param parameters
   * @return
   */
  private def id(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {

      val result = parameters.length match {
        case 2 if parameters.forall(_.isInstanceOf[JString])  =>
          JObject(
            "system" -> parameters.head,
            "value" -> parameters.last
          )
        case 3 if parameters.forall(_.isInstanceOf[JString])  =>
          JObject(
            "system" -> parameters.head,
            "value" -> parameters.apply(1),
            "type" ->
              JObject("coding" -> JArray(List(JObject(
                "system" -> JString("http://terminology.hl7.org/CodeSystem/v2-0203"),
                "code" -> parameters.last
              ))))
          )
        case _ =>
          throw new StructureMappingException("Invalid parameters for transform operation 'id'!")
      }
    Seq(result)
  }

  private def escape(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    throw new NotImplementedError()
  }

  private def cast(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue]= {
    throw new NotImplementedError()
  }



  private def reference(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    throw new NotImplementedError()
  }

  private def dateOp(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    throw new NotImplementedError()
  }

  private def pointer(source:Seq[JValue], parameters:Seq[JValue]):Seq[JValue] = {
    throw new NotImplementedError()
  }
}

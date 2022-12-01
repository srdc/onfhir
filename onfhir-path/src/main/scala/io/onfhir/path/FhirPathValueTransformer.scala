package io.onfhir.path

import org.json4s.JValue
import org.json4s.JsonAST._
import io.onfhir.util.JsonFormatter._

import scala.util.Try

object FhirPathValueTransformer {

  /**
    * Transform a JValue to FhirPathResult
    * @param v
    * @return
    */
  def transform(v:JValue, isContentFhir:Boolean = true):Seq[FhirPathResult] = {
    v match {
      case JArray(arr) => arr.flatMap(i => transform(i, isContentFhir))
      case jobj:JObject => Seq(FhirPathComplex(jobj))
      case JInt(i) => Seq(FhirPathNumber(BigDecimal(i)))
      case JDouble(num) => Seq(FhirPathNumber(num))
      case JDecimal(num) => Seq(FhirPathNumber(num.toDouble))
      case JLong(num) => Seq(FhirPathNumber(num.toDouble))
      case JString(s) if isContentFhir && s.headOption.exists(_.isDigit)  => Seq(resolveFromString(s))
      case JString(s) => Seq(FhirPathString(s))
      case JBool(b) => Seq(FhirPathBoolean(b))
      case _ => Nil
    }
  }

  private def resolveFromString(str:String):FhirPathResult = {
    Try(FhirPathLiteralEvaluator.parseFhirDateTimeBest(str)).toOption
      .map(FhirPathDateTime)
      .getOrElse(
        //If it seems to be a FHIR time, try to parse it
        if(
          (str.length == 5 && str.apply(2) == ':') ||
            (str.length == 8 && str.apply(2) == ':' && str.apply(5) == ':') ||
              (str.length > 9 && str.apply(9) == '.' && str.length < 13 )
            )
          Try(FhirPathLiteralEvaluator.parseFhirTime(str)).toOption
            .map(t => FhirPathTime(t._1, t._2))
            .getOrElse(FhirPathString(str))
        else
          FhirPathString(str)
       )
  }


  def serializeToJson(result:Seq[FhirPathResult]):JValue = {
    val jsonValues = result.map(_.toJson)
    jsonValues.length match {
      case 0 => JNull
      case 1 => jsonValues.head
      case _ => JArray(jsonValues.toList)
    }
  }
}

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
  def transform(v:JValue):Seq[FhirPathResult] = {
    v match {
      case JArray(arr) => arr.flatMap(transform)
      case jobj:JObject => Seq(FhirPathComplex(jobj))
      case jint:JInt => Seq(FhirPathNumber(jint.extract[Int]))
      case JDouble(num) => Seq(FhirPathNumber(num))
      case JDecimal(num) => Seq(FhirPathNumber(num.toDouble))
      case JLong(num) => Seq(FhirPathNumber(num.toDouble))
      case jstr:JString =>
        Seq(
          if(jstr.s.head.isDigit)
            resolveFromString(jstr.s)
          else
            FhirPathString(jstr.s)
        )
      case jb: JBool => Seq(FhirPathBoolean(jb.value))
      case _ => Nil
    }
  }

  private def resolveFromString(str:String):FhirPathResult = {
    Try(FhirPathLiteralEvaluator.parseFhirDateTimeBest(str)).toOption
      .map(FhirPathDateTime)
      .getOrElse(
        //If it seems to be a FHIR time, try to parse it
        if(str.count(_ == ':') == 3)
          Try(FhirPathLiteralEvaluator.parseFhirTime("T"+str)).toOption
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

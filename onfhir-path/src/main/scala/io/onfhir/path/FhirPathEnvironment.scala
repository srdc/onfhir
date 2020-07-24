package io.onfhir.path

import io.onfhir.api.validation.IReferenceResolver

import scala.util.matching.Regex

class FhirPathEnvironment(val _this:FhirPathResult, val referenceResolver: Option[IReferenceResolver]) {
  val vsPattern:Regex = "'vs-[A-Za-z0-9\\-]+'".r
  val extPattern:Regex = "'ext-[A-Za-z0-9\\-]+'".r

  //Resource that is validated
  lazy val resolvedResource =
    referenceResolver
      .map(_.resource).toSeq
      .flatMap(FhirPathValueTransformer.transform(_))

  def getEnvironmentContext(ename:String):Seq[FhirPathResult] = {
    ename match {
      //Trying to access to the root resource validated
      case "%resource" => resolvedResource
      case "%context"  => Seq(_this)
      //Fixed codes
      case "%ucum" => Seq(FhirPathString("http://unitsofmeasure.org"))
      case "%sct" => Seq(FhirPathString("http://snomed.info/sct"))
      case "%loinc" => Seq(FhirPathString("http://loinc.org"))
      case vsPattern(vs) => throw new Exception("Context like 'vs-[valueset]' not implement yet!")
      case extPattern(ext) => throw new Exception("Context like 'ext-[extension]' not implement yet!")
      case oth => sys.env.get(oth.drop(1)).map(v => FhirPathString(v)).toSeq
    }
  }
}

package io.onfhir.path

import io.onfhir.api.validation.IReferenceResolver

import scala.collection.mutable
import scala.util.matching.Regex

/**
 * Fhir Path Engine environment variable store
 * @param _this                 The main context (resource) that FHIR path is evaluated on
 * @param referenceResolver     onFhir reference resolver
 * @param environmentVariables  Other external variables supplied
 */
class FhirPathEnvironment(
                           val _this:Seq[FhirPathResult],
                           val referenceResolver: Option[IReferenceResolver],
                            val environmentVariables:Map[String, Seq[FhirPathResult]] = Map.empty[String, Seq[FhirPathResult]]) {
  val vsPattern:Regex = "'vs-[A-Za-z0-9\\-]+'".r
  val extPattern:Regex = "'ext-[A-Za-z0-9\\-]+'".r

  /**
   * Paths indicated (element name -> array index if exist) by the given FHIR Path expression (path expression)
   * Note: There may be multiple paths due to arrays which corresponds to outer Seq
   * e.g. Observation.code.coding.where(system ='http://loinc.org').code --> Seq(Seq(code -> None, coding-> Some(2), code-> None))
   */
  var foundPaths:Seq[String] = Seq("")
  var lastElementWasArrayEvaluation = false

  //Resource that is validated
  lazy val resolvedResource =
    referenceResolver
      .map(_.resource).toSeq
      .flatMap(FhirPathValueTransformer.transform(_))

  def getEnvironmentContext(ename:String):Seq[FhirPathResult] = {
    ename match {
      //Trying to access to the root resource validated
      case "%resource" => resolvedResource
      case "%context"  => _this
      //Fixed codes
      case "%ucum" => Seq(FhirPathString("http://unitsofmeasure.org"))
      case "%sct" => Seq(FhirPathString("http://snomed.info/sct"))
      case "%loinc" => Seq(FhirPathString("http://loinc.org"))

      case vsPattern(vs) => throw new Exception("Context like 'vs-[valueset]' not implement yet!")
      case extPattern(ext) => throw new Exception("Context like 'ext-[extension]' not implement yet!")
      //If such a environment variable is supplied
      case sv if environmentVariables.isDefinedAt(sv) => environmentVariables(sv)
      //Otherwise check System Environment Variables
      case oth => sys.env.get(oth.drop(1)).map(v => FhirPathString(v)).toSeq
    }
  }
}

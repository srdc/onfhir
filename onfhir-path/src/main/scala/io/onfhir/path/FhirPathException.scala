package io.onfhir.path

class FhirPathException(msg:String, t:Option[Throwable] = None) extends Exception(msg, t.orNull) {

}

object FhirPathException {
  def apply(msg: String): FhirPathException = new FhirPathException(msg)

  def apply(msg: String, t:Throwable): FhirPathException = new FhirPathException(msg,Some(t))
}
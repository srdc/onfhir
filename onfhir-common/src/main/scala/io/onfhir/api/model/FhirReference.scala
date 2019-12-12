package io.onfhir.api.model

abstract class FhirReference {
  def getReference():String
}

/**
 * FHIR Literal reference
 * @param url       Root URL of the server
 * @param rtype     Resource type
 * @param rid       Resorce id
 * @param version   Version id of resource
 */
case class FhirLiteralReference(url:Option[String], rtype:String, rid:String, version:Option[String]) extends FhirReference {
  def getReference():String = s"${url.getOrElse("")}$rtype/$rid${version.map(v => "/_history/"+v).getOrElse("")}"
}

/**
 * FHIR Logical reference
 * @param rtype        Resource type
 * @param system       System id for identifier
 * @param identifier   Value of identifier
 */
case class FhirLogicalReference(rtype:Option[String], system:Option[String], identifier:String) extends FhirReference {
  def getReference():String = s"${rtype.getOrElse("")}/[${system.map(s => "system: "+s + ", ")}identifier: $identifier]"
}

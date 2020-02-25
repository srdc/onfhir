package io.onfhir.util
import io.onfhir.api.Resource
import io.onfhir.api.util.FHIRUtil
import org.json4s.JsonAST.JObject
import org.json4s.Xml.{toJson, toXml}

import scala.xml.{Elem, MetaData, NamespaceBinding, Node}
object XmlFormatter {
  val prettyPrinter = new scala.xml.PrettyPrinter(80, 4)

  class XmlParsable(xmlStr:String){
    def parseXML: Resource = {
      val parsedXml = scala.xml.XML.loadString(xmlStr)
      toJson(parsedXml).asInstanceOf[JObject]
    }
  }

  class XmlConvertable(resource:Resource) {

    def toXml: String = {
      val rtype =  FHIRUtil.extractValue[String](resource, "resourceType")
      val parsedXml = org.json4s.Xml.toXml(resource)
      val root = Elem.apply("", rtype, null, NamespaceBinding.apply("", "http://hl7.org/fhir", null), true, parsedXml:_*)
      root.toString()
    }

    def toPrettyXml:String = {
      val rtype =  FHIRUtil.extractValue[String](resource, "resourceType")
      val parsedXml = org.json4s.Xml.toXml(resource)
      val root = Elem.apply("", rtype, null, NamespaceBinding.apply("", "http://hl7.org/fhir", null), true, parsedXml:_*)

      prettyPrinter.format(root)
    }
  }


  /**
   * Implicit conversion that ties the new JsonParsable class to the Scala Strings
   */
  implicit def parseFromXml(string: String):XmlParsable = new XmlParsable(string)

  /**
   * Implicit conversion that ties the new JsonConvertable class to Scala LinkedHashMaps
   */
  implicit def convertToXml(resource:Resource):XmlConvertable = new XmlConvertable(resource)

}

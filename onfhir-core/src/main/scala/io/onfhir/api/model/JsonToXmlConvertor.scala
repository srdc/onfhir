package io.onfhir.api.model

import io.onfhir.api.{FHIR_COMMON_FIELDS, Resource}
import io.onfhir.exception.UnprocessableEntityException
import org.json4s.JsonAST.{JArray, JBool, JDecimal, JDouble, JInt, JNothing, JNull, JObject, JString, JValue}
import org.json4s.JsonDSL._
import scala.language.implicitConversions
import scala.xml.{Attribute, Elem, MetaData, NamespaceBinding, Node, Null, PrettyPrinter, Text}
import io.onfhir.util.JsonFormatter.formats
class JsonToXmlConvertor {
  var prettyPrinter: PrettyPrinter = new PrettyPrinter(10, 5)
  /**
   * Convert JSON representation to XML
   * @param resource
   * @return
   */
  def convertResourceToXml(resource: Resource):Elem = {
    try {
      //Get the resource type
      val resourceType = resource.findField(_._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE).get._2.extract[String]
      //Parse the children
      val children = convertJObjectToXml(resource.obj.filterNot(_._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE), resourceType == "Bundle")
      //Construct the root
      val root = Elem.apply(null, resourceType, Null, NamespaceBinding.apply(null, "http://hl7.org/fhir", null), true, children: _*)
      root
    }catch {
      case e:Exception =>
        throw new UnprocessableEntityException(Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, FHIRResponse.OUTCOME_CODES.INVALID, None, Some("Problem while converting JSON to XML:" + e.getMessage), Nil)))
    }
  }

  /**
   * Convert objects children to Sequence of nodes
   * @param jobj      JSON object
   * @param isBundle  If this is a bundle or not
   * @return
   */
  def convertJObjectToXml(jobj:JObject, isBundle:Boolean):Seq[Node]  = {
    //Get the extended primitive elements e.g. _birthDate
    val extendedPrimitieveElemMap = jobj.obj.filter(_._1.startsWith("_")).map(e => e._1.drop(1) -> e._2).toMap
    //Only process actual elements
    jobj.obj
      .filterNot(_._1.startsWith("_"))
      .flatMap(field => {
        val result = field match {
          //If we are processing a Bundle and we have a resource
          case ("resource", r:JObject) if isBundle =>
            Seq(Elem.apply(null, "resource", Null, scala.xml.TopScope, true, convertResourceToXml(r)))
          case ("extension", earr:JArray) =>
            earr.arr.map(_.asInstanceOf[JObject]).map(eobj => {
              val extensionChildren = convertJObjectToXml(eobj.obj.filterNot(f => f._1 == FHIR_COMMON_FIELDS.URL || f._1 == FHIR_COMMON_FIELDS.ID), isBundle)
              val idAtttr = eobj.obj.find(_._1 == "id").map(_._2.extract[String]).map(Attribute.apply("", "id", _, Null)).getOrElse(Null)
              val urlAttr = Attribute.apply("", "url", eobj.obj.find(_._1 == "url").map(_._2.extract[String]).get, idAtttr)
              Elem.apply(null, "extension", urlAttr , scala.xml.TopScope, true, extensionChildren:_*)
            })
          case ("div", JString(xhtml)) =>
            val parsedXhtml = scala.xml.XML.loadString(xhtml)
            Seq(parsedXhtml)
          case (fieldName, JArray(arr)) =>
            arr.zipWithIndex.flatMap(i => i._1 match {
              case cobj:JObject =>
                createComplexElement(fieldName, cobj, isBundle)
              case oth:JValue =>
                if(extendedPrimitieveElemMap.contains(fieldName)) {
                  val (idField, children) = extractPrimitiveExtraction(extendedPrimitieveElemMap.apply(fieldName).asInstanceOf[JArray].arr.apply(i._2).asInstanceOf[JObject], isBundle)
                  Some(createExtendedPrimitiveElement(fieldName, extractPrimitiveValue(oth).orNull, idField, children))
                } else
                  extractPrimitiveValue(oth).map(createPrimitiveElement(fieldName, _))
            })
          case (fieldName, cobj:JObject) =>
            Seq(createComplexElement(fieldName, cobj, isBundle))
          //Handle primitives
          case (fieldName, oth) =>
            val value = extractPrimitiveValue(oth)
            if(extendedPrimitieveElemMap.contains(fieldName)) {
              val (idField, children) = extractPrimitiveExtraction(extendedPrimitieveElemMap.apply(fieldName).asInstanceOf[JObject], isBundle)
              Seq(createExtendedPrimitiveElement(fieldName, value.orNull, idField, children))
            } else
              value.map(createPrimitiveElement(fieldName, _)).getOrElse(Nil)
        }
        result
      })
  }

  /**
   * Extract a primitive value as XML Text
   * @param value
   * @return
   */
  private def extractPrimitiveValue(value:JValue):Option[Text] = {
    value match {
      case JString(s) => Some(Text(s))
      case JInt(i) => Some(Text(i.toString()))
      case JDecimal(dec) => Some(Text(dec.toString()))
      case JDouble(db) => Some(Text(db.toString()))
      case JBool(b) => Some(Text(b.toString()))
      case JNull => None
      case JNothing => None
      case _ => None
    }
  }

  /**
   * Extract
   * @param obj
   * @param isBundle
   * @return
   */
  private def extractPrimitiveExtraction(obj:JObject, isBundle:Boolean):(Option[String], Seq[Node]) = {
    val idField = obj.obj.find(_._1 == FHIR_COMMON_FIELDS.ID).map(_._2.extract[String])
    val remaining = obj.obj.filterNot(_._1 == FHIR_COMMON_FIELDS.ID)
    idField -> convertJObjectToXml(remaining, isBundle)
  }

  private def createComplexElement(name:String, obj:JObject, isBundle:Boolean):Elem = {
    val idAtttr = obj.obj.find(_._1 == "id").map(_._2.extract[String]).map(Attribute.apply("", "id", _, Null)).getOrElse(Null)
    val childNodes = convertJObjectToXml(obj.obj.filterNot(_._1 == FHIR_COMMON_FIELDS.ID), isBundle)
    val complex = Elem.apply(null, name, idAtttr, scala.xml.TopScope, true, childNodes:_*)
    complex
  }

  private def createPrimitiveElement(name:String, value:Text):Elem = {
    val x = Elem.apply(null, name, createValueAttribute(value), scala.xml.TopScope, true)
    x
  }

  private def createExtendedPrimitiveElement(name:String, value:Text, id:Option[String], chidren:Seq[Node]) = {

    val attribute = id match {
      case None => createValueAttribute(value)
      case Some(idAttr) =>  Attribute.apply(null, "id", idAttr, createValueAttribute(value))
    }
    Elem.apply(null, name, attribute, scala.xml.TopScope, true, chidren:_*)
  }

  private def createValueAttribute(value:Text):MetaData = {
    val attr =
      if(value == null)
        Null
      else
        Attribute.apply("value", value, Null)
    attr
  }


  class XmlConvertable(resource:Resource) {

    def toXml: String = {
      convertResourceToXml(resource).toString()
    }

    def toPrettyXml:String = {
      val xml = convertResourceToXml(resource)
      prettyPrinter.format(xml)
    }
  }

  /**
   * Implicit conversion that ties the new JsonConvertable class to Scala LinkedHashMaps
   */
  implicit def convertToXml(resource:Resource):XmlConvertable = new XmlConvertable(resource)
}

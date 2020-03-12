package io.onfhir.api.model

import java.io.Reader

import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_DATA_TYPES, Resource}
import io.onfhir.api.util.{BaseFhirProfileHandler, FHIRUtil}
import io.onfhir.api.validation.ProfileRestrictions
import io.onfhir.config.FhirConfig
import io.onfhir.exception.UnprocessableEntityException
import org.json4s.JsonAST.{JArray, JBool, JDecimal, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue}
import org.json4s.JsonDSL._
import org.slf4j.{Logger, LoggerFactory}

import scala.language.implicitConversions
import scala.util.Try
import scala.xml.{Attribute, Elem, NamespaceBinding, Node, NodeSeq, PrettyPrinter}
import io.onfhir.util.JsonFormatter.formats
/**
 * Class that handles conversion of FHIR XML representation to FHIR JSON and back
 * @param fhirConfig
 */
class XmlFormatter(fhirConfig: FhirConfig) extends BaseFhirProfileHandler(fhirConfig) {
  val logger:Logger = LoggerFactory.getLogger(this.getClass)
  var prettyPrinter:PrettyPrinter = new PrettyPrinter(3, 2)

  /**
   * Convert a FHIR resource serialized in XML into JSON
   * @param parsedXml   XML Parsed
   * @return
   */
  def convertResourceToJson(parsedXml:Elem):Resource = {
    try {
      if (!fhirConfig.FHIR_RESOURCE_TYPES.contains(parsedXml.label))
        throw new UnprocessableEntityException(Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, FHIRResponse.OUTCOME_CODES.INVALID, None, Some(s"Invalid resource type ${parsedXml.label} in XML format!"), Seq("resourceType"))))

      val baseProfile: ProfileRestrictions = fhirConfig.getBaseProfile(parsedXml.label)
      val baseProfileChain = fhirConfig.findProfileChain(baseProfile.url)

      val result = ("resourceType" -> parsedXml.label) ~
        convertNodeGroupToJson(parsedXml, baseProfileChain)

      result
    }catch {
      case u:UnprocessableEntityException =>
        throw u
      case e:Exception =>
        throw new UnprocessableEntityException(Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, FHIRResponse.OUTCOME_CODES.INVALID, Some("Unknown Problem while parsing the XML"), None, Nil)))
    }
  }

  /**
   * Convert an XML Node to JValue
   * @param node          XML Node itself
   * @param profileChain  Profile chain for this resource type
   * @param path          JSON Path to the node
   * @return
   */
  private def convertNodeToJson(node:Node, profileChain:Seq[ProfileRestrictions], path:String):JValue = {
    //If an element has id field, make it a JSON field
    val idField = node.attribute("id").map(id => JField("id", JString(id.head.text)))
    node.label match {
      //Extension is serialized in a special way
      case "extension" =>
        val urlElem = node.attribute("url") match {
          case Some(Seq(n)) =>
            JField("url", JString(n.text))
          case None => JField("url", JNull)
        }
        if(idField.isDefined)
          idField.get ~ urlElem ~ convertNodeGroupToJson(node, profileChain, Some(path))
        else
          urlElem ~ convertNodeGroupToJson(node, profileChain, Some(path))
      //div is special, so we embed the content
      case "div" => JString(node.toString())
      case _ =>
        node.attribute("value") match {
          //If it has a 'value' attribute it should be primitive
          case Some(Seq(v)) =>
            //Find the target type
            findTargetTypeOfPath(path, profileChain) match {
              //if not found, convert it to JString
              case None => JString(v.text)
              //Otherwise convert accordingly
              case Some((dt , _)) =>
                convertPrimitive(dt, v.text)
            }
          //If there is no attribute, it should be a complex
          case None =>
            if(idField.isDefined)
              idField.get ~ convertNodeGroupToJson(node, profileChain, Some(path))
            else
              convertNodeGroupToJson(node, profileChain, Some(path))
        }
    }
  }

  /**
   * Convert a FHIR primitive value to JValue
   * @param fhirDataType    FHIR Data type for the value
   * @param value           Value in string representation
   * @return
   */
  private def convertPrimitive(fhirDataType:String, value:String):JValue  = {
    fhirDataType match {
      case FHIR_DATA_TYPES.INTEGER | FHIR_DATA_TYPES.UNSIGNEDINT | FHIR_DATA_TYPES.POSITIVEINT =>
        Try(Integer.parseInt(value)).toOption.map(i => JInt(i)).getOrElse(JString(value))
      case FHIR_DATA_TYPES.DECIMAL =>
        Try(value.toDouble).toOption
          .map(JDouble)
          .getOrElse(
            Try(BigDecimal.apply(value)).toOption.map(JDecimal).getOrElse(JString(value))
          )
      case FHIR_DATA_TYPES.BOOLEAN =>
        Try(value.toBoolean).toOption.map(JBool.apply).getOrElse(JString(value))
      //Otherwise map it to JString
      case _ =>
        JString(value)
    }
  }

  /**
   * Convert all children in a node to JObject
   * @param parent          The parent node
   * @param profileChain    Profile chain for resource type
   * @param parentPath      JSON Path to the parent
   * @return
   */
  private def convertNodeGroupToJson(parent:Node, profileChain:Seq[ProfileRestrictions], parentPath:Option[String] = None):JObject = {
    val elements =
      parent
        .nonEmptyChildren.filter(_.isInstanceOf[Elem])
        .zipWithIndex
        .map(n => n._1.label -> n)
        .groupBy(_._1)
        .map(g => g._1 -> g._2.map(_._2)).toSeq.sortWith((g1, g2) => g1._2.head._2 < g2._2.head._2)
        .map(g => g._1 -> g._2.map(_._1))

    JObject(
      elements.flatMap(elem => convertElementToJson(elem, profileChain, parentPath)).toList
    )
  }

  /**
   * Convert an element to JField
   * @param elem          Element name and values
   * @param profileChain  Profile chain for resource type
   * @param parentPath    JSON Path to the parent node
   * @return
   */
  private def convertElementToJson(elem:(String, Seq[Node]), profileChain:Seq[ProfileRestrictions], parentPath:Option[String] = None):Seq[JField] = {
    val path = FHIRUtil.mergeElementPath(parentPath, elem._1)
    var isArray = true
    val mainField = JField(
      elem._1,
      elem._2 match {
        case Seq(single:Node) =>
          findPathCardinality(path, profileChain) match {
            case true => JArray(List(convertNodeToJson(single, profileChain, path)))
            case false =>
              isArray = false
              convertNodeToJson(single, profileChain, path)
          }
        //If there is more than one element given then it is definitely an array
        case mult: Seq[Node] =>
          JArray(
            mult.map(n => convertNodeToJson(n, profileChain, path)).toList
          )
      }
    )

    val extendedPrimitiveValues =
      elem._2.map(p =>  (p.attribute("id"), p.attribute("value")) match {
        //If it is a FHIR primitive with extension; has a value and has some children
        case (Some(Seq(id)),Some(Seq(_))) if p.child.nonEmpty =>
          JField(FHIR_COMMON_FIELDS.ID, id.text) ~ convertNodeGroupToJson(p, Seq(fhirConfig.getBaseProfile("Element")), None)
        case (None, Some(Seq(_))) if p.child.nonEmpty =>
          convertNodeGroupToJson(p, Seq(fhirConfig.getBaseProfile("Element")), None)
        //Otherwise
        case _ => JNull
      })


    if(extendedPrimitiveValues.exists(_ != JNull)){
      val extendedField =
        JField(s"_${elem._1}",
          if(isArray || extendedPrimitiveValues.length > 1)
            JArray(extendedPrimitiveValues.toList)
          else
            extendedPrimitiveValues.head
        )
      Seq(mainField,extendedField)
    } else
      Seq(mainField)
  }

  class XmlParsable(xmlStr:String){
    def parseXML: Resource = {
      val parsedXml = scala.xml.XML.loadString(xmlStr)
      convertResourceToJson(parsedXml)
    }
  }

  class XmlParsable2(reader:Reader) {
    def parseXML: Resource = {
      val parsedXml = scala.xml.XML.load(reader)
      convertResourceToJson(parsedXml)
    }
  }

  def convertResourceToXml(resource: Resource):Elem = {
    val resourceType = resource.findField(_._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE).get._2.extract[String]

    val children = convertJObjectToXml(resource.removeField(_._1 == FHIR_COMMON_FIELDS.RESOURCE_TYPE).asInstanceOf[JObject])

    createComplexElement(resourceType, children)
  }

  def convertJObjectToXml(jobj:JObject):Seq[Node] = {
    val extendedPrimitieveElemMap = jobj.obj.filter(_._1.startsWith("_")).map(e => e._1.drop(1) -> e._2).toMap

    jobj.obj
      .filterNot(_._1.startsWith("_"))
      .flatMap(field => field match {
        case ("extension", earr:JArray) =>
          earr.arr.map(_.asInstanceOf[JObject]).map(eobj => {
            val extensionChildren = convertJObjectToXml(eobj)
            val urlAttr = Attribute.apply("", "url", eobj.obj.find(_._1 == "url").map(_._2.extract[String]).get, null)
            Elem.apply("", "extension",urlAttr , null, true,extensionChildren:_*)
          })
        case ("div", JString(xhtml)) =>
          val parsedXhtml = scala.xml.XML.loadString(xhtml)
          Seq(createComplexElement("div", Seq(parsedXhtml)))
        case (fieldName, JArray(arr)) =>
          arr.zipWithIndex.map(i => i._1 match {
            case JString || JBool || JInt || JDecimal || JDouble =>
              if(extendedPrimitieveElemMap.contains(fieldName)) {
                extendedPrimitieveElemMap.apply(fieldName).asInstanceOf[JArray].arr.apply(i._2)
              } else
                createPrimitiveElement(fieldName, i._1.extract[String])
            case cobj:JObject =>
              createComplexElement(fieldName, convertJObjectToXml(cobj))
          })
        case (fieldName, cobj:JObject) =>
          Seq(createComplexElement(fieldName, convertJObjectToXml(cobj)))
        //Handle primitives
        case (fieldName, JString(s)) =>
          Seq(createPrimitiveElement(fieldName,s))
        case (fieldName, JInt(i)) =>
          Seq(createPrimitiveElement(fieldName, i.toString()))
        case (fieldName, JDecimal(dec)) =>
          Seq(createPrimitiveElement(fieldName, dec.toString()))
        case (fieldName, JDouble(db)) =>
          Seq(createPrimitiveElement(fieldName,db.toString()))
        case (fieldName, JBool(b)) =>
          Seq(createPrimitiveElement(fieldName, b.toString()))
    })
  }


  private def extractPrimitiveExtraction(obj:JObject):(Option[String], Seq[Node]) = {
    val idField = obj.obj.find(_._1 == FHIR_COMMON_FIELDS.ID).map(_._2.extract[String])
    val remaining = obj.obj.filterNot(_._1 != FHIR_COMMON_FIELDS.ID)
    convertJObjectToXml(remaining)
  }

  private def createComplexElement(name:String, childNodes:Seq[Node]):Elem = {
    Elem.apply("", name, null, null, true, childNodes:_*)
  }

  private def createPrimitiveElement(name:String, value:String):Elem = {
    Elem.apply("", name, createValueAttribute(value), null, true, null)
  }

  private def createValueAttribute(value:String):Attribute = {
    Attribute.apply("", "value", value, null)
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

  implicit def parseFromXml(reader: Reader):XmlParsable2 = new XmlParsable2(reader)

  /**
   * Implicit conversion that ties the new JsonConvertable class to Scala LinkedHashMaps
   */
  implicit def convertToXml(resource:Resource):XmlConvertable = new XmlConvertable(resource)

}

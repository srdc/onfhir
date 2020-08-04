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
import scala.xml.{Elem, Node}

/**
 * Class that handles conversion of FHIR XML representation to FHIR JSON and back
 * @param fhirConfig
 */
class XmlToJsonConvertor(fhirConfig: FhirConfig) extends BaseFhirProfileHandler(fhirConfig) {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)


  /**
   * Convert a FHIR resource serialized in XML into JSON
   *
   * @param parsedXml XML Parsed
   * @return
   */
  def convertResourceToJson(parsedXml: Elem): Resource = {
    try {
      if (!fhirConfig.FHIR_RESOURCE_TYPES.contains(parsedXml.label))
        throw new UnprocessableEntityException(Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, FHIRResponse.OUTCOME_CODES.INVALID, None, Some(s"Invalid resource type ${parsedXml.label} in XML format!"), Seq("resourceType"))))

      val baseProfile: ProfileRestrictions = fhirConfig.getBaseProfile(parsedXml.label)
      val baseProfileChain = fhirConfig.findProfileChain(baseProfile.url)

      val result = ("resourceType" -> parsedXml.label) ~
        convertNodeGroupToJson(parsedXml, baseProfileChain)

      result
    } catch {
      case u: UnprocessableEntityException =>
        throw u
      case e: Exception =>
        throw new UnprocessableEntityException(Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, FHIRResponse.OUTCOME_CODES.INVALID, Some("Unknown Problem while parsing the XML"), None, Nil)))
    }
  }

  /**
   * Convert an XML Node to JValue
   *
   * @param node         XML Node itself
   * @param profileChain Profile chain for this resource type
   * @param path         JSON Path to the node
   * @return
   */
  private def convertNodeToJson(node: Node, profileChain: Seq[ProfileRestrictions], path: String): JValue = {
    //If an element has id field, make it a JSON field
    val idField = node.attribute("id").map(id => JField("id", JString(id.head.text)))
    node.label match {
      case "resource" if profileChain.head.url == fhirConfig.getBaseProfile("Bundle").url =>
        val children = node.nonEmptyChildren.filter(_.isInstanceOf[Elem])
        if (children.length != 1 || !children.head.isInstanceOf[Elem])
          throw new UnprocessableEntityException(Seq(OutcomeIssue(FHIRResponse.SEVERITY_CODES.ERROR, FHIRResponse.OUTCOME_CODES.INVALID, None, Some(s"Invalid resource in Bundle at ${path} in XML format!"), Seq(path))))
        convertResourceToJson(children.head.asInstanceOf[Elem])
      //Extension is serialized in a special way
      case "extension" =>
        val urlElem = node.attribute("url") match {
          case Some(Seq(n)) =>
            JField("url", JString(n.text))
          case None => JField("url", JNull)
        }
        if (idField.isDefined)
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
              case Nil => JString(v.text)
              //Otherwise convert accordingly
              case Seq((_, dt, _, _)) =>
                convertPrimitive(dt, v.text)
              case oth =>
                //TODO
                JNull
            }
          //If there is no attribute, it should be a complex
          case None =>
            if (idField.isDefined)
              idField.get ~ convertNodeGroupToJson(node, profileChain, Some(path))
            else
              convertNodeGroupToJson(node, profileChain, Some(path))
        }
    }
  }

  /**
   * Convert a FHIR primitive value to JValue
   *
   * @param fhirDataType FHIR Data type for the value
   * @param value        Value in string representation
   * @return
   */
  private def convertPrimitive(fhirDataType: String, value: String): JValue = {
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
   *
   * @param parent       The parent node
   * @param profileChain Profile chain for resource type
   * @param parentPath   JSON Path to the parent
   * @return
   */
  private def convertNodeGroupToJson(parent: Node, profileChain: Seq[ProfileRestrictions], parentPath: Option[String] = None): JObject = {
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
   *
   * @param elem         Element name and values
   * @param profileChain Profile chain for resource type
   * @param parentPath   JSON Path to the parent node
   * @return
   */
  private def convertElementToJson(elem: (String, Seq[Node]), profileChain: Seq[ProfileRestrictions], parentPath: Option[String] = None): Seq[JField] = {
    val path = FHIRUtil.mergeElementPath(parentPath, elem._1)
    var isArray = true
    val mainField = JField(
      elem._1,
      elem._2 match {
        case Seq(single: Node) =>
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
      elem._2.map(p => (p.attribute("id"), p.attribute("value")) match {
        //If it is a FHIR primitive with extension; has a value and has some children
        case (Some(Seq(id)), Some(Seq(_))) if p.child.nonEmpty =>
          JField(FHIR_COMMON_FIELDS.ID, id.text) ~ convertNodeGroupToJson(p, Seq(fhirConfig.getBaseProfile("Element")), None)
        case (None, Some(Seq(_))) if p.child.nonEmpty =>
          convertNodeGroupToJson(p, Seq(fhirConfig.getBaseProfile("Element")), None)
        //Otherwise
        case _ => JNull
      })


    if (extendedPrimitiveValues.exists(_ != JNull)) {
      val extendedField =
        JField(s"_${elem._1}",
          if (isArray || extendedPrimitiveValues.length > 1)
            JArray(extendedPrimitiveValues.toList)
          else
            extendedPrimitiveValues.head
        )
      Seq(mainField, extendedField)
    } else
      Seq(mainField)
  }


  class XmlParsable(xmlStr: String) {
    def parseXML: Resource = {
      val parsedXml = scala.xml.XML.loadString(xmlStr)
      convertResourceToJson(parsedXml)
    }
  }

  class XmlParsable2(reader: Reader) {
    def parseXML: Resource = {
      val parsedXml = scala.xml.XML.load(reader)
      convertResourceToJson(parsedXml)
    }
  }

  /**
   * Implicit conversion that ties the new JsonParsable class to the Scala Strings
   */
  implicit def parseFromXml(string: String):XmlParsable = new XmlParsable(string)

  implicit def parseFromXml(reader: Reader):XmlParsable2 = new XmlParsable2(reader)
}



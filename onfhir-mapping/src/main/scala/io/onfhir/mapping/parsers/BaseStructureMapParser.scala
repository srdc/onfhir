package io.onfhir.mapping.parsers

import io.onfhir.api.Resource
import io.onfhir.api.util.FHIRUtil
import io.onfhir.mapping.model.{StructureMap, StructureMapGroup, StructureMapRule, StructureMapSource, StructureMapTarget}
import org.json4s.JsonAST.{JArray, JNothing, JObject}

trait IMappingParser {
  def parseStructureMap(resource:Resource):StructureMap
}

/**
 * Parser used to parse FHIR StructureMap definitions to internal model
 * Can be used for STU3 and R4
 */
class BaseStructureMapParser extends IMappingParser {

  def parseStructureMap(resource:Resource):StructureMap = {
    StructureMap(
      url = FHIRUtil.extractValue[String](resource, "url"),
      imports = FHIRUtil.extractValue[Seq[String]](resource, "import").toSet,
      structureDefs =
        FHIRUtil.extractValueOption[Seq[JObject]](resource, "structure")
          .getOrElse(Nil)
          .map(parseSupportedStructureDefs),
      groups =  FHIRUtil.extractValue[Seq[JObject]](resource, "group")
        .map(parseMappingGroup)
    )
  }

  protected def parseSupportedStructureDefs(el:JObject):(String, String, Option[String]) = {
    (
      FHIRUtil.extractValue[String](el, "mode"),
      FHIRUtil.extractValue[String](el, "url"),
      FHIRUtil.extractValueOption[String](el, "alias")
    )
  }

  protected def parseMappingGroup(group:JObject):StructureMapGroup = {
    val inputs =
      FHIRUtil.extractValue[Seq[JObject]](group, "input")
        .map(parseInput)

    StructureMapGroup(
      name = FHIRUtil.extractValue[String](group, "name"),
      extend = FHIRUtil.extractValueOption[String](group, "extend"),
      sourceInputs = inputs.filter(_._1 == "source").map(i => i._2 -> i._3),
      targetInputs = inputs.filter(_._1 == "target").map(i => i._2 -> i._3),
      rules =
        FHIRUtil.extractValue[Seq[JObject]](group, "rule")
          .map(parseRule)
    )
  }

  protected def parseInput(input:JObject):(String, String, Option[String]) = {
    (
      FHIRUtil.extractValue[String](input, "mode"),
      FHIRUtil.extractValue[String](input, "name"),
      FHIRUtil.extractValueOption[String](input, "type"),
    )
  }

  protected def parseRule(rule:JObject):StructureMapRule = {
    StructureMapRule(
      name = FHIRUtil.extractValue[String](rule, "name"),
      sources =
        FHIRUtil.extractValue[Seq[JObject]](rule, "source")
          .map(parseSource),
      targets = FHIRUtil.extractValue[Seq[JObject]](rule, "target")
        .map(parseTarget),
      childRules = (rule \ "rule") match {
        case JNothing => Nil
        case JArray(arr) => arr.map(_.asInstanceOf[JObject]).map(parseRule)
      },
      dependentRules = (rule \ "dependent") match {
        case JNothing => Nil
        case JArray(arr) => arr.map(_.asInstanceOf[JObject]).map(parseDependentRule)
      }
    )
  }

  protected def parseSource(source:JObject):StructureMapSource = {
    StructureMapSource(
      context = FHIRUtil.extractValue[String](source, "context"),
      min = FHIRUtil.extractValueOption[Int](source, "min"),
      max = FHIRUtil.extractValueOption[String](source, "max").map(m => if(m == "*") -1 else m.toInt),
      _type = FHIRUtil.extractValueOption[String](source, "type"),
      defaultValue = (source \ "defaultValue") match {
        case JNothing => None
        case oth => Some(oth)
      },
      element =  FHIRUtil.extractValueOption[String](source, "element"),
      listMode = FHIRUtil.extractValueOption[String](source, "listMode"),
      variable = FHIRUtil.extractValueOption[String](source, "variable"),
      condition =  FHIRUtil.extractValueOption[String](source, "condition"),
      check =  FHIRUtil.extractValueOption[String](source, "check"),
      logMessage = FHIRUtil.extractValueOption[String](source, "logMessage")
    )
  }

  protected def parseTarget(source:JObject):StructureMapTarget = {
    StructureMapTarget(
      context = FHIRUtil.extractValueOption[String](source, "context"),
      contextType = FHIRUtil.extractValueOption[String](source, "contextType"),
      element = FHIRUtil.extractValueOption[String](source, "element"),
      variable = FHIRUtil.extractValueOption[String](source, "variable"),
      listMode = FHIRUtil.extractValueOption[Seq[String]](source, "listMode").getOrElse(Nil).headOption,
      listRuleId = FHIRUtil.extractValueOption[String](source, "listRuleId"),
      transform = FHIRUtil.extractValueOption[String](source, "transform"),
      parameters = (source \ "parameter") match {
        case JNothing => Nil
        case JArray(arr) => arr.map(_.asInstanceOf[JObject]).map(p => p.findField(_._1.startsWith("value")).map(_._2)).filter(_.isDefined).map(_.get)
      }
    )
  }

  protected def parseDependentRule(dr:JObject):(String, Seq[String]) = {
    FHIRUtil.extractValue[String](dr, "name") ->
      FHIRUtil.extractValue[Seq[String]](dr, "variable")
  }
}

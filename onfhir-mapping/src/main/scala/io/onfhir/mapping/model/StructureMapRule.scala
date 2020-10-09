package io.onfhir.mapping.model

import io.onfhir.path.grammar.FhirPathExprParser
import org.json4s.JsonAST.JValue

case class StructureMapSource(
                               context:String,
                               min:Option[Int],
                               max:Option[Int],
                               _type:Option[String],
                               defaultValue:Option[JValue],
                               element:Option[String],
                               listMode:Option[String],
                               variable:Option[String],
                               condition:Option[String],
                               check:Option[String],
                               logMessage:Option[String]
                             )

case class StructureMapTarget(
                               context:Option[String],
                               contextType:Option[String],
                               element:Option[String],
                               variable:Option[String],
                               listMode:Option[String],
                               listRuleId:Option[String],
                               transform:Option[String],
                               parameters:Seq[JValue]
                             )

/**
 * Represents the rule element in StructureMap
 * @param name
 * @param sources
 * @param targets
 * @param childRules
 * @param dependentRules
 */
case class StructureMapRule(name:String, sources:Seq[StructureMapSource], targets:Seq[StructureMapTarget], childRules:Seq[StructureMapRule], dependentRules:Seq[(String, Seq[String])])

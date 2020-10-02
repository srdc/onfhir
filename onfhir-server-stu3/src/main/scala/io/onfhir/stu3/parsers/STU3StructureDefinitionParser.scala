package io.onfhir.stu3.parsers

import io.onfhir.api.util.FHIRUtil
import io.onfhir.r4.parsers.StructureDefinitionParser
import org.json4s.JsonAST.JObject

class STU3StructureDefinitionParser(fhirComplexTypes:Set[String], fhirPrimitiveTypes:Set[String]) extends StructureDefinitionParser(fhirComplexTypes, fhirPrimitiveTypes){

  override def parseTypeInElemDefinition(typeDef:JObject):(String, Seq[String], Seq[String], Option[String], Seq[String]) = {
    (
      FHIRUtil.extractValue[String](typeDef, "code") match {
        case "http://hl7.org/fhirpath/System.String" => "string" // Some base definitions have these
        case oth => oth
      },
      FHIRUtil.extractValueOption[String](typeDef, "profile").toSeq,
      FHIRUtil.extractValueOption[String](typeDef, "targetProfile").toSeq,
      FHIRUtil.extractValueOption[String](typeDef, "versioning"),
      FHIRUtil.extractValue[Seq[String]](typeDef, "aggregation")
    )
  }
}

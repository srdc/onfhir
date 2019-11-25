package io.onfhir.validation
import io.onfhir.api.Resource
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ValueSetDef, ValueSetRestrictions}
import org.json4s.JsonAST.JObject

object TerminologyParser{

  /**
   * Parse a FHIR value set into a compact form for validation
   * @param valueSetOrCodeSystem FHIR ValueSet or CodeSystem (with an element valueSet inside)
   * @return URL of ValueSet -> Map(URL of CodeSystem -> Set of codes)
   */
  def parseAsValueSet(valueSetOrCodeSystem:Resource):Option[(String, ValueSetRestrictions)] = {
    FHIRUtil.extractValue[String](valueSetOrCodeSystem, "resourceType") match {
      case "ValueSet" => parseValueSet(valueSetOrCodeSystem)
      case "CodeSystem" => parseCodeSystemAsValueSet(valueSetOrCodeSystem)
    }
  }

  /**
   * Parse a Code System and returns a ValueSetRestriction if CodeSystem is used as ValueSet with all codes
   * @param codeSystem
   * @return
   */
  def parseCodeSystemAsValueSet(codeSystem:Resource):Option[(String, ValueSetRestrictions)] = {
    FHIRUtil
      .extractValueOption[String](codeSystem, "valueSet") //If there is valueSet element, this code system with all codes is used as ValueSet
      .flatMap(url =>
        FHIRUtil.extractValueOption[String](codeSystem, "url").map(csUrl =>
          url ->
            ValueSetRestrictions(includes =
              ValueSetDef(Map(csUrl ->
                FHIRUtil.extractValueOptionByPath[Seq[String]](codeSystem, "concept.code").getOrElse(Nil).toSet))
            )
        ))
  }

  /**
   * Parse a ValueSet definition and identify all the codes included or excluded in the ValueSet
   * @param valueSet
   * @return
   */
  private def parseValueSet(valueSet:Resource):Option[(String, ValueSetRestrictions)] ={
    val included = getValueSetDef(valueSet, isIncluded = true)
    val excluded = getValueSetDef(valueSet, isIncluded = false)
    //If there is no direct code, do not put it (TODO handle filters; intensionally defined value sets)
    if(included.isEmpty)
      None
    else {
      FHIRUtil.extractValueOption[String](valueSet, "url")
        .map(url => url -> ValueSetRestrictions(included.head, excluded))
    }
  }

  /**
   * Parse the specific compose part
   * @param valueSet   Given ValueSet content
   * @param isIncluded if true 'include' part, else 'exclude' part
   * @return
   */
  private def getValueSetDef(valueSet:Resource, isIncluded:Boolean):Option[ValueSetDef] = {
    val results =
      FHIRUtil
        .extractValueOptionByPath[Seq[JObject]](valueSet, s"compose.${if(isIncluded) "include" else "exclude"}")
        .getOrElse(Nil)
        .map(inc => {
          val codeLists = FHIRUtil.extractValueOption[String](inc, "system") match {
            case Some(csUrl) =>
              FHIRUtil.extractValueOptionByPath[Seq[String]](inc, "concept.code").getOrElse(Nil) match {
                case Nil => None
                case codes => Some(csUrl -> codes.toSet)
              }
            case None => None
          }
          codeLists -> FHIRUtil.extractValueOption[Seq[String]](inc, "valueSet").getOrElse(Nil)
        })

    val codeSystems = results.flatMap(_._1).toMap
    val valueSets = results.flatMap(_._2).toSet
    if(codeSystems.isEmpty && valueSets.isEmpty)
      None
    else
      Some(ValueSetDef(codeSystems, valueSets))
  }

}

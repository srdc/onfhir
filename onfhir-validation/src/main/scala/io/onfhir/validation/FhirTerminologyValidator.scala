package io.onfhir.validation

import io.onfhir.api.service.{IFhirTerminologyService}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{IFhirTerminologyValidator, ValueSetDef, ValueSetRestrictions}
import io.onfhir.config.{BaseFhirConfig, TerminologyServiceConf}
import io.onfhir.util.JsonFormatter.formats
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{Await, TimeoutException}
import scala.language.postfixOps

/**
 * Validation utility for terminologies (code-binding restrictions)
 * @param fhirConfig            FHIR configurations
 * @param terminologyServices   Integrated terminology services for validation
 */
class FhirTerminologyValidator(fhirConfig:BaseFhirConfig, terminologyServices:Seq[(TerminologyServiceConf, IFhirTerminologyService)]) extends IFhirTerminologyValidator{
  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)
  /**
   * Check if a value set is supported for validation within OnFHir setup
   * @param vsUrl   URL of the value set
   * @param version Version of the value set (If not given, the latest version available is chosen)
   * @return  All codes in value set from each code system within it
   */
  def getAllCodes(vsUrl:String, version:Option[String] = None):Map[String, Set[String]] = {
    getValueSet(vsUrl, version).map(vs => {
      val allMaps:Seq[Map[String, Set[String]]] = vs.includes.valueSets.map(vs => {
        val cr = FHIRUtil.parseCanonicalReference(vs)
        getAllCodes(cr.getUrl(), cr.version)
      }).toSeq ++ Seq(vs.includes.codes)
      allMaps.reduce((m1, m2) => mergeMap(m1,m2))
    }).getOrElse(Map.empty[String, Set[String]])
  }

  private def mergeMap(m1:Map[String, Set[String]], m2:Map[String, Set[String]]) :Map[String, Set[String]] = {
    (m1.keySet ++ m2.keySet).toSeq.map(k =>
       k -> (m1.getOrElse(k, Set.empty[String]) ++ m2.getOrElse(k, Set.empty[String]))
    ).toMap
  }
  /**
   * Check if a given ValueSet is supported for validation in this onFhir setup
   * @param vsUrl   Url of ValueSet
   * @param version Business version of ValueSet
   * @return
   */
  def isValueSetSupported(vsUrl:String, version:Option[String] = None):Boolean = {
    //First check among the integrated terminology services
    findTerminologyService(vsUrl, version).isDefined || //Then internal ValueSet configurations
        getValueSet(vsUrl, version).isDefined
  }

  /**
   * Find the terminology service
   * @param vsUrl     ValueSet url (ValueSet.url)
   * @param version   ValueSet version (ValueSet.version)
   * @return
   */
  private def findTerminologyService(vsUrl:String, version:Option[String] = None):Option[(TerminologyServiceConf, IFhirTerminologyService)] = {
    terminologyServices
      .find {
        case (sconf, _) =>
          sconf.supportedValueSets.contains("*") || //Either all terminology validations are directed to this
            //Or ValueSet url is explicitly listed
            sconf.supportedValueSets
              .get(vsUrl)
              .exists(versions => version.forall(v => versions.forall(_.contains(v)))) ||
            //Or ValueSet url is given as prefix (all ValueSets starting with this prefix e.g. http://aiccelerate.eu/fhir/ValueSet/*
            sconf
              .supportedValueSets.keySet.filter(_.endsWith("/*"))
              .exists(vsPrefix => vsUrl.startsWith(vsPrefix.dropRight(1)))
      }
  }

  /**
   * Validate a given system and code pair against a value set
   * @param vsUrl
   * @param version
   * @param codeSystem
   * @param code
   * @return
   */
  def validateCodeAgainstValueSet(vsUrl:String, version:Option[String], codeSystem:Option[String], code:String):Boolean = {
    findTerminologyService(vsUrl, version) match {
      case Some(sconf -> ts) =>
        validateCodeViaTerminologyService(sconf, ts, vsUrl, version, codeSystem, code)
      //Not found in integrated terminology services
      case None =>
        getValueSet(vsUrl, version) match {
          case None => true
          case Some(vs) =>
            validateCodeBinding(vs, codeSystem, code)
        }
    }
  }

  /**
   *
   * @param sconf
   * @param ts
   * @param vsUrl
   * @param version
   * @param codeSystem
   * @param code
   * @return
   */
  private def validateCodeViaTerminologyService(sconf:TerminologyServiceConf, ts:IFhirTerminologyService, vsUrl:String, version:Option[String], codeSystem:Option[String], code:String):Boolean = {
    try {
      //Call the terminology service's validate-code operation
      val result = Await.result(ts.validateCode(vsUrl, version, code, codeSystem), sconf.timeout)
      val isValid = FHIRUtil.getParameterValueByName(result, "result").exists(_.extract[Boolean])
      isValid
    } catch {
      case t:TimeoutException =>
        logger.error(s"Timeout while using terminology service ${sconf.name} for validation of a code ${codeSystem.getOrElse("")}|$code for ValueSet $vsUrl!")
        false
      case e:Exception =>
        logger.error(s"Timeout while using terminology service ${sconf.name} for validation of a code ${codeSystem.getOrElse("")}|$code for ValueSet $vsUrl!", e)
        false
    }
  }

  /**
   * Validate system and code agains given value set definition
   * @param vs
   * @param system
   * @param code
   * @return
   */
  private def validateCodeBinding(vs: ValueSetRestrictions, system:Option[String], code:String):Boolean = {
    //should not be in excludes
    !vs.excludes
      .exists(exc =>
        getCodeList(system, exc).contains(code) || //if it is in excluded list
          exc.valueSets.map(FHIRUtil.parseCanonicalValue).flatMap(v => getValueSet(v._1, v._2)).exists(validateCodeBinding(_, system, code)))  && //or a member of excluded value sets
      //and should be in includes
      (getCodeList(system, vs.includes).contains(code) || //either direct code
        vs.includes.valueSets.map(FHIRUtil.parseCanonicalValue).flatMap(v => getValueSet(v._1, v._2)).exists(validateCodeBinding(_, system, code))) //or a member of a value set
  }

  /**
   * Get list of codes from ValueSet definition
   * @param system
   * @param vs
   * @return
   */
  private def getCodeList(system:Option[String], vs:ValueSetDef):Set[String] = {
    system match {
      case None => vs.codes.values.flatten.toSet
      case Some(s) => vs.codes.getOrElse(s, Set.empty)
    }
  }

  /**
   * Get the ValueSet
   * @param vsUrl
   * @param version
   * @return
   */
  private def getValueSet(vsUrl:String, version:Option[String] = None):Option[ValueSetRestrictions] = {
    fhirConfig.valueSetRestrictions
      .get(vsUrl)
      .flatMap(versionMap => version match {
        case Some(v) => versionMap.get(v)
        case None  => versionMap.toSeq.sortBy(_._1).headOption.map(_._2)
      })
  }
}

object FhirTerminologyValidator {
  def apply(fhirConfig: BaseFhirConfig, terminologyServices:Seq[(TerminologyServiceConf, IFhirTerminologyService)]): FhirTerminologyValidator = new FhirTerminologyValidator(fhirConfig, terminologyServices)
}

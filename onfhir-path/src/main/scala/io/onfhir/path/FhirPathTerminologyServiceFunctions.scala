package io.onfhir.path

import akka.http.scaladsl.model.Uri
import io.onfhir.api.FHIR_DATA_TYPES
import io.onfhir.api.service.IFhirTerminologyService
import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.annotation.{FhirPathFunction, FhirPathFunctionDocumentation, FhirPathFunctionParameter, FhirPathFunctionReturn}
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import io.onfhir.util.JsonFormatter.formats
import org.json4s.JsonAST.{JArray, JBool, JObject, JString}

import scala.concurrent.Await
import scala.util.Try

/**
 * Implementation of Terminology Service functions in FHIR Path + some more practical ones
 * See https://build.fhir.org/fhirpath.html#txapi
 * TODO Other terminology service functions
 * @param context Context variables
 */
class FhirPathTerminologyServiceFunctions(context:FhirPathEnvironment) extends AbstractFhirPathFunctionLibrary {
  //Codes indicating that codes are equivalant (can be used for mappings)
  val translationEquivalenceCodes = Set("relatedto", "equivalent", "equal", "wider", "subsumes")
  /**
   * See https://build.fhir.org/fhirpath.html#txapi
   * This calls the Terminology Service $translate operation
   * e.g. %terminologies.translate('http://aiccelerate.eu/fhir/ConceptMap/labResultsConceptMap', Observation.code, 'conceptMapVersion=1.0')
   * @param conceptMapExpr   either an actual ConceptMap, or a canonical URL reference to a value set.
   * @param codeExpr         The source to translate: a Coding or code
   * @param paramsExpr       A URL encoded string with other parameters for the validate-code operation (e.g. 'source=http://acme.org/valueset/23&target=http://acme.org/valueset/23')
   * @return
   */
  @FhirPathFunction(
    documentation = FhirPathFunctionDocumentation(
      detail = "This calls the Terminology Service $translate operation.",
      usageWarnings = None,
      parameters = Some(Seq(
        FhirPathFunctionParameter(
          name = "conceptMapExpr",
          detail = "Either an actual ConceptMap, or a canonical URL reference to a value set.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "codeExpr",
          detail = "The source to translate: a Coding or code.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "paramsExpr",
          detail = "A URL encoded string with other parameters for the validate-code operation.",
          examples = Some(Seq(
            "'source=http://acme.org/valueset/23&target=http://acme.org/valueset/23'"
          ))
        )
      )),
      returnValue = FhirPathFunctionReturn(
        detail = None,
        examples = Seq(
          """<JSON>{ "code": "12345", "system": "http://example.org/system", "display": "Example Display" }"""
        )
      ),
      examples = Seq(
        "%terminologies.trms:translate('http://aiccelerate.eu/fhir/ConceptMap/labResultsConceptMap', Observation.code, 'conceptMapVersion=1.0')"
      )
    ),
    insertText = "%terminologies.translate(<conceptMapExpr>, <codeExpr>, <paramsExpr>)",
    detail = "trms",
    label = "%terminologies.translate",
    kind = "Function",
    returnType = Seq(),
    inputType = Seq()
  )
  def translate(conceptMapExpr:ExpressionContext, codeExpr:ExpressionContext, paramsExpr:ExpressionContext):Seq[FhirPathResult] = {
    val terminologyService = checkTerminologyService()
    //Evaluate concept map url
    val conceptMapResult = new FhirPathExpressionEvaluator(context, context._this).visit(conceptMapExpr)
    if(conceptMapResult.length > 1 || !conceptMapResult.forall(_.isInstanceOf[FhirPathString]))
      throw FhirPathException("Invalid function call 'translate'. The conceptMap expression (the function parameter) should evaluate to a single string value which should be concept map url.")
    val conceptMapUrl = conceptMapResult.head.asInstanceOf[FhirPathString].s
    // Evaluate code
    val codeResult = evaluateCodeExpr(codeExpr)
    //Evaluate params
    val paramsOpt = evaluateParamsExpr(paramsExpr)

    //Based on how code is given
      codeResult
        .map {
        //Providing the code directly
        case FhirPathString(code) =>
          getParam("system", paramsOpt) match {
            case None => throw FhirPathException("Invalid function call 'translate'. The system should be provided within params expression as code is given.")
            case Some(system) =>
              try{
                Await.result(
                  terminologyService
                    .translate(code, system, conceptMapUrl,
                      version = getParam("version", paramsOpt),
                      conceptMapVersion = getParam("conceptMapVersion", paramsOpt),
                      reverse = getParam("reverse", paramsOpt).flatMap(p => Try(p.toBoolean).toOption).getOrElse(false)
                    ),
                  terminologyService.getTimeout
                )
              } catch {
                case t:Throwable => throw FhirPathException("Problem while calling terminology service!", t)
              }
          }
        case FhirPathComplex(codingOrCodeableConcept) =>
            try{
                Await.result(
                  terminologyService
                    .translate(codingOrCodeableConcept, conceptMapUrl,
                      conceptMapVersion = getParam("conceptMapVersion", paramsOpt),
                      reverse = getParam("reverse", paramsOpt).flatMap(p => Try(p.toBoolean).toOption).getOrElse(false)
                    ),
                  terminologyService.getTimeout
                )
            } catch {
              case t:Throwable => throw FhirPathException("Problem while calling terminology service!", t)
            }
        case _ => throw FhirPathException("Invalid function call 'translate'. The code expression (the function parameter) should evaluate to a single string value.")
      }
        .map(FhirPathComplex)
        .toList
  }

  /**
   * This calls the Terminology Service $translate operation without a concept map
   * e.g. %terminologies.translate(Observation.code, 'source=http://hus.fi/ValueSet/labResultCodes'&target=http://aiccelerate.eu/ValueSet/labResultCodes)
   * @param codeExpr       The source to translate: a Coding or code
   * @param paramsExpr     A URL encoded string with other parameters for the validate-code operation (e.g. 'source=http://acme.org/valueset/23&target=http://acme.org/valueset/23')
   * @return
   */
  @FhirPathFunction(
    documentation = FhirPathFunctionDocumentation(
      detail = "This calls the Terminology Service $translate operation without a concept map.",
      usageWarnings = None,
      parameters = Some(Seq(
        FhirPathFunctionParameter(
          name = "codeExpr",
          detail = "The source to translate: a Coding or code.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "paramsExpr",
          detail = "A URL encoded string with other parameters for the validate-code operation.",
          examples = Some(Seq(
            "'source=http://acme.org/valueset/23&target=http://acme.org/valueset/23'"
          ))
        )
      )),
      returnValue = FhirPathFunctionReturn(
        detail = None,
        examples = Seq(
          """<JSON>{ "code": "12345", "system": "http://example.org/system", "display": "Example Display" }"""
        )
      ),
      examples = Seq(
        "%terminologies.trms:translate(Observation.code, 'source=http://hus.fi/ValueSet/labResultCodes&target=http://aiccelerate.eu/ValueSet/labResultCodes')"
      )
    ),
    insertText = "%terminologies.translate(<codeExpr>, <paramsExpr>)",
    detail = "trms",
    label = "%terminologies.translate",
    kind = "Function",
    returnType = Seq(),
    inputType = Seq()
  )
  def translate(codeExpr:ExpressionContext, paramsExpr:ExpressionContext):Seq[FhirPathResult] = {
    val terminologyService = checkTerminologyService()
    // Evaluate code
    val codeResult = evaluateCodeExpr(codeExpr)

    //Evaluate params
    val paramsOpt = evaluateParamsExpr(paramsExpr)

    //Based on how code is given
    codeResult.map {
      //Providing the code directly
      case FhirPathString(code) =>
        getParam("system", paramsOpt) match {
          case None => throw FhirPathException("Invalid function call 'translate'. The system should be provided within params expression as code is given.")
          case Some(system) =>
            try{
              Await.result(
                terminologyService
                  .translate(code, system,
                    source = getParam("source", paramsOpt),
                    target = getParam("target", paramsOpt),
                    version = getParam("version", paramsOpt),
                    reverse = getParam("reverse", paramsOpt).flatMap(p => Try(p.toBoolean).toOption).getOrElse(false)
                  ),
                terminologyService.getTimeout
              )
            } catch {
              case t:Throwable => throw FhirPathException("Problem while calling terminology service!", t)
            }
        }
      case FhirPathComplex(codingOrCodeableConcept) =>
        try{
          Await.result(
            terminologyService
              .translate(codingOrCodeableConcept,
                source = getParam("source", paramsOpt),
                target = getParam("target", paramsOpt),
                reverse = getParam("reverse", paramsOpt).flatMap(p => Try(p.toBoolean).toOption).getOrElse(false)
              ),
            terminologyService.getTimeout
          )
        } catch {
          case t:Throwable => throw FhirPathException("Problem while calling terminology service!", t)
        }
      case _ => throw FhirPathException("Invalid function call 'translate'. The code expression (the function parameter) should evaluate to a single string value.")
    }
      .map(FhirPathComplex)
      .toList
  }

  /**
   * See https://build.fhir.org/fhirpath.html#txapi
   * This calls the Terminology Service $lookup operation.
   *  e.g. %terminologies.lookup(Observation.code.coding[0], 'displayLanguage=tr'
   * @param codeExpr    Expression to evaluate to a code, Coding or CodeableConcept
   * @param paramsExpr  Expression to evaluate a URL encoded string with other parameters for the lookup operation (e.g. 'date=2011-03-04&displayLanguage=en')
   * @return
   */
  @FhirPathFunction(
    documentation = FhirPathFunctionDocumentation(
      detail = "This calls the Terminology Service $lookup operation.",
      usageWarnings = None,
      parameters = Some(Seq(
        FhirPathFunctionParameter(
          name = "codeExpr",
          detail = "Expression to evaluate to a code, Coding or CodeableConcept.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "paramsExpr",
          detail = "Expression to evaluate a URL encoded string with other parameters for the lookup operation.",
          examples = Some(Seq(
            "'date=2011-03-04&displayLanguage=en'"
          ))
        )
      )),
      returnValue = FhirPathFunctionReturn(
        detail = None,
        examples = Seq(
          """<JSON>{ "code": "12345", "system": "http://example.org/system", "display": "Example Display" }"""
        )
      ),
      examples = Seq(
        "%terminologies.trms:lookup(Observation.code.coding[0], 'displayLanguage=tr')"
      )
    ),
    insertText = "%terminologies.lookup(<codeExpr>, <paramsExpr>)",
    detail = "trms",
    label = "%terminologies.lookup",
    kind = "Function",
    returnType = Seq(),
    inputType = Seq()
  )
  def lookup(codeExpr:ExpressionContext, paramsExpr:ExpressionContext):Seq[FhirPathResult] = {
    val terminologyService = checkTerminologyService()
    // Evaluate code
    val codeResult = evaluateCodeExpr(codeExpr)

    //Evaluate params
    val paramsOpt = evaluateParamsExpr(paramsExpr)

    //Based on how code is given
    codeResult.flatMap {
      //Providing the code directly
      case FhirPathString(code) =>
        getParam("system", paramsOpt) match {
          case None => throw FhirPathException("Invalid function call 'lookup'. The system should be provided within params expression as code is given.")
          case Some(system) =>
           try{
              Await.result(
                terminologyService
                  .lookup(code, system,
                    version = getParam("version", paramsOpt),
                    date = getParam("date", paramsOpt),
                    displayLanguage = getParam("displayLanguage", paramsOpt),
                    properties = getParamList("property", paramsOpt)
                  ),
                terminologyService.getTimeout
              )
           } catch {
              case t:Throwable => throw FhirPathException("Problem while calling terminology service!", t)
          }
        }
      case FhirPathComplex(codingOrCodeableConcept) =>
        try{
          Await.result(
            terminologyService
              .lookup(codingOrCodeableConcept,
                date = getParam("date", paramsOpt),
                displayLanguage = getParam("displayLanguage", paramsOpt),
                properties = getParamList("property", paramsOpt)
              ),
            terminologyService.getTimeout
          )
        } catch {
          case t:Throwable => throw FhirPathException("Problem while calling terminology service!", t)
        }
      case _ => throw FhirPathException("Invalid function call 'lookup'. The code expression (the function parameter) should evaluate to a single string value.")
    }
      .map(FhirPathComplex)
      .toList

  }

  /**
   * Return the display string in preferred language for the given code+system
   * If code and system can not be found, or displayLanguage does not match with available ones return Nil
   * If code, system or displayLanguage  does not evaluate to a single string, throws error
   * e.g. trms:lookupDisplay('C12', 'http://hus.fi/CodeSystem/labResults', 'tr')
   * If there is a problem during terminology service call, throws error
   * @param codeExpr        Expression to evaluate to the code
   * @param systemExpr      Expression to evaluate to the system
   * @param displayLanguageExpr Expression to evaluate optional displayLanguage
   * @return
   */
  @FhirPathFunction(
    documentation = FhirPathFunctionDocumentation(
      detail = "Return the display string in preferred language for the given code. If code or system cannot be found, or displayLanguage does not match with available ones, return empty. If code, system, or displayLanguage does not evaluate to a single string, throws an error.",
      usageWarnings = Some(Seq("A code system with the specified URL <strong>must</strong> be available in the terminology service, and the mapping job must be configured to use this terminology service to use this function.")),
      parameters = Some(Seq(
        FhirPathFunctionParameter(
          name = "source_code",
          detail = "Source code to find its display value.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "codeSystemUrl",
          detail = "Unique code system URL from the terminology service to be used to perform the lookup operation.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "targetColumn",
          detail = "FHIRPath string literal providing the name of the interested column from the code system. Mostly language name.",
          examples = Some(Seq("en", "de"))
        )
      )),
      returnValue = FhirPathFunctionReturn(
        detail = None,
        examples = Seq(
          "\"Probe von der Haut\""
        )
      ),
      examples = Seq(
        "trms:lookupDisplay('C12', 'http://hus.fi/CodeSystem/labResults', 'tr')"
      )
    ),
    insertText = "trms:lookupDisplay(<source_code>, <codeSystemUrl>, <targetColumn>)",
    detail = "trms",
    label = "trms:lookupDisplay",
    kind = "Function",
    returnType = Seq(FHIR_DATA_TYPES.STRING),
    inputType = Seq()
  )
  def lookupDisplay(codeExpr:ExpressionContext, systemExpr:ExpressionContext, displayLanguageExpr:ExpressionContext):Seq[FhirPathResult] = {
    val terminologyService = checkTerminologyService()
    val code = evaluateToSingleString(codeExpr)
    val system = evaluateToSingleString(systemExpr)
    val displayLanguage = evaluateToSingleString(displayLanguageExpr)
    if(code.isEmpty || system.isEmpty)
      throw FhirPathException(s"Invalid function call lookupDisplay. 'code' and 'system' parameters are mandatory!")

    val resultParameters =
      try{
        Await.result(terminologyService.lookup(code.get, system.get, None, None, displayLanguage, Nil), terminologyService.getTimeout)
      } catch {
        case t:Throwable => throw FhirPathException("Problem while calling terminology service!", t)
      }
    resultParameters match {
      case None => Nil
      case Some(rp) =>
        FHIRUtil.getParameterValueByName(rp, "display") match {
          case Some(JString(display)) => Seq(FhirPathString(display))
          case _ => Nil
        }
    }
  }

  /**
   *  Translate the given Coding or CodeableConcept according to the given conceptMap
   * @param codingExpr          Expression to evaluate to the Coding
   * @param conceptMapUrlExpr   Expression to evaluate the ConceptMap canonical url
   * @return
   */
  @FhirPathFunction(
    documentation = FhirPathFunctionDocumentation(
      detail = "Translates the given Coding or CodeableConcept object according to the given conceptMap from the terminology service and returns a list of Coding objects. Translates the given code+system according to the given conceptMap from the terminology service and returns a list of Coding objects.",
      usageWarnings = Some(Seq("A concept map with the specified URL <strong>must</strong> be available in the terminology service, and the mapping job must be configured to use this terminology service to use this function.")),
      parameters = Some(Seq(
        FhirPathFunctionParameter(
          name = "codeExpr",
          detail = "[Coding](https://build.fhir.org/datatypes.html#Coding) or [CodeableConcept](https://build.fhir.org/datatypes.html#CodeableConcept) object to be translated.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "conceptMapUrlExpr",
          detail = "Unique concept map URL from the terminology service to be used.",
          examples = None
        )
      )),
      returnValue = FhirPathFunctionReturn(
        detail = None,
        examples = Seq(
          "[\n  {\n    \"system\": \"target-system\",\n    \"code\": \"target-code\",\n    \"display\": \"target-display\"\n  },\n  ...\n]"
        )
      ),
      examples = Seq(
        "trms:translateToCoding(codingObject, 'https://www.ncbi.nlm.nih.gov/clinvar')"
      )
    ),
    insertText = "trms:translateToCoding(<coding>, <conceptMapUrl>)",
    detail = "trms",
    label = "trms:translateToCoding",
    kind = "Function",
    returnType = Seq(),
    inputType = Seq()
  )
  def translateToCoding(codingExpr:ExpressionContext, conceptMapUrlExpr:ExpressionContext):Seq[FhirPathResult] = {
    val terminologyService = checkTerminologyService()
    val codingResult = evaluateCodeExpr(codingExpr)
    val conceptMapUrl = evaluateToSingleString(conceptMapUrlExpr)

    codingResult.map {
      case FhirPathComplex(json) =>
        val resultParameters =
          try {
            Await.result(terminologyService.translate(json, conceptMapUrl.get), terminologyService.getTimeout)
          } catch {
            case t: Throwable => throw FhirPathException("Problem while calling terminology service!", t)
          }
        handleTranslationResult(resultParameters)
      case _ =>
        throw FhirPathException(s"Invalid function call translateToCoding. 'coding' parameter should evaluate to FHIR Coding object!")

    }.getOrElse(Nil)
  }

  /**
   * Translate the given code+system according to the given conceptMap
   * @param codeExpr          Expression to evaluate to the code
   * @param systemExpr        Expression to evaluate to the system
   * @param conceptMapUrlExpr Expression to evaluate the ConceptMap canonical url
   * @return                  Matching codes (in Coding data type)
   */
  @FhirPathFunction(
    documentation = FhirPathFunctionDocumentation(
      detail = "Translates the given code+system according to the given conceptMap from terminology service and returns a list of Coding objects.",
      usageWarnings = Some(Seq("A concept map with the specified URL <strong>must</strong> be available in the terminology service, and the mapping job must be configured to use this terminology service to use this function.")),
      parameters = Some(Seq(
        FhirPathFunctionParameter(
          name = "codeExpr",
          detail = "Source code to be translated.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "systemExpr",
          detail = "Source system URL to be translated.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "conceptMapUrlExpr",
          detail = "Unique concept map URL from terminology service to be used to translate.",
          examples = None
        )
      )),
      returnValue = FhirPathFunctionReturn(
        detail = None,
        examples = Seq("[\n  {\n    \"system\": \"target-system\",\n    \"code\": \"target-code\",\n    \"display\": \"target-display\"\n  },\n  ...\n]"
        )
      ),
      examples = Seq(
        "trms:translateToCoding(code, 'http://terminology.hl7.org/CodeSystem/icd9cm', 'https://www.ncbi.nlm.nih.gov/clinvar')"
      )
    ),
    insertText = "trms:translateToCoding(<coding>, <system>, <conceptMapUrl>)",
    detail = "trms",
    label = "trms:translateToCoding",
    kind = "Function",
    returnType = Seq(),
    inputType = Seq()
  )
  def translateToCoding(codeExpr:ExpressionContext, systemExpr:ExpressionContext, conceptMapUrlExpr:ExpressionContext):Seq[FhirPathResult] = {
    val terminologyService = checkTerminologyService()
    val conceptMapUrl = evaluateToSingleString(conceptMapUrlExpr)
    val code = evaluateToSingleString(codeExpr)
    val system = evaluateToSingleString(systemExpr)
    if(conceptMapUrl.isEmpty)
      throw FhirPathException(s"Invalid function call translateToCoding. 'code','system' and 'conceptMapUrl' parameters are mandatory!")

    if(code.isEmpty || system.isEmpty)
      Nil
    else {
      //Execute the terminology service
      val resultParameters =
        try {
          Await.result(terminologyService.translate(code.get, system.get, conceptMapUrl.get), terminologyService.getTimeout)
        } catch {
          case t: Throwable => throw FhirPathException("Problem while calling terminology service!", t)
        }
      handleTranslationResult(resultParameters)
    }
  }

  /**
   * Translate the given code+system according to the given source value set and optional target value set
   * @param codeExpr                Expression to evaluate to the code
   * @param systemExpr              Expression to evaluate to the system
   * @param sourceValueSetUrlExpr   Expression to evaluate to the source value set url
   * @param targetValueSetUrlExpr   Expression to evaluate to the target value set url
   * @return
   */
  @FhirPathFunction(
    documentation = FhirPathFunctionDocumentation(
      detail = "Translates the given code+system according to the given source value set and optional target value set from terminology service and returns a list of Coding objects.",
      usageWarnings = Some(Seq("A concept map with the specified URL <strong>must</strong> be available in the terminology service, and the mapping job must be configured to use this terminology service to use this function.")),
      parameters = Some(Seq(
        FhirPathFunctionParameter(
          name = "codeExpr",
          detail = "Source code to be translated.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "systemExpr",
          detail = "Source system URL to be translated.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "sourceValueSetUrlExpr",
          detail = "Source value set URL to be used to translate.",
          examples = None
        ),
        FhirPathFunctionParameter(
          name = "targetValueSetUrlExpr",
          detail = "Target value set URL to be used to translate.",
          examples = None
        )
      )),
      returnValue = FhirPathFunctionReturn(
        detail = None,
        examples = Seq("[\n  {\n    \"system\": \"target-system\",\n    \"code\": \"target-code\",\n    \"display\": \"target-display\"\n  },\n  ...\n]"
        )
      ),
      examples = Seq(
        "trms:translateToCoding(code, 'https://system', 'https://sourceValueSetUrl', 'https://targetValueSetUrl')"
      )
    ),
    insertText = "trms:translateToCoding(<coding>, <system>, <sourceValueSetUrl>, <targetValueSetUrl>)",
    detail = "trms",
    label = "trms:translateToCoding",
    kind = "Function",
    returnType = Seq(),
    inputType = Seq()
  )
  def translateToCoding( codeExpr:ExpressionContext,
                         systemExpr:ExpressionContext,
                         sourceValueSetUrlExpr:ExpressionContext,
                         targetValueSetUrlExpr:ExpressionContext
                       ):Seq[FhirPathResult] = {

    val terminologyService = checkTerminologyService()
    val sourceVsUrl = evaluateToSingleString(sourceValueSetUrlExpr)
    val targetValueSetUrl = evaluateToSingleString(targetValueSetUrlExpr)
    val code = evaluateToSingleString(codeExpr)
    val system = evaluateToSingleString(systemExpr)
    if(sourceVsUrl.isEmpty)
      throw FhirPathException(s"Invalid function call translateToCoding. 'code','system' and 'sourceValueSetUrl' parameters are mandatory!")
    if(code.isEmpty || system.isEmpty)
      Nil
    else {
      //Execute the terminology service
      val resultParameters =
        try {
          Await.result(terminologyService.translate(code.get, system.get, sourceVsUrl, targetValueSetUrl), terminologyService.getTimeout)
        } catch {
          case t: Throwable => throw FhirPathException("Problem while calling terminology service!", t)
        }

      handleTranslationResult(resultParameters)
    }
  }

  /**
   * Parse the translation result Parameters resource and return the matched concepts (Coding)
   * @param parameters  Response of the translation
   * @return
   */
  private def handleTranslationResult(parameters:JObject):List[FhirPathComplex] = {
    FHIRUtil.getParameterValueByName(parameters, "result") match {
          //If there is some matching
          case Some(JBool(true)) =>
            //Get those matches
            FHIRUtil.getParameterValueByName(parameters, "match") match {
              //If multiple matches
              case Some(JArray(mtchs)) if mtchs.forall(_.isInstanceOf[JArray]) =>
                //For each match
                mtchs
                  .map(_.asInstanceOf[JArray])
                  .map(parts =>
                    parts.arr.map(_.asInstanceOf[JObject]).map(p => FHIRUtil.parseParameter(p))
                  )// Filter the match with these equivalance codes
                  .filter(parts =>
                    parts
                      .find(_._1 == "relationship")
                      .map(_._2.extract[String])
                      .exists(eq => translationEquivalenceCodes.contains(eq))
                  ) //Get the matching concept
                  .flatMap(parts =>
                    parts
                      .find(_._1 == "concept")
                      .map(_._2.asInstanceOf[JObject])
                  ) //Construct the FHIR Path result
                  .map(coding =>
                    FhirPathComplex(coding)
                  )
              //If single match
              case Some(JArray(parts)) if parts.forall(_.isInstanceOf[JObject])=>
                val parameters = parts
                  .map(_.asInstanceOf[JObject])
                  .map(FHIRUtil.parseParameter)

               if(parameters.exists(p => p._1 == "relationship" && translationEquivalenceCodes.contains(p._2.extract[String])))
                  parameters.find(_._1 == "concept").map(_._2.asInstanceOf[JObject]).map(coding => FhirPathComplex(coding)).toList
               else
                  Nil
              //Otherwise return Nil
              case _ => Nil
            }
          //Otherwise return nil
          case _ => Nil
        }
  }

  /**
   * Check if terminology service is supplied
   * @return
   */
  private def checkTerminologyService():IFhirTerminologyService = {
    if(context.terminologyService.isEmpty)
      throw FhirPathException("Terminology service function calls are not supported with the current configuration of onfhir FHIR Path engine!")
    context.terminologyService.get
  }

  /**
   * Evaluate an expression returning the code param
   * @param codeExpr
   * @return
   */
  private def evaluateCodeExpr(codeExpr:ExpressionContext):Option[FhirPathResult] = {
    // Evaluate code
    val codeResult = new FhirPathExpressionEvaluator(context, context._this).visit(codeExpr)
    if(codeResult.length > 1)
      throw FhirPathException("Invalid terminology service function call. The code expression (the function parameter) should evaluate to a single value (either string, Coding, CodeableConcept) providing the code value.")
    codeResult.headOption
  }

  private def evaluateToSingleString(expr:ExpressionContext):Option[String] = {
    // Evaluate code
    val exprResult = new FhirPathExpressionEvaluator(context, context._this).visit(expr)
    if (exprResult.length > 1 || !exprResult.forall(_.isInstanceOf[FhirPathString]))
      throw FhirPathException(s"Invalid function call. The expression '${expr.getText}' (the function parameter) should evaluate to a single string value providing the code value.")
    exprResult.headOption.map(_.asInstanceOf[FhirPathString].s)
  }

  /**
   * Evaluate the params expression in terminology service function calls
   * @param paramsExpr
   * @return
   */
  private def evaluateParamsExpr(paramsExpr:ExpressionContext):Option[Map[String, List[String]]] = {
    //Evaluate params
    val paramsResult = new FhirPathExpressionEvaluator(context, context._this).visit(paramsExpr)
    if(paramsResult.length > 1 || !paramsResult.forall(_.isInstanceOf[FhirPathString]))
      throw FhirPathException("Invalid terminology service function call. The params expression (the function parameter) should evaluate to optional string value which should be the RL encoded string with other parameters for the validate-code operation.")

    paramsResult.headOption.map(prms => Uri.Query(prms.asInstanceOf[FhirPathString].s).toMultiMap)
  }


  /**
   * Get param value from parameters
   * @param paramName
   * @param paramsOpt
   * @return
   */
  private def getParam(paramName:String, paramsOpt:Option[Map[String,List[String]]]):Option[String] =
    paramsOpt.flatMap(_.get(paramName).flatMap(_.headOption))

  private def getParamList(paramName:String, paramsOpt:Option[Map[String,List[String]]]):Seq[String] =
    paramsOpt.flatMap(_.get(paramName)).getOrElse(Nil)
}


object FhirPathTerminologyServiceFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix:String = "trms"
  /**
   *
   * @param context
   * @param current
   * @return
   */
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]): AbstractFhirPathFunctionLibrary = new FhirPathTerminologyServiceFunctions(context)
}
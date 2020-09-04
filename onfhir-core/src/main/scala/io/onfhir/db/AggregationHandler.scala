package io.onfhir.db

import akka.http.scaladsl.server.util.Tuple
import io.onfhir.api.{FHIR_DATA_TYPES, FHIR_PARAMETER_TYPES, FHIR_PREFIXES_MODIFIERS}
import io.onfhir.api.model.Parameter
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.SearchParameterConf
import io.onfhir.exception.InternalServerException
import org.bson.{BsonNull, BsonValue}
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonDocument, BsonString, BsonValue}
import org.mongodb.scala.model.Projections
object AggregationHandler {

  private def findRelatedSearchParameter(paramName:String, parameters:List[Parameter]):Option[Parameter] = {
    parameters.find(p => p.name == paramName || (p.paramType == FHIR_PARAMETER_TYPES.COMPOSITE && p.name.startsWith(paramName+"-")))
  }
  /**
   * Check existence of related search parameter
   * @param parameter
   * @return
   */
  private def checkRelatedSearchParameter(parameter: Option[Parameter]):Parameter = {
    parameter match {
      case None =>
        throw new InternalServerException(s"When grouping is requested on a search parameter than targets an array element, the search parameters also should include the same param to indicate the possible bucket values! e.g. For example, grouping on CodeableConcept type element (e.g. Observation.code) requires the filter on the same element due to multiple codes that can be used in CodeableConcept.")
      case Some(p) => p
    }
  }

  private def getTokenValues(p:Parameter):Seq[String] = {
    p.paramType match {
      case FHIR_PARAMETER_TYPES.COMPOSITE =>
        //TODO For composite parameters we assume that group by parameter is the first one
        p.valuePrefixList.map(_._2).map(_.split('$').head)
      case  _ =>
        p.valuePrefixList.map(_._2)
    }
  }

  /**
   * Get the parameter values and search format for token type search
   * @param p
   * @return
   */
  private def checkAndGetTokenValuesAndFormat(p:Parameter):(Seq[String], Product) = {
        val tokenValues = getTokenValues(p)

        val parsedValues =
          tokenValues
            .map(tv => FHIRUtil.parseTokenValue(tv))

        val queryTokenFormat = parsedValues.head

        //All values should be in the same format
        if(!parsedValues.forall(pv => pv._1.isDefined == queryTokenFormat._1.isDefined && pv._2.isDefined == queryTokenFormat._2.isDefined))
          throw new InternalServerException(s"For query on group by parameter which is token type, all token values should be in the same format!")

        tokenValues -> queryTokenFormat
  }

  /**
   * Construct group by expression for token search parameters with multi token search e.g. [system]|[code], etc
   * @param tokenValues         Given parameter values
   * @param tokenValueFormat    Format of the search
   * @param path                Path to the root element
   * @param subPaths            Sub paths to concat
   * @return
   */
  private def constructGroupByExpressionForTupleTokens(tokenValues:Seq[String], tokenValueFormat:(Option[String], Option[String]), path:String, subPaths:Seq[String]):BsonValue = {
    (tokenValueFormat:  @unchecked) match {
      //[code] query pattern or |[code] query pattern
      case (None, Some(_)) | (Some(""), Some(_)) =>
        //e.g. {code: {
        //    $filter: {
        //       input:"$code.coding.code",
        //       cond: {$in: ["$$this",['39156-5','72166-2']]}
        //}}
        AggregationUtil.constructFilterExpression(
          AggregationUtil.constructPathExpression(FHIRUtil.normalizeElementPath(path) + "." + subPaths.last),
          AggregationUtil.constructInExpression(BsonString("$$this"),
            if(tokenValueFormat._1.contains(""))
              tokenValues.map(tv => BsonString(tv.drop(1)))
            else
              tokenValues.map(tv => BsonString(tv))
        ))
      //[system]| query pattern
      case (Some(_), None) =>
        AggregationUtil
          .constructFilterExpression(
            AggregationUtil.constructPathExpression(FHIRUtil.normalizeElementPath(path) + "." + subPaths.head),
            AggregationUtil.constructInExpression(BsonString("$$this"), tokenValues.map(tv => BsonString(tv.dropRight(1))))
          )
      case (Some(_), Some(_)) =>
        //e.g. {code: {
        //    $filter: {
        //            input:{
        //                $map: {
        //                  input: "$code.coding",
        //                  as:"c",
        //                  in: {$concat: [{$ifNull: ["$$c.system", ""]}, "|", "$$c.code"]}
        //                }
        //             },
        //             cond: {$in: ["$$this",['http://loinc.org|39156-5','http://loinc.org|72166-2']]}
        //}}}
        val mapConcatExpr =
          AggregationUtil.constructMapExpression(
              AggregationUtil.constructPathExpression(FHIRUtil.normalizeElementPath(path)),
              AggregationUtil.constructConcatExpression(
                subPaths
                  .map(sp => BsonString("$$this."+sp))
                  .flatMap(e => Seq(e, BsonString("|")))
                  .dropRight(1)
            )
          )
        AggregationUtil.constructFilterExpression(
          mapConcatExpr,
          AggregationUtil.constructInExpression(BsonString("$$this"), tokenValues.map(BsonString(_)))
        )
    }
  }

  /**
   * Construct GroupBy expression for a token type parameter for given path
   * @param paramName
   * @param parameters
   * @param path
   * @param subPaths
   * @param fullTextPath
   * @return
   */
  private def constructGroupByExpressionForTokenType(paramName:String, parameters:List[Parameter], path:String, subPaths:Seq[String], fullTextPath:String):BsonValue = {
    val relatedSearchParameterOpt = findRelatedSearchParameter(paramName, parameters)
    //If the path is on an array
    if(path.contains("[i]")) {
      //Then there should  be a related search parameter that provides the possible bucket values (as there are multiple values for bucket we should select one of them; the queried one)
      val relatedSearchParam = checkRelatedSearchParameter(relatedSearchParameterOpt)
      relatedSearchParam.suffix match {
        //no modifier than we should group by based on token value format e.g. if system and code is given group by their concatenation
        case "" =>
          val (tokenValues, tokenValueFormat) = checkAndGetTokenValuesAndFormat(relatedSearchParam)
          constructGroupByExpressionForTupleTokens(tokenValues, tokenValueFormat.asInstanceOf[(Option[String], Option[String])], path, subPaths)
        //Text modifier than we should group by  on text
        case FHIR_PREFIXES_MODIFIERS.TEXT =>
          val tokenValues = getTokenValues(relatedSearchParam)
          AggregationUtil.constructFilterExpression(AggregationUtil.constructPathExpression(fullTextPath),
            AggregationUtil.constructInExpression(BsonString("$$this"), tokenValues.map(BsonString(_)))
          )
        //Of type modifier group by based on all 3 elements
        case FHIR_PREFIXES_MODIFIERS.OF_TYPE =>
          val tokenValues = getTokenValues(relatedSearchParam)
          val mapConcatExpr =
            AggregationUtil.constructMapExpression(
              AggregationUtil.constructPathExpression(FHIRUtil.normalizeElementPath(path)),
              AggregationUtil.constructConcatExpression(
                Seq("type.coding.system", "type.coding.code", "value")
                  .map(sp => BsonString("$$this."+sp))
                  .flatMap(e => Seq(e, BsonString("|")))
                  .dropRight(1)
              )
            )
          AggregationUtil.constructFilterExpression(
            mapConcatExpr,
            AggregationUtil.constructInExpression(BsonString("$$this"), tokenValues.map(BsonString(_)))
          )
      }
    } else {
      relatedSearchParameterOpt match {
        //If parameter does not exist, group by concenatenation of subpaths
        case None =>
          AggregationUtil
            .constructConcatExpression(
              subPaths
                .map(s => AggregationUtil.constructIfNullExpression(AggregationUtil.constructPathExpression(s), BsonString("")))
            )
        case Some(relatedSearchParam) =>
          relatedSearchParam.suffix match {
            case "" =>  AggregationUtil.constructConcatExpression(subPaths.map(s => AggregationUtil.constructPathExpression(s"$path.$s")))
            case FHIR_PREFIXES_MODIFIERS.TEXT => AggregationUtil.constructPathExpression(fullTextPath)
            case FHIR_PREFIXES_MODIFIERS.OF_TYPE =>
              AggregationUtil.constructConcatExpression (
                Seq("type.coding.system", "type.coding.code", "value")
                  .map(s => AggregationUtil.constructPathExpression(s))
                  .flatMap(e => Seq(e, BsonString("|")))
                  .dropRight(1)
              )
          }
      }
    }
  }

  /**
   * Construct Mongodb group by expression ($group) based on the given FHIR search parameters for group by and filtering
   * @param groupByParams   Parameters to handle grouping on
   * @param queryParams     Filtering params for query
   * @return                Groupby expression for each parameter
   */
  def constructGroupByExpression(groupByParams:Seq[SearchParameterConf], queryParams:List[Parameter]):Seq[(String, BsonValue)] = {
    if(!groupByParams.forall(pconf => Set(FHIR_PARAMETER_TYPES.REFERENCE, FHIR_PARAMETER_TYPES.TOKEN).contains(pconf.ptype)))
      throw new InternalServerException(s"Querying last or first n results only supports '${FHIR_PARAMETER_TYPES.REFERENCE}' and '${FHIR_PARAMETER_TYPES.TOKEN}' type parameters for grouping!")


      groupByParams
        .map(gbypConf => {
          val pathsAndTargetTypes =
            gbypConf
            .extractElementPathsAndTargetTypes(withArrayIndicators = true)

          val expressionForEachPath =
            pathsAndTargetTypes.to
              .map(pathAndTargetType =>
                constructGroupByExpressionForPath(gbypConf.pname, gbypConf.ptype, queryParams, pathAndTargetType._1, pathAndTargetType._2)
              )


            val mergedExpr = mergeExpressionsByCheckingPathExistence( expressionForEachPath.zip(pathsAndTargetTypes.map(_._1)))

          gbypConf.pname -> mergedExpr
        })
  }

  private def mergeExpressionsByCheckingPathExistence(expr:Seq[(BsonValue, String)]):BsonValue = {
    if(expr.size == 1)
      expr.head._1
    else {
      val firstExpr = expr.head
      AggregationUtil.constructCondExpression(
        AggregationUtil.constructComparisonOpExpression("gt", AggregationUtil.constructPathExpression(firstExpr._2), new BsonNull()), //Check existence of path 1 e.g. $gt: ["$effectiveDateTime", null]
        firstExpr._1,
        mergeExpressionsByCheckingPathExistence(expr.tail)
      )
    }
  }

  /**
   * Construct group by expression for the given parameter and path
   * @param paramName     Search Parameter Name to group by on
   * @param paramType     Search Parameter type
   * @param parameters    All search parameters for the query
   * @param path          Path to the element to group by on
   * @param targetType    Data type of the element to group by on for this path
   * @return
   */
  private def constructGroupByExpressionForPath(paramName:String, paramType:String, parameters:List[Parameter], path:String, targetType:String) = {
    paramType match {
      case FHIR_PARAMETER_TYPES.TOKEN =>
        targetType match {
          //If target type is CodeableConcept
          case FHIR_DATA_TYPES.CODEABLE_CONCEPT =>
            constructGroupByExpressionForTokenType(paramName, parameters, path + ".coding[i]", Seq("system", "code"), path + ".text")
          //If target type is idemtifier
          case FHIR_DATA_TYPES.IDENTIFIER =>
            constructGroupByExpressionForTokenType(paramName, parameters, path, Seq("system", "value"), path + ".type.text")
          case FHIR_DATA_TYPES.CODING =>
            constructGroupByExpressionForTokenType(paramName, parameters, path, Seq("system", "code"), path + ".display")
          case FHIR_DATA_TYPES.STRING | FHIR_DATA_TYPES.CODE | FHIR_DATA_TYPES.URI =>
            if(path.contains("[i]")) {
              val relatedSearchParam = checkRelatedSearchParameter(findRelatedSearchParameter(paramName, parameters))
              val tokenValues = relatedSearchParam.valuePrefixList.map(_._2)

              AggregationUtil.constructFilterExpression(
                  AggregationUtil.constructPathExpression(FHIRUtil.normalizeElementPath(path)),
                  AggregationUtil.constructInExpression(BsonString("$$this"), tokenValues.map(BsonString(_)))
              )
            } else
              //Single path
              AggregationUtil.constructPathExpression(FHIRUtil.normalizeElementPath(path))
          //we dont support other data types
          case oth =>
            throw new InternalServerException(s"Grouping is not supported on elements with data type $oth!")
        }
      //Handle Reference parameter
      case FHIR_PARAMETER_TYPES.REFERENCE =>
        val relatedSearchParamOpt = findRelatedSearchParameter(paramName, parameters)
        if(path.contains("[i]")) {
          val relatedSearchParam = checkRelatedSearchParameter(relatedSearchParamOpt)
          relatedSearchParam.suffix match {
            case "" =>
              val tokenValues = relatedSearchParam.valuePrefixList.map(_._2)
              //e.g. {$filter: {input: "$subject.reference", cond: {$in: ["$$this", ['..', '..']]}
              AggregationUtil.constructFilterExpression(
                AggregationUtil.constructPathExpression( FHIRUtil.normalizeElementPath(path)+".reference"), //e.g. $subject.reference
                AggregationUtil.constructInExpression(BsonString("$$this"), tokenValues.map(BsonString(_)))
              )
            case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
              val (tokenValues, tokenValueFormat) = checkAndGetTokenValuesAndFormat(relatedSearchParam)
              constructGroupByExpressionForTupleTokens(tokenValues, tokenValueFormat.asInstanceOf[(Option[String], Option[String])], path + ".identifier", Seq("system", "value"))
            case _ =>
              throw new InternalServerException(s"When grouping is requested on a search parameter on reference, only '${FHIR_PREFIXES_MODIFIERS.IDENTIFIER}' modifier can be used.")
          }
        } else {
          relatedSearchParamOpt match {
            case Some(relatedSearchParam) if relatedSearchParam.suffix != "" =>
              relatedSearchParam.suffix match {
                case FHIR_PREFIXES_MODIFIERS.IDENTIFIER =>
                  //e.g. $concat : [ {$ifnull: ["$subject.identifier.system", ""]}, "|", {$ifnull: ["$subject.identifier.value", ""]}]
                  AggregationUtil.constructConcatExpression(
                    Seq(
                      path + ".identifier.system",
                      path + ".identifier.value"
                    )
                      .map(p => AggregationUtil.constructIfNullExpression(AggregationUtil.constructPathExpression(p), BsonString("")))
                      .flatMap(e => Seq(e, BsonString("|")))
                      .dropRight(1)
                  )
                case _ =>
                  throw new InternalServerException(s"When grouping is requested on a search parameter on reference, only '${FHIR_PREFIXES_MODIFIERS.IDENTIFIER}' modifier can be used.")
              }
            case None =>
              //e.g. $subject.reference
              AggregationUtil.constructPathExpression( FHIRUtil.normalizeElementPath (path) + ".reference")
          }
        }
    }
  }


}

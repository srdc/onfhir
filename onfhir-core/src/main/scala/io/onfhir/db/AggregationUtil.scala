package io.onfhir.db

import org.bson.BsonInt32
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonString, BsonValue}

object AggregationUtil {

  def constructSliceExpression(arrayField:String, n:Int):BsonValue = {
    BsonDocument(
      "$slice" ->
        BsonArray.fromIterable(Seq(
          BsonString("$"+arrayField),
          new BsonInt32(n)
        ))
    )
  }

  /**
   * Construct aggregation comparision operator e.g. $gt, $gte, ...
   * @param op
   * @param expr1
   * @param expr2
   * @return
   */
  def constructComparisonOpExpression(op:String, expr1:BsonValue, expr2:BsonValue):BsonValue = {
    BsonDocument(
      "$"+op -> BsonArray.fromIterable(Seq(expr1, expr2))
    )
  }

  /**
   * Mongodb path expression
   * @param path
   * @return
   */
  def constructPathExpression(path:String):BsonValue = {
    BsonString("$" + path)
  }

  /**
   * Mongodb ifnull expression
   * @param expr
   * @param elseExpr
   * @return
   */
  def constructIfNullExpression(expr:BsonValue, elseExpr:BsonValue):BsonValue = {
    BsonDocument("$ifnull" -> BsonArray.fromIterable(
      Seq(
        expr,
        elseExpr
      )
    ))
  }

  /**
   * Construct Mongo concat operator expression for given elements
   * @param concanatedElems Expressions indicating the elements to cancat with seperator |
   * @return
   */
  def constructConcatExpression(concanatedElems:Seq[BsonValue]):BsonValue = {
    BsonDocument(
      "$concat" ->
        BsonArray.fromIterable(
          concanatedElems
        )
    )
  }

  /**
   * Mongodb map expression
   * @param inputExpr
   * @param mapExpr
   * @return
   */
  def constructMapExpression(inputExpr:BsonValue, mapExpr:BsonValue):BsonValue = {
    BsonDocument(
      "$map" ->
        BsonDocument(Seq(
          "input" -> inputExpr,
          "in" -> mapExpr
        ))
    )
  }

  /**
   * Mongodb filter expression
   * @param inputExpr
   * @param condExpr
   * @return
   */
  def constructFilterExpression(inputExpr:BsonValue, condExpr:BsonValue):BsonValue = {
    BsonDocument(
      "$filter" ->
        BsonDocument(
          "input" -> inputExpr,
          "cond" -> condExpr
        )
    )
  }

  /**
   * Mongodb in aggregation operator
   * @param path
   * @param values
   * @return
   */
  def constructInExpression(path:BsonValue, values:Seq[BsonValue]):BsonValue = {
    BsonDocument(
      "$in" -> BsonArray.fromIterable(
        Seq(
          path,
          BsonArray.fromIterable(values)
        )
      )
    )
  }

  def constructCondExpression(iff:BsonValue, ithen:BsonValue, ielse:BsonValue):BsonValue = {
    BsonDocument(
      "$cond" ->
        BsonDocument(
          "if" -> iff,
          "then" -> ithen,
          "else" -> ielse
        )
    )
  }
}

package io.onfhir.db

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonString, BsonValue}

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

  /**
   * Construct Mongodb $lookup phase expression with both fields and pipeline
   * @param col               Collection name to join
   * @param localFieldPath    Path to the local field
   * @param foreignFieldPath  Path to the foreign field
   * @param pipeline          Pipeline to execute (at least one should be provided)
   * @param as                The results name
   * @return
   */
  def constructLookupPhaseExpression(col:String, localFieldPath:String, foreignFieldPath:String, pipeline:Seq[BsonDocument], as:String):BsonDocument = {
    BsonDocument(
      "$lookup" -> BsonDocument.apply(
        "from" -> BsonString(col),
        "localField" -> BsonString(localFieldPath),
        "foreignField" -> BsonString(foreignFieldPath),
        "pipeline" -> BsonArray.fromIterable(pipeline),
        "as" -> BsonString(as)
      )
    )
  }

  /**
   * Construct Mongodb $lookup phase expression with both fields and pipeline and let
   *
   * @param col                Collection name to join
   * @param localFieldPath     Path to the local field
   * @param foreignFieldPath   Path to the foreign field
   * @param letVariablePathMap Variable name -> path e.g. rid --> id, rtype --> resourceType
   * @param pipeline           Pipeline to execute (at least one should be provided)
   * @param as                 The results name
   * @return
   */
  def constructLookupPhaseExpression(col: String, localFieldPath: String, foreignFieldPath: String, letVariablePathMap:Map[String, String], pipeline: Seq[BsonDocument], as: String): BsonDocument = {
    BsonDocument(
      "$lookup" -> BsonDocument.apply(
        "from" -> BsonString(col),
        "localField" -> BsonString(localFieldPath),
        "foreignField" -> BsonString(foreignFieldPath),
        "let" -> BsonDocument(letVariablePathMap.map(v => v._1 -> BsonString("$"+v._2))),
        "pipeline" -> BsonArray.fromIterable(pipeline),
        "as" -> BsonString(as)
      )
    )
  }

  /**
   * Construct the Mongodb expression for checking if given array field size is larger than given value
   * @param field Field name
   * @param size  Size
   * @return
   */
  def constructGreaterThanSizeExpression(field:String, size:Int):BsonDocument = {
    BsonDocument(field -> BsonDocument("$gt" -> BsonDocument("$size" -> BsonInt32(size))))
  }

  /**
   *
   * @param field1
   * @param field2
   * @return
   */
  def constructAggEqual(field1:BsonValue, field2:BsonValue):BsonDocument = {
    BsonDocument("$eq" -> BsonArray(field1,field2))
  }

  def constructAggNotEqual(field1:BsonValue, field2:BsonValue): BsonDocument = {
    BsonDocument("$ne" -> BsonArray(field1,field2))
  }
}

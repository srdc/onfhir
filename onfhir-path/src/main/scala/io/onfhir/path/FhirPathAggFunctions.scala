package io.onfhir.path

import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import org.json4s.JsonAST.JObject

/**
 * Default FHIR Path function library for onFhir to calculate some aggregations
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathAggFunctions(context:FhirPathEnvironment, current:Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {

  /**
   * Sum of the input numeric values, 0 if input list is empty
   * @return
   */
  def sum():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Invalid function call 'sum', one of the input values is not a numeric value!")

    current match {
      case Nil => Seq(FhirPathNumber(0))
      case oth =>
        Seq(
          oth
            .map(_.asInstanceOf[FhirPathNumber])
            .reduce((n1, n2) => n1 + n2)
        )
    }
  }

  /**
   * Sum of the values after executing the expression on given input values
   * @param expr Expression to calculate the sum
   * @return
   */
  def sum(expr:ExpressionContext):Seq[FhirPathResult] = {
    val results = current.map(c => {
      val result = new FhirPathExpressionEvaluator(context, Seq(c)).visit(expr)
      if(result.length > 1 || result.headOption.exists(!_.isInstanceOf[FhirPathNumber]))
        throw new FhirPathException(s"Invalid function call 'sum', the expression ${expr.getText} does not return a single numeric value or Nil!")
      result.headOption.map(_.asInstanceOf[FhirPathNumber]).getOrElse(FhirPathNumber(0))
    })

    if(results.isEmpty)
      Seq(FhirPathNumber(0))
    else
      Seq(results.reduce((r1, r2) => r1 + r2))
  }

  /**
   * Getting the average of given expression that returns a numeric value
   * @param expr  Expression to compute the average
   * @return
   */
  def avg(expr:ExpressionContext):Seq[FhirPathResult] = {
    if(current.isEmpty)
      Nil
    else
      Seq(sum(expr).head.asInstanceOf[FhirPathNumber] / FhirPathNumber(current.length))
  }

  /**
   * Average of the input numeric values
   * @return
   */
  def avg():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Invalid function call 'avg' on non numeric content!")
    if(current.isEmpty)
      Nil
    else
      Seq(current
        .map(_.asInstanceOf[FhirPathNumber])
        .reduce((n1, n2) => n1 + n2) / FhirPathNumber(current.length))
  }

  /**
   * Get the minimum of the input comparable values (numeric, dateTime
   * @return
   */
  def min():Seq[FhirPathResult] = {
    if(current.exists(c => c.isInstanceOf[FhirPathComplex] || c.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException(s"Invalid function call 'min' on non comparable values!")
    if(current.map(_.getClass.getName).toSet.size > 1)
      throw new FhirPathException(s"Invalid function call 'min', all compared values should be in same type!")


    if(current.isEmpty)
      Nil
    else {
      type T = FhirPathResult with Ordered[FhirPathResult]
      Seq(current.map(_.asInstanceOf[T]).reduce((r1,r2) => if(r1<r2) r1 else r2))
    }
  }

  /**
   * Getting the minimum of given expression that returns a comparable value
   * @param expr  Expression to calculate
   * @return
   */
  def min(expr:ExpressionContext):Seq[FhirPathResult] = {
    val results =
      current
        .flatMap(c => {
          val result = new FhirPathExpressionEvaluator(context, Seq(c)).visit(expr)
          if (result.length > 1)
            throw new FhirPathException(s"Invalid function call 'min', the expression ${expr.getText} does not return a single value or Nil!")
          result.headOption
        })

    if(results.isEmpty)
      Nil
    else {
      type T = FhirPathResult with Ordered[FhirPathResult]
      Seq(results.map(_.asInstanceOf[T]).reduce((r1,r2) => if(r1<r2) r1 else r2))
    }
  }

  /**
   * Getting the maximum of given expression that returns a comparable value
   * @param expr  Expression to calculate
   * @return
   */
  def max(expr:ExpressionContext):Seq[FhirPathResult] = {
    val results = current
      .flatMap(c => {
        val result = new FhirPathExpressionEvaluator(context, Seq(c)).visit(expr)
        if (result.length > 1)
          throw new FhirPathException(s"Invalid function call 'min', the expression ${expr.getText} does not return a single value or Nil!")
        result.headOption
      })

    if(results.isEmpty)
      Nil
    else {
      type T = FhirPathResult with Ordered[FhirPathResult]
      Seq(results.map(_.asInstanceOf[T]).reduce((r1,r2) => if(r1>r2) r1 else r2))
    }
  }

  /**
   * Getting the maximum of given input values
   * @return
   */
  def max():Seq[FhirPathResult] = {
    if(current.exists(c => c.isInstanceOf[FhirPathComplex] || c.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException(s"Invalid function call 'max' on non comparable values!")
    if(current.map(_.getClass.getName).toSet.size > 1)
      throw new FhirPathException(s"Invalid function call 'max', all compared values should be in same type!")

    if(current.isEmpty)
      Nil
    else {
      type T = FhirPathResult with Ordered[FhirPathResult]
      Seq(current.map(_.asInstanceOf[T]).reduce((r1,r2) => if(r1>r2) r1 else r2))
    }
  }

  /**
   * Group the values based on some expression returning a key and apply the aggregation to each group
   * @param groupByExpr     Expression for group by
   * @param aggregateExpr   Aggregation expression
   * @return                A list of special JSON Objects with field 'bucket' indicating the key for the bucket and 'agg' indicating the resulting aggregated value
   */
  def groupBy(groupByExpr:ExpressionContext, aggregateExpr:ExpressionContext):Seq[FhirPathResult] = {
    if(!current.forall(_.isInstanceOf[FhirPathComplex]))
      throw new FhirPathException("Invalid function call 'groupBy' on current value! The data type for current value should be complex object!")
    //Evaluate group by expression to calculate bucket keys
    val buckets = current.map(c => {
      val bucketKeyResults = new FhirPathExpressionEvaluator(context, Seq(c)).visit(groupByExpr)
      val bucketKey = FhirPathValueTransformer.serializeToJson(bucketKeyResults)
      bucketKey -> c
    })

    buckets
      .groupMap(_._1)(_._2)
      .map(bv => {
        //Evaluate aggregation for each group
        val aggValues = new FhirPathExpressionEvaluator(context, bv._2).visit(aggregateExpr)
        if(aggValues.length != 1 || aggValues.exists(!_.isInstanceOf[FhirPathNumber]))
          throw new FhirPathException(s"Invalid function call 'groupBy' on current value! The aggregation expression does not return single number for bucket ${bv._1}!")

        val aggValue = aggValues.head.toJson
        FhirPathComplex(JObject("bucket" -> bv._1, "agg" -> aggValue))
      }).toSeq
  }

}

object FhirPathAggFunctionsFactory extends IFhirPathFunctionLibraryFactory {
  final val defaultPrefix:String = "agg"
  override def getLibrary(context: FhirPathEnvironment, current: Seq[FhirPathResult]):FhirPathAggFunctions =
    new FhirPathAggFunctions(context, current)
}
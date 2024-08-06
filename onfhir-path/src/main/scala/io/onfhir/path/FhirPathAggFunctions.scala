package io.onfhir.path

import io.onfhir.path.annotation.FhirPathFunction
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the sum of the input numeric values. If the input list is empty, returns 0.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n9, 9.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** valueQuantity.value.agg:sum()",
    insertText = "agg:sum()",detail = "agg", label = "agg:sum", kind = "Method", returnType = Seq("number"), inputType = Seq("number"))
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the sum of the numeric values in the given expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`expr`**  \nThe expression of which the sum will be calculated.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n9, 9.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** agg:sum(valueQuantity.value)",
    insertText = "agg:sum(<expr>)",detail = "agg", label = "agg:sum", kind = "Function", returnType = Seq("number"), inputType = Seq())
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the average of the numeric values in the given expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`expr`**  \nThe expression of which the average will be calculated.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n5, 5.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** agg:avg($this.valueQuantity.value)",
    insertText = "agg:avg(<expr>)",detail = "agg", label = "agg:avg", kind = "Function", returnType = Seq("number"), inputType = Seq())
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the average of the input numeric values.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n5, 5.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** valueQuantity.value.agg:avg()",
    insertText = "agg:avg()",detail = "agg", label = "agg:avg", kind = "Method", returnType = Seq("number"), inputType = Seq("number"))
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the minimum of the input comparable values.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n1, 1.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** valueQuantity.value.agg:min()",
    insertText = "agg:min()",detail = "agg", label = "agg:min", kind = "Method", returnType = Seq("string","number","dateTime","time","quantity"), inputType = Seq("string","number","dateTime","time","quantity"))
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the minimum of the comparable values in the given expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`expr`**  \nThe expression of which the minimum will be found.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n1, 1.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** agg:min($this.valueQuantity.value)",
    insertText = "agg:min(<expr>)",detail = "agg", label = "agg:min", kind = "Function", returnType = Seq("string","number","dateTime","time","quantity"), inputType = Seq())
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the maximum of the comparable values in the given expression.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`expr`**  \nThe expression of which the maximum will be found.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n8, 8.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** agg:max($this.valueQuantity.value)",
    insertText = "agg:max(<expr>)",detail = "agg", label = "agg:max", kind = "Function", returnType = Seq("string","number","dateTime","time","quantity"), inputType = Seq())
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Returns the maximum of the input comparable values.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```\n8, 8.5, etc.\n``` \n\n\uD83D\uDCA1 **E.g.** valueQuantity.value.agg:max()",
    insertText = "agg:max()",detail = "agg", label = "agg:max", kind = "Method", returnType = Seq("string","number","dateTime","time","quantity"), inputType = Seq("string","number","dateTime","time","quantity"))
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
  @FhirPathFunction(documentation = "\uD83D\uDCDC Groups the values based on some expression returning a key and applies the aggregation expression to each of these groups.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`groupByExpr`**  \nAn expression to determine the key for grouping.\n\n\uD83D\uDCDD <span style=\"color:#ff0000;\">_@param_</span> **`aggregateExpr`**  \nThe aggregation expression to apply to each group.\n\n\uD83D\uDD19 <span style=\"color:#ff0000;\">_@return_</span>  \n```json\n[\n  {\n    \"bucket\": \"123\",\n    \"agg\": \"2\"\n  },\n  {\n    \"bucket\": \"456\",\n    \"agg\": \"3\"\n  },\n  {\n    \"bucket\": \"789\",\n    \"agg\": \"1\"\n  }\n]\n``` \n\uD83D\uDCA1 **E.g.** agg:groupBy(Condition.subject.reference.substring(8),count())",
    insertText = "agg:groupBy(<groupByExpr>,<aggregateExpr>)",detail = "agg", label = "agg:groupBy", kind = "Function", returnType = Seq(), inputType = Seq())
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

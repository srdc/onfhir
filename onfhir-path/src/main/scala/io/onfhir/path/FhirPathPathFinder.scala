package io.onfhir.path
import io.onfhir.api.util.FHIRUtil

import scala.jdk.CollectionConverters._
import io.onfhir.path.grammar.FhirPathExprParser
import io.onfhir.path.grammar.FhirPathExprParser.{FunctionInvocationContext, InvocationContext, MemberInvocationContext, ThisInvocationContext}
import io.onfhir.path.util.FhirPathUtil

/**
 * A special FHIR Path evaluator that focus on finding the paths indicated by the expression
 * Expression should indicate a path in the content, which restricts usage of operators and functions, if not throws exception
 * If expression indcates single or multiple paths, the paths can be accessed by calling getFoundPaths
 * @param context      General evaluation Context (input resource, environment variables, etc)
 * @param current      Current evaluation context (latest result)
 * @param targetType   FHIR Data type requested for evaluation of type operators 'as', 'is', etc
 */
class FhirPathPathFinder(context:FhirPathEnvironment, current:Seq[FhirPathResult],  targetType:Option[String] = None) extends FhirPathExpressionEvaluator(context, current, targetType) {
  //Filtering and subsetting functions
  val allowedFunctionsInPathIndicatingStatements = Set("where", "single", "first", "last", "tail", "skip", "skip", "take", "intersect", "exclude", "ofType", "as", "extension")

  /**
   * Return the found paths
   * Each path consist of the element names and array indexes of that element if exist
   * @return
   */
  def getFoundPaths:Seq[Seq[(String, Option[Int])]] = {
    val finalPaths = if(context.lastElementWasArrayEvaluation){
      context.foundPaths
        .map(p => p.substring(0, p.lastIndexOf('['))).distinct
    } else
      context.foundPaths

    finalPaths.map(fp =>
      fp.split('.')
        .map(s =>
          s.split('[').toSeq match {
            case Seq(s) => s -> None
            case Seq(s, e) => s -> Some(e.dropRight(1).toInt)
          }
        ).toSeq
    )
  }

  /**
   * Handle invocation expressions
   * e.g. Member invocation      Patient.given.name
   * e.g. Function invocation    Patient.given.name.exists()
   * @param ctx the parse tree
   *     */
  override def visitInvocationExpression(ctx: FhirPathExprParser.InvocationExpressionContext): Seq[FhirPathResult] = {
    ctx.invocation() match {
      case fn:FunctionInvocationContext =>
        val (fprefix, fname) = FhirPathUtil.getFunctionName(fn.function())
        val parent =
        //If this is a type function 'ofType', evaluate left expression with expected target type
          if(fprefix.isEmpty && typeFunctions.contains(fname)){
            val params = Option(fn.function().paramList()).map(_.expression().asScala).getOrElse(Nil)
            if(params.length != 1)
              throw new FhirPathException(s"Invalid function call $fname, it expects a single parameter which is the FHIR type identifier...")
            val fhirType = params.head.getText

            //Evaluate the parent with the target type
            new FhirPathPathFinder(context, current, Some(fhirType)).visit(ctx.expression())
          } else {
            //Otherwise evaluate left expression with the current context
            visit(ctx.expression())
          }

        new FhirPathPathFinder(context, parent).visitFunctionInvocation(fn)

      case m:MemberInvocationContext =>
        val parent = new FhirPathPathFinder(context, current).visit(ctx.expression())
        new FhirPathPathFinder(context, parent, targetType).visitMemberInvocation(m)
      case t:ThisInvocationContext =>
        visitThisInvocation(t)
      case i:InvocationContext =>
        visit(ctx.expression())
    }
  }


  override def visitMemberInvocation(ctx: FhirPathExprParser.MemberInvocationContext):Seq[FhirPathResult] = {
    //Element path
    val pathName = FhirPathLiteralEvaluator.parseIdentifier(ctx.identifier().getText) + targetType.getOrElse("") //if there is target type add it e.g. Observation.value as Quantity --> search for valueQuantity

    //Execute each path on the current results
    val resultsAndPaths =
      current
        .zip(context.foundPaths).zipWithIndex
        .map(c =>
          (new FhirPathExpressionEvaluator(context, Seq(c._1._1), targetType).visitMemberInvocation(ctx), c._1._2, c._2)
        )
        .map(rp =>
          rp._1 -> (
            rp._1.length match {
              case 0 => Nil
              case 1 => Seq(FHIRUtil.mergeElementPath(rp._2, pathName))
              case _ =>
                rp._1.indices.map(i => s"${FHIRUtil.mergeElementPath(rp._2, pathName)}[$i]")
            })
        )

    val results = resultsAndPaths.flatMap(_._1)
    if(resultsAndPaths.exists(_._1.length > 1))
      context.lastElementWasArrayEvaluation = true
    else
      context.lastElementWasArrayEvaluation = false
    //Update found paths
    context.foundPaths = resultsAndPaths.flatMap(_._2)
    //Return the result
    results
  }

  override def visitIndexerExpression(ctx: FhirPathExprParser.IndexerExpressionContext): Seq[FhirPathResult]  = {
    val result = super.visitIndexerExpression(ctx)
    if(result.nonEmpty) {
      //Evaluate array indice
      val indexValue = visit(ctx.expression(1)).head.asInstanceOf[FhirPathNumber].v.toInt
      context.foundPaths = Seq(context.foundPaths.apply(indexValue))
      context.lastElementWasArrayEvaluation = false
    } else
      context.foundPaths = Nil

    result
  }

  override def visitFunctionInvocation(ctx: FhirPathExprParser.FunctionInvocationContext): Seq[FhirPathResult] = {
    //Get the function name
    val (fprefix, fname) = FhirPathUtil.getFunctionName(ctx.function())
    if(fprefix.isEmpty && allowedFunctionsInPathIndicatingStatements.contains(fname)) {
      val filteredResults = new FhirPathExpressionEvaluator(context, current, targetType).visitFunctionInvocation(ctx)
      filteredResults match {
        case Nil => context.foundPaths = Nil
        case oth =>
          val selectedInd = oth.map(o => current.indexOf(o)).toSet
          context.foundPaths = context.foundPaths.zipWithIndex.filter(fp => selectedInd.contains(fp._2)).map(_._1)
          context.lastElementWasArrayEvaluation = false
      }
      filteredResults
    } else
      throw new FhirPathException(s"Given expression ${ctx.getText} does not indicate a path within FHIR resource!")
  }

  override def visitTypeExpression(ctx: FhirPathExprParser.TypeExpressionContext): Seq[FhirPathResult] = {
    val leftExpression = ctx.expression()
    val fhirType = ctx.typeSpecifier().getText
    val op = ctx.getRuleContext().getChild(1).getText
    //Evaluate the left expression with the expected type
    val result = new FhirPathPathFinder(context, current, Some(fhirType)).visit(leftExpression)
    if(result.length > 1)
      throw new FhirPathException(s"Invalid type operator $op, the expression ${leftExpression.getText} does not evaluate to single item collection!")

    op match {
      case "as" => result
      case "is" => throw new FhirPathException(s"Given expression ${ctx.getText} does not indicate a path within FHIR resource!")
    }
  }

  override def visitPolarityExpression(ctx: FhirPathExprParser.PolarityExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitAdditiveExpression(ctx: FhirPathExprParser.AdditiveExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitMultiplicativeExpression(ctx: FhirPathExprParser.MultiplicativeExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitUnionExpression(ctx: FhirPathExprParser.UnionExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitOrExpression(ctx: FhirPathExprParser.OrExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitAndExpression(ctx: FhirPathExprParser.AndExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitMembershipExpression(ctx: FhirPathExprParser.MembershipExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitInequalityExpression(ctx: FhirPathExprParser.InequalityExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitEqualityExpression(ctx: FhirPathExprParser.EqualityExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitImpliesExpression(ctx: FhirPathExprParser.ImpliesExpressionContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")


  override def visitExternalConstantTerm(ctx: FhirPathExprParser.ExternalConstantTermContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitNullLiteral(ctx: FhirPathExprParser.NullLiteralContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitBooleanLiteral(ctx: FhirPathExprParser.BooleanLiteralContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitStringLiteral(ctx: FhirPathExprParser.StringLiteralContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitDateTimeLiteral(ctx: FhirPathExprParser.DateTimeLiteralContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitTimeLiteral(ctx: FhirPathExprParser.TimeLiteralContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitQuantityLiteral(ctx: FhirPathExprParser.QuantityLiteralContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitExternalConstant(ctx: FhirPathExprParser.ExternalConstantContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitThisInvocation(ctx: FhirPathExprParser.ThisInvocationContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitTotalInvocation(ctx: FhirPathExprParser.TotalInvocationContext): Seq[FhirPathResult] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")
}

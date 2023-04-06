package io.onfhir.path

import io.onfhir.path.grammar.{FhirPathExprBaseVisitor, FhirPathExprParser}
import io.onfhir.path.grammar.FhirPathExprParser.{AndExpressionContext, BooleanLiteralContext, EqualityExpressionContext, ExpressionContext, ExternalConstantTermContext, FunctionContext, FunctionInvocationContext, IdentifierContext, InvocationContext, InvocationExpressionContext, InvocationTermContext, LiteralTermContext, MemberInvocationContext, NumberLiteralContext, ParenthesizedTermContext, StringLiteralContext, TermExpressionContext, ThisInvocationContext, TypeExpressionContext}
import io.onfhir.path.util.FhirPathUtil

import scala.jdk.CollectionConverters._

/**
 * Go over the FHIR Path Expression tree and return the path parts for a FHIR Path path expression (SearchParameter.expression) that defines a path in FHIR content
 * Each path part consist of element name e.g. code (or element name with index e.g coding[1]) and a set of restrictions on the path (e.g. "type" -> "email")
 * e.g.
 *                - Account.subject.where(resolve() is Patient)                            --> Seq(Account -> Nil, subject -> Nil)
 *                - ActivityDefinition.useContext.code                                     --> Seq(ActivityDefinition -> Nil, useContext -> Nil, code -> Nil)
 *                - ActivityDefinition.relatedArtifact.where(type='composed-of').resource  --> Seq(ActivityDefinition -> Nil, relatedArtifact -> Seq(type -> composed-of), resource -> Nil)
 *                - Condition.abatement.as(Age)  * with as                                 --> Seq(Condition -> Nil, abatementAge -> Nil)
 *                - (ActivityDefinition.useContext.value as CodeableConcept)               --> Seq(ActivityDefinition -> Nil, useContext -> Nil, valueCodeableConcept -> Nil)
 *                - Bundle.entry[0].resource   * with an index                             --> Seq(Bundle -> Nil, entry[0] -> Nil, resource -> Nil)

 * @param latestPath Latest constructed path until now
 */
class FhirPathExtractor(latestPath:Seq[(String, Seq[(String, String)])] = Nil) extends FhirPathExprBaseVisitor[Seq[(String, Seq[(String, String)])]] {

  override def visitParenthesizedTerm(ctx: ParenthesizedTermContext): Seq[(String, Seq[(String, String)])] = {
    visit(ctx.expression())
  }

  override def visitTypeExpression(ctx: TypeExpressionContext): Seq[(String, Seq[(String, String)])] = {
    val op = ctx.getRuleContext().getChild(1).getText
    if(op != "as")
      throw new FhirPathException("Invalid FHIR Path path expression!")
    val path = new FhirPathExtractor(latestPath).visit(ctx.expression())

    val lp = path.last
    // If as is used for converting a FHIR resource to a specific resource type, return path
    // e.g. Bundle.entry[0].resource as Composition
    if(lp._1 == "resource" && path.dropRight(1).lastOption.exists(_._1.startsWith("entry")))
      path
    else {
      //Otherwise add the data type to the last path item e.g. Condition.abatement.as(Age) --> Condition, abatementAge
      val dataType = ctx.typeSpecifier().qualifiedIdentifier().getText
      path.dropRight(1) :+ (lp._1 + dataType.capitalize -> lp._2)
    }
  }
  /**
   * Handle invocation expressions
   * e.g. Member invocation      Patient.given.name
   * e.g. Function invocation    Patient.given.name.exists()
   * @param ctx the parse tree
   *     */
  override def visitInvocationExpression(ctx: FhirPathExprParser.InvocationExpressionContext): Seq[(String, Seq[(String, String)])] = {
    ctx.invocation() match {
      case fn:FunctionInvocationContext =>
        val path = new FhirPathExtractor(latestPath).visit(ctx.expression())

        val (fprefix, fname) = FhirPathUtil.getFunctionName(fn.function())
        if(fprefix.isDefined)
          throw new FhirPathException("Invalid FHIR Path path expression ")
        fname match {
          case "where" =>
            val params = fn.function().paramList().expression()
            if(params.size() !=1)
              throw new FhirPathException("Invalid FHIR Path path expression!")
            //This function can only be used if its element is an EqualityExpression with '=' or and AndExpression consisting of same type EqualityExpression
            params.get(0) match {
                //e.g. Account.subject.where(resolve() is Patient)  * with a resolve
                case t:TypeExpressionContext =>
                  val op = t.getRuleContext().getChild(1).getText
                  if(op != "is")
                    throw new FhirPathException("Invalid FHIR Path path expression!")
                  //We allow this only if the first one is resolve
                  checkResolve(t.expression())

                  path
                case a:AndExpressionContext =>
                  val eqExpressions = a.expression().asScala
                  if(!eqExpressions.forall(_.isInstanceOf[EqualityExpressionContext]))
                    throw new FhirPathException("Invalid FHIR Path path expression!")
                  val restrictions = eqExpressions.map(_.asInstanceOf[EqualityExpressionContext]).map(handleEqualityExpression)
                  val lp = path.last._1
                  path.dropRight(1) :+ (lp -> restrictions.toSeq)
                // e.g. ActivityDefinition.relatedArtifact.where(type='composed-of').resource
                case e:EqualityExpressionContext =>
                  val restrictions = Seq(handleEqualityExpression(e))
                  val lp = path.last._1
                  path.dropRight(1) :+ (lp -> restrictions)
                case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
              }
          // Used for search Parameter paths on extensions
          case "extension" =>
            val params = fn.function().paramList().expression()
            if(params.size() != 1)
              throw new FhirPathException("Invalid FHIR Path path expression!")
            //Find extension URL
            val url = getStringLiteral(params.get(0))
            path :+ ("extension" -> Seq("url" -> url))
          //
          case "as" | "ofType" =>
            val params = fn.function().paramList().expression()
            if(params.size() != 1)
              throw new FhirPathException("Invalid FHIR Path path expression!")
            //Find extension URL
            val dataType = getIdentifier(params.get(0))
            val lp = path.last
            path.dropRight(1) :+ (lp._1 + dataType.capitalize -> lp._2)
          case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
        }
      case m:MemberInvocationContext =>
        val path =  new FhirPathExtractor(latestPath).visit(ctx.expression())
        new FhirPathExtractor(path).visitMemberInvocation(m)
      case _ =>
        throw new FhirPathException("Invalid FHIR Path path expression ")
    }
  }

  /**
   * Get the identifier text otherwise return exception
   * @param expr
   * @return
   */
  private  def getIdentifier(expr:ExpressionContext):String = {
    expr match {
      case t:TermExpressionContext => t.term() match {
        case i:InvocationTermContext => i.invocation() match {
          case mi:MemberInvocationContext => FhirPathLiteralEvaluator.parseIdentifier(mi.identifier().getText)
          case _ =>  throw new FhirPathException("Invalid FHIR Path path expression!")
        }
        case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
      }
      case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
    }
  }

  /**
   * Get the string literal otherwise return exception
   * @param expr
   * @return
   */
  private def getStringLiteral(expr:ExpressionContext):String = {
    expr match {
      case t:TermExpressionContext => t.term() match {
        case lt:LiteralTermContext => lt.literal() match {
          case s:StringLiteralContext => s.STRING().getText.drop(1).dropRight(1)
          case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
        }
        case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
      }
      case _ =>  throw new FhirPathException("Invalid FHIR Path path expression!")
    }
  }

  private def checkResolve(expr:ExpressionContext) = {
    expr match {
      case t:TermExpressionContext => t.term() match {
        case i:InvocationTermContext => i.invocation() match {
          case f:FunctionInvocationContext =>
            val (fprefix, fname) = FhirPathUtil.getFunctionName(f.function())
            if((fname != "resolve" || fprefix.isDefined) || (f.function().paramList() != null && f.function().paramList().expression().size() > 0))
              throw new FhirPathException("Invalid FHIR Path path expression!")
          case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
        }
        case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
      }
      case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
    }
  }

  private def handleEqualityExpression(eq:EqualityExpressionContext):(String, String) = {
    val op = eq.getRuleContext().getChild(1).getText
    if(op != "=")
      throw new FhirPathException("Invalid FHIR Path path expression ")

    val elementName = getIdentifier(eq.expression(0))

    val elementValue = eq.expression(1) match {
      case t:TermExpressionContext => t.term() match {
        case l:LiteralTermContext => l.literal() match {
          case s:StringLiteralContext => s.STRING().getText.drop(1).dropRight(1)
          case n:NumberLiteralContext => n.getText
          case b:BooleanLiteralContext => b.getText
          case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
        }
        case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
      }
      case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
    }
    elementName -> elementValue
  }


  override def visitMemberInvocation(ctx: FhirPathExprParser.MemberInvocationContext): Seq[(String, Seq[(String, String)])]  = {
    //Element path
    val pathName = FhirPathLiteralEvaluator.parseIdentifier(ctx.identifier().getText)
    latestPath :+ (pathName -> Nil)
  }

  override def visitIndexerExpression(ctx: FhirPathExprParser.IndexerExpressionContext):Seq[(String, Seq[(String, String)])]  = {
    val path = new FhirPathExtractor(latestPath).visit(ctx.expression(0))
    ctx.expression(1) match {
      case t:TermExpressionContext => t.term() match {
        case l:LiteralTermContext => l.literal() match {
          case n:NumberLiteralContext =>
            val lp = path.last._1 + "[" + n.getText.toInt  +"]"
            path.dropRight(1) :+ (lp -> Nil)
          case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
        }
        case _ =>  throw new FhirPathException("Invalid FHIR Path path expression ")
      }
      case _ =>  throw new FhirPathException("Invalid FHIR Path path expression ")
    }
  }

  override def visitPolarityExpression(ctx: FhirPathExprParser.PolarityExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitAdditiveExpression(ctx: FhirPathExprParser.AdditiveExpressionContext): Seq[(String, Seq[(String, String)])]= throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitMultiplicativeExpression(ctx: FhirPathExprParser.MultiplicativeExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitUnionExpression(ctx: FhirPathExprParser.UnionExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitOrExpression(ctx: FhirPathExprParser.OrExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitAndExpression(ctx: FhirPathExprParser.AndExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitMembershipExpression(ctx: FhirPathExprParser.MembershipExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitInequalityExpression(ctx: FhirPathExprParser.InequalityExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitEqualityExpression(ctx: FhirPathExprParser.EqualityExpressionContext):Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitImpliesExpression(ctx: FhirPathExprParser.ImpliesExpressionContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitExternalConstantTerm(ctx: FhirPathExprParser.ExternalConstantTermContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitNullLiteral(ctx: FhirPathExprParser.NullLiteralContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitBooleanLiteral(ctx: FhirPathExprParser.BooleanLiteralContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitStringLiteral(ctx: FhirPathExprParser.StringLiteralContext): Seq[(String, Seq[(String, String)])]= throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitDateTimeLiteral(ctx: FhirPathExprParser.DateTimeLiteralContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitTimeLiteral(ctx: FhirPathExprParser.TimeLiteralContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitQuantityLiteral(ctx: FhirPathExprParser.QuantityLiteralContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitExternalConstant(ctx: FhirPathExprParser.ExternalConstantContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitThisInvocation(ctx: FhirPathExprParser.ThisInvocationContext): Seq[(String, Seq[(String, String)])] = throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")

  override def visitTotalInvocation(ctx: FhirPathExprParser.TotalInvocationContext): Seq[(String, Seq[(String, String)])]= throw new FhirPathException(s"Given expression  ${ctx.getText} does not indicate a path within FHIR resource!")
}

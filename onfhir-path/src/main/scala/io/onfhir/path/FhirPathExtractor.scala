package io.onfhir.path


import io.onfhir.path.grammar.{FhirPathExprBaseVisitor, FhirPathExprParser}
import io.onfhir.path.grammar.FhirPathExprParser.{AndExpressionContext, BooleanLiteralContext, EqualityExpressionContext, ExpressionContext, ExternalConstantTermContext, FunctionContext, FunctionInvocationContext, IdentifierContext, InvocationContext, InvocationExpressionContext, InvocationTermContext, LiteralTermContext, MemberInvocationContext, NumberLiteralContext, ParenthesizedTermContext, StringLiteralContext, TermExpressionContext, ThisInvocationContext}

import scala.collection.JavaConverters._

class FhirPathExtractor(latestPath:Seq[(String, Seq[(String, String)])]) extends FhirPathExprBaseVisitor[Seq[(String, Seq[(String, String)])]] {

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

        val fname = fn.function().identifier().getText
        fname match {
          case "where" =>
            fn.function().paramList().expression() match {
              case List(wexpr) => wexpr match {
                case a:AndExpressionContext =>
                  val eqExpressions = a.expression().asScala
                  if(!eqExpressions.forall(_.isInstanceOf[EqualityExpressionContext]))
                    throw new FhirPathException("Invalid FHIR Path path expression!")
                  val restrictions = eqExpressions.map(_.asInstanceOf[EqualityExpressionContext]).map(handleEqualityExpression)
                  val lp = path.last._1
                  path.dropRight(1) :+ (lp -> restrictions)
                case e:EqualityExpressionContext =>
                  val restrictions = Seq(handleEqualityExpression(e))
                  val lp = path.last._1
                  path.dropRight(1) :+ (lp -> restrictions)
                case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
              }
              case _ => throw new FhirPathException("Invalid FHIR Path path expression!")
            }
          case "extension" =>
          case "as" =>
          case "ofType" =>
          case "resolve" =>
          case "exists" =>
          case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
        }

        path
      case m:MemberInvocationContext =>
        val path =  new FhirPathExtractor(latestPath).visit(ctx.expression())
        new FhirPathExtractor(path).visitMemberInvocation(m)
      case _ =>
        throw new FhirPathException("Invalid FHIR Path path expression ")
    }
  }

  private def handleEqualityExpression(eq:EqualityExpressionContext):(String, String) = {
    val op = eq.getRuleContext().getChild(1).getText
    if(op != "=")
      throw new FhirPathException("Invalid FHIR Path path expression ")

    val elementName = eq.expression(0) match {
      case t:TermExpressionContext => t.term() match {
        case i:InvocationTermContext => i.invocation() match {
          case idt:IdentifierContext => idt.getText
          case _=> throw new FhirPathException("Invalid FHIR Path path expression ")
        }
        case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
      }
      case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
    }

    val elementValue = eq.expression(0) match {
      case t:TermExpressionContext => t.term() match {
        case l:LiteralTermContext => l.literal() match {
          case s:StringLiteralContext => s.STRING().getText
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


  /*

  def extractPathItems():Seq[(String, Seq[(String, String)])] = {
    visitExpression(expression)
    pathItems
  }

  def visitExpression(context: ExpressionContext):Unit = {

    expression match {
      case i:InvocationExpressionContext => visitInvocationExpressionContext(i)
      case t:TermExpressionContext => visitTermExpressionContext(t)
    }
  }

  def visitTermExpressionContext(t:TermExpressionContext) = {
    t.term() match {
      case it:InvocationTermContext => visitInvocationContext(it.invocation())
      case lt:LiteralTermContext => throw new FhirPathException("Invalid FHIR Path path expression ")
      case ex:ExternalConstantTermContext => throw new FhirPathException("Invalid FHIR Path path expression ")
      case p: ParenthesizedTermContext => throw new FhirPathException("Invalid FHIR Path path expression ")
    }
  }

  def visitInvocationExpressionContext(invocationExpressionContext: InvocationExpressionContext):Unit = {
    val expr1 = invocationExpressionContext.expression()
    val expr2 = invocationExpressionContext.invocation()
    visitExpression(expr1)
    visitInvocationContext(expr2)
  }

  def visitInvocationContext(context: InvocationContext):Unit = {
    context match {
      case i:MemberInvocationContext => visitMemberInvocationContext(i)
      case f:FunctionInvocationContext => throw new FhirPathException("Invalid FHIR Path path expression ")
      case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
    }
  }

  def visitMemberInvocationContext(m:MemberInvocationContext):Unit = {
    val pathItem = getPathItemFromIdentifier(m.identifier())
    //Add the path item
    pathItems :+ (pathItem -> Nil)
  }

  /*
  def extractPathItemsFromTermExpressionContext(termExpressionContext: TermExpressionContext):Seq[(String, Seq[(String, String)])] = {
    termExpressionContext.term() match {
      case it:InvocationTermContext =>
      case lt:LiteralTermContext => throw new FhirPathException("Invalid FHIR Path path expression ")
      case ex:ExternalConstantTermContext => throw new FhirPathException("Invalid FHIR Path path expression ")
      case p: ParenthesizedTermContext => throw new FhirPathException("Invalid FHIR Path path expression ")
    }
  }*/

  def getPathItemFromIdentifier(identifierContext: IdentifierContext):String = {
    identifierContext.getRuleIndex match {
      case 0 => identifierContext.IDENTIFIER().getText
      case _ => throw new FhirPathException("Invalid FHIR Path path expression ")
    }
  }*/

}

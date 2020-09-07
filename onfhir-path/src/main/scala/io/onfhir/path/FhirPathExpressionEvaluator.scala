package io.onfhir.path

import io.onfhir.api.FHIR_COMMON_FIELDS
import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.grammar.FhirPathExprParser.{FunctionInvocationContext, InvocationContext, MemberInvocationContext, ThisInvocationContext}
import io.onfhir.path.grammar.{FhirPathExprBaseVisitor, FhirPathExprParser}
import org.json4s._

import collection.JavaConverters._
import scala.language.implicitConversions
import io.onfhir.config.FhirConfigurationManager.fhirConfig

/**
  * FHIR Path expression evaluator
  * @param context      General evaluation Context (input resource, environment variables, etc)
  * @param current      Current evaluation context (latest result)
  * @param targetType   FHIR Data type requested for evaluation of type operators 'as', 'is', etc
  */
class FhirPathExpressionEvaluator(context:FhirPathEnvironment, current:Seq[FhirPathResult], targetType:Option[String] = None) extends FhirPathExprBaseVisitor[Seq[FhirPathResult]] {

  final val typeFunctions = Set("as", "is", "ofType")

  /**
    * Handle all literals
    * @param ctx the parse tree
    *     */
  override def visitLiteralTerm(ctx: FhirPathExprParser.LiteralTermContext): Seq[FhirPathResult] = {
    FhirPathLiteralEvaluator.visit(ctx.literal())
  }

  /**
    * Handle direct invocation terms
    * @param ctx the parse tree
    *     */
  override def visitInvocationTerm(ctx: FhirPathExprParser.InvocationTermContext):  Seq[FhirPathResult] = {
    ctx.invocation() match {
      case mi:MemberInvocationContext =>
        val pathOrResourceType = FhirPathLiteralEvaluator.parseIdentifier(mi.identifier().getText)
        //If it is a resource type
        if(pathOrResourceType.head.isUpper){
          current
            .filter(c => c.isInstanceOf[FhirPathComplex])
            .map(c => c.asInstanceOf[FhirPathComplex])
            .filter(c => FHIRUtil.extractValueOption[String](c.json, FHIR_COMMON_FIELDS.RESOURCE_TYPE).contains(pathOrResourceType)) //Resource type should match
        } else
          visitMemberInvocation(mi) //Otherwise it is just element path

      //Function invocation at root
      case fi:FunctionInvocationContext => visitFunctionInvocation(fi)

      case t:ThisInvocationContext => current
    }
  }

  /**
    * Handle external environment variables
    * @param ctx the parse tree
    *     */
  override def visitExternalConstantTerm(ctx: FhirPathExprParser.ExternalConstantTermContext):  Seq[FhirPathResult] = {
    val externalConstant = ctx.externalConstant().getText()
    context.getEnvironmentContext(externalConstant)
  }

  override def visitParenthesizedTerm(ctx: FhirPathExprParser.ParenthesizedTermContext): Seq[FhirPathResult] = {
    val result = visit(ctx.expression())
    result
  }

  /**
    * Handle type expressions 'as' and 'is'
    * @param ctx the parse tree
    *     */
  override def visitTypeExpression(ctx: FhirPathExprParser.TypeExpressionContext): Seq[FhirPathResult] = {
    val leftExpression = ctx.expression()
    val fhirType = ctx.typeSpecifier().getText
    val op = ctx.getRuleContext().getChild(1).getText
    //Evaluate the left expression with the expected type
    val result = new FhirPathExpressionEvaluator(context, current, Some(fhirType)).visit(leftExpression)
    if(result.length > 1)
      throw new FhirPathException(s"Invalid type operator $op, the expression ${leftExpression.getText} does not evaluate to single item collection!")

    op match {
      case "as" => result
      case "is" => Seq(FhirPathBoolean(result.nonEmpty))
    }
  }

  /**
    * Evaluating an path selection expression  e.g. name.given --> given
    * @param ctx the parse tree
    *     */
  override def visitMemberInvocation(ctx: FhirPathExprParser.MemberInvocationContext):Seq[FhirPathResult] = {
    //Element path
    val pathName = FhirPathLiteralEvaluator.parseIdentifier(ctx.identifier().getText) + targetType.getOrElse("") //if there is target type add it e.g. Observation.value as Quantity --> search for valueQuantity

    //Execute the path and return
    current
      .filter(_.isInstanceOf[FhirPathComplex]) //Only get the complex objects
      .flatMap(r => {
        FhirPathValueTransformer.transform(r.asInstanceOf[FhirPathComplex].json \ pathName) match { //Execute JSON path for each element
          //The field can be a multi valued so we should check if there is a field starting with the path
          case Nil if targetType.isEmpty =>
            r.asInstanceOf[FhirPathComplex].json.obj
              .find(f => f._1.startsWith(pathName) && f._1.length > pathName.length && f._1.drop(pathName.length).head.isUpper)
              .map(f => FhirPathValueTransformer.transform(f._2))
              .getOrElse(Nil) //If not found still return nil
          //Oth
          case oth => oth
        }
      })
  }

  /**
    * Evaluating a array path e.g. Patient.name[1]
    * @param ctx the parse tree
    *     */
  override def visitIndexerExpression(ctx: FhirPathExprParser.IndexerExpressionContext): Seq[FhirPathResult]  = {
    //Evaluate array indice
    val indexValue = visit(ctx.expression(1))
    if(indexValue.length !=1 || !indexValue.head.isInstanceOf[FhirPathNumber] || !indexValue.head.asInstanceOf[FhirPathNumber].isInteger() )
      throw new FhirPathException(s"Evaluated array index is not a integer: $indexValue, at ${ctx.expression(0).getText}" )
    val arrayIndex = indexValue.head.asInstanceOf[FhirPathNumber].v.toInt
    //Evaluate the left part
    val leftValue = visit(ctx.expression(0))
    if(arrayIndex <0 || arrayIndex >= leftValue.length)
      Nil
    else
      Seq(leftValue.apply(arrayIndex))
  }

  /**
    * Polarity expression e.g. - 3.2
    * @param ctx the parse tree
    *     */
  override def visitPolarityExpression(ctx: FhirPathExprParser.PolarityExpressionContext): Seq[FhirPathResult] = {
    val result = visit(ctx.expression())
    if(result.length != 1 && !(result.head.isInstanceOf[FhirPathNumber] | result.head.isInstanceOf[FhirPathNumber]))
      throw new FhirPathException(s"Unary operators '+' and '-' can be used only on number values, at ${ctx.expression().getText}!!!")

    ctx.getRuleContext().getChild(0).getText match {
      case "-" => Seq(result.head.asInstanceOf[FhirPathNumber].-())
      case "+" => result
    }
  }

  /**
    * Handle Additive expressions e.g. Observation.(value as Quantity).value + 7
    * @param ctx the parse tree
    *     */
  override def visitAdditiveExpression(ctx: FhirPathExprParser.AdditiveExpressionContext): Seq[FhirPathResult]  = {
    val operand1 = visit(ctx.expression(0))
    val operand2 = visit(ctx.expression(1))

    if(operand1.length > 1 || operand2.length > 1)
      throw new FhirPathException(s"Additive operations like ${ctx.getRuleContext().getChild(1).getText} should be applied on single values on both sides !!!")

    val result:FhirPathResult = ctx.getRuleContext().getChild(1).getText match {
      case "+" =>
        if(operand1.isEmpty || operand2.isEmpty)
          null
         else
          (operand1.head, operand2.head) match {
            case (i1:FhirPathNumber, i2:FhirPathNumber) => i1 + i2
            case (s1:FhirPathString, s2:FhirPathString) => s1 + s2
            case (dt:FhirPathDateTime, quantity: FhirPathQuantity) => dt + quantity
            case (t:FhirPathTime, quantity: FhirPathQuantity) => t + quantity
            case (a1, a2) => throw new FhirPathException(s"Invalid additive operation between $a1 and $a2 !!!")
          }
      case "-" =>
        if(operand1.isEmpty || operand2.isEmpty)
          null
        else
          (operand1.head, operand2.head) match {
            case (i1:FhirPathNumber, i2:FhirPathNumber) => i1 - i2
            case (dt:FhirPathDateTime, quantity: FhirPathQuantity) => dt - quantity
            case (t:FhirPathTime, quantity: FhirPathQuantity) => t - quantity
            case (a1, a2) => throw new FhirPathException(s"Invalid additive operation between $a1 and $a2 !!!")
          }
      case "&" => (operand1.headOption, operand2.headOption) match {
        case (Some(s1), Some(s2)) if s1.isInstanceOf[FhirPathString] && s2.isInstanceOf[FhirPathString]  =>
            s1.asInstanceOf[FhirPathString] + s2.asInstanceOf[FhirPathString]

        case (None, s2) => s2.getOrElse(FhirPathString(""))
        case (s1, None) => s1.getOrElse(FhirPathString(""))
        case (a1, a2) => throw new FhirPathException(s"Invalid additive operation between $a1 and $a2 !!!")
      }
    }
    if(result == null) Nil else Seq(result)
  }

  /**
    * Handle multiplicative expressions e.g. 7 * 3
    * @param ctx the parse tree
    *     */
  override def visitMultiplicativeExpression(ctx: FhirPathExprParser.MultiplicativeExpressionContext) :Seq[FhirPathResult]  = {
    val operand1 = visit(ctx.expression(0))
    val operand2 = visit(ctx.expression(1))

    if(operand1.isEmpty || operand2.isEmpty)
      Nil
    else {
      if (operand1.length != 1 || operand2.length != 1)
        throw new FhirPathException(s"Multiplicative operations like ${ctx.getRuleContext().getChild(1).getText} should be applied on single values on both sides !!!")

      val result: FhirPathResult = ctx.getRuleContext().getChild(1).getText match {
        case "*" =>
          (operand1.head, operand2.head) match {
            case (i1: FhirPathNumber, i2: FhirPathNumber) => i1 * i2
            case (a1, a2) => throw new FhirPathException(s"Invalid '*' operation between $a1 and $a2 !!!")
          }
        case "/" =>
          (operand1.head, operand2.head) match {
            case (i1: FhirPathNumber, i2: FhirPathNumber) => i1 / i2
            case (a1, a2) => throw new FhirPathException(s"Invalid '/' operation between $a1 and $a2 !!!")
          }
        case "mod" => (operand1.head, operand2.head) match {
          case (i1: FhirPathNumber, i2: FhirPathNumber) => i1 mod i2
          case (a1, a2) => throw new FhirPathException(s"Invalid 'mod' operation between $a1 and $a2 !!!")
        }
        case "div" => (operand1.head, operand2.head) match {
          case (i1: FhirPathNumber, i2: FhirPathNumber) => i1 div i2
          case (a1, a2) => throw new FhirPathException(s"Invalid 'div' operation between $a1 and $a2 !!!")
        }
      }
      Seq(result)
    }
  }

  /**
    * Handle union '|' expression
    * @param ctx the parse tree
    *     */
  override def visitUnionExpression(ctx: FhirPathExprParser.UnionExpressionContext): Seq[FhirPathResult] = {
    val operand1 = visit(ctx.expression(0))
    val operand2 = visit(ctx.expression(1))
    //Union of unique results
    (operand1 ++ operand2).distinct
  }

  /**
    * Handle or and xor operators
     * @param ctx the parse tree
    *     */
  override def visitOrExpression(ctx: FhirPathExprParser.OrExpressionContext): Seq[FhirPathResult] = {
    val operand1 = visit(ctx.expression(0))

    if (operand1.length > 1)
      throw new FhirPathException(s"Logical operations like 'or' should be applied on single values on both sides !!!")

    ctx.getRuleContext().getChild(1).getText match {
      case "or" =>
        operand1.headOption match {
          //true no need to look second operand
          case Some(FhirPathBoolean (true)) => Seq(FhirPathBoolean (true))
          case oth =>
            val operand2 = visit(ctx.expression(1))
            if (operand2.length > 1)
              throw new FhirPathException(s"Logical operations like 'or' should be applied on single values on both sides !!!")
            (oth, operand2.headOption) match {
              case (None, Some (FhirPathBoolean (true) ) ) => Seq (FhirPathBoolean (true))
              case (None,  Some(FhirPathBoolean (false))) => Nil
              case (None, None) => Nil
              case (Some (FhirPathBoolean (false)), Some (FhirPathBoolean (true) ) ) => Seq (FhirPathBoolean (true))
              case (Some (FhirPathBoolean (false)), Some(FhirPathBoolean (false))) => Seq(FhirPathBoolean (false))
              case (Some (FhirPathBoolean (false)), None) => Nil
              case (a1, a2) => throw new FhirPathException(s"Invalid 'or' operation between non boolean operands $a1 and $a2 !!!")
            }
        }

      case "xor" =>
        val operand2 = visit(ctx.expression(1))
        (operand1.headOption, operand2.headOption) match {
          case (None, _) => Nil
          case (_, None) => Nil
          case (Some(b1: FhirPathBoolean), Some(b2: FhirPathBoolean) ) => Seq(b1 xor b2)
          case (a1, a2) => throw new FhirPathException(s"Invalid 'xor' operation between $a1 and $a2 !!!")
        }
    }
  }

  /**
    * Handle 'and' operator
    * @param ctx the parse tree
    *     */
  override def visitAndExpression(ctx: FhirPathExprParser.AndExpressionContext): Seq[FhirPathResult] = {
    val operand1 = visit(ctx.expression(0))

    if (operand1.length > 1)
      throw new FhirPathException(s"Logical operations like 'and' should be applied on single values on both sides, operand 1 returns multiple values!!!")

    operand1.headOption match {
      case Some(FhirPathBoolean(false)) => Seq(FhirPathBoolean (false))
      case oth =>
        val operand2 = visit(ctx.expression(1))
        if (operand2.length > 1)
          throw new FhirPathException(s"Logical operations like 'and' should be applied on single values on both sides, operand 2 returns multiple values!!!")

        (oth, operand2.headOption) match {
          case (None, Some (FhirPathBoolean (true) ) ) => Nil
          case (None, Some (FhirPathBoolean (false) ) ) => Seq(FhirPathBoolean (false))
          case (None, None) => Nil
          case (Some(FhirPathBoolean(true)), None) => Nil
          case (Some(FhirPathBoolean(true)), Some (FhirPathBoolean (true) )) =>  Seq(FhirPathBoolean (true))
          case (Some(FhirPathBoolean(true)), Some (FhirPathBoolean (false) )) =>  Seq(FhirPathBoolean (false))
          case (a1, a2) => throw new Exception(s"Invalid 'and' operation between non boolean operands $a1 and $a2 !!!")
        }
    }
  }

  /**
    * Handle implies operator
    * @param ctx the parse tree
    *     */
  override def visitImpliesExpression(ctx: FhirPathExprParser.ImpliesExpressionContext): Seq[FhirPathResult] = {
    val operand1 = visit(ctx.expression(0))
    if (operand1.length > 1)
      throw new FhirPathException(s"Logical operations like 'implies' should be applied on single values on left side !!!")

    operand1.headOption match {
      case Some(FhirPathBoolean(true))  =>
        val operand2 = visit(ctx.expression(1))
        if (operand2.length > 1)
          throw new FhirPathException(s"Logical operations like 'implies' should be applied on single values on right side !!!")
        else operand2
      //If left is false, then it is true
      case Some(FhirPathBoolean(false)) => Seq(FhirPathBoolean(true))
      case _ =>
        val operand2 = visit(ctx.expression(1))
        if (operand2.length > 1)
          throw new FhirPathException(s"Logical operations like 'implies' should be applied on single values on right side !!!")
        operand2 match{
          case Seq(FhirPathBoolean(true)) => operand2
          case _ => Nil
        }
    }

  }

  /**
    * Handle 'in' and 'contains' operators
    * @param ctx the parse tree
    *     */
  override def visitMembershipExpression(ctx: FhirPathExprParser.MembershipExpressionContext): Seq[FhirPathResult] = {
    val operand1 = visit(ctx.expression(0))
    val operand2 = visit(ctx.expression(1))
    ctx.getRuleContext().getChild(1).getText match {
      case "in" =>
        operand1 match {
          case Nil => Nil
          case Seq(a) => Seq(FhirPathBoolean(operand2.contains(a)))
          case _ =>   throw new FhirPathException(s"Left operand ${ctx.expression(0).getText} should return single value for 'in' operation !!!")
        }
      case "contains" =>
        operand2 match {
          case Nil => Nil
          //If this is a function call contains on string
          case Seq(FhirPathString(ss)) if operand1.length == 1 && operand1.head.isInstanceOf[FhirPathString] =>
            val leftStr = operand1.head.asInstanceOf[FhirPathString].s
            Seq(FhirPathBoolean(ss == "" || leftStr.contains(ss)))
          case Seq(a) =>
            Seq(FhirPathBoolean(operand1.contains(a)))
          case _ =>   throw new FhirPathException(s"Right operand ${ctx.expression(1).getText} should return single value for 'contains' operation !!!")
        }
    }
  }

  /**
    * Handle comparison operators e.g. 5 > 3
    * @param ctx the parse tree
    *     */
  override def visitInequalityExpression(ctx: FhirPathExprParser.InequalityExpressionContext): Seq[FhirPathResult] = {
    val operand1 = visit(ctx.expression(0))
    val operand2 = visit(ctx.expression(1))
    if(operand1.isEmpty || operand2.isEmpty)
      Nil
    else {
      if (operand1.length != 1 || operand2.length != 1)
        throw new FhirPathException(s"Inequality operations like ${ctx.getRuleContext().getChild(1).getText} should be applied on single values on both sides !!!")

      val op = ctx.getRuleContext().getChild(1).getText

      val comparison = (operand1.head, operand2.head) match {
        case (n1:FhirPathNumber, n2:FhirPathNumber) => n1.compare(n2)
        case (dt1:FhirPathDateTime, dt2:FhirPathDateTime) => dt1.compare(dt2)
        case (t1:FhirPathTime, t2:FhirPathTime) => t1.compare(t2)
        case (s1:FhirPathString, s2:FhirPathString) => s1.compare(s2)
        case (q1:FhirPathQuantity, q2:FhirPathQuantity) => q1.compare(q2)
        case (o1:FhirPathComplex, o2:FhirPathComplex) =>
          val q1 = o1.toQuantity()
          val q2 = o2.toQuantity()
          if(q1.isDefined && q2.isDefined)
            q1.get.compare(q2.get)
          else
            throw new FhirPathException(s"Invalid $op operation between ${o1.toJson} and ${o2.toJson} !!!")
        case (a1, a2) => throw new FhirPathException(s"Invalid $op operation between $a1 and $a2 !!!")
      }
      op match {
        case ">=" => Seq(FhirPathBoolean(comparison >= 0))
        case ">" => Seq(FhirPathBoolean(comparison > 0))
        case "<" => Seq(FhirPathBoolean(comparison < 0))
        case "<=" => Seq(FhirPathBoolean(comparison <= 0))
      }
    }
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
          val fname = fn.function().identifier().getText
          val parent =
            //If this is a type function 'ofType', evaluate left expression with expected target type
            if(typeFunctions.contains(fname)){
              val params = Option(fn.function().paramList()).map(_.expression().asScala).getOrElse(Nil)
              if(params.length != 1)
                throw new FhirPathException(s"Invalid function call $fname, it expects a single parameter which is the FHIR type identifier...")
              val fhirType = params.head.getText
              //Evaluate the parent with the target type
              new FhirPathExpressionEvaluator(context, current, Some(fhirType)).visit(ctx.expression())
            } else {
            //Otherwise evaluate left expression with the current context
              visit(ctx.expression())
            }

        new FhirPathExpressionEvaluator(context, parent).visitFunctionInvocation(fn)
      case m:MemberInvocationContext =>
        val parent = new FhirPathExpressionEvaluator(context, current).visit(ctx.expression())
        new FhirPathExpressionEvaluator(context, parent, targetType).visitMemberInvocation(m)
      case t:ThisInvocationContext =>
        visitThisInvocation(t)
      case i:InvocationContext =>
        visit(ctx.expression())
    }
  }

  override def visitFunctionInvocation(ctx: FhirPathExprParser.FunctionInvocationContext): Seq[FhirPathResult] = {
    //Get the function name
    val fname = ctx.function().identifier().getText()
    //Get the parameter expressions
    val paramExpressions = Option(ctx.function().paramList()).map(_.expression().asScala).getOrElse(Nil)
    //Evaluate the function
    new FhirPathFunctionEvaluator(context, current).callFunction(fname, paramExpressions)
  }

  /**
    * Handle equality operators
    * @param ctx the parse tree
    *     */
  override def visitEqualityExpression(ctx: FhirPathExprParser.EqualityExpressionContext): Seq[FhirPathResult] = {
    val operand1 = visit(ctx.expression(0))
    val operand2 = visit(ctx.expression(1))
    val op = ctx.getRuleContext().getChild(1).getText
    //If any operant is Nil, result is Nil
    if(operand1.isEmpty || operand2.isEmpty)
      Nil
    else {
      //If length are not equal false
      if(operand1.length != operand2.length)
        Seq(FhirPathBoolean(false))
      else {
        //If we are checking equivalency, order is not important
        val result =
          if(op == "~" || op == "!~"){
            Some(operand1.forall(o1 => operand2.exists(o2 => o1.getClass == o2.getClass && o1.isEquivalent(o2))))
          }
          //Otherwise order is important
          else {
            val temp = operand1.zip(operand2).map {
              case (r1, r2) if r1.getClass == r2.getClass => r1.isEqual(r2)
              case _ =>  Some(false)
            }
            //If there is any undefined comparison return undefined
            if(temp.contains(None)) None else Some(temp.flatten.forall( b => b))
          }

        if(result.isEmpty)
          Nil
        else
          if(op.startsWith("!"))
            Seq(FhirPathBoolean(!result.get))
          else
            Seq(FhirPathBoolean(result.get))
      }
    }
  }

}

package io.onfhir.path

import java.time.{LocalDate, LocalDateTime, LocalTime, Year, YearMonth, ZonedDateTime}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext
import org.apache.commons.text.StringEscapeUtils
import org.json4s.JsonAST.JObject

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
 * Evaluator for FHIR Path functions
 * @param context FhirPathContext
 * @param current Current evaluated FhirPath result (the function will execute on this results)
 */
class FhirPathFunctionEvaluator(context:FhirPathEnvironment, current:Seq[FhirPathResult]) extends AbstractFhirPathFunctionLibrary {

  /**
    *
    * @param fprefix    Function library prefix if external library (not an original FHIR Path function)
    * @param fname      Function name
    * @param params     Supplied parameters
    * @return
    */
  def callFunction(fprefix:Option[String], fname:String, params:Seq[ExpressionContext]):Seq[FhirPathResult] = {
    fprefix match {
      //It is an original FHIR Path function or calling it without specificying a prefix
      case None =>
        val functionName = if(fname == "toString") "_toString" else fname
        if(getFunctionSignatures().contains(functionName -> params.length))
          callFhirPathFunction(functionName, params)
        else {
          context
            .functionLibraries
            .values
            .find(_.getLibrary(context, current).getFunctionSignatures().contains(fname -> params.length)) match {
              case None =>  throw new FhirPathException(s"Invalid FHIR Path function call, function $fname does not exist or take ${params.length} arguments !!!")
              case Some(fnlibFactory) =>
                fnlibFactory
                  .getLibrary(context, current)
                  .asInstanceOf[AbstractFhirPathFunctionLibrary]
                  .callFhirPathFunction(fname, params)
          }
        }
      //Otherwise it is a external function
      case Some(prefix) =>
        context
          .functionLibraries
          .get(prefix) match {
            case None => throw new FhirPathException(s"Invalid function call, there is no function library registered with prefix $prefix !!!")
            case Some(fnlibFactory) =>
              fnlibFactory
                .getLibrary(context, current)
                .asInstanceOf[AbstractFhirPathFunctionLibrary]
                .callFhirPathFunction(fname, params)
          }
    }
  }

  /**
   * Resolve a reference
   * @return
   */
  def resolve():Seq[FhirPathResult] = {
    val fhirReferences = current.map {
      case FhirPathString(uri) => FHIRUtil.parseCanonicalReference(uri)
      case FhirPathComplex(o) => FHIRUtil.parseReference(o)
      case _ => throw new FhirPathException("Invalid function call 'resolve', it should be called on a canonical value or FHIR reference!")
    }

    fhirReferences
      .flatMap(fr => context.referenceResolver.flatMap(rr => Await.result(rr.resolveReference(fr), 1 minutes) ))
      .map(FhirPathComplex)
  }

  /**
   * Getting a specific extension
   * @param urlExp
   * @return
   */
  def extension(urlExp:ExpressionContext):Seq[FhirPathResult] = {
    val url = new FhirPathExpressionEvaluator(context, current).visit(urlExp)
    if(url.length != 1 || !url.head.isInstanceOf[FhirPathString])
      throw new FhirPathException(s"Invalid function call 'extension', expression ${urlExp.getText} does not return a url!")

    val expr = FhirPathEvaluator.parse(s"extension.where(url = '${url.head.asInstanceOf[FhirPathString].s}')")

    var result = new FhirPathExpressionEvaluator(context, current).visit(expr)
    result
  }


  /**
    * Type functions, for these basic casting or type checking are done before calling the function on the left expression
    */
  def ofType(typ:ExpressionContext):Seq[FhirPathResult] = current
  def as(typ:ExpressionContext):Seq[FhirPathResult] = current
  def is(typ:ExpressionContext):Seq[FhirPathResult] = {
    current.length match {
      case 0 => Seq(FhirPathBoolean(false))
      case 1 =>  Seq(FhirPathBoolean(true))
      case _ =>  throw new FhirPathException("Invalid function call 'is', it should be called on single item!")
    }
  }


  /**
    * Existence functions http://hl7.org/fhirpath/#existence
    * @return
    */
  def empty():Seq[FhirPathResult] = Seq(FhirPathBoolean(current.isEmpty))

  def not():Seq[FhirPathResult] = current match {
    case Nil => Nil
    case Seq(FhirPathBoolean(b)) => Seq(FhirPathBoolean(!b))
    case _ => throw  new FhirPathException("Function 'not' should run on FHIR path boolean!!!")
  }

  private def exists(expr:Option[ExpressionContext]):Seq[FhirPathResult] = {
    val result = expr match {
      case None =>  current.nonEmpty
      case Some(criteria) =>
        current
          .zipWithIndex
          .exists(c =>
            new FhirPathExpressionEvaluator(context.copy(_index = c._2), Seq(c._1))
              .visit(criteria) match {
                  case Seq(FhirPathBoolean(true)) => true
                  case _ => false
          })
    }
    Seq(FhirPathBoolean(result))
  }
  def exists(expr:ExpressionContext):Seq[FhirPathResult] = exists(Some(expr))
  def exists():Seq[FhirPathResult] = exists(None)

  def all(criteria:ExpressionContext):Seq[FhirPathResult] = {
    val result =
      current
        .zipWithIndex
        .forall(c =>
          new FhirPathExpressionEvaluator(context.copy(_index = c._2), Seq(c._1))
            .visit(criteria) match {
              case Seq(FhirPathBoolean(true)) => true
              case _ => false
        })
    Seq(FhirPathBoolean(result))
  }

  def allTrue():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'allTrue' should run on collection of FHIR Path boolean values!!!")
    val result = current.forall(c => c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  def anyTrue():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'anyTrue' should run on collection of FHIR Path boolean values!!!")
    val result = current.exists(c => c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  def allFalse():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'allFalse' should run on collection of FHIR Path boolean values!!!")
    val result = current.forall(c => !c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  def anyFalse():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new FhirPathException("Function 'anyFalse' should run on collection of FHIR Path boolean values!!!")
    val result = current.exists(c => !c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  def subsetOf(other:ExpressionContext):Seq[FhirPathResult] = {
    val otherCollection = new FhirPathExpressionEvaluator(context, current).visit(other)
    val result = current.forall(c => otherCollection.exists(o => c.isEqual(o).getOrElse(false)))
    Seq(FhirPathBoolean(result))
  }

  def supersetOf(other:ExpressionContext):Seq[FhirPathResult] = {
    val otherCollection = new FhirPathExpressionEvaluator(context, current).visit(other)
    val result =
      if(current.isEmpty)
        false
      else
        otherCollection.forall(o => current.exists(c => c.isEqual(o).getOrElse(false)))
    Seq(FhirPathBoolean(result))
  }

  def isDistinct():Seq[FhirPathResult] = {
    Seq(FhirPathBoolean(current.distinct.length == current.length))
  }

  def distinct():Seq[FhirPathResult] = {
    current.distinct
  }

  def count():Seq[FhirPathResult] = {
    Seq(FhirPathNumber(current.length))
  }

  /**
    * Filtering and projection http://hl7.org/fhirpath/#filtering-and-projection
    */
  def where(criteria : ExpressionContext):Seq[FhirPathResult] = {
    current
      .zipWithIndex
      .filter(c =>
        new FhirPathExpressionEvaluator(context.copy(_index= c._2), Seq(c._1))
          .visit(criteria) match {
            case Seq(FhirPathBoolean(true)) => true
            case Seq(FhirPathBoolean(false)) => false
            case Nil => false
            case _ => throw new FhirPathException(s"Invalid criteria ${criteria.getText} function call 'where', it does not evaluate to boolean!!!")
      })
      .map(_._1)
  }

  def select(projection: ExpressionContext):Seq[FhirPathResult] = {
    current
      .zipWithIndex
      .flatMap(c => {
        val r = new FhirPathExpressionEvaluator(context.copy(_index = c._2), Seq(c._1)).visit(projection)
        r
      })
  }

  def repeat(projection: ExpressionContext):Seq[FhirPathResult] = {
    val firstResults = select(projection)
    if(firstResults.nonEmpty)
      firstResults ++ new FhirPathFunctionEvaluator(context, firstResults).repeat(projection)
    else
      Nil
  }

  /**
    * Subsetting http://hl7.org/fhirpath/#subsetting
    */
  def single():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(s) => Seq(s)
      case _ => throw new FhirPathException(s"Function 'single' is called on a multi item collection $current!!!")
    }
  }

  def first():Seq[FhirPathResult] = current.headOption.toSeq
  def last():Seq[FhirPathResult] = current.lastOption.toSeq
  def tail():Seq[FhirPathResult] = if(current.isEmpty) Nil else current.tail
  def skip(numExpr:ExpressionContext):Seq[FhirPathResult] = {
    val numValue = new FhirPathExpressionEvaluator(context, current).visit(numExpr)
    if(numValue.length != 1 || !numValue.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException(s"Invalid function call 'skip', num expression ${numExpr.getText} does not return a single number!")
    val inum = numValue.head.asInstanceOf[FhirPathNumber]
    if(!inum.isInteger())
      throw new FhirPathException(s"Invalid function call 'skip', num expression ${numExpr.getText} does not return a integer")

    val i = inum.v.toInt
    if(i< 0)
      current
    else if(i > current.length)
      Nil
    else
      current.drop(i)
  }
  def take(numExpr:ExpressionContext):Seq[FhirPathResult] = {
    val numValue = new FhirPathExpressionEvaluator(context, current).visit(numExpr)
    if(numValue.length != 1 || !numValue.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException(s"Invalid function call 'take', num expression ${numExpr.getText} does not return a single number!")
    val inum = numValue.head.asInstanceOf[FhirPathNumber]
    if(!inum.isInteger())
      throw new FhirPathException(s"Invalid function call 'take', num expression ${numExpr.getText} does not return a integer")
    val i = inum.v.toInt
    if(i<=0) Nil
    else
      current.take(i)
  }

  def intersect(otherCollExpr:ExpressionContext):Seq[FhirPathResult] = {
    val otherSet = new FhirPathExpressionEvaluator(context, current).visit(otherCollExpr)
    current.filter(c => otherSet.exists(o => c.isEqual(o).getOrElse(false))).distinct
  }

  def exclude(otherCollExpr:ExpressionContext):Seq[FhirPathResult] = {
    val otherSet = new FhirPathExpressionEvaluator(context, current).visit(otherCollExpr)
    current.filterNot(c => otherSet.exists(o => c.isEqual(o).getOrElse(false))).distinct
  }

  /**
   * Returns the absolute value of the input. When taking the absolute value of a quantity, the unit is unchanged.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def abs():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(n.abs))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(q.v.abs), unit))
      case _ => throw new FhirPathException("Invalid function call 'abs' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the first integer greater than or equal to the input.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def ceiling():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.ceil(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.ceil(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'ceiling' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns e raised to the power of the input.
   * If the input collection contains an Integer, it will be implicitly converted to a Decimal and the result will be a Decimal.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def exp():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.exp(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.exp(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'exp' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the first integer less than or equal to the input.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def floor():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.floor(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.floor(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'floor' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the natural logarithm of the input (i.e. the logarithm base e).
   * When used with an Integer, it will be implicitly converted to a Decimal.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def ln():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.log(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.log(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'ln' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the logarithm base base of the input number.
   * When used with Integers, the arguments will be implicitly converted to Decimal.
   * If base is empty, the result is empty.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @param baseExp
   * @return
   */
  def log(baseExp:ExpressionContext):Seq[FhirPathResult] = {
    val baseResult = new FhirPathExpressionEvaluator(context, current).visit(baseExp)
    if(baseResult.length != 1 || !baseResult.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException("Invalid function call 'log', given base expression should return single numeric value!")
    val base = baseResult.head.asInstanceOf[FhirPathNumber].v
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.log(n.toDouble)/Math.log(base.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.log(q.v.toDouble)/Math.log(base.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'ln' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Raises a number to the exponent power. If this function is used with Integers, the result is an Integer. If the function is used with Decimals, the result is a Decimal. If the function is used with a mixture of Integer and Decimal, the Integer is implicitly converted to a Decimal and the result is a Decimal.
   * If the power cannot be represented (such as the -1 raised to the 0.5), the result is empty.
   * If the input is empty, or exponent is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @param exponentExpr
   * @return
   */
  def power(exponentExpr:ExpressionContext):Seq[FhirPathResult] = {
    val exponentResult = new FhirPathExpressionEvaluator(context, current).visit(exponentExpr)
    if(exponentResult.length != 1 || !exponentResult.head.isInstanceOf[FhirPathNumber])
      throw new FhirPathException("Invalid function call 'power', given exponent expression should return single numeric value!")
    val exponent = exponentResult.head.asInstanceOf[FhirPathNumber].v
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.pow(n.toDouble,exponent.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.pow(q.v.toDouble, exponent.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'power' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Rounds the decimal to the nearest whole number using a traditional round (i.e. 0.5 or higher will round to 1).
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def round():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(Math.round(n.toDouble)))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(Math.round(q.v.toDouble)), unit))
      case _ => throw new FhirPathException("Invalid function call 'power' on multi item collection or non-numeric value!")
    }
  }

  /**
   * The precision argument determines the decimal place at which the rounding will occur.
   * The number of digits of precision must be >= 0 or the evaluation will end and signal an error to the calling environment.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @param precisionExpr
   * @return
   */
  def round(precisionExpr:ExpressionContext):Seq[FhirPathResult] = {
    val precisionResult = new FhirPathExpressionEvaluator(context, current).visit(precisionExpr)
    precisionResult match {
      case Seq(n:FhirPathNumber) if n.isInteger() && n.v >0 =>
        val precision = n.v.toInt
        current match {
          case Nil => Nil
          case Seq(FhirPathNumber(n)) =>
            Seq(FhirPathNumber(n.setScale(precision, RoundingMode.HALF_UP)))
          case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(q.v.setScale(precision, RoundingMode.HALF_UP)), unit))
          case _ => throw new FhirPathException("Invalid function call 'round' on multi item collection or non-numeric value!")
        }
      case _ =>  throw new FhirPathException("Invalid function call 'round', given precision expression should return single positive integer value!")
    }
  }

  /**
   * Returns the square root of the input number as a Decimal.
   * If the square root cannot be represented (such as the square root of -1), the result is empty.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def sqrt():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) =>
        val result = Math.sqrt(n.toDouble)
        if(result.isNaN)
          Nil
        else
          Seq(FhirPathNumber(result))
      case Seq(FhirPathQuantity(q, unit)) =>
        val result = Math.sqrt(q.v.toDouble)
        if(result.isNaN)
          Nil
        else
          Seq(FhirPathQuantity(FhirPathNumber(result), unit))
      case _ => throw new FhirPathException("Invalid function call 'sqrt' on multi item collection or non-numeric value!")
    }
  }

  /**
   * Returns the integer portion of the input.
   * If the input collection is empty, the result is empty.
   * If the input collection contains multiple items, the evaluation of the expression will end and signal an error to the calling environment.
   * @return
   */
  def truncate():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathNumber(n)) => Seq(FhirPathNumber(n.toInt))
      case Seq(FhirPathQuantity(q, unit)) => Seq(FhirPathQuantity(FhirPathNumber(q.v.toInt), unit))
      case _ => throw new FhirPathException("Invalid function call 'truncate' on multi item collection or non-numeric value!")
    }
  }
  
  
  /**
    * Combining http://hl7.org/fhirpath/#combining
    * @param other
    * @return
    */
  def combine(other : ExpressionContext):Seq[FhirPathResult] = {
    val otherCollection = new FhirPathExpressionEvaluator(context, current).visit(other)
    current ++ otherCollection
  }

  /**
    * Conversion functions http://hl7.org/fhirpath/#conversion
    */
  def iif(criterium: ExpressionContext, trueResult:ExpressionContext, otherwiseResult: Option[ExpressionContext]):Seq[FhirPathResult] = {
    val criteriaResult = new FhirPathExpressionEvaluator(context, current).visit(criterium)
    val conditionResult = criteriaResult match {
      case Nil => false
      case Seq(FhirPathBoolean(false)) => false
      case Seq(FhirPathBoolean(true)) => true
    }
    if(conditionResult)
      new FhirPathExpressionEvaluator(context, current).visit(trueResult)
    else
      otherwiseResult.map(ore =>  new FhirPathExpressionEvaluator(context, current).visit(ore)).getOrElse(Nil)
  }
  def iif(criterium: ExpressionContext, trueResult:ExpressionContext, otherwiseResult: ExpressionContext) :Seq[FhirPathResult]= iif(criterium, trueResult, Some(otherwiseResult))
  def iif(criterium: ExpressionContext, trueResult:ExpressionContext):Seq[FhirPathResult] = iif(criterium, trueResult, None)


  /**
   * Integer conversion function
   * @return
   */
  def toInteger():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n:FhirPathNumber) if n.isInteger() => Seq(n)
      case Seq(n:FhirPathNumber) if !n.isInteger() =>  throw new FhirPathException(s"Invalid function call 'toInteger' on value $n !!!")
      case Seq(FhirPathString(s)) => Try(s.toInt).toOption match {
        case Some(i) => Seq(FhirPathNumber(i))
        case None => throw new FhirPathException(s"Invalid function call 'toInteger' on value $s of string type cannot be converted to integer!!")
      }
      case Seq(FhirPathBoolean(b)) => if(b) Seq(FhirPathNumber(1)) else Seq(FhirPathNumber(0))
      case Seq(oth) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toInteger' on multiple values!!!")
    }
  }

  /**
   * Decimal conversion function
   * @return
   */
  def toDecimal():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      //This may be due to parsing strings
      case Seq(FhirPathDateTime(y:Year)) => Seq(FhirPathNumber(y.getValue))
      case Seq(n:FhirPathNumber)  => Seq(n)
      case Seq(FhirPathString(s)) => Try(s.toDouble).toOption match {
        case Some(d) => Seq(FhirPathNumber(d))
        case None => throw new FhirPathException(s"Invalid function call 'toDecimal' on value $s of string type cannot be converted to decimal!!")
      }
      case Seq(FhirPathBoolean(b)) => if(b) Seq(FhirPathNumber(1.0)) else Seq(FhirPathNumber(0.0))
      case Seq(oth) => throw new FhirPathException(s"Invalid function call 'toDecimal' on value $oth !!!")
      case _ => throw new FhirPathException(s"Invalid function call 'toDecimal' on multiple values!!!")
    }
  }

  /**
   * Check if the current item can be converted to decimal
   * @return
   */
  def convertsToDecimal():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathDateTime(_:Year)) =>  Seq(FhirPathBoolean(true))
      case Seq(FhirPathNumber(_)) => Seq(FhirPathBoolean(true))
      case Seq(FhirPathBoolean(_)) => Seq(FhirPathBoolean(true))
      case Seq(FhirPathString(s)) =>
        Try(s.toDouble).toOption match {
          case Some(d) => Seq(FhirPathBoolean(true))
          case None => Seq(FhirPathBoolean(false))
        }
      case Seq(oth) => Seq(FhirPathBoolean(false))
      case _ => throw new FhirPathException(s"Invalid function call 'toDecimal' on multiple values!!!")
    }
  }

  /**
   * Boolean conversion function
   * @return
   */
  def toBoolean():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n:FhirPathNumber) if n.v == 1.0 => Seq(FhirPathBoolean(true))
      case Seq(n:FhirPathNumber) if n.v == 0 => Seq(FhirPathBoolean(false))
      case Seq(FhirPathString(s)) if
        s.equalsIgnoreCase("T") ||
          s.equalsIgnoreCase("true") ||
            s.equalsIgnoreCase("Y") ||
              s.equalsIgnoreCase("yes") ||
                s.equalsIgnoreCase("1") ||  s.equalsIgnoreCase("1.0")  => Seq(FhirPathBoolean(true))

      case Seq(FhirPathString(s)) if
        s.equalsIgnoreCase("F") ||
          s.equalsIgnoreCase("false") ||
          s.equalsIgnoreCase("N") ||
          s.equalsIgnoreCase("no") ||
          s.equalsIgnoreCase("0") ||  s.equalsIgnoreCase("0.0")  => Seq(FhirPathBoolean(false))

      case Seq(b:FhirPathBoolean) => Seq(b)
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toBoolean' on multiple values!!!")
    }
  }

  /**
   * Date conversion function
   * @return
   */
  def toDate():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathDateTime(dt:LocalDateTime)) => Seq(FhirPathDateTime(dt.toLocalDate))
      case Seq(FhirPathDateTime(dt:ZonedDateTime)) => Seq(FhirPathDateTime(dt.toLocalDate))
      case Seq(FhirPathDateTime(_)) => current
      case Seq(FhirPathString(s)) => Try(FhirPathLiteralEvaluator.parseFhirDateBest(s)).toOption match {
        case None => Nil
        case Some(t) => Seq(FhirPathDateTime(t))
      }
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toDate' on multiple values!!!")
    }
  }

  /**
   * DateTime conversion function
   * @return
   */
  def toDateTime():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(FhirPathDateTime(dt:Year)) => Seq(FhirPathDateTime(dt.atMonth(1).atDay(1).atTime(LocalTime.of(0, 0))))
      case Seq(FhirPathDateTime(dt:YearMonth)) => Seq(FhirPathDateTime(dt.atDay(1).atTime(LocalTime.of(0, 0))))
      case Seq(FhirPathDateTime(dt:LocalDate)) => Seq(FhirPathDateTime(dt.atTime(LocalTime.of(0, 0))))
      case Seq(FhirPathDateTime(_)) => current
      case Seq(FhirPathString(s)) => Try(FhirPathLiteralEvaluator.parseFhirDateTimeBest(s)).toOption match {
        case None => Nil
        case Some(t) => Seq(FhirPathDateTime(t))
      }
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toDateTime' on multiple values!!!")
    }
  }

  def toQuantity():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n:FhirPathNumber) => Seq(FhirPathQuantity(n, "1"))
      case Seq(q:FhirPathQuantity) => Seq(q)
      case Seq(FhirPathString(s)) => FhirPathLiteralEvaluator.parseFhirQuantity(s).toSeq
      case Seq(FhirPathBoolean(true)) => Seq(FhirPathQuantity(FhirPathNumber(1.0), "1"))
      case Seq(FhirPathBoolean(false)) => Seq(FhirPathQuantity(FhirPathNumber(0.0), "1"))
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toQuantity' on multiple values!!!")
    }
  }

  def toQuantity(unitExpr:ExpressionContext):Seq[FhirPathResult] = {
    val unitValue = new FhirPathExpressionEvaluator(context, current).visit(unitExpr)
    if(!unitValue.forall(_.isInstanceOf[FhirPathString]))
      throw new FhirPathException(s"Invalid function call 'toQuantity', given expression should return a string value!!!")
    val unit = unitValue.headOption.map(_.asInstanceOf[FhirPathString].s).getOrElse("1")
    current match {
      case Nil => Nil
      case Seq(n:FhirPathNumber) => Seq(FhirPathQuantity(n, unit))
      case Seq(q:FhirPathQuantity) if q.unit == unit => Seq(q)
      case Seq(FhirPathString(s)) => FhirPathLiteralEvaluator.parseFhirQuantity(s).toSeq
      case Seq(FhirPathBoolean(true)) => Seq(FhirPathQuantity(FhirPathNumber(1.0), unit))
      case Seq(FhirPathBoolean(false)) => Seq(FhirPathQuantity(FhirPathNumber(0.0), unit))
      case Seq(_) => Nil
      case _ => throw new FhirPathException(s"Invalid function call 'toQuantity' on multiple values!!!")
    }
  }

  /**
   * String conversion function
   * @return
   */
  def _toString():Seq[FhirPathResult] = {
    current match {
      case Nil => Nil
      case Seq(n:FhirPathNumber) if n.isInteger()  => Seq(FhirPathString(n.v.toLong.toString()))
      case Seq(n:FhirPathNumber) => Seq(FhirPathString(n.v.toString()))
      case Seq(s:FhirPathString) => Seq(s)
      case Seq(dt:FhirPathDateTime) => Seq(FhirPathString(FhirPathLiteralEvaluator.format(dt)))
      case Seq(t:FhirPathTime) =>  Seq(FhirPathString(FhirPathLiteralEvaluator.format(t)))
      case Seq(q:FhirPathQuantity) => Seq(FhirPathString(q.q.v.toString + " " + q.unit))
      case Seq(FhirPathBoolean(b)) => if(b) Seq(FhirPathString("'true'")) else Seq(FhirPathString("'false'"))
      case Seq(oth) => Nil
      case _ => throw new FhirPathException(s"Invalid function call '_toString' on multiple values !!!")
    }
  }

  def toComplex():Seq[FhirPathResult] = {
    current match {
      case Seq(q:FhirPathQuantity) => Seq(FhirPathComplex(q.toJson.asInstanceOf[JObject]))
      case _ => Nil
    }
  }


  /**
    * String manipulation functions ttp://hl7.org/fhirpath/#string-manipulation
    * @return
    */
  private def checkSingleString() =
    if(current.length > 1 || current.headOption.exists(!_.isInstanceOf[FhirPathString]))
      throw new FhirPathException("Invalid function call on multi item collection or non-string value!")

  def indexOf(substringExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(substringExpr) match {
        case Seq(FhirPathString(ss)) => if(ss == "") Seq(FhirPathNumber((0))) else Seq(FhirPathNumber(c.asInstanceOf[FhirPathString].s.indexOf(ss)))
        case _ => throw new FhirPathException(s"Invalid function call 'indexOf', the substring expression ${substringExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def substring(startExpr : ExpressionContext, lengthExpr:Option[ExpressionContext]):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val start = new FhirPathExpressionEvaluator(context, current).visit(startExpr) match {
        case Seq(n:FhirPathNumber) if n.isInteger() => n.v.toInt
        case _ => throw new FhirPathException(s"Invalid function call 'substring', the start expression ${startExpr.getText} does not return integer!")
      }
      val length = lengthExpr.map(lexpr => new FhirPathExpressionEvaluator(context, current).visit(lexpr) match {
        case Seq(n:FhirPathNumber) if n.isInteger() => n.v.toInt
        case _ => throw new FhirPathException(s"Invalid function call 'substring', the length expression ${startExpr.getText} does not return integer!")
      })
      val str = c.asInstanceOf[FhirPathString].s
      if(start > str.length || start < 0)
        Nil
      else {
        if (length.isEmpty)
          Try(Seq(FhirPathString(str.substring(start)))).toOption.getOrElse(Nil)
        else {
          val endIndex = if(start + length.get >= str.length) str.length else start + length.get
            Try(Seq(FhirPathString(str.substring(start, endIndex)))).toOption.getOrElse(Nil)
        }
      }
    }).getOrElse(Nil)
  }
  def substring(startExpr:ExpressionContext):Seq[FhirPathResult] = substring(startExpr, None)
  def substring(startExpr:ExpressionContext, lengthExpr:ExpressionContext):Seq[FhirPathResult] = substring(startExpr, Some(lengthExpr))

  def startsWith(prefixExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(prefixExpr) match {
        case Seq(FhirPathString(ss)) =>
          Seq(FhirPathBoolean( ss == "" || c.asInstanceOf[FhirPathString].s.startsWith(ss) ))
        case oth =>
          throw new FhirPathException(s"Invalid function call 'startsWith', the prefixExpr expression ${prefixExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def endsWith(suffixExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(suffixExpr) match {
        case Seq(FhirPathString(ss)) => Seq(FhirPathBoolean( ss == "" || c.asInstanceOf[FhirPathString].s.endsWith(ss) ))
        case _ => throw new FhirPathException(s"Invalid function call 'endsWith', the suffixExpr expression ${suffixExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def _contains(substringExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(substringExpr) match {
        case Seq(FhirPathString(ss)) => Seq(FhirPathBoolean( ss == "" || c.asInstanceOf[FhirPathString].s.contains(ss) ))
        case _ => throw new FhirPathException(s"Invalid function call 'contains', the substring expression ${substringExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def replace(patternExpr : ExpressionContext, substitutionExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val pattern = new FhirPathExpressionEvaluator(context, current).visit(patternExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the pattern expression ${patternExpr.getText} does not return string!")
      }

      val substitution = new FhirPathExpressionEvaluator(context, current).visit(substitutionExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the substitiution expression ${substitutionExpr.getText} does not return string!")
      }

      Seq(FhirPathString(c.asInstanceOf[FhirPathString].s.replace(pattern, substitution)))
    }).getOrElse(Nil)
  }

  def matches(regexExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(regexExpr) match {
        case Seq(FhirPathString(ss)) =>
          val unexcapedScript = StringEscapeUtils.unescapeEcmaScript(ss)
          val isMatch = c.asInstanceOf[FhirPathString].s.matches(unexcapedScript)
          Seq(FhirPathBoolean(isMatch))
        case _ =>
          throw new FhirPathException(s"Invalid function call 'matches', the regular expression ${regexExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def replaceMatches(regexExpr : ExpressionContext, substitutionExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val regex = new FhirPathExpressionEvaluator(context, current).visit(regexExpr) match {
        case Seq(FhirPathString(ss)) => StringEscapeUtils.unescapeEcmaScript(ss)
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the regex expression ${regexExpr.getText} does not return string!")
      }

      val substitution = new FhirPathExpressionEvaluator(context, current).visit(substitutionExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new FhirPathException(s"Invalid function call 'replace', the substitiution expression ${substitutionExpr.getText} does not return string!")
      }

      Seq(FhirPathString(c.asInstanceOf[FhirPathString].s.replaceAll(regex, substitution)))
    }).getOrElse(Nil)
  }

  def length():Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => Seq(FhirPathNumber(c.asInstanceOf[FhirPathString].s.length))).getOrElse(Nil)
  }

  /**
    * Tree navigation function http://hl7.org/fhirpath/#tree-navigation
    */
  def children():Seq[FhirPathResult] = {
    current
      .filter(_.isInstanceOf[FhirPathComplex])
      .map(_.asInstanceOf[FhirPathComplex])
      .flatMap(pc => pc.json.obj.map(_._2).flatMap(FhirPathValueTransformer.transform))
  }

  def descendants():Seq[FhirPathResult] = {
    val results = children()
    if(results.nonEmpty)
      results ++ new FhirPathFunctionEvaluator(context, results).descendants()
    else
      results
  }

  def hasValue():Seq[FhirPathResult] = {
    Seq(FhirPathBoolean(current.nonEmpty))
  }

  /**
   * FHIR Path aggregate function
   * @param aggExpr         Aggregation expression
   * @param initValueExpr   Given initial value for aggregation
   * @return
   */
  def aggregate(aggExpr:ExpressionContext, initValueExpr:ExpressionContext):Seq[FhirPathResult] = {
    val initValue = new FhirPathExpressionEvaluator(context, current).visit(initValueExpr)
    if(initValue.length > 1)
      throw new FhirPathException(s"Invalid function call 'aggregate', the initValue expression should return a single value!")

    handleAggregate(context.copy(_total = initValue.headOption), aggExpr)
  }

  /**
   * Helper function to evaluate aggregate
   * @param initialCntx Initial context for total
   * @param aggExpr     Aggregation expression
   * @return
   */
  private def handleAggregate(initialCntx:FhirPathEnvironment, aggExpr:ExpressionContext):Seq[FhirPathResult] = {
    val finalContext =
      current
        .zipWithIndex
        .foldLeft(initialCntx) {
          case (cntx, cur) =>
            val totalResult = new FhirPathExpressionEvaluator(cntx.copy(_index = cur._2), Seq(cur._1)).visit(aggExpr)
            cntx.copy(_total = totalResult.headOption)
        }

    finalContext._total.toSeq
  }

  /**
   * FHIR Path aggregate function
   * @param aggExpr Aggregation expression
   * @return
   */
  def aggregate(aggExpr:ExpressionContext):Seq[FhirPathResult] = {
      handleAggregate(context, aggExpr)
  }

  /**
    * Utility functions http://hl7.org/fhirpath/#utility-functions
    */
  def today():Seq[FhirPathResult] = Seq(FhirPathDateTime(LocalDate.now()))
  def now():Seq[FhirPathResult] = Seq(FhirPathDateTime(ZonedDateTime.now()))

  def trace(nameExpr : ExpressionContext):Seq[FhirPathResult] = {
    //TODO log the current value with the given name
    current
  }

  def trace(nameExpr : ExpressionContext, othExpr:ExpressionContext):Seq[FhirPathResult] = {
    current
  }
}

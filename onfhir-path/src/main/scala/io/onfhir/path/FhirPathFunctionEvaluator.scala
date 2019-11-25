package io.onfhir.path

import java.time.{LocalDate, ZonedDateTime}

import io.onfhir.api.Resource
import io.onfhir.path.grammar.FhirPathExprParser.ExpressionContext

import scala.util.Try

class FhirPathFunctionEvaluator(context:FhirPathEnvironment, current:Seq[FhirPathResult]) {

  /**
    *
    * @param fname
    * @param params
    * @return
    */
  def callFunction(fname:String, params:Seq[ExpressionContext]):Seq[FhirPathResult] = {
    try {
      val functionName = if(fname == "toString") "_toString" else fname

      val method = classOf[FhirPathFunctionEvaluator].getMethod(functionName, params.map(_ => classOf[ExpressionContext]): _*)
      val result = method.invoke(this, params:_*)
      result.asInstanceOf[Seq[FhirPathResult]]
    } catch {
      case n:NoSuchMethodException =>
        throw new Exception(s"Invalid function call, function $fname does not exist or take ${params.length} arguments !!!")
    }
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
      case _ =>  throw new Exception("Invalid function call 'is', it should be called on single item!")
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
    case _ => throw  new Exception("Function 'not' should run on FHIR path boolean!!!")
  }

  def exists(expr:Option[ExpressionContext]):Seq[FhirPathResult] = {
    val result = expr match {
      case None =>  current.nonEmpty
      case Some(criteria) => current.exists(c => new FhirPathExpressionEvaluator(context, Seq(c)).visit(criteria) match {
        case Seq(FhirPathBoolean(true)) => true
        case _ => false
      })
    }
    Seq(FhirPathBoolean(result))
  }
  def exists(expr:ExpressionContext):Seq[FhirPathResult] = exists(Some(expr))
  def exists():Seq[FhirPathResult] = exists(None)

  def all(criteria:ExpressionContext):Seq[FhirPathResult] = {
    val result = current.forall(c => new FhirPathExpressionEvaluator(context, Seq(c)).visit(criteria) match {
      case Seq(FhirPathBoolean(true)) => true
      case _ => false
    })
    Seq(FhirPathBoolean(result))
  }

  def allTrue():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new Exception("Function 'allTrue' should run on collection of FHIR Path boolean values!!!")
    val result = current.forall(c => c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  def anyTrue():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new Exception("Function 'anyTrue' should run on collection of FHIR Path boolean values!!!")
    val result = current.exists(c => c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  def allFalse():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new Exception("Function 'allFalse' should run on collection of FHIR Path boolean values!!!")
    val result = current.forall(c => !c.asInstanceOf[FhirPathBoolean].b)
    Seq(FhirPathBoolean(result))
  }

  def anyFalse():Seq[FhirPathResult] = {
    if(current.exists(!_.isInstanceOf[FhirPathBoolean]))
      throw new Exception("Function 'anyFalse' should run on collection of FHIR Path boolean values!!!")
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
    current.filter(c => new FhirPathExpressionEvaluator(context, Seq(c)).visit(criteria) match {
      case Seq(FhirPathBoolean(true)) => true
      case Seq(FhirPathBoolean(false)) => false
      case Nil => false
      case _ => throw new Exception(s"Invalid criteria ${criteria.getText} function call 'where', it does not evaluate to boolean!!!")
    })
  }

  def select(projection: ExpressionContext):Seq[FhirPathResult] = {
    current.flatMap(c => new FhirPathExpressionEvaluator(context, Seq(c)).visit(projection))
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
      case _ => throw new Exception(s"Function 'single' is called on a multi item collection $current!!!")
    }
  }

  def first():Seq[FhirPathResult] = current.headOption.toSeq
  def last():Seq[FhirPathResult] = current.lastOption.toSeq
  def tail():Seq[FhirPathResult] = if(current.isEmpty) Nil else current.tail
  def skip(numExpr:ExpressionContext):Seq[FhirPathResult] = {
    val numValue = new FhirPathExpressionEvaluator(context, current).visit(numExpr)
    if(numValue.length != 1 || !numValue.head.isInstanceOf[FhirPathNumber])
      throw new Exception(s"Invalid function call 'skip', num expression ${numExpr.getText} does not return a single number!")
    val inum = numValue.head.asInstanceOf[FhirPathNumber]
    if(!inum.isInteger())
      throw new Exception(s"Invalid function call 'skip', num expression ${numExpr.getText} does not return a integer")

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
      throw new Exception(s"Invalid function call 'take', num expression ${numExpr.getText} does not return a single number!")
    val inum = numValue.head.asInstanceOf[FhirPathNumber]
    if(!inum.isInteger())
      throw new Exception(s"Invalid function call 'take', num expression ${numExpr.getText} does not return a integer")
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

  def toInteger():Seq[FhirPathResult] = {
    current match {
      case Seq(n:FhirPathNumber) if n.isInteger() => Seq(n)
      case Seq(n:FhirPathNumber) if !n.isInteger() =>  throw new Exception(s"Invalid function call 'toInteger' on value $n !!!")
      case Seq(FhirPathString(s)) => Try(s.toInt).toOption match {
        case Some(i) => Seq(FhirPathNumber(i))
        case None => throw new Exception(s"Invalid function call 'toInteger' on value $s of string type cannot be converted to integer!!")
      }
      case Seq(FhirPathBoolean(b)) => if(b) Seq(FhirPathNumber(1)) else Seq(FhirPathNumber(0))
      case Seq(oth) => throw new Exception(s"Invalid function call 'toInteger' on value $oth !!!")
      case _ => Nil
    }
  }

  def toDecimal():Seq[FhirPathResult] = {
    current match {
      case Seq(n:FhirPathNumber)  => Seq(n)
      case Seq(FhirPathString(s)) => Try(s.toDouble).toOption match {
        case Some(d) => Seq(FhirPathNumber(d))
        case None => throw new Exception(s"Invalid function call 'toDecimal' on value $s of string type cannot be converted to decimal!!")
      }
      case Seq(FhirPathBoolean(b)) => if(b) Seq(FhirPathNumber(1.0)) else Seq(FhirPathNumber(0.0))
      case Seq(oth) => throw new Exception(s"Invalid function call 'toDecimal' on value $oth !!!")
      case _ => Nil
    }
  }

  def _toString():Seq[FhirPathResult] = {
    current match {
      case Seq(FhirPathNumber(n))  => Seq(FhirPathString(n.toString))
      case Seq(s:FhirPathString) => Seq(s)
      case Seq(dt:FhirPathDateTime) => Seq(FhirPathString(FhirPathLiteralEvaluator.format(dt)))
      case Seq(t:FhirPathTime) =>  Seq(FhirPathString(FhirPathLiteralEvaluator.format(t)))
      case Seq(q:FhirPathQuantity) => Seq(FhirPathString(q.q.v.toString + " " + q.unit))
      case Seq(FhirPathBoolean(b)) => if(b) Seq(FhirPathString("'true'")) else Seq(FhirPathString("'false'"))
      case Seq(oth) => throw new Exception(s"Invalid function call 'toDecimal' on value $oth !!!")
      case _ => Nil
    }
  }



  /**
    * String manipulation functions ttp://hl7.org/fhirpath/#string-manipulation
    * @return
    */
  private def checkSingleString() =
    if(current.length > 1 || current.headOption.exists(!_.isInstanceOf[FhirPathString]))
      throw new Exception("Invalid function call 'indexOf' on multi item collection or non-string value!")

  def indexOf(substringExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(substringExpr) match {
        case Seq(FhirPathString(ss)) => if(ss == "") Seq(FhirPathNumber((0))) else Seq(FhirPathNumber(c.asInstanceOf[FhirPathString].s.indexOf(ss)))
        case _ => throw new Exception(s"Invalid function call 'indexOf', the substring expression ${substringExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def substring(startExpr : ExpressionContext, lengthExpr:Option[ExpressionContext]):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val start = new FhirPathExpressionEvaluator(context, current).visit(startExpr) match {
        case Seq(n:FhirPathNumber) if n.isInteger() => n.v.toInt
        case _ => throw new Exception(s"Invalid function call 'substring', the start expression ${startExpr.getText} does not return integer!")
      }
      val length = lengthExpr.map(lexpr => new FhirPathExpressionEvaluator(context, current).visit(lexpr) match {
        case Seq(n:FhirPathNumber) if n.isInteger() => n.v.toInt
        case _ => throw new Exception(s"Invalid function call 'substring', the length expression ${startExpr.getText} does not return integer!")
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
        case _ =>
          throw new Exception(s"Invalid function call 'startsWith', the prefixExpr expression ${prefixExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def endsWith(suffixExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(suffixExpr) match {
        case Seq(FhirPathString(ss)) => Seq(FhirPathBoolean( ss == "" || c.asInstanceOf[FhirPathString].s.endsWith(ss) ))
        case _ => throw new Exception(s"Invalid function call 'endsWith', the suffixExpr expression ${suffixExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def _contains(substringExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(substringExpr) match {
        case Seq(FhirPathString(ss)) => Seq(FhirPathBoolean( ss == "" || c.asInstanceOf[FhirPathString].s.contains(ss) ))
        case _ => throw new Exception(s"Invalid function call 'contains', the substring expression ${substringExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def replace(patternExpr : ExpressionContext, substitutionExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val pattern = new FhirPathExpressionEvaluator(context, current).visit(patternExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new Exception(s"Invalid function call 'replace', the pattern expression ${patternExpr.getText} does not return string!")
      }

      val substitution = new FhirPathExpressionEvaluator(context, current).visit(substitutionExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new Exception(s"Invalid function call 'replace', the substitiution expression ${substitutionExpr.getText} does not return string!")
      }

      Seq(FhirPathString(c.asInstanceOf[FhirPathString].s.replace(pattern, substitution)))
    }).getOrElse(Nil)
  }

  def matches(regexExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      new FhirPathExpressionEvaluator(context, current).visit(regexExpr) match {
        case Seq(FhirPathString(ss)) => Seq(FhirPathBoolean(c.asInstanceOf[FhirPathString].s.matches(ss)))
        case _ => throw new Exception(s"Invalid function call 'matches', the regular expression ${regexExpr.getText} does not return string!")
      }
    }).getOrElse(Nil)
  }

  def replaceMatches(regexExpr : ExpressionContext, substitutionExpr : ExpressionContext):Seq[FhirPathResult] = {
    checkSingleString()
    current.headOption.map(c => {
      val regex = new FhirPathExpressionEvaluator(context, current).visit(regexExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new Exception(s"Invalid function call 'replace', the regex expression ${regexExpr.getText} does not return string!")
      }

      val substitution = new FhirPathExpressionEvaluator(context, current).visit(substitutionExpr) match {
        case Seq(FhirPathString(ss)) => ss
        case _ => throw new Exception(s"Invalid function call 'replace', the substitiution expression ${substitutionExpr.getText} does not return string!")
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

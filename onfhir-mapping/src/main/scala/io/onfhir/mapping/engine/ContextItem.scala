package io.onfhir.mapping.engine

import io.onfhir.mapping.model.ListModes
import org.json4s.JsonAST.{JArray, JNothing, JObject, JValue}

import scala.collection.mutable
trait MappingContext {
  def  toJson():JValue
}

case class SourceContext(name:String, value:Seq[JValue]) extends MappingContext {
  def toJson():JValue =
    if(value.length > 1)
      JArray(value.toList)
    else
      value.headOption.getOrElse(JNothing)
}

case class TargetContextValue(listMode:Option[String], value:Seq[JValue])

/**
 * Represents a node in the target element tree
 * @param name      Name of the variable if it is assigned to a variable
 * @param isArray   If the element is an array
 * @param value     Value of the context (either it is a direct value or a JSON object
 */
case class TargetContext(name:Option[String], isArray:Boolean, var value:Either[Seq[JValue], Seq[mutable.Map[String, TargetContext]]]) extends MappingContext {
  def toJson():JValue = {
    val results = value match {
      case Left(jv) => jv
      case Right(tcs) => tcs.map(tc => JObject(tc.map(i => i._1 -> i._2.toJson()).toList))
    }
    if(isArray)
      JArray(results.toList)
    else
      results.headOption.getOrElse()
  }

  def setElementValue(variable:Option[String], element:String, listMode:Option[String], valuesToSet:Seq[JValue]) = {
    //Overriding variable
    if(value.isLeft)
      value = Right(Seq(new mutable.HashMap()))

    value.right.get.lastOption.flatMap(_.get(element)) match {
      //Element already exists
      case Some(foundElement) =>
        listMode match {
          case Some(ListModes.FIRST) =>
            foundElement.value = Left(valuesToSet ++ foundElement.value.left.getOrElse(Nil))
          case Some(ListModes.LAST) =>
        }
      //Element is not found
      case None =>
        value.right.get.last.+(element, TargetContext(variable, listMode.isDefined, Left(valuesToSet)))
    }

  }

  /**
   * Find a variable
   * @param v
   * @return
   */
  def findVariable(v:String):Option[TargetContext] = {
    if(name.contains(v))
      Some(this)
    else {
      value.right
        .toOption
        .flatMap(_.lastOption)
        .flatMap(m => recursiveFindInList(m.values, v))
    }
  }

  private def recursiveFindInList(contexts:Iterable[TargetContext], v:String):Option[TargetContext] = {
    if(contexts.isEmpty)
      None
    else
      contexts.headOption
        .flatMap(_.findVariable(v))
        .orElse(recursiveFindInList(contexts.tail, v))
  }

}


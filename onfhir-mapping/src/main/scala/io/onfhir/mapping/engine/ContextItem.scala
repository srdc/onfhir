package io.onfhir.mapping.engine

import io.onfhir.mapping.model.{ListModes, StructureMappingException}
import org.json4s.JsonAST.{JArray, JNothing, JNull, JObject, JValue}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ContextRepository(sourceContext: mutable.Map[String, Seq[JValue]], targetContext:mutable.Map[String, TargetContext]) {
  /**
   * Get the source context JSON
   * @param v Context name
   * @return
   */
  def getSourceContext(v:String):Option[JValue] = {
    sourceContext
      .get(v)
      .flatMap {
        case Nil => None
        case Seq(s) => Some(s)
        case oth => Some(JArray(oth.toList))
      }
      .orElse(getTargetContextInJson(v))
  }

  def getTargetContextInJson(v:String):Option[JValue] = {
     targetContext
       .get(v)
       .map(tc => tc.toJson())
  }

  def setSourceContext(v:String, value: Seq[JValue]) =
    sourceContext.put(v, value)

  def addRootContext(variable:String, value:Seq[JValue]):Unit = {
    targetContext.put(variable, TargetContext(value = value))
  }

  /**
   * Add an element to context
   * @param ruleId
   * @param context
   * @param element
   * @param variable
   * @param values
   */
  def addElement(ruleId:String, context:String, element:String, variable:Option[String], listMode:Option[String], values:Seq[JValue]) = {
      targetContext.get(context) match {
        case None => throw new StructureMappingException(s"No such context $context defined previously!")
        case Some(ctx) =>

          //Initialize the list
          if(!ctx.elements.isDefinedAt(element))
            ctx.elements.put(element, new ListBuffer[TargetContextValue]())

          //If we are working on the same rule again
          if(ctx.elements(element).lastOption.map(_.ruleId).contains(ruleId)) {
            ctx.elements(element).last.addValue(values) //just add the values
          } else {
            val (newContext, contextValue) = variable match {
              case None => None -> LeafTargetContextValue(ruleId, listMode, values)
              case Some(v) =>
                //Generate a new context
                val newTargetContext = TargetContext(value = values)
                //Refer that context
                Some(newTargetContext) -> RefTargetContextValue(ruleId, listMode, newTargetContext)
            }
            //Add the context value
            ctx.elements(element).:+(contextValue)

            //If variable is defined, update the context map
            if(newContext.isDefined)
              targetContext.put(variable.get, newContext.get)
          }
      }
  }
}

trait TargetContextValue {
  val ruleId:String
  val listMode:Option[String]
  def getItems:Seq[JValue]
  def addValue(values:Seq[JValue]):Unit
}
case class LeafTargetContextValue(ruleId:String, listMode:Option[String], var value:Seq[JValue]) extends TargetContextValue {
  override def getItems:Seq[JValue] = value
  override def addValue(values:Seq[JValue]) = value = value ++ values
}
case class RefTargetContextValue(ruleId:String, listMode:Option[String], value:TargetContext) extends TargetContextValue {
  override def getItems: Seq[JValue] = value.toJson() match {
    case JArray(arr) => arr
    case JNothing  => Nil
    case oth => Seq(oth)
  }
  override def addValue(values:Seq[JValue]):Unit = {
    value.value = value.value ++ values
  }
}

case class TargetContext(var elements:mutable.Map[String, mutable.Seq[TargetContextValue]] = new mutable.HashMap[String, mutable.Seq[TargetContextValue]](), var value:Seq[JValue]) {
  private def getValue(values:Seq[JValue]):JValue = {
    values.length match {
      case 0 => JNothing
      case 1 => values.head
      case _ => JArray(values.toList)
    }
  }

  def toJson():JValue = {
    getValue(value) merge
          JObject(
            elements
              .map(e => e._1 ->
                (
                  if(e._2.length > 1 || e._2.exists(_.listMode.isDefined))
                    JArray(orderLists(e._2).flatMap(_.getItems).toList)
                  else
                    e._2.flatMap(_.getItems).headOption.getOrElse(JNull))).toList
          )
  }

  def orderLists(tcvs:Seq[TargetContextValue]):Seq[TargetContextValue] = {
    //TODO Not sure the semantics of collate and these also
    tcvs.filter(_.listMode.contains(ListModes.FIRST)) ++
      tcvs.filter(_.listMode.contains(ListModes.SHARE)) ++
        tcvs.filter(_.listMode.contains(ListModes.LAST))
  }
}


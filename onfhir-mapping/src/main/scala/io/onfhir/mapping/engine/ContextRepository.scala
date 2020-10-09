package io.onfhir.mapping.engine

import io.onfhir.mapping.model.{ListModes, StructureMappingException}
import org.json4s.JsonAST.{JArray, JNothing, JNull, JObject, JValue}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait IContextRepository {
  def getSourceContext(v:String):Option[JValue]
  def getTargetContext(v:String):Option[TargetContext]

}

case class RootContextRepository(sourceContext: Map[String, JValue], targetContext:Map[String, TargetContext]) extends IContextRepository {
  def getSourceContext(v:String):Option[JValue] = {
    sourceContext
      .get(v)
      .orElse(getTargetContext(v).map(_.toJson()))
  }

  def getTargetContext(v:String):Option[TargetContext] = {
    targetContext.get(v)
  }
}
/**
 * Context repository for a group or rule
 * @param parentRepository   Parent repository for contexts
 * @param mappedParams       Mapped params in param repository (param name in this -> param name in parent)
 */
case class ContextRepository(parentRepository:IContextRepository, mappedParams:Option[Map[String, String]] = None) extends IContextRepository {
  //Newly created source context
  val newSourceContext:mutable.Map[String, Seq[JValue]] = new mutable.HashMap[String, Seq[JValue]]()
  //Newly created target context
  val newTargetContext:mutable.Map[String, TargetContext] = new mutable.HashMap[String, TargetContext]()

  private def getSourceContextFromParent(v:String):Option[JValue] = {
    mappedParams match {
      case None => parentRepository.getSourceContext(v)
      case Some(mp) => mp.get(v).flatMap(pv => parentRepository.getSourceContext(pv))
    }
  }

  private def getTargetContextFromParent(v:String):Option[TargetContext] = {
    mappedParams match {
      case None =>   parentRepository.getTargetContext(v)
      case Some(mp) =>  mp.get(v).flatMap(pv => parentRepository.getTargetContext(pv))
    }
  }

  /**
   * Get the source context JSON
   * @param v Context name
   * @return
   */
  def getSourceContext(v:String):Option[JValue] = {
    getSourceContextFromParent(v)
      .orElse(
        newSourceContext
          .get(v)
          .flatMap {
            case Nil => None
            case Seq(s) => Some(s)
            case oth => Some(JArray(oth.toList))
          }
          .orElse(getTargetContextInJson(v))
      )
  }

  override def getTargetContext(v: String): Option[TargetContext] = {
    getTargetContextFromParent(v)
      .orElse(newTargetContext.get(v))
  }

  def getTargetContextInJson(v:String):Option[JValue] = {
    getTargetContext(v).map(tc => tc.toJson())
  }

  def setSourceContext(v:String, value: Seq[JValue]) =
    newSourceContext.put(v, value)

  def addRootContext(variable:String, value:Seq[JValue]):Unit = {
    newTargetContext.put(variable, TargetContext(value = value))
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
    getTargetContext(context) match {
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
            ctx.elements(element).append(contextValue)

            //If a new variable is defined, update the context map
            if(newContext.isDefined)
              newTargetContext.put(variable.get, newContext.get)
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

case class TargetContext(var elements:mutable.Map[String, mutable.ListBuffer[TargetContextValue]] = new mutable.HashMap[String, mutable.ListBuffer[TargetContextValue]](), var value:Seq[JValue]) {
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


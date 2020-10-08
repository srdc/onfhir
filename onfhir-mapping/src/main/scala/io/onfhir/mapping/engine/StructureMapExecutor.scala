package io.onfhir.mapping.engine

import io.onfhir.api.Resource
import io.onfhir.mapping.model.{ListModes, StructureMap, StructureMapGroup, StructureMapRule, StructureMapSource, StructureMapTarget, StructureMappingException}
import io.onfhir.path.FhirPathEvaluator
import org.json4s.JsonAST.{JNothing, JObject, JValue}
import org.json4s.{JsonAST, _}
import org.slf4j.LoggerFactory

trait IRuleOrGroupFinder {
  def findRuleOrGroup(name:String):Option[Either[StructureMapGroup, StructureMapRule]]
}
/**
 *
 * @param structureMap
 * @param inputs
 * @param mappingRepository
 */
class StructureMapExecutor(structureMap:StructureMap, inputs:Map[String, Resource], mappingRepository: IMappingRepository) {
  //Imported maps
  val importedStructureMaps = structureMap.imports.map(url => url -> mappingRepository.getStructureMap(url))



  /**
   * Execute the mapping on given inputs and generate the outputs
   * @return
   */
  def executeMapping():Map[String, Resource] = {


  }

}

class StructureMapGroupExecutor(structureGroup:StructureMapGroup, context:Map[String, Seq[JValue]]) extends IRuleOrGroupFinder {

  def executeMapping() = {

  }

  override def findRuleOrGroup(name: String): Option[Either[StructureMapGroup, StructureMapRule]] = {

  }
}

case class StructureMapRuleExecutor(structureMapRule:StructureMapRule, ctxRepository:IContextRepository) {
  val logger = LoggerFactory.getLogger(classOf[StructureMapRuleExecutor])

  /**
   * Apply a mapping rule

   * @param context           Context (Variables and inputs)
   * @return
   */
 def executeMapping() = {
   //Evaluate the source definitions
   val resultingValues =
     structureMapRule
       .sources
       .map(s => s -> evaluateSource(s))
       .filter(s => s._2.isDefined)
       .map(s => s._1 -> s._2.get)

   //If there is no source, no change on context
   if(resultingValues.nonEmpty) {
     // Permutate results in case multiple sources
     val permutationOfResults:Traversable[Traversable[JValue]] = crossJoin(resultingValues.map(r => r._2))

     //Evaluate the targets
     permutationOfResults.foreach(sources =>
       structureMapRule.targets.foreach(target => evaluateTarget(target, sources.toSeq))
     )

     //Construct rule executors for child rules and execute them
     val childExecutors =
       structureMapRule.childRules
        .map(cr => StructureMapRuleExecutor(cr, ContextRepository(ctxRepository)))
     childExecutors.foreach(_.executeMapping())

     structureMapRule.dependentRules.map(dr => )


   }
 }

  /**
   * Evaluate a target transformation
    * @param target
   * @param sources
   * @return
   */
 private def evaluateTarget(target:StructureMapTarget, sources:Seq[JValue]) = {
   val targetValue = target.transform.map(tf => TransformationHandler.transform(sources, tf,  target.parameters)).getOrElse(Seq(JObject()))
   (target.context, target.contextType) match {
     //If there is a context defined in target find it
     case (Some(context), Some("variable")) =>
       ctxRepository.addElement(structureMapRule.name, context, target.element.get, target.variable, target.listMode, targetValue)
     case (None, _) =>
       ctxRepository.addRootContext(target.variable.get, targetValue)
   }
 }

  /**
   * Evaluate a rule.source element to find the values
   * @param source        Source definition
   * @return
   */
  private def evaluateSource(source:StructureMapSource):Option[Seq[JValue]] = {
    var sources = ctxRepository
      .getSourceContext(source.context)
      .flatMap(s =>
        //If there is an element, get the field value otherwise source is itself
        source.element match {
          case None => Some(Seq(s))
          case Some(e) =>
            //TODO Handle polymorphic types even if type is not given
            val elemName = e + source._type.map(_.capitalize).getOrElse("")
            (s \ elemName) match {
              case JNothing => None
              case JArray(arr) => Some(arr.toSeq)
              case oth => Some(Seq(oth))
            }
        }
      )
      .orElse(source.defaultValue.map(Seq(_))) //Get the defaultValue if source does not exist and default exist


    //Check min cardinality
    if(source.min.nonEmpty)
      sources.foreach(sc =>
        if(source.min.get > sc.length)
          throw new StructureMappingException(s"Min cardinality expected ${source.min.get} is not satisfied by given source context while evaluating rule ${structureMapRule.name} and source ${source.context}!")
      )
    //Check max cardinality
    if(source.max.nonEmpty)
      sources.foreach(sc =>
        if(source.max.get != -1 && source.max.get < sc.length)
          throw new StructureMappingException(s"Max cardinality expected ${source.max.get} is not satisfied by given source context while evaluating rule ${structureMapRule.name} and source ${source.context}!")
      )

    //Apply the listMode
    if(source.listMode.nonEmpty)
      sources = sources.map(sc =>
        source.listMode.get match {
          case ListModes.FIRST => sc.headOption.toSeq
          case ListModes.NOT_FIRST => sc.tail
          case ListModes.LAST => sc.lastOption.toSeq
          case ListModes.NOT_LAST => sc.dropRight(1)
          case ListModes.ONLY_ONE =>
            if(sc.length != 1)
              throw new StructureMappingException(s"List mode 'only_one' is not satisfied by given source context while evaluating rule ${structureMapRule.name} and source ${source.context}")
            else
              sc
        }
      )

    //Apply the condition
    if(source.condition.nonEmpty)
      sources = sources.map(sc => sc.filter(FhirPathEvaluator().satisfies(source.condition.get, _)))

    //Apply the check if source context is not empty
    if(source.check.nonEmpty)
      sources.foreach(sc => sc.foreach(sci =>
        if(!FhirPathEvaluator().satisfies(source.check.get, sci))
          throw new StructureMappingException(s"Check condition is not satisfied by given source context while evaluating rule ${structureMapRule.name} and source ${source.context}"))
      )
    //Log the messages
    if(source.logMessage.nonEmpty) {
      sources.foreach(sc => sc.foreach(sci => {
        val logString = FhirPathEvaluator().evaluateString(source.logMessage.get, sci)
        logString.foreach(logger.info)
      }))
    }

    if(source.variable.nonEmpty)
      ctxRepository.setSourceContext(source.variable.get, sources.getOrElse(Nil))
    sources
  }

  /**
   * Permutate a list of lists items
   * @param list
   * @tparam T
   * @return
   */
    private def crossJoin[T](list: Traversable[Traversable[T]]): Traversable[Traversable[T]] =
      list match {
        case xs :: Nil => xs map (Traversable(_))
        case x :: xs => for {
          i <- x
          j <- crossJoin(xs)
        } yield Traversable(i) ++ j
      }

  }

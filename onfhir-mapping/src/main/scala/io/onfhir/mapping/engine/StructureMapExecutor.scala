package io.onfhir.mapping.engine

import io.onfhir.api.Resource
import io.onfhir.mapping.model.{ListModes, StructureMap, StructureMapGroup, StructureMapRule, StructureMapSource, StructureMapTarget, StructureMappingException}
import io.onfhir.path.FhirPathEvaluator
import org.json4s.JsonAST.{JNothing, JObject, JValue}
import org.json4s.{JsonAST, _}
import org.slf4j.LoggerFactory

trait IMappingExecutor {
  /**
   * Find rule or group in this context of execution
   * @param name
   * @return
   */
  def findRuleOrGroup(name:String):Option[Either[StructureMapGroup, StructureMapRule]]

  def executeMapping():Unit
}
/**
 *
 * @param structureMap
 * @param inputs
 * @param mappingUtility
 */
class StructureMapExecutor(structureMap:StructureMap, inputs:Map[String, Resource], mappingUtility: IMappingUtility) extends IMappingExecutor {
  val targetDefinitions =
    structureMap
      .structureDefs
      .filter(_._1 == "target") //filter the target mode

  //Imported maps
  val importedStructureGroups:Map[String, StructureMapGroup] =
    structureMap
      .imports
      .flatMap(url => mappingUtility.getStructureMap(url))
      .flatMap(sm => sm.groups)
      .map(s => s.name -> s)
      .toMap

  var rootContextRepository:RootContextRepository = _

  /**
   * Get the mapping results after execution
   * @return
   */
  def getMappingResults():Map[String, JObject] = {
    if(rootContextRepository != null){
      rootContextRepository.targetContext.mapValues(_.toJson().asInstanceOf[JObject])
    } else
      throw new StructureMappingException(s"Structure mapping with ${structureMap.url} not executed yet!")
  }
  def executeMapping():Unit = {
    executeMapping(structureMap.groups.head)
  }
  /**
   * Execute the mapping on given inputs and generate the outputs
   * @return
   */
  def executeMapping(groupToStart:StructureMapGroup):Unit = {
    val mainGroupRequiredInputs = groupToStart.sourceInputs.map(_._1).toSet
    val notGivenInputs = mainGroupRequiredInputs.diff(inputs.keySet)
    if(notGivenInputs.nonEmpty)
      throw new StructureMappingException(s"Some of the inputs ${notGivenInputs.mkString(", ")} not given to the main mapping group ${groupToStart.name}!")

    val mainGroupTargets = groupToStart.targetInputs

    rootContextRepository =
      RootContextRepository(
        inputs.filterKeys(mainGroupRequiredInputs.contains),
        mainGroupTargets
          .map(t => t._1 -> TargetContext(value = t._2.toSeq.map(rt => createResourceType(rt)))) //Create the type if exist
          .toMap
      )
    val mainGroupExecutor = StructureMapGroupExecutor(this, groupToStart, ContextRepository(rootContextRepository), mappingUtility)
    mainGroupExecutor.executeMapping()
  }

  private def createResourceType(rt:String):JValue = {
    mappingUtility.createResource(
      rt,
      targetDefinitions
        .find(_._3.contains(rt)).map(_._2) //Find a profile with th given type alias
        .orElse(targetDefinitions.find(_._3.isEmpty).map(_._2)) //or find the profile given for target without alias
    )
  }

  override def findRuleOrGroup(name: String): Option[Either[StructureMapGroup, StructureMapRule]] = {
    structureMap
      .groups  //Try to find within the map itself
      .find(_.name == name)
      .map(Left(_))
      .orElse(importedStructureGroups.get(name).map(Left(_))) //Or imports
  }
}

case class StructureMapGroupExecutor(parent:IMappingExecutor, structureGroup:StructureMapGroup, ctxRepository:IContextRepository, mappingUtility: IMappingUtility) extends IMappingExecutor {

  def executeMapping():Unit = {
    //Execute the extended group rules first
    structureGroup.extend.foreach(g => parent.findRuleOrGroup(g) match {
      case Some(Left(group)) =>
        StructureMapGroupExecutor(this, group, ctxRepository, mappingUtility)
          .executeMapping()
      case _ =>
        throw new StructureMappingException(s"Extended group ${g} referred in group ${structureGroup.name} not found in StructureMap hierarchy!")
    })

    //Execute the rules
    val ruleExecutors = structureGroup.rules.map(r => StructureMapRuleExecutor(this, r, ContextRepository(ctxRepository), mappingUtility))
    ruleExecutors.foreach(_.executeMapping())
  }

  /**
   * Find rule or group
   * @param name
   * @return
   */
  override def findRuleOrGroup(name: String): Option[Either[StructureMapGroup, StructureMapRule]] = {
     structureGroup
       .rules
       .find(_.name == name)
       .map(Right(_))
       .orElse(parent.findRuleOrGroup(name))
  }
}

case class StructureMapRuleExecutor(parent:IMappingExecutor, structureMapRule:StructureMapRule, ctxRepository:ContextRepository, mappingUtility: IMappingUtility) extends IMappingExecutor {
  val logger = LoggerFactory.getLogger(classOf[StructureMapRuleExecutor])

  val transformationHandler = new TransformationHandler(mappingUtility)

  override def findRuleOrGroup(name: String): Option[Either[StructureMapGroup, StructureMapRule]] = {
    structureMapRule
      .childRules
      .find(_.name == name)
      .map(Right(_))
      .orElse(parent.findRuleOrGroup(name))
  }


  def isTargetParam(v:String):Boolean = {
    ctxRepository.getTargetContext(v).isDefined
  }

  /**
   * Execute a mapping rule
   * @return
   */
 def executeMapping():Unit = {
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
        .map(cr => StructureMapRuleExecutor(parent, cr, ContextRepository(ctxRepository), mappingUtility))
     childExecutors.foreach(_.executeMapping())

     //Execute dependent rules or groups
     val dependentExecutors = structureMapRule.dependentRules.map(dr => parent.findRuleOrGroup(dr._1) match {
       case None => throw new StructureMappingException(s"Dependent rule/group ${dr._1} referred in rule ${structureMapRule.name} not found in StructureMap hierarchy!")
       case Some(Left(group)) =>
         //Check if we have enough target params
         val requiredTargetParams = group.targetInputs.map(_._1)
         val requiredSourceParams = group.sourceInputs.map(_._1)
         val targetParamsShared = dr._2.filter(isTargetParam)
         val sourceParamsShared = dr._2.diff(targetParamsShared)
         if(targetParamsShared.length < requiredTargetParams.length)
           throw new StructureMappingException(s"Target input parameters of dependent group does not match with the shared variables!")

         if(group.sourceInputs.length  != (sourceParamsShared.length + requiredTargetParams.length - targetParamsShared.length))
           throw new StructureMappingException(s"Source input parameters of dependent group does not match with the shared variables!")

         val variableMap =
           (requiredSourceParams ++ requiredTargetParams)
           .zip(sourceParamsShared ++ targetParamsShared)
           .toMap

         StructureMapGroupExecutor(parent, group, ContextRepository(ctxRepository, Some(variableMap)), mappingUtility)
       case Some(Right(rule)) =>
         StructureMapRuleExecutor(parent, rule, ctxRepository, mappingUtility)
     })
     dependentExecutors.foreach(_.executeMapping())

   }
 }

  /**
   * Evaluate a target transformation
    * @param target
   * @param sources
   * @return
   */
 private def evaluateTarget(target:StructureMapTarget, sources:Seq[JValue]) = {
   val targetValue = target.transform.map(tf => transformationHandler.transform(sources, tf,  target.parameters)).getOrElse(Seq(JObject()))
   (target.context, target.contextType) match {
     //If there is a context defined in target find it
     case (Some(context), Some("variable")) =>
       ctxRepository.addElement(structureMapRule.name, context, target.element.get, target.variable, target.listMode, targetValue)
     case (None, _) =>
       ctxRepository.addRootContext(target.variable.get, targetValue)
     case _ =>
       throw new StructureMappingException(s"Type mappings not supported yet!")
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

package io.onfhir.config

import io.onfhir.api.util.IOUtil
import org.json4s.JsonAST.JArray
import org.slf4j.{Logger, LoggerFactory}
import io.onfhir.util.JsonFormatter._

import scala.io.Source

/**
  * Created by tuncay on 1/16/2017.
  */
object IndexConfigurator {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)
  //Configuration object for each resource listing the search parameters to be indexed (indexes)
  case class ResourceIndexConfiguration(resourceType:String, shardKey:Option[Seq[String]], indexes:Set[String])

  final val ELEM_CREATE_INDEX_FOR_COMPARTMENTS = "createIndexForCompartments"
  final val ELEM_RESOURCES = "resources"
  final val ELEM_RESOURCE = "resource"
  final val ELEM_INDEXES = "indexes"
  final val ELEM_SHARD_KEY = "shardKey"

  //val defaultIndexes = Set("_id")

  /**
    * Read and parse the index configuration file and return index configuration for each resource type
    * @param configPath Path to the configuration file
    * @param defaultPath Default path within the project
    * @return
    */
  def parseIndexConfigurationFile(configPath:Option[String], defaultPath:String, compartmentRelations:Map[String, Map[String, Set[String]]]):Map[String, ResourceIndexConfiguration] = {
    //Read the configuration file
    val parsedIndexConfiguration = configPath match {
      case Some(path) =>
        logger.info(s"Reading DB Index Configuration file from '$path'")
        //Source.fromFile(path)
        IOUtil.readResource(path)
      case None =>
        logger.info(s"Reading DB Index Configuration file path '$defaultPath'")
        IOUtil.readInnerResource(defaultPath)
        //Source.fromInputStream( getClass.getResourceAsStream(defaultPath))
    }
    //Convert to the map
    //val parsedIndexConfiguration = source.getLines().mkString.parseJson

    //Parse the json and create ResourceIndexConfiguration objects
    val createIndexForCompartments:Boolean =  (parsedIndexConfiguration \ ELEM_CREATE_INDEX_FOR_COMPARTMENTS).extract[Boolean]
    val resourceIndexConfigurations = (parsedIndexConfiguration \ ELEM_RESOURCES).asInstanceOf[JArray].arr.map(resIndexConf => {
      val resourceType = (resIndexConf \ ELEM_RESOURCE).extract[String]
      val definedIndexes:Set[String] = (resIndexConf \ ELEM_INDEXES).extractOpt[List[String]].map(_.toSet).getOrElse(Set.empty)//.union(defaultIndexes)
      val allIndexes =
        if(createIndexForCompartments)
          definedIndexes.union(compartmentRelations.flatMap(cr => cr._2.getOrElse(resourceType, Set.empty)).toSet)
        else
          definedIndexes

      ResourceIndexConfiguration(
        resourceType,
        (resIndexConf \ ELEM_SHARD_KEY).extractOpt[Seq[String]],
        allIndexes
      )
    })
    /*
    //From supported resources that are not mentioned in the index configuration file only support default indexes
    val resourceNamesIndexNotDefined = FhirConfig.supportedProfiles.keySet.diff(resourceIndexConfigurations.map(_.resourceType).toSet)
    val resoucesIndexNotDefined = resourceNamesIndexNotDefined.map(rtype =>
      ResourceIndexConfiguration(
        rtype,
        None,
        defaultIndexes))*/

    //(resourceIndexConfigurations ++ resoucesIndexNotDefined).map(rc => rc.resourceType -> rc).toMap
    resourceIndexConfigurations.map(rc => rc.resourceType -> rc).toMap
  }
}

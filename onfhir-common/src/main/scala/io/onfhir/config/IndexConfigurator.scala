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

  /**
    * Read and parse the index configuration file and return index configuration for each resource type
    * @param configPath Path to the configuration file
    * @param defaultPath Default path within the project
    * @return
    */
  def parseIndexConfigurationFile(configPath:Option[String], defaultPath:String, compartmentRelations:Map[String, Map[String, Set[String]]]):Map[String, ResourceIndexConfiguration] = {
    //Read the configuration file
    val indexConfigurationJson = configPath match {
      case Some(path) =>
        logger.info(s"Reading DB Index Configuration file from '$path'")
        //Source.fromFile(path)
        IOUtil.readResource(path)
      case None =>
        logger.info(s"Reading DB Index Configuration file path '$defaultPath'")
        IOUtil.readInnerResource(defaultPath)
    }
    //Parse the json into the model
    val parsedIndexConfiguration = indexConfigurationJson.extract[OnfhirIndexConfig]

    parsedIndexConfiguration
      .resources
      .map(rind =>
        if(parsedIndexConfiguration.createIndexForCompartments){
          val compartmentParams =
            compartmentRelations
              .flatMap(cr => cr._2.getOrElse(rind.resource, Set.empty) - "_id")
              .toSeq

          rind.resource -> rind.copy(indexes = rind.indexes ++ compartmentParams.diff(rind.indexes).distinct.map(p => OnfhirIndex(parameters = Seq(p))))
        } else
          rind.resource -> rind
      )
      .toMap
  }
}

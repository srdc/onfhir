package io.onfhir.api.util

import java.io.{File, FileInputStream, InputStream, InputStreamReader}
import java.util.zip.{ZipEntry, ZipInputStream}

import io.onfhir.api.Resource
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.InitializationException
import io.onfhir.util.OnFhirZipInputStream
import org.apache.commons.io.input.BOMInputStream
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.io.Source

/**
 * Utility functions to read FHIR resources (in JSON format) from file system
 */
object IOUtil {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  def readResource(filePath:String):Resource = {
    JsonMethods.parse( new InputStreamReader(new BOMInputStream(new FileInputStream(new File(filePath))))).asInstanceOf[JObject]
  }

  def readInnerResource(resourcePath:String):Resource = {
    JsonMethods.parse(new InputStreamReader(new BOMInputStream(getClass.getResourceAsStream(resourcePath)))).asInstanceOf[JObject]
  }

  /**
   * Read all JSON resources in a folder or zip and parse them
   * @param folderPath          Path to the zip file or folder if given
   * @param defaultFolderPath   Default folder path within project resources
   * @return
   */
  def readResourcesInFolderOrZip(folderPath:Option[String], defaultFolderPath:String):Seq[Resource] = {
    folderPath match {
      case Some(path) =>
        logger.info(s"Reading resources from folder or zip file '$path'")
        val givenFile = new File(path)
        if (!givenFile.exists())
          throw new InitializationException(s"Folder or zip file not found in $path ...")

        if (path.endsWith(".zip")) {
          //Given as zip file
          val zipInputStream = new OnFhirZipInputStream(new FileInputStream(givenFile))
          val results = readAllResourcesInZip(zipInputStream)
          zipInputStream.forceClose()
          results
        } else {
          if (!givenFile.isDirectory)
            throw new InitializationException(s"Given path '$path' is not a folder or zip file ...")
          //Given as folder
          givenFile.listFiles().toSeq.map(file => {
            JsonMethods.parse(new InputStreamReader(new BOMInputStream(new FileInputStream(file)))).asInstanceOf[JObject]
          })
        }
      case None =>
        logger.info(s"Reading resources from default folder '$defaultFolderPath'")
        val insp = getClass.getClassLoader.getResourceAsStream(defaultFolderPath)
        if (insp != null) {
          //Read the default files packaged as zip
          val zipInputStream = new OnFhirZipInputStream(insp)
          val results = readAllResourcesInZip(zipInputStream)
          zipInputStream.forceClose()
          results
        } else Nil
    }
  }

  /**
   * Read and parse FHIR Bundle file
   * @param fileName            Name of the file in the FHIR standard definitions zip e.g. profiles-resources.json --> Base FHIR resource definitions (StructureDefinitions)
   * @param resourceTypeFilter  Types of the resources to extract from the Bundle
   * @return
   */
  def readStandardBundleFile(fileName:String, resourceTypeFilter:Set[String]) :Seq[Resource] = {
    readResourceInZip(OnfhirConfig.baseDefinitions, "definitions.json.zip", fileName, s"Reading FHIR Bundle file $fileName and extracting resource types $resourceTypeFilter...") match {
      case None => Nil
      case Some(is) =>
        //Parse the Bundle
        val bundle = JsonMethods.parse(new InputStreamReader(new BOMInputStream(is))).asInstanceOf[JObject]
        //Get the resources
        FHIRUtil.extractValueOptionByPath[Seq[Resource]](bundle, "entry.resource")
          .getOrElse(Nil)
          .filter(r =>
            resourceTypeFilter
              .contains(FHIRUtil.extractValue[String](r, "resourceType")) //Filter the specific Resource types
          )
    }
  }

  /**
   * Read the file as input stream within a zip file
   * @param zipFilePath
   * @param defaultZipFilePath
   * @param resourceName
   * @param descriptionForLog
   * @return
   */
  def readResourceInZip(zipFilePath:Option[String], defaultZipFilePath:String, resourceName:String, descriptionForLog:String):Option[InputStream] = {
    val inputStream = zipFilePath match {
      case Some(path) =>
        logger.info(s"Reading $descriptionForLog from '$path'")
        new FileInputStream(new File(path))
      case None =>
        logger.info(s"Reading $descriptionForLog from default path '$defaultZipFilePath'")
        getClass.getClassLoader.getResourceAsStream(defaultZipFilePath)
    }

    val zipInputStream = new ZipInputStream(inputStream)
    findFileInZip(zipInputStream, resourceName)
  }

  /**
   * Find a file within a Zip file and return its InputStream
   * @param zipStream  The input stream for the zip file
   * @param fileName The name of the file to be searched within the zip file
   * @return
   */
  def findFileInZip(zipStream:ZipInputStream, fileName:String): Option[InputStream] = {
    var zipEntry : ZipEntry = zipStream.getNextEntry
    while(zipEntry != null){
      if(zipEntry.getName.equalsIgnoreCase(fileName))
        return Some(zipStream)
      else
        zipEntry = zipStream.getNextEntry
    }
    None
  }

  /**
   * Read all JSONs in a zip file
   * @param zipStream
   * @return
   */
    private def readAllResourcesInZip(zipStream:ZipInputStream):Seq[Resource] = {
      val resources:mutable.ListBuffer[Resource] = new mutable.ListBuffer[Resource]
      var zipEntry : ZipEntry = zipStream.getNextEntry

      while(zipEntry != null){
        val reader = new InputStreamReader(new BOMInputStream(zipStream), "UTF-8")
        resources.append(JsonMethods.parse(reader).asInstanceOf[JObject])
        zipStream.closeEntry()
        zipEntry = zipStream.getNextEntry
      }
      resources
   }

}

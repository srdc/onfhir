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

/**
 * Utility functions to read FHIR resources (in JSON format) from file system
 */
object IOUtil {
  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

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

  private def readFhirResourcesInFolderOrZip(folderPath:Option[String], defaultFolderPath:String, rtype:String):Seq[Resource] = {
    folderPath match {
      case Some(path) =>
        logger.info(s"Reading  $rtype definitions from folder '$path'")
        val givenFile = new File(path)
        if(!givenFile.exists())
          throw new InitializationException(s"Folder not found in $path ...")

        if(path.endsWith(".zip")) {
          //Given as zip file
          val zipInputStream = new OnFhirZipInputStream(new FileInputStream(givenFile))
          val results = readInfrastructureResourcesInZip(rtype, zipInputStream)
          zipInputStream.forceClose()
          results
        } else {
          if(!givenFile.isDirectory)
            throw new InitializationException(s"Given path '$path' is not a folder or zip file ...")
          //Given as folder
          givenFile.listFiles().toSeq.map(file => {
            JsonMethods.parse(new InputStreamReader(new BOMInputStream(new FileInputStream(file)), "UTF-8")).asInstanceOf[JObject]
          })
        }
      case None =>
        logger.info(s"Reading $rtype definitions from default folder '$defaultFolderPath'")
        val insp = getClass.getResourceAsStream(defaultFolderPath)
        if(insp!=null) {
          //Read the default files packaged as zip
          val zipInputStream = new OnFhirZipInputStream(insp)
          val results = readInfrastructureResourcesInZip(rtype, zipInputStream)
          zipInputStream.forceClose()
          results
        }else Nil
    }
  }

  private def readInfrastructureResourcesInZip(rtype:String, zipStream:ZipInputStream):Seq[Resource] = {
    //val zipStream = new ZipInputStream(getClass.getResourceAsStream(zipPath))
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

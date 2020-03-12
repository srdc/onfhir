package io.onfhir.api.util

import java.io.{File, FileInputStream, InputStream, InputStreamReader, Reader}
import java.util.zip.{ZipEntry, ZipInputStream}

import io.onfhir.api.{DEFAULT_RESOURCE_PATHS, Resource}
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

  def readResource(resourcePath:Option[String], defaultPath:String, rtype:String):Resource = {
    try {
      resourcePath match {
        case Some(path) =>
          logger.info(s"Reading a $rtype from '$path'")
          readResource(path)
        case None =>
          logger.info(s"Reading a $rtype from default path '$defaultPath'")
          readInnerResource(defaultPath)
      }
    } catch {
      case e:Exception =>
        throw new InitializationException(s"Cannot read resource $rtype from path ${resourcePath.getOrElse(defaultPath)}!", Some(e))
    }
  }

  def readResource(filePath:String):Resource = {
    parseResource( new InputStreamReader(new BOMInputStream(new FileInputStream(new File(filePath)))), filePath).asInstanceOf[JObject]
  }

  def readInnerResource(resourcePath:String):Resource = {
    parseResource(new InputStreamReader(new BOMInputStream(getClass.getClassLoader.getResourceAsStream(resourcePath))), resourcePath).asInstanceOf[JObject]
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
          val results = readAllResourcesInZip(zipInputStream, path)
          zipInputStream.forceClose()
          results
        } else {
          if (!givenFile.isDirectory)
            throw new InitializationException(s"Given path '$path' is not a folder or zip file ...")
          //Given as folder
          givenFile.listFiles().toSeq.map(file => {
            try {
              parseResource(new InputStreamReader(new BOMInputStream(new FileInputStream(file))), file.getAbsolutePath).asInstanceOf[JObject]
            } catch {
              case e:Exception =>
                throw new InitializationException(s"Cannot parse resource in ${path + "/" + file.getName}  ...", Some(e))
            }
          })
        }
      case None =>
        logger.info(s"Reading resources from default folder '$defaultFolderPath'")
        val insp = getClass.getClassLoader.getResourceAsStream(defaultFolderPath)
        if (insp != null) {
          //Read the default files packaged as zip
          val zipInputStream = new OnFhirZipInputStream(insp)
          val results = readAllResourcesInZip(zipInputStream, defaultFolderPath)
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
    readResourceInZip(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS, fileName, resourceTypeFilter.mkString(",")) match {
      case None =>
        throw new InitializationException(s"Cannot find  resource bundle '$fileName' in path ${OnfhirConfig.baseDefinitions.getOrElse(DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS)}!")
      case Some(is) =>
        try {
          //Parse the Bundle
          val bundle = parseResource(new InputStreamReader(new BOMInputStream(is)), fileName).asInstanceOf[JObject]
          //Get the resources
          val resources = FHIRUtil.extractValueOptionByPath[Seq[Resource]](bundle, "entry.resource").getOrElse(Nil)

          resources
            .filter(r =>
              resourceTypeFilter
                .contains(FHIRUtil.extractValue[String](r, "resourceType")) //Filter the specific Resource types
            )
        }catch {
          case e:Exception =>
            throw new InitializationException(s"Cannot parse resource bundle from path ${OnfhirConfig.baseDefinitions.getOrElse(DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS)} into JSON!", Some(e))
        }
    }
  }

  /**
   * Read the file as input stream within a zip file
   * @param zipFilePath
   * @param defaultZipFilePath
   * @param resourceName
   * @param rtype
   * @return
   */
  def readResourceInZip(zipFilePath:Option[String], defaultZipFilePath:String, resourceName:String, rtype:String):Option[InputStream] = {
    try {
      val inputStream = zipFilePath match {
        case Some(path) =>
          logger.info(s"Reading resources of type $rtype from '$path'")
          new FileInputStream(new File(path))
        case None =>
          logger.info(s"Reading resources of type $rtype from default path '$defaultZipFilePath'")
          getClass.getClassLoader.getResourceAsStream(defaultZipFilePath)
      }

      val zipInputStream = new ZipInputStream(inputStream)
      findFileInZip(zipInputStream, resourceName)
    } catch {
      case e:Exception =>
        throw new InitializationException(s"Cannot read foundation resource package from path ${zipFilePath.getOrElse(defaultZipFilePath)}!", Some(e))
    }
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
    private def readAllResourcesInZip(zipStream:ZipInputStream, path:String):Seq[Resource] = {
      val resources:mutable.ListBuffer[Resource] = new mutable.ListBuffer[Resource]
      var zipEntry : ZipEntry = zipStream.getNextEntry

      while(zipEntry != null){
        val reader = new InputStreamReader(new BOMInputStream(zipStream), "UTF-8")
        resources.append(parseResource(reader, zipEntry.getName))
        zipStream.closeEntry()
        zipEntry = zipStream.getNextEntry
      }
      resources
   }

  private def parseResource(reader:Reader, path:String):Resource = {
    if(path.endsWith(".json"))
      try {
          JsonMethods.parse(reader).asInstanceOf[JObject]
      }
      catch {
        case e:Exception =>
          throw new InitializationException(s"Cannot read parse resource from path $path!", Some(e))
      }
    else
      throw new InitializationException(s"Cannot read parse resource from path $path, it should be JSON file!")
  }

}

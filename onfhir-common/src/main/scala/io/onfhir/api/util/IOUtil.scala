package io.onfhir.api.util

import io.onfhir.api.Resource
import io.onfhir.exception.InitializationException
import io.onfhir.util.OnFhirZipInputStream
import org.apache.commons.io.input.BOMInputStream
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods
import org.slf4j.{Logger, LoggerFactory}

import java.io._
import java.util.zip.{ZipEntry, ZipInputStream}
import scala.collection.mutable

/**
 * Utility functions to read FHIR resources (in JSON format) from file system
 */
object IOUtil {
  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Read a FHIR resource from given path or default path (within project resources)
   *
   * @param resourcePath Given file path
   * @param defaultPath  Default resource path (within project resources)
   * @param rtype        Resource type
   * @return
   */
  def readResource(resourcePath: Option[String], defaultPath: String, rtype: String): Resource = {
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
      case e: Exception =>
        throw new InitializationException(s"Cannot read resource $rtype from path ${resourcePath.getOrElse(defaultPath)}!", Some(e))
    }
  }

  /**
   * Read a FHIR resource from given File path
   *
   * @param filePath File path
   * @return
   */
  def readResource(filePath: String): Resource = {
    parseResource(new InputStreamReader(BOMInputStream.builder.setInputStream(new FileInputStream(filePath)).get()), filePath)
  }

  /**
   * Read a FHIR resource from project resources with a path
   *
   * @param resourcePath Resource path
   * @return
   */
  def readInnerResource(resourcePath: String): Resource = {
    parseResource(new InputStreamReader(BOMInputStream.builder.setInputStream(getClass.getClassLoader.getResourceAsStream(resourcePath)).get()), resourcePath)
  }

  /**
   *
   * @param resourcePath Resource path
   * @return
   */
  def readModuleResource(resourcePath: String): Resource = {
    parseResource(new InputStreamReader(BOMInputStream.builder.setInputStream(getClass.getResourceAsStream(resourcePath)).get()), resourcePath)
  }

  /**
   * Read all JSON resources in a folder or zip and parse them
   *
   * @param folderPath        Path to the zip file or folder if given
   * @param defaultFolderPath Default folder path within project resources
   * @return
   */
  def readResourcesInFolderOrZip(folderPath: Option[String], defaultFolderPath: String): Seq[Resource] = {
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
          getFilesFromFolder(folder = givenFile, recursively = true, ignoreHidden = true, withExtension = None)
            .map(file => {
              try {
                parseResource(new InputStreamReader(BOMInputStream.builder.setInputStream(new FileInputStream(file)).get()), file.getAbsolutePath)
              } catch {
                case e: Exception =>
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
   * Get the list of files from the given folder.
   *
   * @param folder        The folder to retrieve the files from.
   * @param recursively   If true, the folder will be searched recursively to retrieve all files within.
   * @param ignoreHidden  If true, hidden files and directories will be excluded from the result.
   * @param withExtension An optional extension (e.g., .json) if the files need to be filtered.
   * @return A sequence of files matching the criteria.
   */
  def getFilesFromFolder(
                          folder: File,
                          recursively: Boolean,
                          ignoreHidden: Boolean,
                          withExtension: Option[String]
                        ): Seq[File] = {
    if (folder.exists && folder.isDirectory) {
      val files = folder.listFiles().toSeq // List all available files in the given folder

      // Filter hidden files if ignoreHidden is true
      val nonHiddenFiles = if (ignoreHidden) files.filterNot(f => f.isHidden || f.getName.startsWith(".")) else files

      val filteredFiles = withExtension
        .map(ext => nonHiddenFiles.filter(_.getName.endsWith(ext))).getOrElse(nonHiddenFiles)
        .filterNot(_.isDirectory)

      if (recursively) {
        val subFolders = nonHiddenFiles.filter(_.isDirectory)
        filteredFiles ++ subFolders.flatMap(f => getFilesFromFolder(f, recursively, ignoreHidden, withExtension))
      } else {
        filteredFiles
      }
    } else {
      throw new IllegalArgumentException(s"Given folder is not valid. Path: ${folder.getAbsolutePath}")
    }
  }


  /**
   * Given a filename, removes its extension if an extension exists (e.g., admissions.json -> admissions)
   *
   * @param fileName
   * @return
   */
  def removeFileExtension(fileName: String): String = {
    val lastDotIndex = fileName.lastIndexOf('.')
    if (lastDotIndex != -1) {
      fileName.substring(0, lastDotIndex)
    } else {
      fileName // No extension found, return the original file name
    }
  }

  /**
   * Read and parse FHIR Bundle file
   *
   * @param baseDefinitionsPath        Path to retrieve base FHIR definitions zip
   * @param defaultBaseDefinitionsPath Default path within onFhir in class resources for base definitions if path is not given
   * @param fileName                   Name of the file in the FHIR standard definitions zip e.g. profiles-resources.json --> Base FHIR resource definitions (StructureDefinitions)
   * @param resourceTypeFilter         Types of the resources to extract from the Bundle
   * @return
   */
  def readStandardBundleFile(baseDefinitionsPath: Option[String], defaultBaseDefinitionsPath: String, fileName: String, resourceTypeFilter: Set[String]): Seq[Resource] = {
    readResourceInZip(baseDefinitionsPath, defaultBaseDefinitionsPath, fileName, resourceTypeFilter.mkString(",")) match {
      case None =>
        throw new InitializationException(s"Cannot find  resource bundle '$fileName' in path ${baseDefinitionsPath.getOrElse(defaultBaseDefinitionsPath)}!")
      case Some(is) =>
        try {
          //Parse the Bundle
          val bundle = parseResource(new InputStreamReader(BOMInputStream.builder.setInputStream(is).get()), fileName)
          //Get the resources
          val resources = FHIRUtil.extractValueOptionByPath[Seq[Resource]](bundle, "entry.resource").getOrElse(Nil)

          resources
            .filter(r =>
              resourceTypeFilter
                .contains(FHIRUtil.extractValue[String](r, "resourceType")) //Filter the specific Resource types
            )
        } catch {
          case e: Exception =>
            throw new InitializationException(s"Cannot parse resource bundle from path ${baseDefinitionsPath.getOrElse(defaultBaseDefinitionsPath)} into JSON!", Some(e))
        }
    }
  }

  /**
   * Read the file as input stream within a zip file
   *
   * @param zipFilePath        Zip file path
   * @param defaultZipFilePath Default zip file path if zipFilePath is not given
   * @param resourceName       Resource name within the zip
   * @param rtype              Resource type to filter
   * @return
   */
  private def readResourceInZip(zipFilePath: Option[String], defaultZipFilePath: String, resourceName: String, rtype: String): Option[InputStream] = {
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
      case e: Exception =>
        throw new InitializationException(s"Cannot read foundation resource package from path ${zipFilePath.getOrElse(defaultZipFilePath)}!", Some(e))
    }
  }

  /**
   * Find a file within a Zip file and return its InputStream
   *
   * @param zipStream The input stream for the zip file
   * @param fileName  The name of the file to be searched within the zip file
   * @return
   */
  private def findFileInZip(zipStream: ZipInputStream, fileName: String): Option[InputStream] = {
    var zipEntry: ZipEntry = zipStream.getNextEntry
    while (zipEntry != null) {
      if (zipEntry.getName.equalsIgnoreCase(fileName))
        return Some(zipStream)
      else
        zipEntry = zipStream.getNextEntry
    }
    None
  }

  /**
   * Read all JSONs in a zip file
   *
   * @param zipStream Zip file input stream
   * @return
   */
  private def readAllResourcesInZip(zipStream: ZipInputStream, path: String): Seq[Resource] = {
    val resources: mutable.ListBuffer[Resource] = new mutable.ListBuffer[Resource]
    var zipEntry: ZipEntry = zipStream.getNextEntry

    while (zipEntry != null) {
      val reader = new InputStreamReader(BOMInputStream.builder.setInputStream(zipStream).get(), "UTF-8")
      resources.append(parseResource(reader, zipEntry.getName))
      zipStream.closeEntry()
      zipEntry = zipStream.getNextEntry
    }
    resources.toSeq
  }

  /**
   * Parse a JSON resource
   *
   * @param reader Reader
   * @param path   File path it is read from
   * @return
   */
  private def parseResource(reader: Reader, path: String): Resource = {
    if (path.endsWith(".json"))
      try {
        JsonMethods.parse(reader).asInstanceOf[JObject]
      }
      catch {
        case e: Exception =>
          throw new InitializationException(s"Cannot read parse resource from path $path!", Some(e))
      }
    else
      throw new InitializationException(s"Cannot read parse resource from path $path, it should be JSON file!")
  }

}

package io.onfhir.config

import java.io._
import java.util.zip.{ZipEntry, ZipInputStream}

import ca.uhn.fhir.context.FhirContext
import ca.uhn.fhir.parser.IParser
import ca.uhn.fhir.validation.IValidatorModule
import io.onfhir.api._
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{FHIRResourceValidator, IFhirResourceValidator}
import io.onfhir.config.IndexConfigurator.ResourceIndexConfiguration
import io.onfhir.db.DBInitializer
import io.onfhir.exception.InitializationException
import io.onfhir.util.OnFhirZipInputStream
import io.onfhir.util.JsonFormatter._
import org.apache.commons.io.input.BOMInputStream
import org.hl7.fhir.instance.model.api.IBaseResource
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
  * Created by tuncay on 11/15/2016.
  * Base class to handle the setup and configuration of the platform and FHIR API to be served
  * In order to support a new FHIR version, a class should be implemented which extend this base class
  *
  * @param fhirContext HAPI FhirContext object
  * @tparam CONF Class to represent conformance statement in HAPI FHIR model for the FHIR version
  * @tparam STRUCTDEF Class to represent StructureDefinition in HAPI FHIR model for the FHIR version
  * @tparam SEARCHPARAM Class to represent SearchParameter in HAPI FHIR model for the FHIR version
  * @tparam COMPARTMENTDEF Class to represent CompartmentDefinition in HAPI FHIR model for the FHIR version
  * @tparam VALSET Class to represent Valueset in HAPI FHIR model for the FHIR version
  * @tparam BUNDLE Class to represent Bundle in HAPI FHIR model for the FHIR version
  * @tparam OPDEF Class to represent OperationDefinition in HAPI FHIR model for the FHIR version
  * @tparam CODESYSTEM Class to represent CodeSystem in HAPI FHIR model for the FHIR version
  */
abstract class AbstractFhirConfigurator[CONF <: IBaseResource,
  STRUCTDEF <: IBaseResource,
  SEARCHPARAM <: IBaseResource,
  COMPARTMENTDEF <: IBaseResource,
  VALSET <: IBaseResource,
  BUNDLE <: IBaseResource,
  OPDEF <: IBaseResource,
  CODESYSTEM <: IBaseResource](val fhirContext:FhirContext)(implicit c:ClassTag[CONF], s:ClassTag[STRUCTDEF], s2:ClassTag[SEARCHPARAM], comp:ClassTag[COMPARTMENTDEF], v:ClassTag[VALSET], b:ClassTag[BUNDLE], o:ClassTag[OPDEF], cs:ClassTag[CODESYSTEM]) extends IFhirConfigurator {

  protected val logger:Logger = LoggerFactory.getLogger(this.getClass)

  //Name of the files that includes the Bundle for search parameter definitions in base FHIR specification
  val SEARCH_PARAMETERS_BUNDLE_FILE_NAME = "search-parameters.xml"
  //Name of the file that includes the Bundle for Structure Definitions for Resources in base FHIR specification
  val PROFILES_RESOURCES_BUNDLE_FILE_NAME = "profiles-resources.xml"
  //Name of the file that includes the Bundle for Structure Definitions for Types in base FHIR specification
  val PROFILES_TYPES_BUNDLE_FILE_NAME = "profiles-types.xml"

  //XML and JSON parsers for the given FHIR context
  def getXmlParser(rtype:String = "Bundle"):IParser = fhirContext.newXmlParser().setStripVersionsFromReferences(false)
  def getJsonParser(rtype:String = "Bundle"):IParser = fhirContext.newJsonParser().setStripVersionsFromReferences(false)


  /**
    * Configure and initialize the platform based on FHIR infrastructure resources (optional: Setup the platform)
    * @param fromConfig If true, the platform will be configured from configuration files (FHIR infrastructure resources).
    *                   If false, we assume that platform is already set up and infrastructure resources will be read from database
    * @return onFHIR FHIR configurations which defines FHIR capabilities of the server
    */
  def initializePlatform(fromConfig:Boolean = false):(FhirConfig, CONF, Seq[STRUCTDEF], Seq[SEARCHPARAM], Seq[COMPARTMENTDEF], Seq[OPDEF], Seq[VALSET],  Seq[CODESYSTEM], Map[String, ResourceIndexConfiguration]) = {
    logger.info("Reading FHIR infrastructure resources to start configuration of platform ...")
    //Read the FHIR Conformance statement
    val conformance:CONF = getConformance(fromConfig)
    //Read the StructureDefinitions for defined profiles
    val profiles:Seq[STRUCTDEF] = getResourceProfiles(fromConfig)
    //Read the search parameters defined further
    val searchParameters:Seq[SEARCHPARAM] = getSearchParameters(fromConfig)
    //Read the compartment definitions
    val compartmentDefinitions:Seq[COMPARTMENTDEF] = getCompartmentDefinitions(fromConfig)
    //Read operation definitions
    val operationDefinitions:Seq[OPDEF] = getOperationDefinitions(fromConfig)
    //Read value sets
    val valueSets:Seq[VALSET] = getValueSets(fromConfig)
    //Read code systems
    val codeSystems:Seq[CODESYSTEM] = getCodeSystems(fromConfig)

    logger.info("Configuring the platform accordingly ...")
    //Configure the platform by using the infrastructure resources
    val fhirConfig = getPlatformConfigurations(conformance, profiles, searchParameters, compartmentDefinitions, valueSets, operationDefinitions, codeSystems)

    //Parse Index configurations, and set configured shard keys
    val indexConfigurations = IndexConfigurator.parseIndexConfigurationFile(OnfhirConfig.dbIndexConfigurationPath, DEFAULT_RESOURCE_PATHS.INDEX_CONF_PATH, fhirConfig.compartmentRelations)
    if(OnfhirConfig.mongoShardingEnabled)
      fhirConfig.shardKeys = indexConfigurations.mapValues(iconf => iconf.shardKey.getOrElse(Nil).toSet)


    (fhirConfig, conformance, profiles, searchParameters, compartmentDefinitions, operationDefinitions, valueSets, codeSystems, indexConfigurations)
  }

  /**
    * Setups the platform (create the MongoDB collections and indexes and
    * store the infrastructure resources into the database
    * @param conformance Conformance statement for the FHIR repository
    * @param profiles StructureDefinitions for the profiles (resources, extensions, data types)
    * @param searchParameters Search Parameter definitions for extra search parameters supported
    * @param compartmentDefinitions CompartmentDefinitions for supported compartments
    * @param valueSets ValueSets used in profiles
    */
  def setupPlatform(fhirConfig: FhirConfig,
                             conformance:IBaseResource,
                             profiles:Seq[IBaseResource],
                             searchParameters:Seq[IBaseResource],
                             compartmentDefinitions: Seq[IBaseResource],
                             operationDefinitions:Seq[IBaseResource],
                             valueSets:Seq[IBaseResource],
                             codeSystems:Seq[IBaseResource],
                             indexConfigurations:Map[String, ResourceIndexConfiguration]):Unit = {
    logger.info("Setting up (or updating) the platform as requested ...")
    //Basic preparation for database
    DBInitializer.prepareDatabase()

    //Get supported resources and versioning mechanism
    val supportedResourcesAndVersions =
      fhirConfig
        .supportedResources
        .map(rtype =>
          rtype ->
            fhirConfig.profileConfigurations.get(rtype).map(_.versioning).getOrElse(OnfhirConfig.fhirDefaultVersioning)
        ).toMap
    //Create collections in database for each supported resource
    DBInitializer.createCollections(supportedResourcesAndVersions)
    //Create the indexes in DB
    DBInitializer.createIndexes(supportedResourcesAndVersions, fhirConfig.resourceQueryParameters, fhirConfig.commonQueryParameters, indexConfigurations)
    //Store the infrastructure resources into DB
    storeInfrastructureResources(conformance, profiles, searchParameters, compartmentDefinitions, valueSets, operationDefinitions, codeSystems)
  }


  /**
    * Return necessary configurations that are used in FHIR API services based on FHIR foundation category resource definitions
    * @param conformance Conformance statement for the configured FHIR repository
    * @param profiles StructureDefinitions for the profiles (resources, extensions, data types) to be used in configured FHIR repository
    * @param searchParameters Search Parameter definitions for extra search parameters supported
    * @param compartmentDefinitions CompartmentDefinitions for supported compartments for the configured FHIR repository
    * @param valueSets ValueSet definition for supported value sets within configured FHIR profiles
    * @param operationDefinitions FHIR Operations supported for the configured FHIR repository
    * @param codeSystems FHIR CodeSystem definitions supported for the configured FHIR profiles
    * @return onFHIR FHIR configuration defining FHIR capabilities
    */
  protected def getPlatformConfigurations(conformance:CONF,
                                          profiles:Seq[STRUCTDEF],
                                          searchParameters:Seq[SEARCHPARAM],
                                          compartmentDefinitions: Seq[COMPARTMENTDEF],
                                          valueSets:Seq[VALSET],
                                          operationDefinitions:Seq[OPDEF],
                                          codeSystems:Seq[CODESYSTEM]):FhirConfig

  /**
    * Construct resource validator
    * @return
    */
  def getResourceValidator():IFhirResourceValidator = new FHIRResourceValidator(fhirContext, getValidatorModule())


  /**
    * Return the version specific FHIR Validator Module from the HAPI library
    * @return
    */
  def getValidatorModule():IValidatorModule


  /**
    * Extract the SearchParameter definitions from a definition Bundle
    * @param bundle
    * @return
    */
  protected def extractSearchParametersFromBundle(bundle:BUNDLE):Seq[SEARCHPARAM]

  /**
    * Extract the StructureDefinitions from a definition Bundle
    * @param bundle
    * @return
    */
  protected def extractStructureDefinitionsFromBundle(bundle:BUNDLE):Seq[STRUCTDEF]

  /**
    * Extract the OperationDefinitions from a definition Bundle
    * @param bundle
    * @return
    */
  protected def extractOperationDefinitionsFromBundle(bundle:BUNDLE):Seq[OPDEF]

  /**
    * Store the infrastructure resources into the database
    * @param conformance Conformance statement for the FHIR repository
    * @param profiles StructureDefinitions for the profiles (resources, extensions, data types)
    * @param searchParameters Search Parameter definitions for extra search parameters supported
    * @param compartmentDefinitions CompartmentDefinitions for supported compartments
    * @param valueSets ValueSets used in profiles
    */
  private def storeInfrastructureResources(conformance:IBaseResource,
                                           profiles:Seq[IBaseResource],
                                           searchParameters:Seq[IBaseResource],
                                           compartmentDefinitions: Seq[IBaseResource],
                                           valueSets:Seq[IBaseResource],
                                           operationDefinitions:Seq[IBaseResource],
                                           codeSystems:Seq[IBaseResource]) = {
    logger.info("Storing infrastructure resources to database ...")
    //Store the Conformance statement (Set a fixed id for the conformance)
    DBInitializer.storeInfrastructureResources(FHIR_CONFORMANCE, Seq(FHIRUtil.setId(encodeResource(FHIR_CONFORMANCE, conformance), SERVER_CONFORMANCE_STATEMENT_ID)))
    //Store the Profiles (Structure Definitions)
    DBInitializer.storeInfrastructureResources(FHIR_STRUCTURE_DEFINITION,  profiles.map(profile => encodeResource(FHIR_STRUCTURE_DEFINITION, profile)))
    //Store the Search Parameters
    DBInitializer.storeInfrastructureResources(FHIR_SEARCH_PARAMETER, searchParameters.map(param => encodeResource(FHIR_SEARCH_PARAMETER, param)))
    //Store the CompartmentDefinitions
    DBInitializer.storeInfrastructureResources(FHIR_COMPARTMENT_DEFINITION,  compartmentDefinitions.map(compt => encodeResource(FHIR_COMPARTMENT_DEFINITION, compt)))
    //Store the ValueSets
    DBInitializer.storeInfrastructureResources(FHIR_VALUE_SET,  valueSets.map(vs => encodeResource(FHIR_VALUE_SET, vs)))
    //Store the OperationDefinitions
    DBInitializer.storeInfrastructureResources(FHIR_OPERATION_DEFINITION,  operationDefinitions.map(vs => encodeResource(FHIR_OPERATION_DEFINITION, vs)))
    //Store the Code systems
    DBInitializer.storeInfrastructureResources(FHIR_CODE_SYSTEM,  codeSystems.map(cs => encodeResource(FHIR_CODE_SYSTEM, cs)))
  }


  /**
    * Read and parse Conformance statement (either from the Config or Default conformance path)
    * @return
    */
  protected def getConformance(fromConfig:Boolean = false): CONF = {
    fromConfig match {
      //If read from configuration folders/files
      case true => //Read the Conformance Statement from path
        val conformanceSource = readInfrastructureResource(OnfhirConfig.conformancePath, DEFAULT_RESOURCE_PATHS.CONFORMANCE_PATH, "FHIR Conformance")
        //Parse the statement to object model (HAPI)
        parseInfrastructureResource[CONF](FHIR_CONFORMANCE, conformanceSource.bufferedReader(), OnfhirConfig.conformancePath.getOrElse(DEFAULT_RESOURCE_PATHS.CONFORMANCE_PATH))

      //If from database
      case false =>
        logger.info(s"Reading $FHIR_CONFORMANCE from database ...")
        val conformanceResource = DBInitializer.getConformance(FHIR_CONFORMANCE)
        parseInfrastructureResource[CONF](FHIR_CONFORMANCE, conformanceResource)
    }
  }

  /**
    * Read and parse the SearchParameters defined in the base FHIR standard (either from the Config or Default validation package path)
    * IMPORTANT: It uses the XML definition package and the bundle search-parameters.xml
    * @return
    */
  protected def getBaseSearchParameters: Seq[SEARCHPARAM] = {
    val searchParametersBundleStream = readInfrastructureResourceInZip(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS, SEARCH_PARAMETERS_BUNDLE_FILE_NAME, "base SearchParameters")
    if(searchParametersBundleStream.isEmpty)
      throw new InitializationException(s"Problem with FHIR definition package, " +
        s"the file $SEARCH_PARAMETERS_BUNDLE_FILE_NAME does not exist " +
        s"in ${OnfhirConfig.baseDefinitions.getOrElse(DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS)} ...")

    val searchParametersBundle = getXmlParser().parseResource(b.runtimeClass.asInstanceOf[Class[BUNDLE]], new InputStreamReader(searchParametersBundleStream.get))
    extractSearchParametersFromBundle(searchParametersBundle.asInstanceOf[BUNDLE])
  }


  /**
    * Read and parse the StructureDefinitions defined in the base FHIR standard (either from the Config or Default validation package path)
    * IMPORTANT: It use the XML Validation package and the bundle profiles-resources.xml
    * @return
    */
  protected def getBaseStructureDefinitions: Seq[STRUCTDEF] = {
    val resourcesBundleStream = readInfrastructureResourceInZip(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS, PROFILES_RESOURCES_BUNDLE_FILE_NAME, "base StructureDefinitions for Resources")
    if(resourcesBundleStream.isEmpty)
      throw new InitializationException(s"Problem with FHIR validation package, " +
        s"the file $PROFILES_RESOURCES_BUNDLE_FILE_NAME does not exist " +
        s"in ${OnfhirConfig.baseDefinitions.getOrElse(DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS)} ...")

    val resourcesBundle:IBaseResource = getXmlParser().parseResource(b.runtimeClass.asInstanceOf[Class[BUNDLE]], new InputStreamReader(resourcesBundleStream.get))

    val typesBundleStream = readInfrastructureResourceInZip(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS, PROFILES_TYPES_BUNDLE_FILE_NAME, "base StructureDefinitions for Types")
    if(typesBundleStream.isEmpty)
      throw new InitializationException(s"Problem with FHIR validation package, " +
        s"the file $PROFILES_TYPES_BUNDLE_FILE_NAME does not exist " +
        s"in ${OnfhirConfig.baseDefinitions.getOrElse(DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS)} ...")

    val typesBundle:IBaseResource = getXmlParser().parseResource(b.runtimeClass.asInstanceOf[Class[BUNDLE]], new InputStreamReader(typesBundleStream.get))

    extractStructureDefinitionsFromBundle(resourcesBundle.asInstanceOf[BUNDLE]) ++
      extractStructureDefinitionsFromBundle(typesBundle.asInstanceOf[BUNDLE])
  }

  /**
    * Read and parse the OperationDefinitions defined in base FHIR standard (either from the Config or Default validation package path)
    * IMPORTANT. It use the XML Validation package and the bundle profiles-resources.xml
    * @return
    */
  protected def getBaseOperationDefinitions: Seq[OPDEF] = {
    val operationDefinitionsBundleStream = readInfrastructureResourceInZip(OnfhirConfig.baseDefinitions, DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS, PROFILES_RESOURCES_BUNDLE_FILE_NAME, "Base Operation Definitions")
    if(operationDefinitionsBundleStream.isEmpty)
      throw new InitializationException(s"Problem with FHIR validation package, " +
        s"the file $PROFILES_RESOURCES_BUNDLE_FILE_NAME does not exist " +
        s"in ${OnfhirConfig.baseDefinitions.getOrElse(DEFAULT_RESOURCE_PATHS.BASE_DEFINITONS)} ...")

    val resourcesBundle:IBaseResource = getXmlParser().parseResource(b.runtimeClass.asInstanceOf[Class[BUNDLE]], new InputStreamReader(operationDefinitionsBundleStream.get))

    extractOperationDefinitionsFromBundle(resourcesBundle.asInstanceOf[BUNDLE])
  }


  /**
    * Read and parse the extra SearchParameters defined (either from the Config or Default folder path for SearchParameters)
    * @return
    */
  protected def getSearchParameters(fromConfig:Boolean = false) : Seq[SEARCHPARAM] = {
    fromConfig match {
      //If configuration from files
      case true=>
        readInfrastructureResourcesInFolderOrZip[SEARCHPARAM](OnfhirConfig.searchParametersPath, DEFAULT_RESOURCE_PATHS.SEARCH_PARAMETER, FHIR_SEARCH_PARAMETER)
      //If configuration from db
      case false=>
        logger.info(s"Reading $FHIR_SEARCH_PARAMETER definitions from database ...")
        DBInitializer.getInrastructureResources(FHIR_SEARCH_PARAMETER).map(searchParam => {
          parseInfrastructureResource[SEARCHPARAM](FHIR_SEARCH_PARAMETER, searchParam)
        })
    }
  }

  /**
    * Read and parse the resource profiles (Structure Definitions) (either from the Config or Default folder path for StructureDefinitions)
    * @return
    */
  protected def getResourceProfiles(fromConfig:Boolean = false): Seq[STRUCTDEF] = {
    fromConfig match {
      //If reading StructureDefinitions from configuration folder
      case true => readInfrastructureResourcesInFolderOrZip[STRUCTDEF](OnfhirConfig.profilesPath, DEFAULT_RESOURCE_PATHS.PROFILES_FOLDER, FHIR_STRUCTURE_DEFINITION)
      //If reading from DB
      case false =>
        logger.info(s"Reading $FHIR_STRUCTURE_DEFINITION definitions from database ...")
        DBInitializer.getInrastructureResources(FHIR_STRUCTURE_DEFINITION).map(profile => {
          parseInfrastructureResource[STRUCTDEF](FHIR_STRUCTURE_DEFINITION, profile)
        })
    }
  }

  /**
    * Read and parse the power2dm.compartment definition (either from the Config or Default folder path for CompartmentDefinitions)
    * IMPORTANT: As CompartmentDefinition is not a resource in DSTU2 and still we use it for configuration,
    * it is handled in an exceptional way
    * @return
    */
  protected def getCompartmentDefinitions(fromConfig:Boolean = false): Seq[COMPARTMENTDEF] = {
    fromConfig match {
      //If reading CompartmentDefinition from configuration folder
      case true =>
        logger.info("Compartment Definition Path:"+OnfhirConfig.compartmentDefinitionsPath)
        readInfrastructureResourcesInFolderOrZip[COMPARTMENTDEF](OnfhirConfig.compartmentDefinitionsPath, DEFAULT_RESOURCE_PATHS.COMPARTMENTS_PATH, FHIR_COMPARTMENT_DEFINITION)
      //If reading from DB
      case false=>
        logger.info(s"Reading $FHIR_COMPARTMENT_DEFINITION definitions from database ...")
        DBInitializer.getInrastructureResources(FHIR_COMPARTMENT_DEFINITION).map(cdef => {
          parseInfrastructureResource[COMPARTMENTDEF](FHIR_COMPARTMENT_DEFINITION, cdef)
        })
    }
  }

  /**
    * Read and parse OperationDefinition resources (either from the Config or Default folder path for CompartmentDefinitions)
    * @param fromConfig
    * @return
    */
  protected def getOperationDefinitions(fromConfig:Boolean = false): Seq[OPDEF] = {
    fromConfig match {
      //If reading CompartmentDefinition from configuration folder
      case true =>
        logger.info("Operation Definitions Path:"+OnfhirConfig.operationDefinitionsPath)
        readInfrastructureResourcesInFolderOrZip[OPDEF](OnfhirConfig.operationDefinitionsPath, DEFAULT_RESOURCE_PATHS.OPDEFS_PATH, FHIR_OPERATION_DEFINITION)
      //If reading from DB
      case false=>
        logger.info(s"Reading $FHIR_OPERATION_DEFINITION definitions from database ...")
        DBInitializer.getInrastructureResources(FHIR_OPERATION_DEFINITION).map(cdef => {
          parseInfrastructureResource[OPDEF](FHIR_OPERATION_DEFINITION,cdef)
        })
    }
  }

  /**
    * Read and parse ValueSet definitions (either from the Config or Default folder path for ValueSet)
    * @return
    */
  protected def getValueSets(fromConfig:Boolean = false):Seq[VALSET] = {
    fromConfig match {
      //If reading ValueSets from configuration folder
      case true =>
        logger.info("ValueSets Path:" + OnfhirConfig.valueSetsPath)
        readInfrastructureResourcesInFolderOrZip[VALSET](OnfhirConfig.valueSetsPath, DEFAULT_RESOURCE_PATHS.VALUESETS_PATH, FHIR_VALUE_SET)
      case false=>
        logger.info(s"Reading $FHIR_VALUE_SET definitions from database ...")
        DBInitializer.getInrastructureResources(FHIR_VALUE_SET).map(cdef => {
          parseInfrastructureResource[VALSET](FHIR_VALUE_SET,cdef)
        })
    }
  }

  /**
    * Read and parse CodeSystem definitions
    * @param fromConfig
    * @return
    */
  protected def getCodeSystems(fromConfig:Boolean = false):Seq[CODESYSTEM] = {
    fromConfig match {
      //If reading ValueSets from configuration folder
      case true =>
        logger.info("CodeSystems Path:" + OnfhirConfig.codeSystemsPath)
        readInfrastructureResourcesInFolderOrZip[CODESYSTEM](OnfhirConfig.codeSystemsPath, DEFAULT_RESOURCE_PATHS.CODESYSTEMS_PATH, FHIR_CODE_SYSTEM)
      case false=>
        logger.info(s"Reading $FHIR_CODE_SYSTEM definitions from database ...")
        DBInitializer.getInrastructureResources(FHIR_CODE_SYSTEM).map(cdef => {
          parseInfrastructureResource[CODESYSTEM](FHIR_CODE_SYSTEM, cdef)
        })
    }
  }


  /**
    * Read a FHIR infrastructure Resource for configuration as scala.Source based on given path and default paths
    * @param resourcePath The file path to access to resource, if None the default path will be used
    * @param defaultPath Default path to acess to the resource
    * @param descriptionForLog Description of the resource to be used in the logs
    * @return
    */
  protected def readInfrastructureResource(resourcePath:Option[String], defaultPath:String,descriptionForLog:String):BufferedSource = {
    resourcePath match {
      case Some(path) =>
        logger.info(s"Reading $descriptionForLog from '$path'")
        Source.fromFile(path)
      case None =>
        logger.info(s"Reading $descriptionForLog from default path '$defaultPath'")
        Source.fromInputStream(getClass.getResourceAsStream(defaultPath))
    }
  }

  /**
    * Checks the file extension to decide whether the Resource definition is in XML or JSON format and parse it
    * @param reader Reader that will read the content of the resource
    * @param filePath File path to the resource
    * @tparam T Class that represents the object model for the resource
    * @return
    */
  protected def parseInfrastructureResource[T <: IBaseResource :ClassTag](rtype:String, reader:Reader, filePath:String): T = {
    //Retrieve the correspondinf parser according to the file path (json or xml)
    def ctag = implicitly[reflect.ClassTag[T]]
    val parser = getParser[T](rtype, filePath, ctag)
    //Parse the resource
    parser.parseResource(ctag.runtimeClass.asInstanceOf[Class[T]], reader)
  }

  /**
    * Return a corresponding parser (make an exception to CompartmentDefinition class)
    * @param filePath
    * @tparam T
    * @return
    */
  private def getParser[T<:IBaseResource](rtype:String, filePath:String, t:ClassTag[T]):IParser = {
    if(filePath.endsWith(".json")) getJsonParser(rtype) else getXmlParser(rtype)
  }

  /**
    * Parse the resource from the BSON format
    * @param resource The content of the resource in Map format
    * @tparam T Class that represents the object model for the resource
    * @return
    */
  protected def parseInfrastructureResource[T<: IBaseResource :ClassTag](rtype:String, resource:Resource): T = {
    val resourceReader = new StringReader(resource.toJson)
    getJsonParser(rtype)
      .parseResource(scala.reflect.classTag[T].runtimeClass.asInstanceOf[Class[T]], resourceReader)
  }

  /**
    * Read an infrastructure resource within a Zip file (e.g. within the FHIR validation package)
    * @param zipFilePath  The path to zip file
    * @param defaultZipFilePath The default path to zip file
    * @param resourceName Type of the resource (FHIR Resource Type)
    * @param descriptionForLog Description of the resource to be used in the logs
    * @return
    */
  protected def readInfrastructureResourceInZip(zipFilePath:Option[String], defaultZipFilePath:String, resourceName:String, descriptionForLog:String):Option[InputStream] = {
    val inputStream = zipFilePath match {
      case Some(path) =>
        logger.info(s"Reading $descriptionForLog from '$path'")
        new FileInputStream(new File(path))
      case None =>
        logger.info(s"Reading $descriptionForLog from default path '$defaultZipFilePath'")
        getClass.getResourceAsStream(defaultZipFilePath)
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
  private def findFileInZip(zipStream:ZipInputStream, fileName:String): Option[InputStream] = {
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
    * Read the resource definitions in a complete folder or within a zip file, all resources should be same type given as T e.g. StructureDefinition, CompartmentDefinition, etc
    * @param folderPath
    * @param defaultFolderPath
    * @param descriptionForLog
    * @tparam T
    * @return
    */
  protected def readInfrastructureResourcesInFolderOrZip[T <: IBaseResource :ClassTag](folderPath:Option[String], defaultFolderPath:String, rtype:String):Seq[T] = {
    folderPath match {
      case Some(path) =>
        logger.info(s"Reading  $rtype definitions from folder '$path'")
        val givenFile = new File(path)
        if(!givenFile.exists())
          throw new InitializationException(s"Folder not found in $path ...")

        if(path.endsWith(".zip")) {
          //Given as zip file
          val zipInputStream = new OnFhirZipInputStream(new FileInputStream(givenFile))
          val results = readInfrastructureResourcesInZip[T](rtype, zipInputStream)
          zipInputStream.forceClose()
          results
        } else {
          if(!givenFile.isDirectory)
            throw new InitializationException(s"Given path '$path' is not a folder or zip file ...")
          //Given as folder
          givenFile.listFiles().toSeq.map(file => {
            parseInfrastructureResource[T](rtype, new InputStreamReader(new BOMInputStream(new FileInputStream(file)), "UTF-8"), file.getPath)
          })
        }
      case None =>
        logger.info(s"Reading $rtype definitions from default folder '$defaultFolderPath'")
        val insp = getClass.getResourceAsStream(defaultFolderPath)
        if(insp!=null) {
          //Read the default files packaged as zip
          val zipInputStream = new OnFhirZipInputStream(insp)
          val results = readInfrastructureResourcesInZip[T](rtype, zipInputStream)
          zipInputStream.forceClose()
          results
        }else Nil
    }
  }

  /**
    * Read and parse all files in the given ZipInputStream,  all resources should be same type given a T
    * @param zipStream
    * @tparam T
    * @return
    */
  private def readInfrastructureResourcesInZip[T <: IBaseResource:ClassTag](rtype:String, zipStream:ZipInputStream):Seq[T] = {
    //val zipStream = new ZipInputStream(getClass.getResourceAsStream(zipPath))
    val resources:mutable.ListBuffer[T] = new mutable.ListBuffer[T]
    var zipEntry : ZipEntry = zipStream.getNextEntry

    while(zipEntry != null){
      val reader = new InputStreamReader(new BOMInputStream(zipStream), "UTF-8")
      resources.append(parseInfrastructureResource[T](rtype, reader, zipEntry.getName))
      zipStream.closeEntry()
      zipEntry = zipStream.getNextEntry
    }
    resources
  }

  /**
    * Encode the HAPI object to our raw Resource format
    * @param resource The object that represents the resource content
    * @tparam T Class that represents the object model for the resource
    * @return
    */
  def encodeResource[T <: IBaseResource](rtype:String, resource:T):Resource = {
    val resourceContent = getJsonParser(rtype).encodeResourceToString(resource.asInstanceOf[IBaseResource])
    //Prepare the resource for other operations (Default)
    resourceContent.parseJson
    //resourceMap = FHIRUtil.populateResourceWithMeta(resourceMap, None, 1L, DateTime.now )
    //FHIRUtil.populateResourceWithExtraFields(resourceMap, FHIR_METHOD_NAMES.METHOD_POST, StatusCodes.Created)
  }

  /*
  def serializeResourceToOriginalJson[T <: IBaseResource](resource:T, notBackwardCompatible:Boolean = false):String = {
      if(notBackwardCompatible && fhirContext.getVersion.getVersion.isOlderThan(FhirVersionEnum.DSTU3))
        FhirContext.forDstu3().newJsonParser().encodeResourceToString(resource.asInstanceOf[IBaseResource])
      else
        jsonParser.encodeResourceToString(resource.asInstanceOf[IBaseResource])
  }*/

  /**
    * Load class if possible
    * @param classPath Class path
    * @return
    */
  def loadOperationClass(classPath:String):Option[Class[_]] = {
    Try(this.getClass.getClassLoader.loadClass(classPath)) match {
      case Success(opClass) => Some(opClass)
      case Failure(e) => Try(ClassLoader.getSystemClassLoader.loadClass(classPath)).toOption
    }
  }

}

package io.onfhir.r4.validation

import java.util.concurrent.TimeUnit

import akka.http.caching.LfuCache
import akka.http.caching.scaladsl.{Cache, CachingSettings}
import ca.uhn.fhir.context.FhirContext
import io.onfhir.Onfhir
import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.FhirConfigurationManager.fhirConfig
import io.onfhir.db.ResourceManager
import io.onfhir.util.JsonFormatter._
import org.hl7.fhir.r4.hapi.ctx.DefaultProfileValidationSupport
import org.hl7.fhir.r4.model.{CodeSystem, StructureDefinition, ValueSet}
import org.mongodb.scala.model.Filters

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future}

class OnFhirValidationSupportR4 extends DefaultProfileValidationSupport {

  implicit val executionContext:ExecutionContext = Onfhir.actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")

  //Chache settings
  val defaultCachingSettings = CachingSettings(Onfhir.actorSystem)
  val cachingSettings =
    defaultCachingSettings
      .withLfuCacheSettings(
        defaultCachingSettings.lfuCacheSettings
          .withInitialCapacity(20)
          .withMaxCapacity(50)
          .withTimeToLive(new FiniteDuration(120, TimeUnit.SECONDS))
          .withTimeToIdle(new FiniteDuration(60, TimeUnit.SECONDS)))

  //Code system cache
  val codeSystemCache: Cache[String, CodeSystem] = LfuCache(cachingSettings)
  //Value set cache
  val valueSetCache: Cache[String, ValueSet] = LfuCache(cachingSettings)

  /**
    * Supported Resource and Type profiles managed by this FHIR server
     */
  var profiles:Map[String, StructureDefinition] = Map.empty

  /**
    *
    */
  private def loadAllStructureDefinitions(): Unit ={
    val profileUrls =
      fhirConfig.supportedProfiles.values.flatten.toSet ++
      fhirConfig.dataTypeProfiles

    val (_, sdefinitions) =
      Await.result(
        ResourceManager.queryResourcesDirectly("StructureDefinition", Some(constructUrlQuery(profileUrls)), profileUrls.size, excludeExtraFields = true),
        new FiniteDuration(60, TimeUnit.SECONDS))

    profiles =
      sdefinitions.map(sdef =>
        FHIRUtil.extractValue[String](sdef, "url") ->
          fhirConfig.jsonParser.parseResource[StructureDefinition](classOf[StructureDefinition],sdef.toJson)
      ).toMap
  }

  /**
    * Fetch all Profiles supported by this FHIR server (apart from FHIR base definitions)
    * @param context
    * @return
    */
  override def fetchAllStructureDefinitions(context:FhirContext):java.util.List[StructureDefinition] = {
    if(profiles.isEmpty){
      loadAllStructureDefinitions()
    }
    profiles.values.toList.asJava
  }

  /**
    * Fetch a specific structure definition
    * @param context
    * @param profileUrl
    * @return
    */
  override def fetchStructureDefinition(context:FhirContext, profileUrl: String): StructureDefinition = {
    if(profiles.isEmpty){
      loadAllStructureDefinitions()
    }
    profiles.getOrElse(profileUrl, null)
  }

  override def isCodeSystemSupported(context:FhirContext, csUrl: String): Boolean = {
    fhirConfig.supportedCodeSystems.contains(csUrl)
  }

  /**
    * Fetch a CodeSystem defined in our server
    * @param context
    * @param csUrl
    * @return
    */
  override def fetchCodeSystem(context:FhirContext, csUrl: String): CodeSystem = {
    if(fhirConfig.supportedCodeSystems.contains(csUrl)){
      Await.result(codeSystemCache.getOrLoad(csUrl, fetchCodeSystemFromDb), new FiniteDuration(60, TimeUnit.SECONDS))
    } else
      null
  }

  /**
    * Fetch a ValueSet defined in our server
    * @param context
    * @param vsUrl
    * @return
    */
  override def fetchValueSet(context:FhirContext, vsUrl: String): ValueSet = {
    if(fhirConfig.supportedValueSets.contains(vsUrl)){
      Await.result(valueSetCache.getOrLoad(vsUrl, fetchValueSetFromDb), new FiniteDuration(60, TimeUnit.SECONDS))
    } else
      null
  }

  /**
    * Fetch CodeSystem definition from Onfhir DB
    * @param csUrl
    * @return
    */
  private def fetchCodeSystemFromDb(csUrl: String):Future[CodeSystem] = {
    ResourceManager.queryResourcesDirectly("CodeSystem", Some(constructUrlQuery(Set(csUrl))), 1, excludeExtraFields = true) map {
      case (1, Seq(foundCs)) =>
        fhirConfig.jsonParser.parseResource[CodeSystem](classOf[CodeSystem],foundCs.toJson)
      case _ => null
    }
  }

  /**
    * Fetch CodeSystem definition from Onfhir DB
    * @param csUrl
    * @return
    */
  private def fetchValueSetFromDb(vsUrl: String):Future[ValueSet] = {
    ResourceManager.queryResourcesDirectly("ValueSet", Some(constructUrlQuery(Set(vsUrl))), 1, excludeExtraFields = true) map {
      case (1, Seq(foundVs)) =>
        fhirConfig.jsonParser.parseResource[ValueSet](classOf[ValueSet],foundVs.toJson)
      case _ => null
    }
  }

  private def constructUrlQuery(urls:Set[String]) = {
    //All FHIR structural resources has the "url" attribute
    Filters.in("url", urls.toSeq:_*)
  }

  /*
  override def validateCode(var1: FhirContext, var2: String, var3: String, var4: String): IValidationSupport.CodeValidationResult = {
    //super.validateCode(var1, var2, var3, var4)
    new IValidationSupport.CodeValidationResult(new ConceptDefinitionComponent().setCode(var3).setDisplay(var4))
  }


    override def expandValueSet(var1: FhirContext, var2: ValueSet.ConceptSetComponent): ValueSetExpander.ValueSetExpansionOutcome = {
      //super.expandValueSet(var1, var2)
      null
    }

    override def fetchResource[T <: IBaseResource](var1: FhirContext, var2: Class[T], var3: String): T = {
      null.asInstanceOf[T]
    }
  */


}

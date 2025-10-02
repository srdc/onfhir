package io.onfhir.authz

import io.onfhir.api.Resource
import io.onfhir.api.model.FHIRRequest
import io.onfhir.api.util.IOUtil
import io.onfhir.exception.InitializationException
import io.onfhir.path.FhirPathEvaluator
import io.onfhir.util.JsonFormatter.formats
import org.apache.commons.io.input.BOMInputStream
import org.json4s.jackson.JsonMethods
import org.json4s.{JArray, JNull, JObject, JString, JValue}

import java.io.{FileInputStream, InputStreamReader}
import scala.util.Try

abstract class BaseAuthorizer(furtherConstraintsPath:Option[String]) extends IAuthorizer {
  /**
   * Parsed authorization constraints
   */
  protected val authzConstraintRules:Seq[FhirAuthzConstraintRule] =
    furtherConstraintsPath
      .map(path =>
        Try(readConstraintsFile(path))
          .getOrElse(throw new InitializationException(s"Cannot read onfhir authorization constraint rules file given in path $path!"))
      )
      .getOrElse(Nil)

  private def readConstraintsFile(path:String):Seq[FhirAuthzConstraintRule] = {
    val reader =
      if(path.startsWith("classpath:"))
        new InputStreamReader(BOMInputStream.builder.setInputStream(getClass.getClassLoader.getResourceAsStream(path.stripPrefix("classpath:"))).get())
      else
        new InputStreamReader(BOMInputStream.builder.setInputStream(new FileInputStream(path)).get())

    JsonMethods.parse(reader).extract[Seq[FhirAuthzConstraintRule]]
  }

  /**
   *
   * @param rtype
   * @param fhirInteraction
   * @param resource
   * @param context
   * @return
   */
  protected def getRelatedRules(rtype:String, fhirInteraction:String, resource:Option[Resource], context: Map[String, JValue]):Seq[FhirAuthzConstraintRule] = {
    authzConstraintRules
      .filter(_.resourceType.contains(rtype))
      .filter(_.fhirAction.contains(fhirInteraction))
      .filter(r => r.precondition.forall( pc =>
        FhirPathEvaluator.apply(context).satisfies(pc, resource.getOrElse(JNull))
      ))
  }

}

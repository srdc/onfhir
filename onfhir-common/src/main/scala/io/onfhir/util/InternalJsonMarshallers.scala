package io.onfhir.util

import java.time.Instant

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{ContentTypeRange, ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshaller}
import io.onfhir.api.model.{FhirSubscription, FhirSubscriptionChannel, InternalEntity, Parameter}
import io.onfhir.config.SearchParameterConf
import io.onfhir.event.{FhirNamedEvent, FhirTimeEvent, ResourceAccessed, ResourceCreated, ResourceDeleted, ResourceUpdated}
import org.json4s
import org.json4s.{CustomSerializer, Formats, JString, ShortTypeHints}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
/**
 * JSON Parser and marshallers for some internal models shared between onFhir.io modules
 */
object InternalJsonMarshallers {
  /** A `CustomSerializer` for FHIR instant or dateTime. */
  class FhirDateTimeSerializer extends CustomSerializer[Instant](_ => (
    {
      case JString(s) => DateTimeUtil.parseFhirDateTimeOrInstant(s)
    },
    {
      case t: Instant => JString(DateTimeUtil.serializeInstant(t))
    }
  ))


  implicit lazy val formats: Formats =
    Serialization.formats(ShortTypeHints.apply(List(
      classOf[Parameter],
      classOf[SearchParameterConf],
      classOf[FhirSubscription],
      classOf[FhirSubscriptionChannel],
      classOf[ResourceCreated],
      classOf[ResourceUpdated],
      classOf[ResourceDeleted],
      classOf[ResourceAccessed],
      classOf[FhirNamedEvent],
      classOf[FhirTimeEvent]
    ))) + new FhirDateTimeSerializer

  def parseAndExtract[T](content:String)(implicit manifest:Manifest[T]):T = {
    parse(content).extract[T]
  }

  implicit val internalEntityUnmarshaller:Unmarshaller[HttpEntity, Seq[InternalEntity]] =
    Unmarshaller
      .stringUnmarshaller
      .forContentTypes(ContentTypeRange.apply(ContentTypes.`application/json`))
      .mapWithInput {
        case (entity: HttpEntity, data: String) =>
          if (entity.isKnownEmpty() || data.isEmpty)
            null
          else
            parseAndExtract[Seq[InternalEntity]](data)
      }


  /**
   * Unmarshaller for full FHIR Response
   */
  implicit val internalEntityMarshaller:ToResponseMarshaller[Seq[InternalEntity]] =
    Marshaller.withFixedContentType[Seq[InternalEntity], HttpResponse](ContentTypes.`application/json`)(marshalInternalEntitySeq)


  def marshalInternalEntitySeq(entities:Seq[InternalEntity]):HttpResponse = {
    HttpResponse.apply(StatusCodes.OK).withEntity(HttpEntity(ContentTypes.`application/json`, serializeToJson(entities)))
  }

  def serializeToJson(internalEntities: Seq[InternalEntity]):String = {
    Serialization.write(internalEntities)
  }

  def serializeToJson(internalEntity: InternalEntity):String = {
    Serialization.write(internalEntity)
  }



}

package io.onfhir.subscription.channel

import akka.NotUsed
import akka.actor.ActorLogging
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.{Merge, Source}
import io.onfhir.subscription.FhirNotificationHandler.{GetFhirNotificationStream, GetFhirNotificationStreamResponse}
import akka.http.scaladsl.model.ws.TextMessage
import akka.stream.SourceRef
import akka.stream.typed.scaladsl.ActorSink
import akka.util.Timeout
import io.onfhir.subscription.FhirNotificationHandler
import io.onfhir.subscription.model.{CborSerializable, FhirNotification}
import io.onfhir.subscription.config.SubscriptionConfig

import scala.util.{Failure, Success}

object WebSocketHandler {
  sealed trait Command extends CborSerializable

  case class GetWebSocketHandler(sid:String, replyTo:ActorRef[Source[TextMessage, Any]]) extends Command

  case class AdaptedGetFhirNotificationStreamResponse(sid:String, notificationStream:SourceRef[FhirNotification], replyTo:ActorRef[Source[TextMessage, Any]]) extends Command
  case class BindingProblem(sid:String, ex:Throwable, replyTo:ActorRef[Source[TextMessage, Any]]) extends Command

  case class StreamCompleted(ex:Option[Throwable]) extends Command
  case class StreamData(n:FhirNotification) extends Command


  def apply( subscriptionConfig: SubscriptionConfig,shardRegionNotificationHandler: ActorRef[FhirNotificationHandler.Command]):Behavior[Command] = {
    Behaviors.setup { ctx =>
      implicit val responseTimeout: Timeout = subscriptionConfig.processorAskTimeout
      implicit val materializer = ctx.system
      Behaviors.receiveMessage[Command] {
        //Request for web socket notification stream
        case GetWebSocketHandler(sid, replyTo) =>
          ctx.ask[FhirNotificationHandler.Command, FhirNotificationHandler.Response](shardRegionNotificationHandler, r => GetFhirNotificationStream(sid, r)) {
            case Success(resp) => AdaptedGetFhirNotificationStreamResponse(sid, resp.asInstanceOf[GetFhirNotificationStreamResponse].notificationStream, replyTo)
            case Failure(ex) => BindingProblem(sid, ex, replyTo)
          }
          Behaviors.same

        //Received the notification stream successfully, now constructing the source
        case AdaptedGetFhirNotificationStreamResponse(sid, notificationStream, replyTo) =>
          ctx.log.debug(s"A client bound to the subscription $sid over web socket")

          //This is initial response as described in FHIR Subscription
          val initialResponse:Source[TextMessage, NotUsed] = Source.fromIterator(() => Seq(TextMessage(s"bound $sid")).iterator)
          val notificationPingStream:Source[TextMessage, NotUsed] =
            notificationStream.source.map(n=> TextMessage(s"ping ${n.sid}"))

          //Merge the source
          val mergedSource:Source[TextMessage, NotUsed] =
            Source.combine(
              initialResponse,
              notificationPingStream
            )(Merge(_))


          replyTo ! mergedSource
          //replyTo !  Source.empty[TextMessage]
          Behaviors.same

        case BindingProblem(sid, ex, replyTo) =>
          ctx.log.error(s"Problem while web socket binding to subscription $sid", ex)
          val errorReplySource = Source.fromIterator(() => Seq(TextMessage(s"error $sid 'Cannot bind to the given notification due to internal error!'")).iterator)
          replyTo ! errorReplySource
          Behaviors.same


        case StreamCompleted(None) =>
          ctx.log.debug("Stream completed!")
          Behaviors.same
        case StreamCompleted(Some(ex)) =>
          ctx.log.error("Stream completed with error!!!!", ex)
          Behaviors.same
        case StreamData(n) =>
          ctx.log.debug(s"DATA: ${n.sid}")
          Behaviors.same
      }
    }
  }

}

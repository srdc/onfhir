package io.onfhir.subscription.channel

import java.nio.charset.StandardCharsets

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpHeader.ParsingResult.{Error, Ok}
import akka.http.scaladsl.model._
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import io.onfhir.subscription.FhirNotificationHandler.NotificationResult
import org.slf4j.Logger

import scala.util.{Failure, Success, Try}

object RestChannelManager extends IPushBasedChannelManager {
  case object StreamCompleted extends Command
  case class StreamFailure(ex:Throwable) extends Command

  case object ActorClosed extends Command
  case class ActorFailure(ex:Throwable) extends Command

  case class RestNotificationResult(result:Boolean, notificationCommand:SendNotification) extends Command

  def constructActorSink(actor:ActorRef[Command]):Sink[(Try[HttpResponse],SendNotification), NotUsed] = {
    val httpResponseToNotificationResult =
      Flow[(Try[HttpResponse], SendNotification)]
      .map(r => r._1 match {
        case Success(httpResponse) => RestNotificationResult(httpResponse.status.isSuccess(), r._2)
        case Failure(exception) =>
          //TODO log the failure
          RestNotificationResult(false, r._2)
      })

    val actorSink =
      ActorSink
      .actorRef[Command](actor, onCompleteMessage = StreamCompleted, onFailureMessage = (t:Throwable) => StreamFailure(t))

    httpResponseToNotificationResult.to(actorSink)
  }

  def constructActorSource():Source[(HttpRequest, SendNotification), ActorRef[Command]] = {
    ActorSource
      .actorRef[Command](
        completionMatcher = {
          case ActorClosed => Unit
        },
        failureMatcher = {
          case ActorFailure(ex) => ex
        },
        bufferSize = 200,
        overflowStrategy = OverflowStrategy.dropNew
      )
      .collect {
        case s:SendNotification =>
          val httpHeaders: Seq[HttpHeader] = s.fhirNotification.headers.flatMap (h => {
            val headerParts = h.split (':').map (_.trim)
            HttpHeader.parse (headerParts.head, headerParts.last) match {
              case Ok (header, errors) => Seq (header)
              case Error (e) => Nil
            }
          })

          var httpRequest =
            HttpRequest (method = HttpMethods.POST)
              .withUri (s.fhirNotification.endpoint.get)
              .withHeaders (httpHeaders: _*)

          if (s.fhirNotification.payload.nonEmpty)
            httpRequest = httpRequest.withEntity (ContentTypes.`application/json`, s.fhirNotification.payload.get.getBytes (StandardCharsets.UTF_8.name () ) )

          httpRequest -> s
      }
  }


  override def apply(implicit actorSystem:ActorSystem[_]): Behavior[Command] = {
    //Create the super http client connection pool
    val restConnectionFlow = Http().superPool[SendNotification]()

    Behaviors.setup { ctx =>
      val restChannelSink = constructActorSink(ctx.self)
      val restChannelSource = constructActorSource()
      //Run it with actor sink and source
      val (sourceActorRef, _) = restConnectionFlow.runWith(restChannelSource, restChannelSink)

      running(ctx.log, sourceActorRef)
    }
  }

  def running(log:Logger, sourceActorRef:ActorRef[Command]):Behavior[Command] = {
      Behaviors.receiveMessage[Command] {
        //Nothing to do automatically goes to stream
        case s:SendNotification =>
          sourceActorRef.tell(s)
          Behaviors.same
        //Delegate result handling back to notification handler
        case RestNotificationResult(result, sn) =>
          sn.replyTo.tell(NotificationResult(sn.fhirNotification.sid, result, sn.fhirNotification.latestStatus))
          Behaviors.same

        case StreamCompleted =>
          log.warn("HTTP super pool stream completed!")
          sourceActorRef.tell(ActorClosed)
          Behaviors.same

        case StreamFailure(t) =>
          log.error("HTTP super pool stream failure", t)
          sourceActorRef.tell(ActorClosed)
          Behaviors.same
      }
  }
}

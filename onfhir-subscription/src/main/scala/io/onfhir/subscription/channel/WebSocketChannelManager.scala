package io.onfhir.subscription.channel

import akka.{Done, NotUsed}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.javadsl.model.AttributeKeys
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, Uri}
import akka.stream.scaladsl.{Flow, Keep, Merge, RunnableGraph, Sink, Source}
import io.onfhir.subscription.config.SubscriptionConfig
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.javadsl.model.ws.BinaryMessage
import akka.util.ByteString
import io.onfhir.subscription.cache.{ActivateSubscription, DeactivateSubscription, Response, UpdateSubscriptionResponse}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.SourceRef
import io.onfhir.subscription.FhirNotificationHandler
import io.onfhir.subscription.FhirNotificationHandler.GetFhirNotificationStream
import io.onfhir.subscription.model.FhirNotification

import scala.concurrent.Future

class WebSocketChannelManager(
                               subscriptionConfig: SubscriptionConfig,
                               subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
                               shardRegionNotificationHandler: ActorRef[FhirNotificationHandler.Command]
                             )(implicit actorSystem:ActorSystem[_]) {
  implicit val executionContext = actorSystem.executionContext

  /**
   * Web socket subscriptio handler
   * @return
   */
  def fhirWebSocketSubscriptionHandler: Flow[Message, Message, Any] =
      Flow[Message].flatMapMerge(10, {
        case t:TextMessage if t.getStrictText.matches("bind \\w*$") =>
          val parts = t.getStrictText.split(' ')
          //Subscription id
          val sid = parts.last

          //Activate the subscription
          val subsActivated = subscriptionCache.ask[Response](replyTo => ActivateSubscription(sid, replyTo))(subscriptionConfig.processorAskTimeout, actorSystem.scheduler)

          val fSource:Future[Source[TextMessage, _]] =
            subsActivated.map {
              case UpdateSubscriptionResponse(true) =>
                val initialResponse:Source[TextMessage, _] = Source.fromIterator(() => Seq(TextMessage(s"bound $sid")).iterator)

                val notificationSource:Source[TextMessage, _] =
                  Source.future(
                    shardRegionNotificationHandler
                      .ask[SourceRef[FhirNotification]](replyTo => GetFhirNotificationStream(sid, replyTo))(subscriptionConfig.processorAskTimeout, actorSystem.scheduler)
                  ).map(fn => TextMessage(s"ping $fn"))

                val resultSource:Source[TextMessage, _] =
                  Source.combine(
                    initialResponse,
                    notificationSource
                  )(Merge(_))

                resultSource

              case UpdateSubscriptionResponse(false) =>
                val errorMsg = "'Given subscription not found or does not have a web socket channel'"

                Source.fromIterator(() => Seq(TextMessage(s"error $sid $errorMsg")).iterator)
            }

          t.getStreamedText.runWith(Sink.ignore, actorSystem)

          Source.futureSource(fSource)

        case bm: BinaryMessage =>
          // ignore binary messages but drain content to avoid the stream being clogged
          bm.getStreamedData.runWith(Sink.ignore, actorSystem)
          Source.empty
      })


  /**
   * Web socket route
   */
  val websocketRoute =
    path("wss") {
      handleWebSocketMessages(fhirWebSocketSubscriptionHandler)
    }

}

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
import io.onfhir.subscription.cache.{ActivateSubscription, DeactivateSubscription, GetSubscription, GetSubscriptionResponse, Response, UpdateSubscriptionResponse}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.SourceRef
import io.onfhir.api.SubscriptionChannelTypes
import io.onfhir.subscription.FhirNotificationHandler
import io.onfhir.subscription.FhirNotificationHandler.GetFhirNotificationStream
import io.onfhir.subscription.model.FhirNotification
import org.slf4j.LoggerFactory

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Success, Try}

class WebSocketChannelManager(
                               subscriptionConfig: SubscriptionConfig,
                               subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
                               shardRegionNotificationHandler: ActorRef[FhirNotificationHandler.Command]
                             )(implicit actorSystem:ActorSystem[_]) {
  implicit val executionContext = actorSystem.executionContext

  lazy val log = LoggerFactory.getLogger(classOf[WebSocketChannelManager])
  /**
   * Web socket subscriptio handler
   * @return
   */
  def fhirWebSocketSubscriptionHandler: Flow[Message, Message, Any] =
      Flow[Message].flatMapMerge(10, {
        case t:TextMessage if t.getStrictText.matches("bind [^\\s]+") =>
          val parts = t.getStrictText.split(' ')
          //Subscription id
          val sid = parts.last

          //Activate the subscription
          val subscriptionFound = subscriptionCache.ask[Response](replyTo => GetSubscription(sid, replyTo))(subscriptionConfig.processorAskTimeout, actorSystem.scheduler)

          val fSource:Future[Source[TextMessage, NotUsed]] =
            subscriptionFound.map {
              case GetSubscriptionResponse(Some(s)) if s.channel.channelType == SubscriptionChannelTypes.WebSocket =>
                //This is initial response as described in FHIR Subscription
                val initialResponse:Source[TextMessage, NotUsed] = Source.fromIterator(() => Seq(TextMessage(s"bound $sid")).iterator)
                //Get the notification source from notification handler
                val futureSource:Future[Source[TextMessage, NotUsed]] = shardRegionNotificationHandler
                      .ask[SourceRef[FhirNotification]](replyTo => GetFhirNotificationStream(sid, replyTo))(subscriptionConfig.processorAskTimeout, actorSystem.scheduler)
                      .map(notificationSource => notificationSource.source.map(n => TextMessage(s"ping ${n.sid}")))
                //Merge the source
                val mergedSource:Future[Source[TextMessage, NotUsed]] = futureSource.map(notificationSource => {
                     Source.combine(
                            initialResponse,
                            notificationSource
                     )(Merge(_))
                 })
                //TODO find a different way without waiting
                //Wait the future
                Try(Await.result(mergedSource, 60 seconds)).recover {
                  case e:Throwable =>
                    log.error(s"Problem while web socket binding to subscription $sid", e)
                    Source.fromIterator(() => Seq(TextMessage(s"error $sid 'Cannot bind to the given notification due to internal error!'")).iterator)
                }.toOption.get
              case _ =>
                val errorMsg = "'Given subscription not found or does not have a web socket channel'"

                Source.fromIterator(() => Seq(TextMessage(s"error $sid $errorMsg")).iterator)
            }

          t.getStreamedText.runWith(Sink.ignore, actorSystem)

          Source.futureSource[TextMessage, NotUsed](fSource)

        case bm: BinaryMessage =>
          // ignore binary messages but drain content to avoid the stream being clogged
          bm.getStreamedData.runWith(Sink.ignore, actorSystem)
          Source.empty
      })


  /**
   * Web socket route
   */
  val websocketRoute =
    path("fhir-wss") {
      handleWebSocketMessages(fhirWebSocketSubscriptionHandler)
    }

}

package io.onfhir.subscription.channel


import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.{Flow, Sink, Source}
import io.onfhir.subscription.config.SubscriptionConfig
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.javadsl.model.ws.BinaryMessage
import io.onfhir.subscription.cache.{GetSubscription, GetSubscriptionResponse, Response, UpdateSubscriptionResponse}
import akka.http.scaladsl.server.Directives._
import io.onfhir.api.SubscriptionChannelTypes
import io.onfhir.subscription.channel.WebSocketHandler.GetWebSocketHandler
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

class WebSocketChannelManager(
                               subscriptionConfig: SubscriptionConfig,
                               subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command],
                               webSocketHandler:ActorRef[WebSocketHandler.Command]
                               //shardRegionNotificationHandler: ActorRef[FhirNotificationHandler.Command]
                             )(implicit actorSystem:ActorSystem[_]) {
  implicit val executionContext = actorSystem.executionContext

  lazy val log = LoggerFactory.getLogger(classOf[WebSocketChannelManager])


  /**
   * Web socket subscriptio handler
   * @return
   */
  def fhirWebSocketSubscriptionHandler(): Flow[Message, Message, Any] =
      Flow[Message].flatMapMerge(subscriptionConfig.maxNumberOfWebSocketBindingsPerNode, {
        case t:TextMessage if t.getStrictText.matches("bind [^\\s]+") =>
          val parts = t.getStrictText.split(' ')
          //Subscription id
          val sid = parts.last

          //Activate the subscription
          val subscriptionFound = subscriptionCache.ask[Response](replyTo => GetSubscription(sid, replyTo))(subscriptionConfig.processorAskTimeout, actorSystem.scheduler)

          val fSource:Future[Source[TextMessage, Any]] =
            subscriptionFound.flatMap {
              case GetSubscriptionResponse(Some(s)) if s.channel.channelType == SubscriptionChannelTypes.WebSocket =>
                webSocketHandler
                  .ask[Source[TextMessage, Any]](replyTo => GetWebSocketHandler(sid, replyTo))(subscriptionConfig.processorAskTimeout, actorSystem.scheduler)
            case _ =>
                val errorMsg = "'Given subscription not found or does not have a web socket channel'"
                Future.apply(Source.fromIterator(() => Seq(TextMessage(s"error $sid $errorMsg")).iterator))
          }

          t.getStreamedText.runWith(Sink.ignore, actorSystem)

          val rsource = Await.result(fSource, 60 seconds)

          rsource
            .watchTermination(){(mt, done) =>
              done.onComplete {
                case Success(_) =>
                  log.debug(s"Web socket binding for subscription $sid completed successfully...")
                case Failure(ex) =>
                  log.warn(s"Web socket handling for subscription $sid completed with failure!",ex)
              }
              mt
            }.wireTap(t => log.debug(s"Web socket message sent: ${t.getStrictText}"))


          //Source.futureSource(fSource)
          //return the source
          //Source.futureSource[TextMessage, Any](fSource)
        case bm: BinaryMessage =>
          // ignore binary messages but drain content to avoid the stream being clogged
          bm.getStreamedData.runWith(Sink.ignore, actorSystem)
          Source.empty
      })


  /**
   * Web socket route
   */
  val websocketRoute = {
    path("fhir-wss") {
      log.debug("A client connected to web socket endpoint...")
      val websocketHandling = fhirWebSocketSubscriptionHandler().watchTermination() { (_, done) =>
        done.onComplete {
          case Success(_) =>
            log.debug("Web socket flow completed successfully")

          case Failure(ex) =>
            log.error(s"Web socket flow completed with failure : $ex")
        }
      }
      handleWebSocketMessages(websocketHandling)
    }
  }

}

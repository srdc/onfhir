package io.onfhir.subscription

import akka.actor.typed.ActorSystem
import akka.stream.{OverflowStrategy, QueueOfferResult, SourceRef}
import akka.stream.scaladsl.{BroadcastHub, Keep, Sink, Source, StreamRefs}
import com.typesafe.sslconfig.util.LoggerFactory
import io.onfhir.subscription.model.FhirNotification
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Keep a broadcast hub for pull based notifications
 * @param actorSystem
 */
class PullBasedNotificationHandler(implicit val actorSystem:ActorSystem[_]) {
  implicit val executionContext = actorSystem.executionContext

  val log = LoggerFactory.getLogger(classOf[PullBasedNotificationHandler])

  val (pullBasedNotificationQueue, notificationPublisher) =
    Source
      .queue[FhirNotification](256, OverflowStrategy.dropHead)
      .toMat(BroadcastHub.sink(bufferSize = 256))(Keep.both)
      .run()

  notificationPublisher.watchTermination()((_ ,done) => done.onComplete {
    case Success(_) => log.debug("Pull based notification queue completed...")
    case Failure(ex) => log.warn("Pull based notification queue completed with error!", ex)
  })

  //Consume the events when there is no subscriber
  notificationPublisher.runWith(Sink.ignore)
  //notificationPublisher.runWith(Sink.foreach(n => println(s"Subscription: ${n.sid}!!!")))
  //notificationPublisher.filter(_.sid == "0a011a9e-d9b9-4813-b9ef-7a871f1acb21").runWith(Sink.foreach(n => println(s"Subscription Local: ${n.sid}!!!")))

  /**
   *
   * @param sid
   * @return
   */
  def getNotificationSourceRefForSubscription(sid:String):SourceRef[FhirNotification] = {
    /*notificationPublisher
      .filter(_.sid == sid).runWith(Sink.foreach(n => println(s"Local: ${n.sid}!!!")))

    val dedicatedSource =
      notificationPublisher
        .filter(_.sid == sid) //Get the stream for the specific subscription
        .preMaterialize()._2*/

    val streamRef:SourceRef[FhirNotification] =
      notificationPublisher
        .filter(_.sid == sid)
        .runWith(StreamRefs.sourceRef())

    streamRef
  }

  def newNotification(fhirNotification: FhirNotification):Future[QueueOfferResult] = {
    pullBasedNotificationQueue.offer(fhirNotification)
  }

}

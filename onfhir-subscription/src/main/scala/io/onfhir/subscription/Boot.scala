package io.onfhir.subscription

import akka.Done

import scala.concurrent.{Future, Promise, blocking}
import scala.io.StdIn
import scala.util.Try

object Boot {

  def main(args: Array[String]): Unit = {
    //Get web socket port
    val webSocketPort = Try(args.apply(0).toInt).toOption
    val actorSystem = Guardian.apply(webSocketPort)
    implicit val executionContext =  actorSystem.executionContext
    val promise = Promise[Done]()
    sys.addShutdownHook {
      promise.trySuccess(Done)
    }
    Future {
      blocking {
        if (StdIn.readLine("Write 'quit' to stop the onfhir-subscription server...\n").equalsIgnoreCase("quit"))
          promise.trySuccess(Done)
      }
    }

    promise.future.onComplete(_ => actorSystem.terminate())
  }

}

package io.onfhir.subscription

import akka.Done

import scala.concurrent.{Future, Promise, blocking}
import scala.io.StdIn

object SampleFhirSubscriptionNode {
  /**
   * Start a FHIR Subscription node
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val nodeIndice = args.apply(0).toInt

    val actorSystem = Guardian.apply(Some(8081 + (nodeIndice -1 )))
    implicit val executionContext =  actorSystem.executionContext
    val promise = Promise[Done]()
    sys.addShutdownHook {
      promise.trySuccess(Done)
    }
    Future {
      blocking {
        if (StdIn.readLine(s"Write 'quit' to stop the onfhir-subscription node $nodeIndice ...\n").equalsIgnoreCase("quit"))
          promise.trySuccess(Done)
      }
    }

    promise.future.onComplete(_ => actorSystem.terminate())
  }

}

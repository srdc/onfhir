package io.onfhir.subscription.channel

import akka.{Done, actor}
import akka.actor.CoordinatedShutdown
import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration._

/**
 * HTTP server for OnFhir subscription web socket
 */
object WebSocketHttpServer {

  /**
   * Logic to bind the given routes to a HTTP port and add some logging around it
   */
  def start(routes: Route, port: Int, system: ActorSystem[_]): Future[Http.ServerBinding] = {
    import akka.actor.typed.scaladsl.adapter._
    implicit val classicSystem: actor.ActorSystem = system.toClassic
    implicit  val exexutionContext: ExecutionContextExecutor = system.executionContext
    val shutdown = CoordinatedShutdown(classicSystem)

    val binding =
      Http()
      .newServerAt("0.0.0.0", port)
      .bind(routes)


    binding.onComplete {
        case Success(binding) =>
          val address = binding.localAddress
          system.log.info(
            "OnFhir Subscription Web Socket Server is online at http://{}:{}/",
            address.getHostString,
            address.getPort
          )

          shutdown.addTask(
            CoordinatedShutdown.PhaseServiceRequestsDone,
            "http-graceful-terminate"
          ) { () =>
            binding.terminate(10.seconds).map { _ =>
              system.log.info(
                "OnFhir Subscription Web Socket Server http://{}:{}/ graceful shutdown completed",
                address.getHostString,
                address.getPort
              )
              Done
            }
          }
        case Failure(ex) => //handled at guardian


      }

    binding
  }



}

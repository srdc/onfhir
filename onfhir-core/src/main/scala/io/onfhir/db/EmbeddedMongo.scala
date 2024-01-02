package io.onfhir.db


import de.flapdoodle.embed.mongo.config.Net
import de.flapdoodle.embed.mongo.distribution.Version
import de.flapdoodle.embed.mongo.transitions.{Mongod, RunningMongodProcess}
import de.flapdoodle.embed.mongo.types.DatabaseDir
import de.flapdoodle.reverse.TransitionWalker
import de.flapdoodle.reverse.transitions.Start
import org.slf4j.LoggerFactory

import java.nio.file.Paths

/**
 * Embedded MongoDB Server
 */
object EmbeddedMongo {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val FOLDER_EXT: String = ".emb.mongo"

  private var mongodProcess: TransitionWalker.ReachedState[RunningMongodProcess] = _

  def start(appName: String, host: String, port: Int, withTemporaryDatabaseDir: Boolean): Unit = {

    logger.info("Starting an Embedded Mongo instance on {}:{}", host, port)

    var mongodBuilder = Mongod.builder()
      .net(Start.to(classOf[Net]).initializedWith(Net.of(host, port, de.flapdoodle.net.Net.localhostIsIPv6())))
    if(!withTemporaryDatabaseDir) {
      mongodBuilder = mongodBuilder.databaseDir(Start.to(classOf[DatabaseDir]).initializedWith(DatabaseDir.of(Paths.get(s"./${appName.filterNot(_.isWhitespace)}$FOLDER_EXT"))))
    }

    val mongod = mongodBuilder.build()

    val version = Version.Main.V4_4
    mongodProcess = mongod.start(version)
    logger.info("Embedded Mongo is listening at {}", mongodProcess.current().getServerAddress.toString)
  }

  def stop(): Unit = {
    mongodProcess.close()
  }

}

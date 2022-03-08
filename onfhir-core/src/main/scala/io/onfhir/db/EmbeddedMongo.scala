package io.onfhir.db


import de.flapdoodle.embed.mongo.{MongodExecutable, MongodStarter}
import de.flapdoodle.embed.mongo.config.{MongodConfig, Net, Storage}
import de.flapdoodle.embed.mongo.distribution.Version
import de.flapdoodle.embed.process.runtime.Network
import org.slf4j.LoggerFactory

/**
 * Embedded MongoDB Server
 */
object EmbeddedMongo {

  private val logger  = LoggerFactory.getLogger("EmbeddedMongo")

  val FOLDER_EXT: String = ".emb.mongo"

  private var mongodExecutable: MongodExecutable = _

  def start(appName:String, host: String, port: Int): Unit = {

    logger.info("Starting an Embedded Mongo instance on {}:{}", host, port)

    val conf: MongodConfig =
      MongodConfig
        .builder()
        .version(Version.Main.PRODUCTION)
        .replication(new Storage(s"./${appName.filterNot(_.isWhitespace)}${FOLDER_EXT}",null,0))
        .net(new Net(host, port, Network.localhostIsIPv6()))
        .build()

    mongodExecutable = MongodStarter.getDefaultInstance.prepare(conf)

    mongodExecutable.start()
    logger.info("Embedded Mongo is listening on {}:{}", host, port)
  }

  def stop(): Unit = {
    mongodExecutable.stop()
  }

}

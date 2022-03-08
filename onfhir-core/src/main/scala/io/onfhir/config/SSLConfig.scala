package io.onfhir.config

import java.io.{File, FileInputStream}
import java.security.{KeyStore, SecureRandom}

import akka.http.scaladsl.{ConnectionContext, HttpsConnectionContext}
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import io.onfhir.api.DEFAULT_ROOT_FOLDER

trait SSLConfig {
  /**
    * Defaults
    */
  final val DEFAULT_KEYSTORE_PATH :String = s"$DEFAULT_ROOT_FOLDER/keystore.jks"
  final val DEFAULT_KEYSTORE_PASSWORD = "fhir-repository"
  final val DEFAULT_PROTECTED_RESOURCE_METADATA_PATH :String = s"$DEFAULT_ROOT_FOLDER /protected-resource-server-metadata.json"
  final val DEFAULT_PROTECTED_RESOURCE_JWKS_FILE_NAME = "fhir-server.jwks"
  final val DEFAULT_PROTECTED_RESOURCE_DYNAMIC_REGISTRATION_METADATA_FILE_NAME = "protected-resource-server-metadata-dynamic.json"
  /**
    * Create https and ssl context
    * @return
    */
  lazy val https: HttpsConnectionContext = {
    val keyStore = getKeystore()
    val password =  OnfhirConfig.sslKeystorePasword.getOrElse(DEFAULT_KEYSTORE_PASSWORD)

    val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(keyStore, password.toCharArray)

    val trustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    trustManagerFactory.init(keyStore)

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, new SecureRandom)

    val https: HttpsConnectionContext = ConnectionContext.httpsClient(sslContext)
    https
  }

  /**
    * Load key store and return
    * @return
    */
  def getKeystore():KeyStore = {
    val ksInputStream = OnfhirConfig.sslKeystorePath match {
      case None => getClass.getResourceAsStream(DEFAULT_KEYSTORE_PATH)
      case Some(path) =>  new FileInputStream(new File(path))
    }
    val keyStore = KeyStore.getInstance("jks")
    keyStore.load(ksInputStream, OnfhirConfig.sslKeystorePasword.getOrElse(DEFAULT_KEYSTORE_PASSWORD).toCharArray)
    keyStore
  }

}

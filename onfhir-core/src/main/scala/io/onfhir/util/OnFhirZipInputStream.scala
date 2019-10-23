package io.onfhir.util

import java.io.InputStream
import java.util.zip.ZipInputStream

/***
  * Related with bug JDK-6539065 : XMLStreamReader close the underlaying stream
  * In order to prevent the closing of stream within ZipInputStream we have extended this and override close to do nothing
  * @param is
  */
class OnFhirZipInputStream(is:InputStream) extends ZipInputStream(is) {

  override def close(): Unit ={
    //Nothing
  }

  def forceClose() = super.close()

}

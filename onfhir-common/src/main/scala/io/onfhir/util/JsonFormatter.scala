package io.onfhir.util

import io.onfhir.api.Resource
import java.io

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization


import scala.language.implicitConversions

object JsonFormatter{
  implicit lazy val formats: Formats = Serialization.formats(NoTypeHints)


  class JsonParsable2(reader:io.Reader){
    def parseJson: Resource = {
      parse(reader).asInstanceOf[JObject]
    }
  }

  /**
    * Scala class that adds "parseJson" method to Strings
    */
  class JsonParsable(json:String) {
    def parseJson: Resource = {
      parse(json).asInstanceOf[JObject]
    }
  }

  /**
    * Scala class that adds "toJson" method for LinkedHashMap's
    */
  class JsonConvertable(resource:Resource) {
    def toJson: String = {
      Serialization.write(resource)
    }

    def toPrettyJson:String = {
      Serialization.writePretty(resource)
    }
  }

  /**
    * Implicit conversion that ties the new JsonParsable class to the Scala Strings
    */
  implicit def parseFromJson(string: String):JsonParsable = new JsonParsable(string)

  /**
    * Implicit conversion that ties the new JsonConvertable class to Scala LinkedHashMaps
    */
  implicit def convertToJson(resource:Resource):JsonConvertable = new JsonConvertable(resource)

  /**
   * Implicit conversion that ties the new JsonParsable class to the java readers
   * @param reader
   * @return
   */
  implicit def parseFromReader(reader:io.Reader):JsonParsable2 = new JsonParsable2(reader)
}

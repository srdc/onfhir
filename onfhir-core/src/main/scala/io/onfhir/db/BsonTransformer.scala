package io.onfhir.db

import akka.http.scaladsl.model.DateTime
import io.onfhir.api.util.FHIRUtil
import org.bson.BsonValue
import org.json4s.JsonAST._
import org.mongodb.scala.bson._
import org.mongodb.scala.bson.collection.immutable
import io.onfhir.api.{FHIR_COMMON_FIELDS, FHIR_EXTRA_FIELDS}
import io.onfhir.util.DateTimeUtil
import org.apache.commons.lang3.time.FastDateFormat

import scala.collection.JavaConverters._
import scala.language.implicitConversions

/**
  * Import this object to implicitly convert Scala Maps to BsonDocuments via the "toBson" method
  */
object BsonTransformer{
  // DateTime formats, must use a thread-safe formatter
  private val dateTimeWMiliFormat = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
  private val dateTimeWSecFormat = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ssXXX")
  private val dateTimeFormat = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mmXXX")
  // DateTime regular expression
  private val dateTimeRegex = """-?[1-2]{1}[0|1|8|9][0-9]{2}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?(\.[0-9]+)?(Z|(\+|-|\s)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?$""".r

  /**
    * An implicit object that extends org.mongodb.scala.bson.BsonTransformer
    * to convert any Map to BsonDocument
    */
  implicit object TransformMap extends org.mongodb.scala.bson.BsonTransformer[JObject] {
    def apply(value: JObject): BsonDocument = {
      val document = BsonDocument()
      value.obj.foreach { field =>
        document.put(field._1, field._2 match {
          case b: JBool => BsonBoolean(b.value)
          case n: JInt => if(n.num.isValidInt) BsonInt32(n.num.intValue()) else BsonInt64(n.num.longValue())
          case l: JLong => BsonInt64(l.num)
          case d: JDouble => BsonDouble(d.num)
          case dec:JDecimal => BsonDecimal128(dec.num)
          case s: JString => transformString(field._1, s.s) //Either parse it to special BsonDocument including time related info or simple BsonString
          /*{
            val str:BsonValue = handleString(field._1, s.s)
            if (str.isInstanceOf[BsonDateTime])
              document.put(FHIR_EXTRA_FIELDS.TIME_TEXT_PREFIX + field._1, BsonString(s.s))
            str
          }*/
          case x: JArray => transform(x)(TransformSeq)
          case m: JObject => transform(m)(TransformMap)
          case _ => BsonNull()
        })
      }
      document
    }
  }


  /**
    * An implicit object that extends org.mongodb.scala.bson.BsonTransformer to convert any Seq to BsonArray
    */
  implicit object TransformSeq extends org.mongodb.scala.bson.BsonTransformer[JArray]{
    def apply(value: JArray): BsonArray = BsonArray.fromIterable {
      value.arr map {
        case b: JBool => BsonBoolean(b.value)
        case n: JInt => if(n.num.isValidInt) BsonInt32(n.num.intValue()) else BsonInt64(n.num.longValue())
        case l: JLong => BsonInt64(l.num)
        case d: JDouble => BsonDouble(d.num)
        case dec:JDecimal => BsonDecimal128(dec.num)
        case s: JString => transformString("", s.s) //Either parse it to special BsonDocument including time related info or simple BsonString
        case x: JArray => transform(x)(TransformSeq)
        case m: JObject  => transform(m)(TransformMap)
        case _ => BsonNull()
      }
    }
  }

  /**
    * Scala class that adds "toBson" method for Scala Maps
    */
  class BsonTransformable(obj:JObject) {
    def toBson: BsonDocument = transform(obj).asDocument()
  }

  /**
    * Scala class that adds "fromBson" method for BSON Documents
    *
    * @param document document to be transformed
    */
  class BsonConvertable(document: Document) {
    def fromBson: JObject = transformDocument(document)
  }

  class BsonConvertable2(bsonValue: BsonValue) {
    def fromBson: JValue = bsonValue match {
      case d:BsonDocument => new BsonConvertable(d).fromBson
      case s:BsonString => JString(s.getValue)
      case b: BsonBoolean => JBool(b.getValue)
      case i: BsonInt32 => JInt(i.getValue)
      case l: BsonInt64 => JLong(l.getValue)
      case d: BsonDouble =>   JDouble(d.getValue)
      case t: BsonDateTime => JString(BsonDateTimeToString(t)) //this should not be called, as we handle it before within handleDocument
      case x: BsonArray => transformArray(x)
      case boi: BsonObjectId => JString(boi.getValue.toString)
      case _ => JNull
    }
  }

  /**
    * Implicit conversion that ties the new BsonTransformable class to the Scala immutable Map class
    */
  implicit def transformToBson(obj:JObject):BsonTransformable = new BsonTransformable(obj)

  /**
    * Implicit conversion that ties the new BsonConvertable class to the Bson Documents
    */
  implicit def transformFromBson(document:Document):BsonConvertable = new BsonConvertable(document)

  implicit  def transformFromBsonValue(bsonValue:BsonValue):BsonConvertable2 = new BsonConvertable2(bsonValue)

  /**
    * A helper method to transform any scala type to corresponding BsonValue's
    *
    * @param v object to be transformed
    * @param transformer implicit transformer that extends org.mongo.db.scala.bson.BsonTransformer
    * @tparam T Type of the object to be transformed
    * @return corresponding BsonValue object
    */
  private def transform[T](v: T)(implicit transformer: BsonTransformer[T]): BsonValue = transformer(v)




  /**
    * A converter to convert Document to mutable Maps
    */
  private def transformDocument(document:Document):JObject = {
    //val map = mutable.LinkedHashMap[String, Any]()
    val fields = document.map { field =>
      (field._1 ->
        ( field._2 match {
        case b: BsonBoolean => JBool(b.getValue)
        case i: BsonInt32 => JInt(i.getValue)
        case l: BsonInt64 => JLong(l.getValue)
        case d: BsonDouble => JDouble(d.getValue)
        case s: BsonString => JString(s.getValue)
        case t: BsonDateTime => JString(BsonDateTimeToString(t)) //this should not be called, as we handle it before within handleDocument
        case x: BsonArray => transformArray(x)
        case m: BsonDocument => if (!m.isEmpty) handleDocument(m) else JNull
        case boi: BsonObjectId => JString(boi.getValue.toString)
        case _ => JNull
      }))
    }

    JObject(fields.filterNot(f => f._2 == JNull).toList)
  }

  /**
    * A converter to convert BsonArrays to mutable Seqs
    */
  private def transformArray(array: BsonArray):JArray = {
    //val seq =  scala.collection.mutable.ListBuffer[Any]()
    val results:Seq[JValue] = array.getValues.asScala.map {
        case b:BsonBoolean  => JBool(b.getValue)
        case i:BsonInt32    => JInt(i.getValue)
        case l:BsonInt64    => JLong(l.getValue)
        case d:BsonDouble   => JDouble(d.getValue)
        case dec:BsonDecimal128 => JDecimal(dec.getValue.bigDecimalValue())
        case s:BsonString   => JString(s.getValue)
        case t:BsonDateTime => JString(BsonDateTimeToString(t)) //this should not be called, as we handle it before within handleDocument
        case x:BsonArray    => transformArray(x)
        case m:BsonDocument => if(!m.isEmpty) handleDocument(m) else JNull
        case _              => JNull
    }

    JArray(results.toList)
  }

  /**
    * Tries to parse string as BsonDateTime if the string is not
    * in date format returns string as BsonString.
    *
    * @param field field name
    * @param value value
    * @return BsonString | BsonDateTime
    */
  private def transformString(field:String, value:String):BsonValue = {
    field match {
      //To prevent parsing an id resembling a year as date
      case FHIR_COMMON_FIELDS.ID | FHIR_COMMON_FIELDS.CODE => BsonString(value)
      //For literal reference, we parse the reference and set individual parts of the uri (Ignore the ones with internal references)
      case FHIR_COMMON_FIELDS.REFERENCE if !value.startsWith("#") =>
        val (url, rtype, rid, version) = FHIRUtil.parseReferenceValue(value)
        BsonDocument(
          Seq(
            if(url.isDefined) Some(FHIR_EXTRA_FIELDS.REFERENCE_URL -> BsonString(url.get)) else None,
            Some(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE -> BsonString(rtype)),
            Some(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID -> BsonString(rid)),
            if(version.isDefined) Some(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_VERSION -> BsonString(version.get)) else None
          ).flatten
        )
      //Otherwise try to parse as a date, and if not it is string
      case _ =>
          convertFhirDateToMongoField(field, value) //Try to parse if it is a date
            .getOrElse(BsonString(value)) //Otherwise it is simple string
    }
  }

  /**
    * Converts implicit ranges to the string for
    * representation
    * @param dateTime stored ranges
    * @return string representation
    */
  def dateImplicitRangeToString(dateTime:BsonDocument) : String = {
    // Get server generated ranges from the document
    val serverRangeStart = DateTime(dateTime.get(FHIR_EXTRA_FIELDS.TIME_RANGE_START).asInstanceOf[BsonDateTime].getValue).toIsoDateTimeString
    val serverRangeEnd = DateTime(dateTime.get(FHIR_EXTRA_FIELDS.TIME_RANGE_END).asInstanceOf[BsonDateTime].getValue).toIsoDateTimeString
    // Longest prefix match to trim the server generated parts(e.g. server range 2016-10-10T00:00/2016-10-10T23:59 trimmed to 2016-10-10)
    (serverRangeStart, serverRangeEnd).zipped.takeWhile(Function.tupled(_==_)).unzip._1.mkString.dropRight(1)
  }

  /**
    * Converts date time string to the ISO date format
    * for the MongoDB
    *
    * @param string DateTime string
    * @return BsonValue of the DateTime
    */
  def dateToISODate(string:String) : BsonValue = {
    val timePrecision = string.count(_ == ':')
    // Conversion condtions
    if ((string.contains('Z') && timePrecision == 2) || timePrecision == 3) {

      if(!string.contains('.'))
      // ..HH:MM:SSZ|..HH:MM:SS+03:30 (All time indexes are converted to 0)
        BsonDateTime(dateTimeWSecFormat.parse(string).getTime)
      else
      // ..HH:MM:SS.SSSZ|..HH:MM:SS.SSS+03:30 (All time indexes are converted to 0)
        BsonDateTime(dateTimeWMiliFormat.parse(string).getTime)
    } else if (string.contains('Z') || string.contains('+') || string.count(_ == '-') == 3) {
      // ..HH:MMZ|..HH:MM+..|Y-M-DTHH:MM-.. (Parsing automatically appends seconds, all time inedexes are converted to 0)
      BsonDateTime(dateTimeFormat.parse(string).getTime)
    } else if (!string.contains('Z') && timePrecision == 1) {
      // ..HH:MM (Parsing automatically appends seconds)
      BsonDateTime(dateTimeFormat.parse(string + "Z").getTime)
    } else {
      throw new IllegalArgumentException
    }
  }


  /**
    * Handle the Period.start if the date is given as year, year-month, year-month-date
    * @param value FHIR dateTime value
    * @return
    */
  private def populatePeriodStart(value:String): BsonValue = {
    val datePrecision = value.count(_ == '-')
    datePrecision match {
      case 0 => BsonDateTime(dateTimeWSecFormat.parse(value + "-01-01T00:00:00Z").getTime)
      case 1 =>  BsonDateTime(dateTimeWSecFormat.parse(value + "-01T00:00:00Z").getTime)
      case 2 =>  BsonDateTime(dateTimeWSecFormat.parse(value + "T00:00:00Z").getTime)
    }
  }

  /**
    * Handle the Period.start if the date is given as year, year-month, year-month-date
    * @param value FHIR dateTime value
    * @return
    */
  private def populatePeriodEnd(value:String): BsonValue = {
    val datePrecision = value.count(_ == '-')
    datePrecision match {
      case 0 => BsonDateTime(dateTimeWMiliFormat.parse(value + "-12-31T23:59:59.999Z").getTime)
      case 1 =>  BsonDateTime(dateTimeWMiliFormat.parse(value + DateTimeUtil.dayMonthConversionDateTime(value)).getTime)
      case 2 =>  BsonDateTime(dateTimeWMiliFormat.parse(value + "T23:59:59.999Z").getTime)
    }
  }

  /**
    * Populates implicit ranges for to store dates
    * it in the MongoDB
    *
    * @param string DateTime String
    * @return Range (start, end) of BsonDateTime
    */
  private def datePopulateImplicitRanges(string:String) : (BsonDateTime, BsonDateTime) = {
    val datePrecision = string.count(_ == '-')
    datePrecision match {
      case 0 => //IMPORTANT May conflict with similar syntax (2015 might be inputted as id)
        val rangeStart = dateTimeWSecFormat.parse(string + "-01-01T00:00:00Z").getTime
        val rangeEnd = dateTimeWMiliFormat.parse(string + "-12-31T23:59:59.999Z").getTime
        BsonDateTime(rangeStart) -> BsonDateTime(rangeEnd)
      case 1 =>
        // Append start dateTime to input type YYYY-MM
        val rangeStart = dateTimeWSecFormat.parse(string + "-01T00:00:00Z").getTime
        // Populate end dateTime(may differ from month to month)
        val rangeEnd = dateTimeWMiliFormat.parse(string + DateTimeUtil.dayMonthConversionDateTime(string)).getTime
        BsonDateTime(rangeStart) -> BsonDateTime(rangeEnd)
      case 2 =>
        // Append start and end time to input type YYYY-MM-DD
        val rangeStart = dateTimeWSecFormat.parse(string + "T00:00:00Z").getTime
        val rangeEnd = dateTimeWMiliFormat.parse(string + "T23:59:59.999Z").getTime
        BsonDateTime(rangeStart) -> BsonDateTime(rangeEnd)
    }
  }

  /**
    * Convert a FHIR date, datetime, instant field value to our corresponding schema in Mongo
    * @param field  Field name
    * @param value  Fhir dateTime,date,instant value
    * @return
    */
  private def convertFhirDateToMongoField(field:String, value:String):Option[BsonValue] = {
    dateTimeRegex
      .findPrefixMatchOf(value) // If it matches any of the regex (FHIR date, dateTime, instant)
      .map(_ => {
      //If it is a FHIR datetime or instant
      if(value.contains("T")) {
        createBsonTimeObject(value)
      } else {
        field match {
          //If this is a FHIR period element with start and end fields that has given as FHIR date
          case FHIR_COMMON_FIELDS.START =>
            BsonDocument(
              FHIR_EXTRA_FIELDS.TIME_ORIGINAL -> value,
              FHIR_EXTRA_FIELDS.TIME_TIMESTAMP -> populatePeriodStart(value)
            )
          case FHIR_COMMON_FIELDS.END =>
            BsonDocument(
              FHIR_EXTRA_FIELDS.TIME_ORIGINAL -> value,
              FHIR_EXTRA_FIELDS.TIME_TIMESTAMP -> populatePeriodEnd(value)
            )
          case _ =>
            //Otherwise it is FHIR date type (xs:gYear, xs:gYearMonth, xs:Date), so we should keep the period for easy querying
            val (srange,erange) = datePopulateImplicitRanges(value)
            BsonDocument(
              FHIR_EXTRA_FIELDS.TIME_ORIGINAL -> value,
              FHIR_EXTRA_FIELDS.TIME_RANGE_START -> srange,
              FHIR_EXTRA_FIELDS.TIME_RANGE_END -> erange
            )
        }
      }
    })
  }

  /**
    * Converts BsonDateTime to String if narrative is generated for BsonDateTime
    * returns the narrative else parses it to the ISO date time string.
    *
    * @param fieldValue value
    * @return DateTime string
    */
  private def BsonDateTimeToString(fieldValue:BsonDateTime):String = {
      DateTime(fieldValue.getValue).toIsoDateTimeString + "Z" // Convert dateTime to readable version
  }
/*
  /**
    * BsonDocument is either a regular document or implicit date time
    * values generated by the server.
    *
    * @param document document to convert
    * @return Map[String, Any] | String
    */
  private def CheckImplicitDateTime(document:BsonDocument):JValue = {
    // If date time is created by the server, convert it back to the inserted shape
    if(document.containsKey(FHIR_EXTRA_FIELDS.TIME_RANGE_START)) {
      JString(DateTimeUtil.dateImplicitRangeToString(document))
    } else {
      transformDocument(immutable.Document(document))
    }
  }*/

  /**
    * BsonDocument is either a regular document or a special one representing FHIR date, datetime or instant
    * @param document
    * @return
    */
  private def handleDocument(document:BsonDocument):JValue = {
    //If the document represents our special FHIR date/datetime representation in Mongo, just get the original string back
    if(document.containsKey(FHIR_EXTRA_FIELDS.TIME_ORIGINAL)){
      JString(document.getString(FHIR_EXTRA_FIELDS.TIME_ORIGINAL).getValue)
    } else if(document.containsKey(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID)) {
      //Reconstruct reference path
      val url = if(document.containsKey(FHIR_EXTRA_FIELDS.REFERENCE_URL)) Some(document.getString(FHIR_EXTRA_FIELDS.REFERENCE_URL).getValue) else None
      val rtype = document.getString(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_TYPE).getValue
      val rid = document.getString(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID).getValue
      val version = if(document.containsKey(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_VERSION)) Some(document.getString(FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_VERSION).getValue) else None

      JString(s"${url.map(u => s"$u/").getOrElse("")}$rtype/$rid${version.map(v => "/_history/" + v).getOrElse("")}")
    } else {
      transformDocument(immutable.Document(document)) //otherwise it is regular document
    }
  }

  def createBsonTimeObject(value:String) = {
    val ts = dateToISODate(value)
    val onlyDate:BsonValue = BsonDateTime(dateTimeWSecFormat.parse(value.split("T").head + "T00:00:00Z").getTime)
    //Create a Json object with original time string, a parsed timestamp ISODate, and only date part
    BsonDocument(
      FHIR_EXTRA_FIELDS.TIME_ORIGINAL -> value,
      FHIR_EXTRA_FIELDS.TIME_TIMESTAMP -> ts,
      FHIR_EXTRA_FIELDS.TIME_DATE -> onlyDate
    )
  }

  /**
    * Filtering algorithm to get rid of unnecassary values.
    */
  //private def filterDocument(mapTuple:(String, Any)): Boolean = mapTuple._1.startsWith(FHIR_EXTRA_FIELDS.TIME_TEXT_PREFIX) || mapTuple._2 == null

}

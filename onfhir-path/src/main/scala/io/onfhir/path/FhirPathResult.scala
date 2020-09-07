package io.onfhir.path

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, Period, Year, YearMonth, ZoneId, ZonedDateTime}
import java.time.temporal.{ChronoField, ChronoUnit, Temporal, TemporalAmount}

import io.onfhir.api.util.FHIRUtil
import io.onfhir.path.FhirPathValueTransformer.transform
import org.json4s.JsonAST.{JBool, JDecimal, JLong, JObject, JString, JValue}

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.math.{BigDecimal, Ordered, min}

sealed trait FhirPathResult {
  /**
    * Override scala object equality
    * @param obj
    * @return
    */
  override def equals(obj: Any): Boolean = this.getClass == obj.getClass && isEqual(obj.asInstanceOf[FhirPathResult]).getOrElse(false)

  /**
    * FHIR path equality check for FHIR Path types
    * @param that Other object
    * @return
    */
  def isEqual(that:FhirPathResult):Option[Boolean]

  /**
    * FHIR path equivalence check (~) for FHIR Path types, defualt is same with isEqual
    * @param that
    * @return
    */
  def isEquivalent(that:FhirPathResult):Boolean = isEqual(that).getOrElse(false)

  /**
   * Convert the result to json4s.model
   * @return
   */
  def toJson:JValue
}

/**
  * FHIR Path string type
  * @param s  string value
  */
case class FhirPathString(s:String) extends FhirPathResult with Ordered[FhirPathString] {

  def + (that:FhirPathString):FhirPathString = FhirPathString(s + that.s)

  override def compare(that:FhirPathString):Int = s.compareTo(that.s)

  override def isEqual(that:FhirPathResult):Option[Boolean] =  that match {
    case s2:FhirPathString => Some(compare(s2) == 0)
    case _ => Some(false)
  }

  override def isEquivalent(that:FhirPathResult):Boolean = that match {
    case FhirPathString(s2) => s.trim().replaceAll(" +", " ").equalsIgnoreCase(s2.trim().replaceAll(" +", " "))
    case _ => false
  }

  def toJson:JValue = JString(s)
}

/**
  * FHIR Path decimal and integer types
  * @param v
  */
case class FhirPathNumber(v:BigDecimal) extends FhirPathResult with Ordered[FhirPathNumber] {
  def + (that:FhirPathNumber):FhirPathNumber = FhirPathNumber(v + that.v)
  def - (that:FhirPathNumber):FhirPathNumber = FhirPathNumber(v - that.v)
  def * (that:FhirPathNumber):FhirPathNumber = FhirPathNumber(v * that.v)
  def / (that:FhirPathNumber):FhirPathNumber = FhirPathNumber(v / that.v)
  def mod (that:FhirPathNumber):FhirPathNumber = FhirPathNumber( v.toInt % that.v.toInt)
  def div (that:FhirPathNumber):FhirPathNumber = FhirPathNumber( v.toInt / that.v.toInt)
  def - () = FhirPathNumber(-1 * v)

  override def compare(that:FhirPathNumber):Int = v.compareTo(that.v)

  override def isEqual(that:FhirPathResult):Option[Boolean] = that match {
    case FhirPathNumber(v2) => Some(v == v2)
    case _ => Some(false)
  }

  override def isEquivalent(that:FhirPathResult):Boolean =
    that match {
      case FhirPathNumber(v2) =>
        val precision = min(v.scale, v2.scale)
        v.setScale(precision,RoundingMode.HALF_UP) == v2.setScale(precision,RoundingMode.HALF_UP)
      case _ => false
    }

  def isInteger():Boolean = v - v.toInt == 0

  def toJson:JValue = if(isInteger()) JLong(v.toLong) else JDecimal(v)
}

/**
  * FHIR Path dateTime type
  * @param dt
  */
case class FhirPathDateTime(dt:Temporal) extends FhirPathResult with Ordered[FhirPathDateTime] {
  def + (that:FhirPathQuantity):FhirPathDateTime = {
    FhirPathDateTime(addOrRemove(that.q.v.toLong, that.unit))
  }

  def - (that:FhirPathQuantity):FhirPathDateTime = {
    FhirPathDateTime(addOrRemove(that.q.v.toLong * -1, that.unit))
  }

  /**
    * Comparison done in lowest precision
    * @param that
    * @return
    */
  override def isEquivalent(that:FhirPathResult):Boolean = {
      that.isInstanceOf[FhirPathDateTime] &&
        (
          (dt, that.asInstanceOf[FhirPathDateTime].dt) match {
            case (y1:Year, o2) => y1.getValue == o2.get(ChronoField.YEAR)
            case (o1, y2:Year) => y2.getValue == o1.get(ChronoField.YEAR)
            case (m1:YearMonth, o2) => m1.getYear == o2.get(ChronoField.YEAR)  && m1.getMonthValue == o2.get(ChronoField.MONTH_OF_YEAR)
            case (o1, m2:YearMonth) => m2.getYear == o1.get(ChronoField.YEAR)  && m2.getMonthValue == o1.get(ChronoField.MONTH_OF_YEAR)
            case (d1:LocalDate, o2) => d1.getYear == o2.get(ChronoField.YEAR) && d1.getMonthValue == o2.get(ChronoField.MONTH_OF_YEAR) && d1.getDayOfMonth == o2.get(ChronoField.DAY_OF_MONTH)
            case (o1, d2:LocalDate) => d2.getYear == o1.get(ChronoField.YEAR) && d2.getMonthValue == o1.get(ChronoField.MONTH_OF_YEAR) && d2.getDayOfMonth == o1.get(ChronoField.DAY_OF_MONTH)
            case (dt1:LocalDateTime, o2) => dt1.getYear == o2.get(ChronoField.YEAR) && dt1.getMonthValue == o2.get(ChronoField.MONTH_OF_YEAR) && dt1.getDayOfMonth == o2.get(ChronoField.DAY_OF_MONTH) &&
                                              dt1.getHour == o2.get(ChronoField.HOUR_OF_DAY) && dt1.getMinute == o2.get(ChronoField.MINUTE_OF_HOUR) && dt1.getSecond == o2.get(ChronoField.SECOND_OF_MINUTE)
            case (o2, dt2:LocalDateTime) => dt2.getYear == o2.get(ChronoField.YEAR) && dt2.getMonthValue == o2.get(ChronoField.MONTH_OF_YEAR) && dt2.getDayOfMonth == o2.get(ChronoField.DAY_OF_MONTH) &&
              dt2.getHour == o2.get(ChronoField.HOUR_OF_DAY) && dt2.getMinute == o2.get(ChronoField.MINUTE_OF_HOUR) && dt2.getSecond == o2.get(ChronoField.SECOND_OF_MINUTE)
            case (zdt1:ZonedDateTime, zdt2:ZonedDateTime) => zdt1.isEqual(zdt2)
            case (oth1, oth2) => throw new FhirPathException(s"Invalid datetime comparison between $oth1 and $oth2 ...")
          }
       )
  }

  /**
    * Comparison done in highest precision
    * @param that
    * @return
    */
  override def compare(that:FhirPathDateTime):Int = {
    (dt, that.dt) match {
      case (y1:Year, y2:Year) => y1.compareTo(y2)
      case (y1:Year, m2:YearMonth) => y1.atMonth(1).compareTo(m2)
      case (y1:Year, d2:LocalDate) =>   y1.atMonth(1).atDay(1).compareTo(d2)
      case (y1:Year, dt2:LocalDateTime) =>   y1.atMonth(1).atDay(1).atTime(0,0).compareTo(dt2)
      case (y1:Year, zdt2:ZonedDateTime) =>   y1.atMonth(1).atDay(1).atTime(0,0).atZone(ZoneId.of("Z")).compareTo(zdt2)
      case (m1:YearMonth, y2:Year) => m1.compareTo(y2.atMonth(1))
      case (m1:YearMonth, m2:YearMonth) => m1.compareTo(m2)
      case (m1:YearMonth, d2:LocalDate) =>   m1.atDay(1).compareTo(d2)
      case (m1:YearMonth, dt2:LocalDateTime) =>   m1.atDay(1).atTime(0,0).compareTo(dt2)
      case (m1:YearMonth, zdt2:ZonedDateTime) =>   m1.atDay(1).atTime(0,0).atZone(ZoneId.of("Z")).compareTo(zdt2)
      case (d1:LocalDate, y2:Year) => d1.compareTo(y2.atMonth(1).atDay(1))
      case (d1:LocalDate, m2:YearMonth) => d1.compareTo(m2.atDay(1))
      case (d1:LocalDate, d2:LocalDate) => d1.compareTo(d2)
      case (d1:LocalDate, dt2:LocalDateTime) => d1.atTime(0,0).compareTo(dt2)
      case (d1:LocalDate, zdt2:ZonedDateTime) => d1.atTime(0,0).atZone(ZoneId.of("Z")).compareTo(zdt2)
      case (dt1:LocalDateTime, y2:Year) => dt1.compareTo(y2.atMonth(1).atDay(1).atTime(0, 0))
      case (dt1:LocalDateTime, m2:YearMonth) => dt1.compareTo(m2.atDay(1).atTime(0, 0))
      case (dt1:LocalDateTime, d2:LocalDate) => dt1.compareTo(d2.atTime(0, 0))
      case (dt1:LocalDateTime, dt2:LocalDateTime) => dt1.compareTo(dt2)
      case (dt1:LocalDateTime, zdt2:ZonedDateTime) => dt1.atZone(ZoneId.of("Z")).compareTo(zdt2)
      case (zdt1:ZonedDateTime, y2:Year) => zdt1.compareTo(y2.atMonth(1).atDay(1).atTime(0, 0).atZone(ZoneId.of("Z")))
      case (zdt1:ZonedDateTime, m2:YearMonth) => zdt1.compareTo(m2.atDay(1).atTime(0, 0).atZone(ZoneId.of("Z")))
      case (zdt1:ZonedDateTime, d2:LocalDate) => zdt1.compareTo(d2.atTime(0, 0).atZone(ZoneId.of("Z")))
      case (zdt1:ZonedDateTime, dt2:LocalDateTime) => zdt1.compareTo(dt2.atZone(ZoneId.of("Z")))
      case (zdt1:ZonedDateTime, zdt2:ZonedDateTime) => zdt1.compareTo(zdt2)
      case (oth1, oth2) => throw new FhirPathException(s"Invalid datetime comparison between $oth1 and $oth2 ...")
    }
  }

  override def isEqual(that:FhirPathResult):Option[Boolean] =
    that match {
      case FhirPathDateTime(dt2) => (dt, dt2) match {
        case (y1:Year, y2:Year) => Some(y1 == y2)
        case (m1:YearMonth, m2:YearMonth) => Some(m1 == m2)
        case (d1:LocalDate, d2:LocalDate) => Some(d1 == d2)
        case (dt1:LocalDateTime, dt2:LocalDateTime) => Some(dt1 == dt2)
        case (zdt1:ZonedDateTime, zdt2:ZonedDateTime) =>
          Some(zdt1 == zdt2)
        case (_, _) => None
      }
      case _ => Some(false)
    }

  private def addOrRemove(amountToAdd:Long, unit:String):Temporal = {
    try {
      val duration = unit match {
        case "'a'" => Period.ofYears(amountToAdd.toInt)
        case "'mo'" => Period.ofMonths(amountToAdd.toInt)
        case "'wk'" => Period.ofWeeks(amountToAdd.toInt)
        case "'d'" => Period.ofDays(amountToAdd.toInt)
        case "'h'" => Duration.of(amountToAdd, ChronoUnit.HOURS)
        case "'min'" => Duration.of(amountToAdd, ChronoUnit.MINUTES)
        case "'s'" => Duration.of(amountToAdd, ChronoUnit.SECONDS)
        case "'ms'" => Duration.of(amountToAdd, ChronoUnit.MILLIS)
      }

      dt match {
        case y:Year =>
          duration match {
            case p:Period => y.plusYears(p.getYears + (p.getMonths / 12) + (p.getDays / 365))
            case du:Duration => y.plusYears(du.toDays / 365)
          }
        case ym:YearMonth =>
          duration match {
            case p:Period => ym.plusMonths(p.getYears * 12 + p.getMonths + (p.getDays / 30))
            case du:Duration => ym.plusMonths(du.toDays / 31)
          }
        case d:LocalDate => duration match {
          case p:Period => d.plus(p)
          case du:Duration => d.plusDays(du.toDays)
        }
        case dt:LocalDateTime => dt.plus(duration)
        case zdt:ZonedDateTime => zdt.plus(duration)
      }
    } catch {
      case e:Exception => throw new FhirPathException(s"Invalid datetime arithmetic on $dt for quantity $amountToAdd and unit $unit !")
    }
  }

  def toJson:JValue = dt match {
    case y:Year => JString(y.toString)
    case ym:YearMonth => JString(ym.toString)
    case ld:LocalDate => JString(ld.toString)
    case ldt:LocalDateTime => JString(ldt.toString)
    case zdt:ZonedDateTime => JString(zdt.toString)
  }
}

/**
  * FHIR Path time type
  * @param lt   time part
  * @param zone optional zone part
  */
case class FhirPathTime(lt:LocalTime, zone:Option[ZoneId] = None) extends FhirPathResult with Ordered[FhirPathTime] {
  def + (that:FhirPathQuantity):FhirPathTime = FhirPathTime(addOrRemove(that.q.v.toLong, that.unit), zone)
  def - (that:FhirPathQuantity):FhirPathTime = FhirPathTime(addOrRemove(that.q.v.toLong * -1, that.unit), zone)

  private def addOrRemove(amountToAdd:Long, unit:String):LocalTime = {
    try {
      unit match {
        case "'h'" => lt.plus(amountToAdd, ChronoUnit.HOURS)
        case "'min'" => lt.plus(amountToAdd, ChronoUnit.MINUTES)
        case "'s'" => lt.plus(amountToAdd, ChronoUnit.SECONDS)
        case "'ms'" => lt.plus(amountToAdd, ChronoUnit.MILLIS)
      }
    } catch {
      case e:Exception => throw new FhirPathException(s"Invalid datetime arithmetic on $lt with zone $zone for quantity $amountToAdd and unit $unit !")
    }
  }

  override def compare(that:FhirPathTime):Int = {
    this.toFhirDateTime().compare(that.toFhirDateTime())
  }

  def toFhirDateTime():FhirPathDateTime =
    FhirPathDateTime(
      zone match {
        case Some(z) => LocalDate.of(1900, 1, 1).atTime(lt).atZone(z)
        case None => LocalDate.of(1900, 1, 1).atTime(lt)
      }
    )

  override def isEqual(that:FhirPathResult):Option[Boolean] =
    that match {
      case t2:FhirPathTime => Some(t2.lt == lt && t2.zone == zone)
      case _ => Some(false)
    }

  override def isEquivalent(that: FhirPathResult): Boolean = {
    that match {
      case t2:FhirPathTime =>
        this.toFhirDateTime().isEquivalent(t2.toFhirDateTime())
      case _ => false
    }
  }

  def toJson:JValue = JString(lt.toString + zone.map(z=> z.toString).getOrElse(""))
}

/**
  * FHIR path boolean type
  * @param b
  */
case class FhirPathBoolean(b:Boolean) extends FhirPathResult {
  def or (that:FhirPathBoolean) = FhirPathBoolean(b || that.b)
  def and (that:FhirPathBoolean) = FhirPathBoolean(b && that.b)
  def xor (that:FhirPathBoolean) = FhirPathBoolean(b ^ that.b)

  override def isEqual(that:FhirPathResult):Option[Boolean] =
    that match {
      case FhirPathBoolean(b2) => Some(b == b2)
      case _ => Some(false)
    }

  def toJson:JValue = JBool(b)
}

/**
  * FHIR Path quantity type
  * @param q
  * @param unit
  */
case class FhirPathQuantity(q:FhirPathNumber, unit:String) extends FhirPathResult with Ordered[FhirPathQuantity]  {
  override def isEqual(that:FhirPathResult):Option[Boolean] = {
    that match {
      case FhirPathQuantity(q2, u2) => Some(q.isEqual(q2).getOrElse(false) && unit == u2)
      case _ => Some(false)
    }
  }

  override def compare(that:FhirPathQuantity):Int = {
    q.compareTo(that.q)
  }

  def toJson:JValue = JObject("value" -> q.toJson ,  "system" -> JString("http://unitsofmeasure.org"), "code"-> JString(unit),"unit" -> JString(unit))
}

/**
  * FHIR Path Object type (JSON)
  * @param json
  */
case class FhirPathComplex(json:JObject) extends FhirPathResult {
  override def isEqual(that:FhirPathResult):Option[Boolean] = {
    that match {
      case FhirPathComplex(j2) =>
        Some(json.equals(j2))
      case _ => Some(false)
    }
  }

  /**
   * Try to convert it to Quantity if it is
   * @return
   */
  def toQuantity():Option[FhirPathQuantity] = {
    val fields = json.obj.map(_._1).toSet
    if(fields.contains("value") && fields.subsetOf(Set("value", "unit", "system", "code", "comparator", "extension", "id"))){
      val n = FhirPathNumber(FHIRUtil.extractValue[BigDecimal](json, "value"))

      val unit = FHIRUtil.extractValueOption[String](json, "code") match {
        case None => FHIRUtil.extractValueOption[String](json, "unit")
        case Some(x) => Some(x)
      }
      Some(FhirPathQuantity.apply(n, unit.getOrElse("")))
    } else
      None
  }

  def toJson:JValue = json
}
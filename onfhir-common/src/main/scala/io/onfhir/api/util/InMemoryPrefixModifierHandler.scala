package io.onfhir.api.util

import java.time.Instant
import io.onfhir.util.JsonFormatter._
import io.onfhir.api._
import io.onfhir.exception.{InitializationException, InternalServerException, InvalidParameterException, UnsupportedParameterException}
import io.onfhir.util.DateTimeUtil
import org.json4s.JsonAST.{JObject, JValue}

import scala.math.pow

object InMemoryPrefixModifierHandler {

  /**
    * Handle missing modifier in search
    * @param paths
    * @param bool
    * @param resource
    * @return
    */
  def missingHandler(paths: Seq[String], bool: String, resource: Resource): Boolean = {
    val values = paths.map(p => FHIRUtil.applySearchParameterPath(p, resource))
    misssingHandler(bool, values)
  }

  def misssingHandler(bool:String, values:Seq[Seq[JValue]]):Boolean = {
    bool match {
      case MISSING_MODIFIER_VALUES.STRING_TRUE =>
        values.forall(_.isEmpty)//all paths should be empty
      case MISSING_MODIFIER_VALUES.STRING_FALSE =>
        values.exists(_.nonEmpty)
      case _ =>
        throw new InvalidParameterException("Correct Boolean Value Should be Provided")
    }
  }

  /**
   * Handle text search on tokens
   * @param value         Expected value
   * @param targetType    Target type of actual value
   * @param actualValue   Extracted value from resource
   * @return
   */
  def handleTokenTextModifier(value: String, targetType:String, actualValue:JValue): Boolean = {
    // Get the token :text field for target type
    val textFields = TOKEN_TYPE_SEARCH_DISPLAY_PATHS.get(targetType)
    if(textFields.isEmpty)
      throw new InitializationException(s"The modifier :text cannot be used for elements with type $targetType !!!")

    textFields.get.exists(textField => {
      val actualValues = FHIRUtil.applySearchParameterPath(textField, actualValue)
      //TODO Better handle case insensivity and furher insensitivity for FHIR specified characters
      actualValues.exists(avalue => avalue.extract[String].toLowerCase.startsWith(value.toLowerCase))
    })
  }


  /*def handleTokenTextModifier(path: String, value: String, targetType:String, resource:JValue): Boolean = {
    // Get the token :text field for target type
    val textFields = TOKEN_TYPE_SEARCH_DISPLAY_PATHS.get(targetType)
    if(textFields.isEmpty)
      throw new InitializationException(s"The modifier :text cannot be used for elements with type $targetType !!!")

    textFields.get.exists(textField => {
      val actualValues = FHIRUtil.applySearchParameterPath(FHIRUtil.mergeElementPath(path,textField), resource)
      //TODO Better handle case insensivity and furher insensitivity for FHIR specified characters
      actualValues.exists(avalue => avalue.extract[String].toLowerCase.startsWith(value.toLowerCase))
    })
  }*/

  /**
    *
    * @param systemPath Path to the system part e.g. Coding.system, Identifier.system
    * @param codePath Path to the code part
    * @param system Expected system value
    * @param code Expected code value
    * @param modifier
    * @param element
    * @return
    */
  def tokenModifierHandler(systemPath:String, codePath:String, system:Option[String], code:Option[String], modifier:String, element:JObject):Boolean = {
    modifier match {
      //Without modifier
      case "" =>
        handleTokenCodeSystemQuery(systemPath, codePath, system, code, element)
      case FHIR_PREFIXES_MODIFIERS.IN | FHIR_PREFIXES_MODIFIERS.NOT_IN | FHIR_PREFIXES_MODIFIERS.BELOW | FHIR_PREFIXES_MODIFIERS.ABOVE =>
        throw new UnsupportedParameterException("Modifier is not supported by onFhir.io system yet!")
      case other =>
        throw new InvalidParameterException(s"Modifier $other is not supported for FHIR token queries!")
    }
  }

  /**
    * Handle the Token query on system and code fields
    * @param systemPath Path to the system part e.g. Coding.system, Identifier.system
    * @param codePath Path to the code part
    * @param system Expected system value
    * @param code Expected code value
    * @param element Parent element to search
    * @return
    */
  private def handleTokenCodeSystemQuery(systemPath:String, codePath:String, system:Option[String], code:Option[String], element:JObject):Boolean = {
    system match {
      // Query like [code] -> the value of [code] matches a Coding.code or Identifier.value irrespective of the value of the system property
      case None =>
        FHIRUtil
          .applySearchParameterPath(codePath ,element).headOption
          .exists(_.extractOpt[String].contains(code.get))
      // Query like |[code] -> the value of [code] matches a Coding.code or Identifier.value, and the Coding/Identifier has no system property
      case Some("") =>
        FHIRUtil
          .applySearchParameterPath(systemPath ,element).isEmpty &&
          FHIRUtil
            .applySearchParameterPath(codePath ,element).headOption
            .exists(_.extractOpt[String].contains(code.get))
      // Query like [system][code] -> the value of [code] matches a Coding.code or Identifier.value, and the value of [system] matches the system property of the Identifier or Coding
      case Some(sys) =>
        code match {
          //[system]| --> should macth only system
          case None =>
            FHIRUtil
              .applySearchParameterPath(systemPath ,element)
              .headOption
              .exists(_.extractOpt[String].contains(sys))
          // Query like [system][code]
          case Some(cd) =>
            FHIRUtil
              .applySearchParameterPath(systemPath ,element)
              .headOption
              .exists(_.extractOpt[String].contains(sys)) &&
              FHIRUtil
                .applySearchParameterPath(codePath ,element).headOption
                .exists(_.extractOpt[String].contains(cd))
        }
    }
  }

  /**
    * Handle the system code/unit query on Quantity
    * @param system
    * @param code
    * @param systemPath
    * @param codePath
    * @param unitPath
    * @param element
    * @return
    */
  def handleQuantityCodeSystemQuery(system:Option[String], code:Option[String], systemPath:String, codePath:String, unitPath:String, element:JObject) = {
    (system, code) match {
      //Only query on value
      case (None, None) =>
        true
      //Query on value + unit
      case (None, Some(c)) =>
         FHIRUtil
           .applySearchParameterPath(codePath, element)
           .exists(_.extractOpt[String].contains(c)) ||
           FHIRUtil
             .applySearchParameterPath(unitPath, element)
             .exists(_.extractOpt[String].contains(c))
      //query on value + system + code
      case (Some(s), Some(c)) =>
        FHIRUtil
          .applySearchParameterPath(codePath, element)
          .exists(_.extractOpt[String].contains(c)) &&
          FHIRUtil
            .applySearchParameterPath(systemPath, element)
            .exists(_.extractOpt[String].contains(s))

      case _ => throw new InternalServerException("Invalid state!")
    }

  }


  /**
    * Handles prefixes for integer values
    *
    * @param value  Compared value
    * @param prefix prefix of the parameter
    * @param actualValue Actual value in resource
    * @return
    */
  def intPrefixHandler(value:String, prefix:String, actualValue:Int): Boolean = {
    // Prefix matching and query filter generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => actualValue == value.toInt
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => actualValue > value.toInt
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => actualValue < value.toInt
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => actualValue >= value.toInt
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => actualValue <= value.toInt
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => actualValue != value.toInt
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE => actualValue >= value.toDouble * 0.9 && actualValue <= value.toDouble * 1.1
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER | FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE => throw new IllegalArgumentException("Prefixes sa and eb can not be used with integer values.")
    }
  }

  /**
    * Handle query on decimal values with given prefix
    * @param value Compared value
    * @param prefix prefix
    * @param actualValue actual value in resource
    * @return
    */
  def decimalPrefixHandler(value:String, prefix:String, actualValue:Double): Boolean = {
    // Calculation of precision to generate implicit ranges
    val precision = if(!value.contains('.')) 0.5 else pow(0.1, value.length - (value.indexOf(".") + 1)) * 0.5
    // Generated function values for comparison
    val floor = value.toDouble - precision
    val ceil = value.toDouble + precision

    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => actualValue >= floor && actualValue < ceil
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => actualValue > value.toDouble
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => actualValue < value.toDouble
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL =>
        decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, actualValue) || decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.EQUAL, actualValue)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL =>
        decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.LESS_THAN, actualValue) || decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.EQUAL, actualValue)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => actualValue < floor || actualValue >= ceil
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER =>
        decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, actualValue) && decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.LESS_THAN, actualValue)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE =>
        decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, actualValue) && decimalPrefixHandler(value, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, actualValue)
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        decimalPrefixHandler((value.toDouble*0.9).toString, FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL, actualValue) && decimalPrefixHandler((value.toDouble*1.1).toString, FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL, actualValue)
    }
  }

  /**
    * Handle query on FHIR Range values with given prefix
    * @param value Value in the query to be compared
    * @param prefix FHIR search prefix
    * @param rangeValue Actual range JSON object in resource
    * @return
    */
  def rangePrefixHandler(value:String, prefix:String, rangeObject:JValue, isSampleData:Boolean =  false): Boolean = {
    // Calculation of precision to generate implicit ranges
    val precision = if(!value.contains('.')) 0.5 else pow(0.1, value.length - (value.indexOf(".") + 1)) * 0.5
    // Paths to the range structure's high and low values
    val actualLow = if(isSampleData)  (rangeObject \ FHIR_COMMON_FIELDS.LOWER_LIMIT).extractOpt[Double] else (rangeObject \ FHIR_COMMON_FIELDS.LOW).extractOpt[Double]
    val actualHigh =  if(isSampleData) (rangeObject \ FHIR_COMMON_FIELDS.UPPER_LIMIT).extractOpt[Double] else (rangeObject \ FHIR_COMMON_FIELDS.HIGH).extractOpt[Double]
    // Implicit input value ranges
    val floor = value.toDouble - precision
    val ceil = value.toDouble + precision
    // Prefix matching and query result
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => actualLow.forall(l => l>= floor) && actualHigh.forall(h => h < ceil)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => actualHigh.forall(h => h > ceil)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => actualLow.forall(l => l < floor)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL =>
        rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.EQUAL, rangeObject) || rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, rangeObject)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL =>
        rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.EQUAL, rangeObject) || rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.LESS_THAN, rangeObject)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL =>
        rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.LESS_THAN, rangeObject) || rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, rangeObject)
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER =>
        rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, rangeObject) || rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.LESS_THAN, rangeObject)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE =>
        rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, rangeObject) || rangePrefixHandler(value, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, rangeObject)
      case FHIR_PREFIXES_MODIFIERS.APPROXIMATE =>
        actualLow.forall(l => decimalPrefixHandler((value.toDouble*0.9).toString, FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL, l)) &&
          actualHigh.forall(h => decimalPrefixHandler((value.toDouble*1.1).toString, FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL, h))
    }
  }

  /**
    * Handle queries on FHIR Period elements
    * @param value
    * @param prefix
    * @param periodObj
    * @return
    */
  def periodPrefixHandler(valueRange:(Instant, Instant), prefix:String, periodObj:JValue): Boolean = {
    //Get the start of a period
    val sdate = (periodObj \ FHIR_COMMON_FIELDS.START)
                    .extractOpt[String]
                    .map(s => DateTimeUtil.populateImplicitDateTimeRanges(s)._1) // if e.g. 2018 given in start it means 2018-01-01...
                    .map(s => Instant.parse(s))

    val edate = (periodObj \ FHIR_COMMON_FIELDS.END)
                  .extractOpt[String]
                  .map(s => DateTimeUtil.populateImplicitDateTimeRanges(s)._2)
                  .map(s => Instant.parse(s))


    dateTimeQueryBuilder(valueRange, prefix, sdate -> edate)
  }

  /**
    *
    * @param valueRange
    * @param prefix
    * @param actualTime
    * @return
    */
  def dateTimeQueryBuilder(valueRange:(Instant, Instant), prefix:String, actualTime:Instant):Boolean = {
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL => valueRange._1.isBefore(actualTime) && actualTime.isBefore( valueRange._2) ||  valueRange._1.equals(actualTime)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => actualTime.isAfter(valueRange._2)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => actualTime.isBefore(valueRange._1)
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL => actualTime.isAfter(valueRange._2) || (valueRange._1.isBefore(actualTime) && actualTime.isBefore( valueRange._2))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL => actualTime.isBefore(valueRange._1) || (valueRange._1.isBefore(actualTime) && actualTime.isBefore( valueRange._2))
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL => actualTime.isAfter(valueRange._2) || actualTime.isBefore(valueRange._1)
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER =>
        dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, actualTime) || dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.LESS_THAN, actualTime)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE =>
        dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, actualTime) || dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, actualTime)
    }
  }

  /**
    *
    * @param valueRange
    * @param prefix
    * @param actualTime
    * @return
    */
  def dateTimeQueryBuilder(valueRange:(Instant, Instant), prefix:String, actualTime:(Option[Instant], Option[Instant])):Boolean = {
    // Prefix matching and query generation
    prefix match {
      case FHIR_PREFIXES_MODIFIERS.BLANK_EQUAL | FHIR_PREFIXES_MODIFIERS.EQUAL =>
        actualTime._1.forall(at => at.isAfter(valueRange._1) || at.equals(valueRange._1)) && actualTime._2.forall(at => at.isBefore(valueRange._2) || at.equals(valueRange._2))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN | FHIR_PREFIXES_MODIFIERS.GREATER_THAN_M => actualTime._2.forall(_.isAfter(valueRange._2))
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN | FHIR_PREFIXES_MODIFIERS.LESS_THAN_M => actualTime._1.forall(_.isBefore(valueRange._1))
      case FHIR_PREFIXES_MODIFIERS.GREATER_THAN_EQUAL =>
        dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.EQUAL, actualTime) || dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, actualTime)
      case FHIR_PREFIXES_MODIFIERS.LESS_THAN_EQUAL =>
        dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.EQUAL, actualTime) || dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.LESS_THAN, actualTime)
      case FHIR_PREFIXES_MODIFIERS.NOT_EQUAL =>
        dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.LESS_THAN, actualTime) || dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, actualTime)
      case FHIR_PREFIXES_MODIFIERS.STARTS_AFTER =>
        dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, actualTime) || dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.LESS_THAN, actualTime)
      case FHIR_PREFIXES_MODIFIERS.ENDS_BEFORE =>
        dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.NOT_EQUAL, actualTime) || dateTimeQueryBuilder(valueRange, FHIR_PREFIXES_MODIFIERS.GREATER_THAN, actualTime)
    }
  }

  /**
   * FHIR string query handling
   * @param actualValue
   * @param modifier
   * @param expectedValue
   * @return
   */
  def stringQueryHandler(actualValue:JValue, modifier:String,  expectedValue:String):Boolean = {
    modifier match {
      case FHIR_PREFIXES_MODIFIERS.EXACT =>
        // Exact match provided with $eq mongo operator
        actualValue.extract[String].equalsIgnoreCase(expectedValue)
      case FHIR_PREFIXES_MODIFIERS.CONTAINS =>
        actualValue.extract[String].toLowerCase.contains(expectedValue.toLowerCase())
      case _ =>
        // Case insensitive, partial matches at the end of string
        actualValue.extract[String].toLowerCase.startsWith(expectedValue.toLowerCase())
    }
  }

  /**
   * FHIR Uri Query Handling
   * @param actualValue
   * @param modifier
   * @param expectedUri
   * @return
   */
  def uriQueryHandler(actualValue:JValue, modifier:String, expectedUri:String):Boolean = {
    modifier match {
      case FHIR_PREFIXES_MODIFIERS.ABOVE =>
        val parentUri = expectedUri.split('/').dropRight(2).mkString("/")
        actualValue.extract[String].startsWith(parentUri)
      case FHIR_PREFIXES_MODIFIERS.BELOW =>
        actualValue.extract[String].startsWith(expectedUri)
      case "" =>
        actualValue.extract[String].equals(expectedUri)
    }
  }

}

package io.onfhir.path

import java.time.{LocalDate, LocalDateTime, LocalTime, Year, YearMonth, ZoneId, ZonedDateTime}
import io.onfhir.api.Resource
import io.onfhir.api.model.{FhirCanonicalReference, FhirLiteralReference, FhirReference}
import io.onfhir.api.validation.IReferenceResolver
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JInt, JNull, JObject, JString}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class FhirPathEvaluatorTest extends Specification {

  val observation = Source.fromInputStream(getClass.getResourceAsStream("/observation.json")).mkString.parseJson
  val observation2 = Source.fromInputStream(getClass.getResourceAsStream("/observation2.json")).mkString.parseJson
  val questionnaire = Source.fromInputStream(getClass.getResourceAsStream("/questionnaire.json")).mkString.parseJson
  val questionnaire2 = Source.fromInputStream(getClass.getResourceAsStream("/questionnaire2.json")).mkString.parseJson
  val bundle = Source.fromInputStream(getClass.getResourceAsStream("/bundle.json")).mkString.parseJson
  val bundleOp = Source.fromInputStream(getClass.getResourceAsStream("/bundle-op.json")).mkString.parseJson
  val conditions = Seq(
    Source.fromInputStream(getClass.getResourceAsStream("/condition1.json")).mkString.parseJson,
    Source.fromInputStream(getClass.getResourceAsStream("/condition2.json")).mkString.parseJson,
    Source.fromInputStream(getClass.getResourceAsStream("/condition3.json")).mkString.parseJson
  )

  val encounter = Source.fromInputStream(getClass.getResourceAsStream("/encounter.json")).mkString.parseJson

  val patient = Source.fromInputStream(getClass.getResourceAsStream("/patient2.json")).mkString.parseJson

  val emptyBundle = Source.fromInputStream(getClass.getResourceAsStream("/emptybundle.json")).mkString.parseJson

  val medicationAdministration = Source.fromInputStream(getClass.getResourceAsStream("/med-adm.json")).mkString.parseJson

  sequential

  "FHIR Path Evaluator" should {

    "evaluate simple path expression not starting with resource type" in {
      var result = FhirPathEvaluator().evaluate("subject", observation)
      result.length mustEqual 1
      result.head.isInstanceOf[FhirPathComplex] mustEqual true

      result = FhirPathEvaluator().evaluate("subject.reference", observation)
      result.length mustEqual 1
      result.head.isInstanceOf[FhirPathString] mustEqual true
      result.head.asInstanceOf[FhirPathString].s mustEqual "Patient/f001"
    }

    "evaluate simple path expression starting with resource type" in {
      var result = FhirPathEvaluator().evaluate("Observation.subject", observation)
      result.length mustEqual 1
      result.head.isInstanceOf[FhirPathComplex] mustEqual true

      result = FhirPathEvaluator().evaluate("Observation.subject.reference", observation)
      result.length mustEqual 1
      result.head.isInstanceOf[FhirPathString] mustEqual true
      result.head.asInstanceOf[FhirPathString].s mustEqual "Patient/f001"

      result = FhirPathEvaluator().evaluate("Observation.code.coding", observation)
      result.length mustEqual 2

      result = FhirPathEvaluator().evaluate("Observation.code.coding.code", observation)
      result.length mustEqual 2
      result.map(_.asInstanceOf[FhirPathString].s) must contain("15074-8")
      result.map(_.asInstanceOf[FhirPathString].s) must contain("4544556")
    }

    "return empty for non-matching element path" in {
      var result = FhirPathEvaluator().evaluate("Observation.method", observation)
      result must empty

      result = FhirPathEvaluator().evaluate("Observation.method.coding", observation)
      result must empty

      result = FhirPathEvaluator().evaluate("method", observation)
      result must empty
    }

    "evaluate path with array indices" in {
      var result = FhirPathEvaluator().evaluate("Observation.code.coding[0]", observation)
      result.length mustEqual 1

      result = FhirPathEvaluator().evaluate("Observation.code[0].coding[0]", observation)
      result.length mustEqual 1

      result = FhirPathEvaluator().evaluate("Observation.code.coding[0].code", observation)
      result.length mustEqual 1
      result.map(_.asInstanceOf[FhirPathString].s) must contain("15074-8")

      result = FhirPathEvaluator().evaluate("Observation.code.coding[1].code", observation)
      result.length mustEqual 1
      result.map(_.asInstanceOf[FhirPathString].s) must contain("4544556")

      result = FhirPathEvaluator().evaluate("Observation.code.coding[2].code", observation)
      result must empty

      result = FhirPathEvaluator().evaluate("Observation.code[1].coding[0].code", observation)
      result must empty
    }

    "evaluate path type cast ('as')" in {
      var result = FhirPathEvaluator().evaluate("Observation.value as Quantity", observation)
      result.length mustEqual 1

      result = FhirPathEvaluator().evaluate("(Observation.value as Quantity).value", observation)
      result.length mustEqual 1
      result.head.isInstanceOf[FhirPathNumber] mustEqual true
      result.head.asInstanceOf[FhirPathNumber].v mustEqual 6.3

      result = FhirPathEvaluator().evaluate("(Observation.value as CodeableConcept).coding.code", observation)
      result must empty
    }

    "evaluate path with arithmetic operators on numbers" in {
      var result = FhirPathEvaluator().evaluateOptionalNumerical("7.2+3.1", observation)
      result must beSome(10.3)

      result = FhirPathEvaluator().evaluateOptionalNumerical("7.2 + 3.1", observation)
      result must beSome(10.3)

      result = FhirPathEvaluator().evaluateOptionalNumerical("7.1 - 3.1", observation)
      result must beSome(4)

      result = FhirPathEvaluator().evaluateOptionalNumerical("2 * 4 - 1", observation)
      result must beSome(7)

      result = FhirPathEvaluator().evaluateOptionalNumerical("4.2/2 ", observation)
      result must beSome(2.1)

      result = FhirPathEvaluator().evaluateOptionalNumerical("5 div 2 ", observation)
      result must beSome(2)

      result = FhirPathEvaluator().evaluateOptionalNumerical("5 mod 2 ", observation)
      result must beSome(1)

      result = FhirPathEvaluator().evaluateOptionalNumerical("(Observation.value as Quantity).value + 2.2", observation)
      result must beSome(8.5)

      result = FhirPathEvaluator().evaluateOptionalNumerical("(Observation.value as Quantity).value - (Observation.value as Quantity).value", observation)
      result must beSome(0)
    }

    "evaluate path with arithmetic operators on datetime and time" in {
      var result = FhirPathEvaluator().evaluateOptionalDateTime("@2018 + 1 year", observation)
      //Operations on Year
      result must beSome(Year.of(2019))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018 + 2 months", observation)
      result must beSome(Year.of(2018))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018 + 26 months", observation) // Rounded to 2 years
      result must beSome(Year.of(2020))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018 + 365 days", observation) // Rounded to 1 year
      result must beSome(Year.of(2019))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018 - 365 days", observation) // Rounded to 1 year
      result must beSome(Year.of(2017))
      //Operations on YearMonth
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11 + 60 days", observation) // Rounded to 2 month
      result must beSome(YearMonth.of(2019, 1))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11 + 59 days", observation) // Rounded to 1 month
      result must beSome(YearMonth.of(2018, 12))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11 + 2 years", observation) // Rounded to 1 month
      result must beSome(YearMonth.of(2020, 11))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11 - 5 months", observation) // Rounded to 1 month
      result must beSome(YearMonth.of(2018, 6))
      //Operations on Date
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10 + 2 years", observation)
      result must beSome(LocalDate.of(2020, 11, 10))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10 - 2 months", observation)
      result must beSome(LocalDate.of(2018, 9, 10))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10 - 32 days", observation)
      result must beSome(LocalDate.of(2018, 10, 9))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10 + 32 hours", observation) //rounded to 1 day
      result must beSome(LocalDate.of(2018, 11, 11))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10 + 2880 minutes", observation) //rounded to 2 day
      result must beSome(LocalDate.of(2018, 11, 12))
      //Operations on LocalDate
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05 + 2 years", observation)
      result must beSome(LocalDateTime.of(2020, 11, 10, 10, 0, 5))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05 + 2 hours", observation)
      result must beSome(LocalDateTime.of(2018, 11, 10, 12, 0, 5))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05 + 40 minutes", observation)
      result must beSome(LocalDateTime.of(2018, 11, 10, 10, 40, 5))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05 + 30 seconds", observation)
      result must beSome(LocalDateTime.of(2018, 11, 10, 10, 0, 35))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05 + 40 minutes + 30 seconds", observation)
      result must beSome(LocalDateTime.of(2018, 11, 10, 10, 40, 35))
      //Operations on ZonedDateTime
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05Z + 1 month", observation)
      result must beSome(ZonedDateTime.of(2018, 12, 10, 10, 0, 5, 0, ZoneId.of("Z")))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05.003+02:00 + 2 minutes", observation)
      result must beSome(ZonedDateTime.of(2018, 11, 10, 10, 2, 5, 3000000, ZoneId.of("+02:00")))
      result = FhirPathEvaluator().evaluateOptionalDateTime("@2018-11-10T10:00:05.003+02:00 + 2 'ms'", observation)
      result must beSome(ZonedDateTime.of(2018, 11, 10, 10, 0, 5, 5000000, ZoneId.of("+02:00")))

      //Operations on Time
      val (lt, z) = FhirPathEvaluator().evaluateOptionalTime("@T10:00:05 + 1 hour", observation).get
      lt === LocalTime.of(11, 0, 5)
      z must empty

      val (lt2, z2) = FhirPathEvaluator().evaluateOptionalTime("@T10:00:05 + 1 'ms'", observation).get
      lt2 === LocalTime.of(10, 0, 5, 1000000)
      z2 must empty
    }

    "evaluate path with arithmetic operators on string" in {
      var result = FhirPathEvaluator().evaluateString("'ali' + 'veli'", observation)
      result.head mustEqual "aliveli"

      result = FhirPathEvaluator().evaluateString("'ali' + {}", observation)
      result must empty

      result = FhirPathEvaluator().evaluateString("'ali' & {}", observation)
      result.head mustEqual "ali"

      result = FhirPathEvaluator().evaluateString("Observation.code.coding[0].code + 'veli'", observation)
      result.head mustEqual "15074-8veli"

      //Firs item returns multiple strings, so it is erroneous
      FhirPathEvaluator().evaluateString("Observation.code.coding.code + 'veli'", observation) must throwA[Exception]

      FhirPathEvaluator().evaluateString("5 + 'veli'", observation) must throwA[Exception]
    }

    "evaluate path with equality operators" in {
      //On strings
      FhirPathEvaluator().satisfies("Observation.subject.display = 'P. van de Heuvel'", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.subject.display = 'P. van de HEuvel'", observation) mustEqual false // = requires exactly same
      FhirPathEvaluator().satisfies("Observation.subject.display ~ 'P. van de     HEUVEL'", observation) mustEqual true //Ignore case and white spaces
      //On numbers
      FhirPathEvaluator().satisfies("(Observation.value as Quantity).value = 6.3", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.value as Quantity).value = 6.25", observation) mustEqual false
      FhirPathEvaluator().satisfies("(Observation.value as Quantity).value ~ 6.29", observation) mustEqual true //~ means rounding to least precision
      FhirPathEvaluator().satisfies("6.25 ~ 6.2487", observation) mustEqual true //~ means rounding to least precision
      FhirPathEvaluator().satisfies("6.256 ~ 6.2551", observation) mustEqual false
      //On datetime
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start = @2013-04-02T09:30:10+01:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start = @2013-04-02T10:30:10+02:00", observation) mustEqual false // Shoud be exactly same
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start = @2013-04-02T09:40:10+01:00", observation) mustEqual false
      FhirPathEvaluator().evaluate("(Observation.effective as Period).start = @2013-04-02", observation) must empty //comparison with partial returns empty for =
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start != @2013-04-02T09:30:10+01:00", observation) mustEqual false
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start != @2013-04-02T10:30:10+01:00", observation) mustEqual true

      FhirPathEvaluator().satisfies("(Observation.effective as Period).start ~ @2013-04-02T09:30:10+01:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start ~ @2013-04-02T10:30:10+02:00", observation) mustEqual true //equal in terms of temporal
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start ~ @2013-04-02", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start ~ @2013", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start ~ @2013-05", observation) mustEqual false
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start !~ @2013-05", observation) mustEqual true
      //On time
      FhirPathEvaluator().satisfies("@T10:00 = @T10", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T10:00 = @T10:01", observation) mustEqual false
      FhirPathEvaluator().satisfies("@T10 = @T10:01", observation) mustEqual false
      FhirPathEvaluator().satisfies("@T10:00 = @T10:00:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T10:00 ~ @T10:00:00.000", observation) mustEqual true
      //On Complex object
      FhirPathEvaluator().satisfies("Observation.code.coding[0] = Observation.code.coding[1]", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.code.coding[0] = Observation.code.coding[0]", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding = (Observation.code.coding.tail() | Observation.code.coding.take(1))", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.code.coding = (Observation.code.coding.take(1) | Observation.code.coding.tail())", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding ~ (Observation.code.coding.tail() | Observation.code.coding.take(1))", observation) mustEqual true //when ~ order is not important
    }

    "evaluate path with comparison operators" in {
      //On strings
      FhirPathEvaluator().satisfies("Observation.subject.display < 'R'", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.subject.display > 'Asdfsdf'", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.subject.display >= 'P. van de Heuvel'", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.subject.display > 'P. van de Heuvel'", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.subject.display <= 'P. van de Heuvel asdsad'", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.subject.display <= 'P. van de'", observation) mustEqual false
      //On numbers
      FhirPathEvaluator().satisfies("(Observation.value as Quantity).value > 6.3", observation) mustEqual false
      FhirPathEvaluator().satisfies("(Observation.value as Quantity).value >= 6.3", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.value as Quantity).value < 6.5", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.value as Quantity).value <= 6.3", observation) mustEqual true
      //On datetime
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start > @2013-04-01T09:30:10+01:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start > @2013-04-02", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start < @2013-04-03", observation) mustEqual true
      FhirPathEvaluator().satisfies("(Observation.effective as Period).start <= @2013-04-02", observation) mustEqual false
      //On time
      FhirPathEvaluator().satisfies("@T11:00 > @T10:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T11:00:00 > @T10:00:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T11:00:00 <= @T12:00:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T10:00 >= @T12:00:00", observation) mustEqual false
    }

    "evaluate path with collection operators" in {
      // | operator
      var result = FhirPathEvaluator().evaluate("Observation.code.coding | Observation.code.coding", observation) //union should eliminate the duplicates
      result.length mustEqual 2

      result = FhirPathEvaluator().evaluate("Observation.code.coding.combine(Observation.code.coding) | {}", observation) //Union with empty
      result.length mustEqual 2

      // in operator
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code", observation) mustEqual true
      FhirPathEvaluator().satisfies("'15074-9' in Observation.code.coding.code", observation) mustEqual false
      FhirPathEvaluator().satisfies("'15074-9' in {}", observation) mustEqual false
      FhirPathEvaluator().evaluate("{} in Observation.code.coding.code", observation) must empty
      FhirPathEvaluator().evaluate("Observation.code.coding.code in Observation.code.coding.code", observation) must throwA[Exception]

      //contains operator
      FhirPathEvaluator().satisfies("Observation.code.coding.code contains '15074-8'", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.code contains '15074-9'", observation) mustEqual false
      FhirPathEvaluator().satisfies("{} contains '15074-9'", observation) mustEqual false
      FhirPathEvaluator().evaluate("Observation.code.coding.code contains Observation.code.coding.code", observation) must throwA[Exception]
    }

    "evaluate path with boolean logic operators" in {
      // and
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code and 5 > 3", observation) mustEqual true
      FhirPathEvaluator().satisfies("'15074-9' in Observation.code.coding.code and 5 > 3", observation) mustEqual false
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code and 5 < 3", observation) mustEqual false
      FhirPathEvaluator().evaluate("Observation.method and 5 > 3", observation) must empty //emtpy and true --> empty
      FhirPathEvaluator().satisfies("Observation.method and 5 < 3", observation) mustEqual false //empty and false --> false
      FhirPathEvaluator().evaluate("Observation.method and {}", observation) must empty
      //or
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code or 5 < 3", observation) mustEqual true
      FhirPathEvaluator().satisfies("'15074-9' in Observation.code.coding.code or 5 > 3", observation) mustEqual true
      FhirPathEvaluator().satisfies("'15074-9' in Observation.code.coding.code or 5 < 3", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.method or 5 > 3", observation) mustEqual true
      FhirPathEvaluator().evaluate("Observation.method or 5 < 3", observation) must empty
      FhirPathEvaluator().evaluate("Observation.method and {}", observation) must empty
      //xor
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code xor 5 > 3", observation) mustEqual false
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code xor 5 < 3", observation) mustEqual true
      FhirPathEvaluator().satisfies("'15074-9' in Observation.code.coding.code xor 5 > 3", observation) mustEqual true
      FhirPathEvaluator().evaluate("Observation.method xor 5 < 3", observation) must empty
      FhirPathEvaluator().evaluate("5 > 3 xor Observation.method", observation) must empty
      //implies
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code implies 5 > 3", observation) mustEqual true
      FhirPathEvaluator().satisfies("'15074-8' in Observation.code.coding.code implies 5 < 3", observation) mustEqual false
      FhirPathEvaluator().evaluate("'15074-8' in Observation.code.coding.code implies {}", observation) must empty
      FhirPathEvaluator().satisfies("'15074-9' in Observation.code.coding.code implies 5 > 3", observation) mustEqual true
      FhirPathEvaluator().satisfies("'15074-9' in Observation.code.coding.code implies 5 < 3", observation) mustEqual true
      FhirPathEvaluator().evaluate("Observation.method implies {}", observation) must empty
      FhirPathEvaluator().satisfies("Observation.method implies  5 > 3", observation) mustEqual true
      FhirPathEvaluator().evaluate("Observation.method implies 5 < 3", observation) must empty
    }

    "evaluate path with existence function" in {
      //empty()
      FhirPathEvaluator().satisfies("Observation.method.empty()", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.empty()", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.code.coding.empty(5)", observation) must throwA[Exception]
      //not()
      FhirPathEvaluator().satisfies("(5 > 3).not()", observation) mustEqual false
      FhirPathEvaluator().satisfies("(5 < 3).not()", observation) mustEqual true
      FhirPathEvaluator().evaluate("Observation.method.not()", observation) must empty
      //exists
      FhirPathEvaluator().satisfies("Observation.method.exists()", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.code.coding.exists()", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.exists($this.code = '15074-8')", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.exists($this.code = '15074-9')", observation) mustEqual false
      //all
      FhirPathEvaluator().satisfies("Observation.method.all(coding.code = 'asd')", observation) mustEqual true // if empty return true
      FhirPathEvaluator().satisfies("Observation.code.coding.all($this.code = '15074-9')", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.code.coding.all(system.exists())", observation) mustEqual true
      //all true
      FhirPathEvaluator().satisfies("Observation.method.allTrue()", observation) mustEqual true
      FhirPathEvaluator().satisfies("((3 > 2) | (4 > 2) | (5 > 2) ).allTrue()", observation) mustEqual true
      FhirPathEvaluator().satisfies("((3 > 2) | (4 > 2) | (5 < 2) ).allTrue()", observation) mustEqual false
      //any true
      FhirPathEvaluator().satisfies("Observation.method.anyTrue()", observation) mustEqual false
      FhirPathEvaluator().satisfies("((3 < 2) | (4 > 2) | (5 < 2) ).anyTrue()", observation) mustEqual true
      FhirPathEvaluator().satisfies("((3 < 2) | (4 < 2) | (5 < 2) ).anyTrue()", observation) mustEqual false
      //all false
      FhirPathEvaluator().satisfies("Observation.method.allFalse()", observation) mustEqual true
      FhirPathEvaluator().satisfies("((3 < 2) | (4 > 2) | (5 < 2) ).allFalse()", observation) mustEqual false
      FhirPathEvaluator().satisfies("((3 < 2) | (4 < 2) | (5 < 2) ).allFalse()", observation) mustEqual true
      //any false
      FhirPathEvaluator().satisfies("Observation.method.anyFalse()", observation) mustEqual false
      FhirPathEvaluator().satisfies("((3 > 2) | (4 > 2) | (5 > 2) ).anyFalse()", observation) mustEqual false
      FhirPathEvaluator().satisfies("((3 > 2) | (4 > 2) | (5 < 2) ).anyFalse()", observation) mustEqual true
      //subsetof
      FhirPathEvaluator().satisfies("Observation.code.coding.subsetOf(%context.code.coding))", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding[0].subsetOf(%context.code.coding))", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.subsetOf(%context.code.coding[0]))", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.method.subsetOf(%context.code.coding))", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.subsetOf({})", observation) mustEqual false
      //supersetof
      FhirPathEvaluator().satisfies("Observation.code.coding.supersetOf(%context.code.coding))", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.supersetOf(%context.code.coding[0]))", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding[0].supersetOf(%context.code.coding))", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.method.supersetOf(%context.code.coding))", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.code.coding.supersetOf({})", observation) mustEqual true
      //isDistinct
      FhirPathEvaluator().satisfies("Observation.code.coding.isDistinct()", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.method.isDistinct()", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.code.coding.combine(%context.code.coding).isDistinct()", observation) mustEqual false
      //distinct
      FhirPathEvaluator().evaluate("Observation.code.coding.combine(%context.code.coding)", observation).length mustEqual 4
      FhirPathEvaluator().evaluate("Observation.code.coding.combine(%context.code.coding).distinct()", observation).length mustEqual 2
      FhirPathEvaluator().evaluate("Observation.method.distinct()", observation) must empty
      //count
      FhirPathEvaluator().evaluateOptionalNumerical("Observation.code.coding.count()", observation) must beSome(2)
      FhirPathEvaluator().evaluateOptionalNumerical("Observation.method.count()", observation) must beSome(0)
    }

    "evaluate paths with filtering/projection functions" in {
      //where
      FhirPathEvaluator().evaluate("Observation.code.coding.where(code = '15074-8')", observation).length mustEqual 1
      FhirPathEvaluator().evaluate("Observation.code.coding.where({})", observation).length mustEqual 0
      FhirPathEvaluator().evaluate("Observation.code.coding.where(code = '15074-8' and system = %loinc)", observation).length mustEqual 1
      FhirPathEvaluator().evaluate("Observation.code.coding.where(code = '15074-9')", observation).length mustEqual 0
      //select
      FhirPathEvaluator().evaluateString("Observation.code.coding.select(code)", observation).length mustEqual 2
      FhirPathEvaluator().evaluateString("Observation.code.coding.select(code)", observation) must contain("15074-8")
      FhirPathEvaluator().evaluateString("Observation.code.coding.select(x)", observation) must empty
      FhirPathEvaluator().evaluateString("Observation.code.coding.combine(%context.code.coding).select(code)", observation).length mustEqual 4 //flattening
      FhirPathEvaluator().evaluateString("Observation.method.select(coding)", observation) must empty
      //repeat
      FhirPathEvaluator().evaluateString("Questionnaire.repeat(item).linkId", questionnaire).toSet mustEqual Set("1", "2", "2.1", "2.2", "2.3", "2.4", "3", "3.1", "3.2")
      FhirPathEvaluator().evaluateString("Questionnaire.repeat(code).code", questionnaire).toSet mustEqual Set("VL 1-1, 18-65_1.2.2")
      FhirPathEvaluator().evaluate("Questionnaire.repeat(x)", questionnaire) must empty
      //ofType
      FhirPathEvaluator().evaluateOptionalNumerical("Observation.value.ofType(Quantity).value", observation) must beSome(6.3)
      FhirPathEvaluator().evaluate("Observation.method.ofType(Quantity)", observation) must empty
      FhirPathEvaluator().evaluate("Observation.component.value.ofType(Quantity)", observation2) must empty
      FhirPathEvaluator().evaluate("Observation.component.value.ofType(CodeableConcept)", observation2).length mustEqual 5
      FhirPathEvaluator().evaluate("Observation.component.value.ofType(CodeableConcept).coding.code", observation2).length > 5
      FhirPathEvaluator().evaluateOptionalNumerical("Observation.value.as(Quantity).value", observation) must beSome(6.3)
      FhirPathEvaluator().satisfies("Observation.value.is(Quantity)", observation) mustEqual true
    }

    "evaluate paths with subsetting functions" in {
      //single
      FhirPathEvaluator().evaluate("Observation.component.single()", observation2) must throwA[Exception] //if not single should throw exception
      FhirPathEvaluator().evaluate("Observation.method.single()", observation2) must empty //if empty return empty
      FhirPathEvaluator().evaluate("Observation.valueQuantity.single()", observation).length mustEqual 1 //if single return single
      //first
      FhirPathEvaluator().evaluateString("Observation.component.first().code.coding.first().code", observation2).head mustEqual "32411-1"
      FhirPathEvaluator().evaluate("Observation.method.first()", observation2) must empty //if empty return empty
      FhirPathEvaluator().evaluateOptionalNumerical("Observation.valueQuantity.first().value", observation) must beSome(6.3)
      //last
      FhirPathEvaluator().evaluateString("Observation.component.last().code.coding.last().code", observation2).head mustEqual "249224006"
      FhirPathEvaluator().evaluate("Observation.method.last()", observation2) must empty //if empty return empty
      //tail
      FhirPathEvaluator().evaluateString("Observation.component.tail().first().code.coding.first().code", observation2).head mustEqual "32412-9"
      FhirPathEvaluator().evaluate("Observation.method.tail()", observation2) must empty //if empty return empty
      //skip
      FhirPathEvaluator().evaluateString("Observation.component.skip(2).first().code.coding.first().code", observation2).head mustEqual "32414-5"
      FhirPathEvaluator().evaluateString("Observation.component.skip(3).first().code.coding.first().code", observation2).head mustEqual "32413-7"
      FhirPathEvaluator().evaluate("Observation.component.skip(5).first().code.coding.first().code", observation2) must empty
      FhirPathEvaluator().evaluateString("Observation.component.skip(-1).first().code.coding.first().code", observation2).head mustEqual "32411-1"
      //take
      FhirPathEvaluator().evaluateString("Observation.component.take(2).last().code.coding.first().code", observation2).head mustEqual "32412-9"
      FhirPathEvaluator().evaluateString("Observation.component.take(6).last().code.coding.first().code", observation2).head mustEqual "32415-2"
      FhirPathEvaluator().evaluate("Observation.method.take(3)", observation2) must empty //if empty return empty
      //intersect
      FhirPathEvaluator().evaluateString("Observation.component.take(3).code.coding.code.intersect(%context.component.skip(2).code.coding.code)", observation2) mustEqual Seq("32414-5", "249226008")
      //exclude
      FhirPathEvaluator().evaluateString("Observation.component.take(3).code.coding.code.exclude(%context.component.take(2).code.coding.code)", observation2) mustEqual Seq("32414-5", "249226008")
    }
    "evaluate paths with conversion functions" in {
      //iif
      FhirPathEvaluator().evaluateNumerical("Observation.component.iif(code.coding.code contains '32412-9', 5)", observation2) mustEqual Seq(5)
      FhirPathEvaluator().evaluate("Observation.component.iif(code.coding.code contains '32416-9', 5)", observation2) must empty
      FhirPathEvaluator().evaluateNumerical("Observation.component.iif(code.coding.code contains '32416-9', 5, 2)", observation2) mustEqual Seq(2)
      FhirPathEvaluator().evaluateNumerical("Observation.component.iif(ali, 5, 2)", observation2) mustEqual Seq(2) //if criteria returns empty, again false case
      //toInteger
      FhirPathEvaluator().evaluateNumerical("Observation.component.code.coding[1].code.toInteger()", observation2) mustEqual Seq(249227004) //from string
      FhirPathEvaluator().evaluateNumerical("Observation.component.code.coding[1].code.toInteger().toInteger()", observation2) mustEqual Seq(249227004) //from integer
      FhirPathEvaluator().evaluateNumerical("Observation.component.exists().toInteger()", observation2) mustEqual Seq(1) //from boolean
      FhirPathEvaluator().evaluateNumerical("Observation.component.empty().toInteger()", observation2) mustEqual Seq(0) //from boolean
      FhirPathEvaluator().evaluate("Observation.effectiveDateTime.toInteger()", observation2).isEmpty mustEqual true
      //toDecimal
      FhirPathEvaluator().evaluateNumerical("Observation.component.code.coding[1].code.toDecimal()", observation2) mustEqual Seq(249227004) //from string
      FhirPathEvaluator().evaluateNumerical("Observation.valueQuantity.value.toDecimal()", observation) mustEqual Seq(6.3)
      FhirPathEvaluator().evaluateNumerical("Observation.component.exists().toDecimal()", observation2) mustEqual Seq(1) //from boolean
      FhirPathEvaluator().evaluateNumerical("Observation.component.empty().toDecimal()", observation2) mustEqual Seq(0) //from boolean
      //toString
      FhirPathEvaluator().evaluateString("Observation.valueQuantity.value.toString()", observation).head mustEqual "6.3" //from string
      FhirPathEvaluator().evaluateString("Observation.component.exists().toString()", observation2).head mustEqual "'true'" //from boolean
      FhirPathEvaluator().evaluateString("Observation.component.empty().toString()", observation2).head mustEqual "'false'" //from boolean
      FhirPathEvaluator().evaluateString("(1 'U').toString()", observation2).head mustEqual "1.0 'U'" //from boolean
      FhirPathEvaluator().evaluateString("Observation.effectivePeriod.start.toString()", observation).head mustEqual "2013-04-02T09:30:10+01:00" //from boolean
    }

    "evaluate paths with mathematical functions" in {
      FhirPathEvaluator().evaluateOptionalNumerical("Observation.valueQuantity.value.abs()", observation).map(_.toDouble) must beSome(6.3)
      FhirPathEvaluator().evaluateOptionalNumerical("(-5.1).abs()", observation).map(_.toDouble) must beSome(5.1)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.abs()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5 | -3).abs()", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("Observation.valueQuantity.value.ceiling()", observation).map(_.toDouble) must beSome(7)
      FhirPathEvaluator().evaluateOptionalNumerical("(-3.4).ceiling()", observation).map(_.toDouble) must beSome(-3)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.ceiling()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).ceiling()", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("2.exp()", observation).map(_.toDouble) must beSome(Math.exp(2))
      FhirPathEvaluator().evaluateOptionalNumerical("{}.exp()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).exp()", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("(-3.4).floor()", observation).map(_.toDouble) must beSome(-4)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.floor()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).floor()", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("2.exp().ln()", observation).map(_.toDouble) must beSome(2)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.ln()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).ln()", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("100.log(10)", observation).map(_.toDouble) must beSome(2)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.log(10)", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).log(10)", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("2.power(5)", observation).map(_.toDouble) must beSome(32)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.power(10)", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).power(10)", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("2.5.round()", observation).map(_.toDouble) must beSome(3)
      FhirPathEvaluator().evaluateOptionalNumerical("2.4.round()", observation).map(_.toDouble) must beSome(2)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.round()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).round()", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("2.52648.round(2)", observation).map(_.toDouble) must beSome(2.53)
      FhirPathEvaluator().evaluateOptionalNumerical("2.52648.round(3)", observation).map(_.toDouble) must beSome(2.526)
      FhirPathEvaluator().evaluateOptionalNumerical("2.52648.round(4)", observation).map(_.toDouble) must beSome(2.5265)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.round(3)", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).round(3)", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("16.sqrt()", observation).map(_.toDouble) must beSome(4)
      FhirPathEvaluator().evaluateOptionalNumerical("(-1).sqrt()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("{}.sqrt()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).sqrt()", observation) must throwA[FhirPathException]

      FhirPathEvaluator().evaluateOptionalNumerical("2.8.truncate()", observation).map(_.toDouble) must beSome(2)
      FhirPathEvaluator().evaluateOptionalNumerical("{}.truncate()", observation).map(_.toDouble) must beEmpty
      FhirPathEvaluator().evaluateOptionalNumerical("(5.1 | -3.2).truncate(10)", observation) must throwA[FhirPathException]
    }

    "evaluate paths with string manipulation functions" in {
      //indexOf
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.indexOf('Langeveld')", observation) mustEqual Seq(3)
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.indexOf('ali')", observation) mustEqual Seq(-1)
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.indexOf('')", observation) mustEqual Seq(0)
      //substring
      FhirPathEvaluator().evaluateString("Observation.performer.display.substring(3)", observation).head mustEqual "Langeveld"
      FhirPathEvaluator().evaluateString("Observation.performer.display.substring(3, 2)", observation).head mustEqual "La"
      FhirPathEvaluator().evaluateString("Observation.performer.display.substring(3, 17)", observation).head mustEqual "Langeveld"
      FhirPathEvaluator().evaluateString("Observation.performer.display.substring(17, 5)", observation) must empty
      //startsWith
      FhirPathEvaluator().satisfies("Observation.performer.display.startsWith('A. La')", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.performer.display.startsWith('xxx')", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.performer.display.startsWith('')", observation) mustEqual true
      //endsWith
      FhirPathEvaluator().satisfies("Observation.performer.display.endsWith('veld')", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.performer.display.endsWith('xxx')", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.performer.display.endsWith('')", observation) mustEqual true
      //contains
      FhirPathEvaluator().satisfies("Observation.performer.display.contains('ang')", observation) mustEqual true
      FhirPathEvaluator().satisfies("Observation.performer.display.contains('xxx')", observation) mustEqual false
      FhirPathEvaluator().satisfies("Observation.performer.display.contains('')", observation) mustEqual true
      //replace
      FhirPathEvaluator().evaluateString("Observation.performer.display.replace('Lang', 'Lan')", observation).head mustEqual "A. Laneveld"
      FhirPathEvaluator().evaluateString("Observation.performer.display.replace('A. ', '')", observation).head mustEqual "Langeveld"
      FhirPathEvaluator().evaluateString("Observation.performer.display.replace('', 'x')", observation).head mustEqual "xAx.x xLxaxnxgxexvxexlxdx"
      FhirPathEvaluator().evaluateNumerical("'7,48'.replace(',', '.').toDecimal()", observation).head.toDouble mustEqual 7.48
      FhirPathEvaluator().evaluateNumerical("'7.48'.replace(',', '.').toDecimal()", observation).head.toDouble mustEqual 7.48
      //matches
      FhirPathEvaluator().satisfies("Observation.performer.display.matches('.*Lan.*')", observation) mustEqual true
      FhirPathEvaluator().satisfies("'M31.2'.matches('M31\\.[0-3]')", observation) mustEqual true
      FhirPathEvaluator().satisfies("'M31.4'.matches('M31\\.[0-3]')", observation) mustEqual false
      FhirPathEvaluator().satisfies("'K26.4'.matches('K2[5-8](\\..*)?')", observation) mustEqual true
      FhirPathEvaluator().satisfies("'K26'.matches('K2[5-8](\\..*)?')", observation) mustEqual true
      FhirPathEvaluator().satisfies("'C43.5'.matches('C4[0135](\\..*)?')", observation) mustEqual true
      FhirPathEvaluator().satisfies("'C40'.matches('C4[0135](\\..*)?')", observation) mustEqual true
      FhirPathEvaluator().satisfies("'C42.5'.matches('C4[0135](\\..*)?')", observation) mustEqual false
      //replaceMatches
      FhirPathEvaluator().evaluateString("Observation.performer.display.replaceMatches('L.n', 'Lun')", observation).head mustEqual "A. Lungeveld"
      //length
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.length()", observation) mustEqual Seq(12)
      FhirPathEvaluator().evaluate("{}", observation) must empty
      //encode
      FhirPathEvaluator().evaluateString("Observation.status.encode()", observation).head mustEqual "ZmluYWw="
    }
    "evaluate paths with tree navigation functions" in {
      FhirPathEvaluator().evaluate("Questionnaire.children()", questionnaire).length mustEqual 12
      FhirPathEvaluator().evaluate("Questionnaire.children().linkId", questionnaire).length mustEqual 3
      FhirPathEvaluator().evaluate("Questionnaire.descendants().linkId", questionnaire).length mustEqual 9
    }
    "evaluate paths with utility functions" in {
      FhirPathEvaluator().satisfies("today() = @" + LocalDate.now().toString, questionnaire) mustEqual true
      FhirPathEvaluator().satisfies("today() = @" + LocalDate.now().plusDays(2).toString, questionnaire) mustEqual false
      FhirPathEvaluator().satisfies("now() > @2019-09-09T09:59:00+03:00", questionnaire) mustEqual true
    }

    "evaluate paths with backticks" in {
      FhirPathEvaluator().satisfies("text.`div`.exists()", observation)
    }

    "evaluate FHIR path aggregate and index" in {
      FhirPathEvaluator().evaluateNumerical("Observation.valueQuantity.value.aggregate($this + $total, 0)", JArray(List(observation, observation2))) mustEqual Seq(16.3)

      FhirPathEvaluator().evaluateString("('ali' | 'veli' | 'deli').aggregate($total & ',' & $this, {})", observation) mustEqual Seq(",ali,veli,deli")
      FhirPathEvaluator().evaluateString("('ali' | 'veli' | 'deli').aggregate($total & ',' & $this)", observation) mustEqual Seq(",ali,veli,deli")

      FhirPathEvaluator().evaluateString("Observation.code.coding.where($index > 0).code", observation) mustEqual Seq("4544556")

      FhirPathEvaluator().evaluateNumerical("Observation.valueQuantity.value.aggregate($this * ($index+1) + $total, 0)", JArray(List(observation, observation2))) mustEqual Seq(26.3)
    }

    "evaluate special functions" in {
      var referenceResolver: IReferenceResolver = new IReferenceResolver {
        override val resource: Resource = JObject()
        override val bundle: Option[(Option[String], Resource)] = None

        override def resolveReference(reference: FhirReference): Future[Option[Resource]] = {
          Future.apply(
            reference match {
              case FhirLiteralReference(url, "Patient", rid, version) =>
                Some(Source.fromInputStream(getClass.getResourceAsStream("/patient.json")).mkString.parseJson)

              case FhirCanonicalReference("http://example.org", "Questionnaire", "zika-virus-exposure-assessment", Some("2.0"), fragment) =>
                Some(questionnaire2)
              case _ =>
                None
            }
          )
        }

        override def isReferencedResourceExist(reference: FhirReference, profiles: Set[String]): Future[Boolean] = {
          Future.apply(true)
        }
      }

      //Resolving a literal reference
      FhirPathEvaluator(referenceResolver).evaluate("Observation.subject.resolve().gender", observation) mustEqual Seq(FhirPathString("male"))
      FhirPathEvaluator(referenceResolver).evaluate("Observation.subject.resolve().birthDate", observation) mustNotEqual Seq(FhirPathString("1974-12-26"))


      //Resolving a canonical reference
      val ld = LocalDate.of(2016, 11, 14)
      FhirPathEvaluator(referenceResolver).evaluate("Questionnaire.derivedFrom.resolve().date", questionnaire) mustEqual Seq(FhirPathDateTime(ld))

      FhirPathEvaluator(referenceResolver).evaluate("Questionnaire.item.extension.where(url = 'http://example.org/additional-information2').valueAttachment.title", questionnaire2) mustEqual Seq(FhirPathString("Ali"))
      FhirPathEvaluator(referenceResolver).evaluate("Questionnaire.item.extension('http://example.org/additional-information2').valueAttachment.title", questionnaire2) mustEqual Seq(FhirPathString("Ali"))
    }

    "evaluate onfhir aggregation functions" in {
      val evaluator = FhirPathEvaluator().withDefaultFunctionLibraries()
      val results = evaluator.evaluate("groupBy(Condition.subject.reference.substring(8),count())", JArray(conditions.toList))
      results.length mustEqual (2)

      val pids = evaluator.evaluateString("agg:groupBy(Condition.subject.reference.substring(8),count()).where(agg >= 2).bucket", JArray(conditions.toList))
      pids mustEqual (Seq("p1"))

      var results2 = evaluator.evaluateNumerical("groupBy($this.notexist, agg:sum($this.valueQuantity.value))[0].agg", JArray(Seq(observation, observation2).toList))
      results2 mustEqual Seq(16.3)

      results2 = evaluator.evaluateNumerical("groupBy($this.notexist, min($this.valueQuantity.value))[0].agg", JArray(Seq(observation, observation2).toList))
      results2 mustEqual Seq(6.3)

      results2 = evaluator.evaluateNumerical("groupBy($this.notexist, max($this.valueQuantity.value))[0].agg", JArray(Seq(observation, observation2).toList))
      results2 mustEqual Seq(10)
    }

    "evaluate primitive extensions" in {
      val evaluator = FhirPathEvaluator().withDefaultFunctionLibraries()

      val result = evaluator.evaluateBoolean("gender.extension('http://fhir.de/StructureDefinition/gender-amtlich-de').exists()", patient).head
      result mustEqual true
      val result2 = evaluator.evaluateBoolean("birthDate.extension('http://fhir.de/StructureDefinition/birthdate').exists()", patient).head
      result2 mustEqual false
      val result3 = evaluator.evaluateBoolean("address[0].extension('http://example.org/fhir/StructureDefinition/address-verified').exists()", patient).head
      result3 mustEqual true
      val result4 = evaluator.evaluateBoolean("identifier[0].assigner.identifier.extension('http://example.org/fhir/StructureDefinition/identifier-verified').exists()", patient).head
      result4 mustEqual true
    }

    "evaluate new constraints in FHIR 4.0.1" in {
      var result = FhirPathEvaluator().satisfies("empty() or ($this = '*') or (toInteger() >= 0)", JString("*"))
      result mustEqual true

      result = FhirPathEvaluator().satisfies("matches('[^\\\\s\\\\.,:;\\\\\\'\"\\\\/|?!@#$%&*()\\\\[\\\\]{}]{1,64}(\\\\.[^\\\\s\\\\.,:;\\\\\\'\"\\\\/|?!@#$%&*()\\\\[\\\\]{}]{1,64}(\\\\[x\\\\])?(\\\\:[^\\\\s\\\\.]+)?)*')", JString("CodeableConcept.coding"))
      result mustEqual true
    }

    "evaluate empty array cases" in {
      var result = FhirPathEvaluator().satisfies("entry.search.empty() or (type = 'searchset')", emptyBundle)
      result mustEqual true

      result = FhirPathEvaluator().satisfies("entry.all(request.exists() = (%resource.type = 'batch' or %resource.type = 'transaction' or %resource.type = 'history'))", emptyBundle)
      result mustEqual true

      result = FhirPathEvaluator().satisfies("entry.all(response.exists() = (%resource.type = 'batch-response' or %resource.type = 'transaction-response' or %resource.type = 'history'))", emptyBundle)
      result mustEqual true

      result = FhirPathEvaluator().satisfies("(type = 'history') or entry.where(fullUrl.exists()).select(fullUrl&resource.meta.versionId).isDistinct()", emptyBundle)
      result mustEqual true
    }

    "evaluate onfhir utility functions" in {
      val evaluator = FhirPathEvaluator().withDefaultFunctionLibraries()
      var result = evaluator.evaluateNumerical("effectivePeriod.utl:getPeriod(start, @2020-09-07T10:00:00Z, 'years')", observation).head.toLong
      result mustEqual 7

      result = evaluator.evaluateNumerical("effectivePeriod.getPeriod(start, @2013-08-07T10:00:00Z, 'months')", observation).head.toLong
      result mustEqual 4

      result = evaluator.evaluateNumerical("effectivePeriod.getPeriod(start, @2013-04-07T09:30:10+01:00, 'days')", observation).head.toLong
      result mustEqual 5

      //Due to given zoned date time in resource
      evaluator.evaluateNumerical("effectivePeriod.getPeriod(start, @2020-09-07, 'years')", observation) must throwA[FhirPathException]

      result = evaluator.evaluateNumerical("period.getPeriod(start, end, 'days')", encounter).head.toLong
      result mustEqual 9

      evaluator.evaluateNumerical("period.getPeriod(start, end, 'hours')", encounter).head.toInt mustEqual 216

      evaluator.evaluateNumerical("period.getPeriod(s, end, 'days')", encounter) must empty

      var quantity = evaluator.evaluateAndReturnJson("effectivePeriod.utl:getDurationAsQuantityObject(start, @2013-04-02T12:30:10+01:00)", observation).head
      (quantity \ "code").extract[String] mustEqual "min"
      (quantity \ "unit").extract[String] mustEqual "min"
      (quantity \ "system").extract[String] mustEqual "http://unitsofmeasure.org"
      (quantity \ "value").extract[Double] mustEqual 180

      quantity = evaluator.evaluateAndReturnJson("effectivePeriod.utl:getDurationAsQuantityObject(start, @2013-04-05T15:30:10+01:00)", observation).head
      (quantity \ "code").extract[String] mustEqual "d"
      (quantity \ "unit").extract[String] mustEqual "d"
      (quantity \ "system").extract[String] mustEqual "http://unitsofmeasure.org"
      (quantity \ "value").extract[Double] mustEqual 3.25

      quantity = evaluator.evaluateAndReturnJson("effectivePeriod.utl:getDurationAsQuantityObject(start, @2013-07-02T09:30:10+01:00)", observation).head
      (quantity \ "code").extract[String] mustEqual "mo"
      (quantity \ "unit").extract[String] mustEqual "mo"
      (quantity \ "system").extract[String] mustEqual "http://unitsofmeasure.org"
      (quantity \ "value").extract[Double] must be_>(3.0)

      quantity = evaluator.evaluateAndReturnJson("effectivePeriod.utl:getDurationAsQuantityObject(start, @2013-07-02)", observation).head
      (quantity \ "code").extract[String] mustEqual "d"
      (quantity \ "unit").extract[String] mustEqual "d"
      (quantity \ "system").extract[String] mustEqual "http://unitsofmeasure.org"
      (quantity \ "value").extract[Double] mustEqual 90.0

      quantity = evaluator.evaluateAndReturnJson("effectivePeriod.utl:getDurationAsQuantityObject(start, @2013-07)", observation).head
      (quantity \ "code").extract[String] mustEqual "d"
      (quantity \ "unit").extract[String] mustEqual "d"
      (quantity \ "system").extract[String] mustEqual "http://unitsofmeasure.org"
      (quantity \ "value").extract[Double] mustEqual 89.0

      val fhirRef = evaluator.evaluateAndReturnJson("utl:createFhirReference('Observation', id)", observation).head
      fhirRef mustEqual JObject(List("reference" -> JString("Observation/f001")))

      var cdc = evaluator.evaluateAndReturnJson("utl:createFhirCodeableConcept('http://loinc.org', code.coding.code.first(), {})", observation).head
      (cdc \ "coding" \ "code").extract[Seq[String]] mustEqual Seq("15074-8")
      (cdc \ "coding" \ "system").extract[Seq[String]] mustEqual Seq("http://loinc.org")
      (cdc \ "coding" \ "display").extractOpt[String] must beEmpty

      cdc = evaluator.evaluateAndReturnJson("utl:createFhirCodeableConcept('http://loinc.org', code.coding.code.first(), 'Glucose')", observation).head
      (cdc \ "coding" \ "code").extract[Seq[String]] mustEqual Seq("15074-8")
      (cdc \ "coding" \ "system").extract[Seq[String]] mustEqual Seq("http://loinc.org")
      (cdc \ "coding" \ "display").extract[Seq[String]] mustEqual Seq("Glucose")

      val splittedStr = evaluator.evaluateString("code.coding.first().code.utl:split('-')", observation)
      splittedStr mustEqual Seq("15074", "8")

      evaluator.evaluateString("'1+1+2'.utl:split('+')", observation) mustEqual Seq("1", "1", "2")

      val decs = evaluator.evaluateNumerical("code.coding.first().code.utl:split('-').select($this.toDecimal())", observation)
      decs mustEqual Seq(15074, 8)

      var subStr = evaluator.evaluateString("'VERY LOW.'.utl:takeUntil('.' | '*')", observation)
      subStr mustEqual Seq("VERY LOW")
      subStr = evaluator.evaluateString("'VERY LOW*.'.utl:takeUntil('.' | '*')", observation)
      subStr mustEqual Seq("VERY LOW")
      subStr = evaluator.evaluateString("'NORMAL.  LARGE PLATELETS PRESENT.'.utl:takeUntil('.' | '*')", observation)
      subStr mustEqual Seq("NORMAL")

      var qnt = evaluator.evaluateAndReturnJson("utl:createFhirQuantity(15.2, 'mg')", observation).head
      (qnt \ "unit").extract[String] mustEqual "mg"
      (qnt \ "system").extractOpt[String] must beEmpty
      (qnt \ "value").extract[Double] mustEqual 15.2

      qnt = evaluator.evaluateAndReturnJson("utl:createFhirQuantity(15.2, %ucum, 'mg')", observation).head
      (qnt \ "unit").extract[String] mustEqual "mg"
      (qnt \ "system").extract[String] mustEqual "http://unitsofmeasure.org"
      (qnt \ "value").extract[Double] mustEqual 15.2

      var indices = evaluator.evaluateNumerical("utl:indices(1, 10)", observation)
      indices.map(_.toInt) mustEqual (1 to 10)

      indices = evaluator.evaluateNumerical("Observation.code.coding.utl:indicesWhere($this.system='http://snomed.info/sct')", observation)
      indices.map(_.toInt) mustEqual Seq(2)

      val codings = evaluator.evaluateAndReturnJson("utl:evaluateExpression(utl:indices(0, 1).select('code.coding[' & $this.toString() &']').mkString(' | '))", observation)
      codings must beSome

      val fhirDateTime = evaluator.evaluateDateTime("'2012-01-13 22:10:45'.utl:toFhirDateTime()", JNull)
      val dt = LocalDateTime.from(fhirDateTime.head)
      dt.getYear mustEqual 2012
      dt.getDayOfMonth mustEqual 13
      dt.getMinute mustEqual 10

      val fhirDateTimeParam = evaluator.evaluateDateTime("'20120113.22:10:45'.utl:toFhirDateTime('yyyy-MM-ddHH:mm:ss' | 'yyyyMMdd.HH:mm:ss')", JNull)
      val dtParam = LocalDateTime.from(fhirDateTimeParam.head)
      dtParam.getYear mustEqual 2012
      dtParam.getDayOfMonth mustEqual 13
      dtParam.getMinute mustEqual 10

      val fhirDate = evaluator.evaluateDateTime("'20120113'.utl:toFhirDateTime('yyyyMMdd' | 'yyyyMMdd.HH:mm:ss')", JNull)
      fhirDate.head mustEqual LocalDate.of(2012, 1, 13)
    }

    "evaluate fixed bugs" in {
      var result = FhirPathEvaluator().satisfies("(type='history') or entry.where(fullUrl.exists()).select(fullUrl&resource.meta.versionId).isDistinct()", bundleOp)
      result mustEqual true
    }

    "find paths indicated by FHIR Path expression" in {
      var result = FhirPathEvaluator().evaluateToFindPaths("Observation.code", observation)
      result.length mustEqual (1)
      result.head.length mustEqual (1)
      result.head.head mustEqual "code" -> None

      result = FhirPathEvaluator().evaluateToFindPaths("Observation.code.coding", observation)
      result.length mustEqual 1
      result.head mustEqual Seq("code" -> None, "coding" -> None)

      result = FhirPathEvaluator().evaluateToFindPaths("Observation.code.coding.system", observation)
      result.length mustEqual 2
      result.head mustEqual Seq("code" -> None, "coding" -> Some(0), "system" -> None)
      result.last mustEqual Seq("code" -> None, "coding" -> Some(1), "system" -> None)

      result = FhirPathEvaluator().evaluateToFindPaths("code.coding[0]", observation)
      result.length mustEqual 1
      result.head mustEqual Seq("code" -> None, "coding" -> Some(0))

      result = FhirPathEvaluator().evaluateToFindPaths("code.coding.where(system = 'http://snomed.info/sct')", observation)
      result.length mustEqual 1
      result.head mustEqual Seq("code" -> None, "coding" -> Some(1))

      result = FhirPathEvaluator().evaluateToFindPaths("code.coding.first()", observation)
      result.length mustEqual 1
      result.head mustEqual Seq("code" -> None, "coding" -> Some(0))

      result = FhirPathEvaluator().evaluateToFindPaths("(value as Quantity).value", observation)
      result.length mustEqual 1
      result.head mustEqual Seq("valueQuantity" -> None, "value" -> None)

      result = FhirPathEvaluator().evaluateToFindPaths("value.ofType(Quantity).value", observation)
      result.length mustEqual 1
      result.head mustEqual Seq("valueQuantity" -> None, "value" -> None)

      result = FhirPathEvaluator().evaluateToFindPaths("value.as(Quantity).value", observation)
      result.length mustEqual 1
      result.head mustEqual Seq("valueQuantity" -> None, "value" -> None)
    }

    "get path items and restrictions defined on them indicated by FHIR Path path expression" in {
      //Simple expression
      var result = FhirPathEvaluator().getPathItemsWithRestrictions("ActivityDefinition.useContext.code")
      result mustEqual Seq("ActivityDefinition" -> Nil, "useContext" -> Nil, "code" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("code")
      result mustEqual Seq("code" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("ActivityDefinition.relatedArtifact.where(type='composed-of').resource")
      result mustEqual Seq("ActivityDefinition" -> Nil, "relatedArtifact" -> Seq("type" -> "composed-of"), "resource" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("ActivityDefinition.relatedArtifact.where(type='true').resource")
      result mustEqual Seq("ActivityDefinition" -> Nil, "relatedArtifact" -> Seq("type" -> "true"), "resource" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("ActivityDefinition.relatedArtifact.where(type='composed-of' and x='ali').resource")
      result mustEqual Seq("ActivityDefinition" -> Nil, "relatedArtifact" -> Seq("type" -> "composed-of", "x" -> "ali"), "resource" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("Condition.abatement.as(Age)")
      result mustEqual Seq("Condition" -> Nil, "abatementAge" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("Observation.value.as(string)")
      result mustEqual Seq("Observation" -> Nil, "valueString" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("(ActivityDefinition.useContext.value as CodeableConcept)")
      result mustEqual Seq("ActivityDefinition" -> Nil, "useContext" -> Nil, "valueCodeableConcept" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("Account.subject.where(resolve() is Patient)")
      result mustEqual Seq("Account" -> Nil, "subject" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("Bundle.entry[0].resource")
      result mustEqual Seq("Bundle" -> Nil, "entry[0]" -> Nil, "resource" -> Nil)

      result = FhirPathEvaluator().getPathItemsWithRestrictions("Observation.extension('http://a.b.com/x').extension('c').value as Quantity")
      result mustEqual Seq("Observation" -> Nil, "extension" -> Seq("url" -> "http://a.b.com/x"), "extension" -> Seq("url" -> "c"), "valueQuantity" -> Nil)

      FhirPathEvaluator().getPathItemsWithRestrictions("Observation.where(code.coding.first.code='x').valueQuantity") must throwA[FhirPathException]
      FhirPathEvaluator().getPathItemsWithRestrictions("Observation.code or Observation.component.code") must throwA[FhirPathException]
      //Special case "contains" is a keyword in FHIR path grammar
      result = FhirPathEvaluator().getPathItemsWithRestrictions("ValueSet.expansion.contains.code")
      result.length mustEqual 4

      result = FhirPathEvaluator().getPathItemsWithRestrictions("Bundle.entry[0].resource as Composition")
      result mustEqual Seq("Bundle" -> Nil, "entry[0]" -> Nil, "resource" -> Nil)
    }

    "parse literal values" in {
      FhirPathLiteralEvaluator.parseFhirQuantity("1 'mg'") must beSome(FhirPathQuantity(FhirPathNumber(1), "mg"))
      FhirPathLiteralEvaluator.parseFhirQuantity("3 days") must beSome(FhirPathQuantity(FhirPathNumber(3), "d"))
    }

    "check with non-fhir-content" in {
      val fhirPath = "iif(icd_code.length()=3 or (icd_code.startsWith('E') and icd_version=9 and icd_code.length()=4), icd_code, iif(icd_code.startsWith('E') and icd_version=9, icd_code.substring(0,4)&'.'&icd_code.substring(4), icd_code.substring(0,3) & '.' & icd_code.substring(3)))"
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("0010"), "icd_version" -> JInt(9))).headOption must beSome("001.0")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("00329"), "icd_version" -> JInt(9))).headOption must beSome("003.29")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("037"), "icd_version" -> JInt(9))).headOption must beSome("037")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("C505"), "icd_version" -> JInt(10))).headOption must beSome("C50.5")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("C5051"), "icd_version" -> JInt(10))).headOption must beSome("C50.51")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("C50511"), "icd_version" -> JInt(10))).headOption must beSome("C50.511")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("C51"), "icd_version" -> JInt(10))).headOption must beSome("C51")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("E8582"), "icd_version" -> JInt(9))).headOption must beSome("E858.2")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("E8582"), "icd_version" -> JInt(10))).headOption must beSome("E85.82")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("V9081"), "icd_version" -> JInt(9))).headOption must beSome("V90.81")
      new FhirPathEvaluator(isContentFhir = false).evaluateString(fhirPath, JObject("icd_code" -> JString("V9081"), "icd_version" -> JInt(10))).headOption must beSome("V90.81")
    }

    /**
     * This a integration test and needs a terminology service
     * "evaluate terminology service functions" in {
     * //Test with LOINC's terminology server
     * val baseUrl = "https://fhir.loinc.org/"
     * val username = "<FILL HERE>"
     * val password = "<FILL HERE>"
     * implicit val actorSystem = ActorSystem("FhirPathTest")
     * val terminologyServiceClient = new TerminologyServiceClient(OnFhirNetworkClient.apply(baseUrl, new BasicAuthenticationInterceptor(username, password)))
     * val evaluator = FhirPathEvaluator().withTerminologyService(terminologyServiceClient)
     *
     * var result = evaluator.evaluate("%terminologies.lookup(Observation.code.coding.where(system='http://loinc.org').first(), {})", observation)
     * result.nonEmpty shouldEqual(true)
     *
     * var loincDisplay = evaluator.evaluateOptionalString("%terminologies.lookup(Observation.code.coding.where(system='http://loinc.org').first(), {}).parameter.where(name='display').valueString", observation)
     * loincDisplay shouldEqual Some("Glucose [Moles/volume] in Blood")
     *
     * loincDisplay = evaluator.evaluateOptionalString("trms:lookupDisplay('15074-8', 'http://loinc.org', {})", observation)
     * loincDisplay shouldEqual Some("Glucose [Moles/volume] in Blood")
     *
     * val foundMatching = evaluator.evaluateOptionalBoolean("%terminologies.translate('https://www.ncbi.nlm.nih.gov/clinvar', Observation.code.coding.where(system='http://loinc.org').first(), {}).parameter.exists(name='result' and valueBoolean=true)", observation)
     * foundMatching shouldEqual Some(true)
     *
     * result = evaluator.evaluate("trms:translateToCoding(Observation.code.coding.where(system='http://loinc.org').first(), 'https://www.ncbi.nlm.nih.gov/clinvar')", observation)
     * result.length shouldEqual(2)
     * } */
    /**
     * This a integration test and needs a fhir server
     * "evaluate identity service functions" in {
     * val baseUrl = "http://localhost:8080/fhir"
     * implicit val actorSystem = ActorSystem("FhirPathTest")
     * val onFhirClient = OnFhirNetworkClient.apply(baseUrl)
     * val identityService = new IdentityServiceClient(onFhirClient)
     * val evaluator = FhirPathEvaluator().withIdentityService(identityService)
     *
     * evaluator.evaluateOptionalString("idxs:resolveIdentifier('Patient', '12345', 'urn:oid:1.2.36.146.595.217.0.1')", observation) shouldEqual Some("580daafe-bed7-43db-a74b-e8d74a62eeb0")
     * } */

    "evaluate further" in {
      val result = new FhirPathEvaluator(isContentFhir = true).evaluateOptionalString("MedicationAdministration.medicationCodeableConcept.coding.where(system='http://www.whocc.no/atc').first().code.select(iif($this.exists($this.startsWith('D') or $this.startsWith('S') or $this.startsWith('V') or $this.startsWith('G') or $this.startsWith('A')), $this.substring(0,1), $this.substring(0,3)))", medicationAdministration)
      result must beSome("C02")
    }

    "evaluate comparable operation" in {
      val result = FhirPathEvaluator().evaluate("(Observation.valueQuantity.toQuantity()).comparable(Observation.referenceRange.high.toQuantity())", observation)
      result.length mustEqual 1
      result.head.isInstanceOf[FhirPathBoolean] mustEqual true
      result.head.asInstanceOf[FhirPathBoolean].b mustEqual true
    }

    "evaluate boundary operations" in {
      val evaluator = FhirPathEvaluator().withDefaultFunctionLibraries()

      // lowBoundary on numbers
      evaluator.evaluateNumerical("2.386.lowBoundary(3)", JNull).headOption.map(_.toString()) must beSome("2.38550")
      evaluator.evaluateNumerical("2.386.lowBoundary()", JNull).headOption.map(_.toString()) must beSome("2.38550")
      evaluator.evaluateNumerical("2.386.lowBoundary(3)", JNull).headOption must beSome(BigDecimal(2.38550))
      evaluator.evaluateNumerical("2.386.lowBoundary()", JNull).headOption must beSome(BigDecimal(2.38550))
      evaluator.evaluateNumerical("2.386.lowBoundary(7)", JNull).headOption.map(_.toString()) must beSome("2.3855000")
      evaluator.evaluateNumerical("2.386.lowBoundary(8)", JNull).headOption.map(_.toString()) must beSome("2.38550000")
      evaluator.evaluateNumerical("2.1.lowBoundary(4)", JNull).headOption.map(_.toString()) must beSome("2.0500")
      val two = evaluator.evaluateNumerical("2.lowBoundary()", JNull).headOption
      two must beSome(BigDecimal(1.95))
      evaluator.evaluateNumerical("0.0.lowBoundary()", JNull).headOption must beSome(BigDecimal(-0.05))

      // lowBoundary on dates and times
      YearMonth.from(evaluator.evaluateDateTime("'2018'.utl:toFhirDateTime('yyyy').lowBoundary(6)", JNull).head).format(DateTimeFormatter.ofPattern("yyyy-MM")) mustEqual "2018-01"
      LocalDate.from(evaluator.evaluateDateTime("'2015'.utl:toFhirDateTime('yyyy').lowBoundary(8)", JNull).head).format(DateTimeFormatter.ofPattern("yyyy-MM-dd")) mustEqual "2015-01-01"
      LocalDate.from(evaluator.evaluateDateTime("'2015-03'.utl:toFhirDateTime('yyyy-MM').lowBoundary(8)", JNull).head).format(DateTimeFormatter.ofPattern("yyyy-MM-dd")) mustEqual "2015-03-01"
      LocalDateTime.from(evaluator.evaluateDateTime("'2015-03-0307'.utl:toFhirDateTime('yyyy-MM-ddHH').lowBoundary(17)", JNull).head).format(DateTimeFormatter.ISO_DATE_TIME) mustEqual "2015-03-03T07:00:00"
      LocalDateTime.from(evaluator.evaluateDateTime("'2015'.utl:toFhirDateTime('yyyy').lowBoundary()", JNull).head).format(DateTimeFormatter.ISO_DATE_TIME) mustEqual "2015-01-01T00:00:00"
      evaluator.evaluateOptionalTime("'09:05'.utl:toFhirTime('HH:mm').lowBoundary()", JNull).head._1.format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS")) mustEqual "09:05:00.000"
      evaluator.evaluateOptionalTime("'09:05:00'.utl:toFhirTime().lowBoundary(9)", JNull).head._1.format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS")) mustEqual "09:05:00.000"

      // lowBoundary on numbers
      evaluator.evaluateNumerical("2.386.highBoundary(3)", JNull).headOption.map(_.toString()) must beSome("2.38650")
      evaluator.evaluateNumerical("2.386.highBoundary()", JNull).headOption.map(_.toString()) must beSome("2.38650")
      evaluator.evaluateNumerical("2.386.highBoundary(7)", JNull).headOption.map(_.toString()) must beSome("2.3865000")
      evaluator.evaluateNumerical("2.386.highBoundary(8)", JNull).headOption.map(_.toString()) must beSome("2.38650000")
      evaluator.evaluateNumerical("1.95.highBoundary()", JNull).headOption.map(_.toString()) must beSome("1.955")

      // highBoundary on dates and times
      YearMonth.from(evaluator.evaluateDateTime("'2018'.utl:toFhirDateTime('yyyy').highBoundary(6)", JNull).head).format(DateTimeFormatter.ofPattern("yyyy-MM")) mustEqual "2018-12"
      LocalDate.from(evaluator.evaluateDateTime("'2015'.utl:toFhirDateTime('yyyy').highBoundary(8)", JNull).head).format(DateTimeFormatter.ofPattern("yyyy-MM-dd")) mustEqual "2015-12-31"
      LocalDate.from(evaluator.evaluateDateTime("'2015-03'.utl:toFhirDateTime('yyyy-MM').highBoundary(8)", JNull).head).format(DateTimeFormatter.ofPattern("yyyy-MM-dd")) mustEqual "2015-03-31"
      LocalDate.from(evaluator.evaluateDateTime("'1985-02'.utl:toFhirDateTime('yyyy-MM').highBoundary(8)", JNull).head).format(DateTimeFormatter.ofPattern("yyyy-MM-dd")) mustEqual "1985-02-28"
      LocalDateTime.from(evaluator.evaluateDateTime("'2015'.utl:toFhirDateTime('yyyy').highBoundary()", JNull).head).format(DateTimeFormatter.ISO_DATE_TIME) mustEqual "2015-12-31T23:59:59"
      evaluator.evaluateOptionalTime("'09:05'.utl:toFhirTime('HH:mm').highBoundary()", JNull).head._1.format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS")) mustEqual "09:05:00.999"
    }
  }
}

package io.onfhir.path

import java.time.{LocalDate, LocalDateTime, LocalTime, Year, YearMonth, ZoneId, ZonedDateTime}

import io.onfhir.api.Resource
import io.onfhir.api.model.{FhirCanonicalReference, FhirLiteralReference, FhirReference}
import io.onfhir.api.validation.IReferenceResolver
import io.onfhir.util.JsonFormatter._
import org.json4s.JsonAST.{JArray, JObject, JString}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.io.Source

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

  val emptyBundle = Source.fromInputStream(getClass.getResourceAsStream("/emptybundle.json")).mkString.parseJson

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
      var result = FhirPathEvaluator().evaluateNumerical("7.2+3.1", observation)
      result mustEqual 10.3

      result = FhirPathEvaluator().evaluateNumerical("7.2 + 3.1", observation)
      result mustEqual 10.3

      result = FhirPathEvaluator().evaluateNumerical("7.1 - 3.1", observation)
      result mustEqual 4

      result = FhirPathEvaluator().evaluateNumerical("2 * 4 - 1", observation)
      result mustEqual 7

      result = FhirPathEvaluator().evaluateNumerical("4.2/2 ", observation)
      result mustEqual 2.1

      result = FhirPathEvaluator().evaluateNumerical("5 div 2 ", observation)
      result mustEqual 2

      result = FhirPathEvaluator().evaluateNumerical("5 mod 2 ", observation)
      result mustEqual 1

      result = FhirPathEvaluator().evaluateNumerical("(Observation.value as Quantity).value + 2.2", observation)
      result mustEqual 8.5

      result = FhirPathEvaluator().evaluateNumerical("(Observation.value as Quantity).value - (Observation.value as Quantity).value", observation)
      result mustEqual 0
    }

    "evaluate path with arithmetic operators on datetime and time" in {
      var result = FhirPathEvaluator().evaluateDateTime("@2018 + 1 year", observation)
      //Operations on Year
      result mustEqual Year.of(2019)
      result = FhirPathEvaluator().evaluateDateTime("@2018 + 2 months", observation)
      result mustEqual Year.of(2018)
      result = FhirPathEvaluator().evaluateDateTime("@2018 + 26 months", observation) // Rounded to 2 years
      result mustEqual Year.of(2020)
      result = FhirPathEvaluator().evaluateDateTime("@2018 + 365 days", observation) // Rounded to 1 year
      result mustEqual Year.of(2019)
      result = FhirPathEvaluator().evaluateDateTime("@2018 - 365 days", observation) // Rounded to 1 year
      result mustEqual Year.of(2017)
      //Operations on YearMonth
      result = FhirPathEvaluator().evaluateDateTime("@2018-11 + 60 days", observation) // Rounded to 2 month
      result mustEqual YearMonth.of(2019, 1)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11 + 59 days", observation) // Rounded to 1 month
      result mustEqual YearMonth.of(2018, 12)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11 + 2 years", observation) // Rounded to 1 month
      result mustEqual YearMonth.of(2020, 11)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11 - 5 months", observation) // Rounded to 1 month
      result mustEqual YearMonth.of(2018, 6)
      //Operations on Date
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10 + 2 years", observation)
      result mustEqual LocalDate.of(2020, 11, 10)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10 - 2 months", observation)
      result mustEqual LocalDate.of(2018, 9, 10)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10 - 32 days", observation)
      result mustEqual LocalDate.of(2018, 10, 9)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10 + 32 hours", observation) //rounded to 1 day
      result mustEqual LocalDate.of(2018, 11, 11)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10 + 2880 minutes", observation) //rounded to 2 day
      result mustEqual LocalDate.of(2018, 11, 12)
      //Operations on LocalDate
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05 + 2 years", observation)
      result mustEqual LocalDateTime.of(2020, 11, 10, 10, 0, 5)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05 + 2 hours", observation)
      result mustEqual LocalDateTime.of(2018, 11, 10, 12, 0, 5)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05 + 40 minutes", observation)
      result mustEqual LocalDateTime.of(2018, 11, 10, 10, 40, 5)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05 + 30 seconds", observation)
      result mustEqual LocalDateTime.of(2018, 11, 10, 10, 0, 35)
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05 + 40 minutes + 30 seconds", observation)
      result mustEqual LocalDateTime.of(2018, 11, 10, 10, 40, 35)
      //Operations on ZonedDateTime
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05Z + 1 month", observation)
      result mustEqual ZonedDateTime.of(2018, 12, 10, 10, 0, 5, 0, ZoneId.of("Z"))
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05.003+02:00 + 2 minutes", observation)
      result mustEqual ZonedDateTime.of(2018, 11, 10, 10, 2, 5, 3000000, ZoneId.of("+02:00"))
      result = FhirPathEvaluator().evaluateDateTime("@2018-11-10T10:00:05.003+02:00 + 2 'ms'", observation)
      result mustEqual ZonedDateTime.of(2018, 11, 10, 10, 0, 5, 5000000, ZoneId.of("+02:00"))

      //Operations on Time
      val (lt, z) = FhirPathEvaluator().evaluateTime("@T10:00:05 + 1 hour", observation)
      lt mustEqual LocalTime.of(11, 0, 5)
      z must empty

      val (lt2, z2) = FhirPathEvaluator().evaluateTime("@T10:00:05 + 1 'ms'", observation)
      lt2 mustEqual LocalTime.of(10, 0, 5, 1000000)
      z2 must empty

      val (lt3, z3) = FhirPathEvaluator().evaluateTime("@T10:00:05+02:00 + 1 'ms'", observation)
      lt3 mustEqual LocalTime.of(10, 0, 5, 1000000)
      z3 mustEqual Some(ZoneId.of("+02:00"))
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
      FhirPathEvaluator().satisfies("@T10:00 = @T10:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T10:00 = @T10:01", observation) mustEqual false
      FhirPathEvaluator().satisfies("@T10 = @T10:01", observation) mustEqual false
      FhirPathEvaluator().satisfies("@T10:00 = @T10:00:00Z", observation) mustEqual false
      FhirPathEvaluator().satisfies("@T10:00 ~ @T10:00:00Z", observation) mustEqual true
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
      FhirPathEvaluator().satisfies("@T11:00:00Z > @T10:00:00+01:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T11:00:00Z <= @T12:00:00+01:00", observation) mustEqual true
      FhirPathEvaluator().satisfies("@T10:00:00Z >= @T12:00:00+01:00", observation) mustEqual false
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
      FhirPathEvaluator().evaluateNumerical("Observation.code.coding.count()", observation) mustEqual 2
      FhirPathEvaluator().evaluateNumerical("Observation.method.count()", observation) mustEqual 0
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
      FhirPathEvaluator().evaluateNumerical("Observation.value.ofType(Quantity).value", observation) mustEqual 6.3
      FhirPathEvaluator().evaluate("Observation.method.ofType(Quantity)", observation) must empty
      FhirPathEvaluator().evaluate("Observation.component.value.ofType(Quantity)", observation2) must empty
      FhirPathEvaluator().evaluate("Observation.component.value.ofType(CodeableConcept)", observation2).length mustEqual 5
      FhirPathEvaluator().evaluate("Observation.component.value.ofType(CodeableConcept).coding.code", observation2).length > 5
      FhirPathEvaluator().evaluateNumerical("Observation.value.as(Quantity).value", observation) mustEqual 6.3
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
      FhirPathEvaluator().evaluateNumerical("Observation.valueQuantity.first().value", observation) mustEqual 6.3
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
      FhirPathEvaluator().evaluateNumerical("Observation.component.iif(code.coding.code contains '32412-9', 5)", observation2) mustEqual 5
      FhirPathEvaluator().evaluate("Observation.component.iif(code.coding.code contains '32416-9', 5)", observation2) must empty
      FhirPathEvaluator().evaluateNumerical("Observation.component.iif(code.coding.code contains '32416-9', 5, 2)", observation2) mustEqual 2
      FhirPathEvaluator().evaluateNumerical("Observation.component.iif(ali, 5, 2)", observation2) mustEqual 2 //if criteria returns empty, again false case
      //toInteger
      FhirPathEvaluator().evaluateNumerical("Observation.component.code.coding[1].code.toInteger()", observation2) mustEqual 249227004 //from string
      FhirPathEvaluator().evaluateNumerical("Observation.component.code.coding[1].code.toInteger().toInteger()", observation2) mustEqual 249227004 //from integer
      FhirPathEvaluator().evaluateNumerical("Observation.component.exists().toInteger()", observation2) mustEqual 1 //from boolean
      FhirPathEvaluator().evaluateNumerical("Observation.component.empty().toInteger()", observation2) mustEqual 0 //from boolean
      FhirPathEvaluator().evaluate("Observation.effectiveDateTime.toInteger()", observation2) must throwA[Exception]
      //toDecimal
      FhirPathEvaluator().evaluateNumerical("Observation.component.code.coding[1].code.toDecimal()", observation2) mustEqual 249227004 //from string
      FhirPathEvaluator().evaluateNumerical("Observation.valueQuantity.value.toDecimal()", observation) mustEqual 6.3
      FhirPathEvaluator().evaluateNumerical("Observation.component.exists().toDecimal()", observation2) mustEqual 1 //from boolean
      FhirPathEvaluator().evaluateNumerical("Observation.component.empty().toDecimal()", observation2) mustEqual 0 //from boolean
      //toString
      FhirPathEvaluator().evaluateString("Observation.valueQuantity.value.toString()", observation).head mustEqual "6.3" //from string
      FhirPathEvaluator().evaluateString("Observation.component.exists().toString()", observation2).head mustEqual "'true'" //from boolean
      FhirPathEvaluator().evaluateString("Observation.component.empty().toString()", observation2).head mustEqual "'false'" //from boolean
      FhirPathEvaluator().evaluateString("(1 'U').toString()", observation2).head mustEqual "1.0 'U'" //from boolean
      FhirPathEvaluator().evaluateString("Observation.effectivePeriod.start.toString()", observation).head mustEqual "2013-04-02T09:30:10.000+01:00" //from boolean
    }
    "evaluate paths with string manipulation functions" in {
      //indexOf
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.indexOf('Langeveld')", observation) mustEqual 3
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.indexOf('ali')", observation) mustEqual -1
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.indexOf('')", observation) mustEqual 0
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
      //matches
      FhirPathEvaluator().satisfies("Observation.performer.display.matches('.*Lan.*')", observation) mustEqual true
      //replaceMatches
      FhirPathEvaluator().evaluateString("Observation.performer.display.replaceMatches('L.n', 'Lun')", observation).head mustEqual "A. Lungeveld"
      //length
      FhirPathEvaluator().evaluateNumerical("Observation.performer.display.length()", observation) mustEqual 12
      FhirPathEvaluator().evaluate("{}", observation) must empty
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

    "evaluate special functions" in {
      var referenceResolver: IReferenceResolver = new IReferenceResolver {
        override val resource: Resource = JObject()
        override val bundle: Option[(String, Resource)] = None
        override def resolveReference(reference: FhirReference): Option[Resource] = {
           reference match {
             case FhirLiteralReference(url, "Patient", rid, version) =>
               Some(Source.fromInputStream(getClass.getResourceAsStream("/patient.json")).mkString.parseJson)

             case FhirCanonicalReference("http://example.org", "Questionnaire", "zika-virus-exposure-assessment", Some("2.0"), fragment) =>
               Some(questionnaire2)
             case _ =>
               None
           }
        }
        override def isReferencedResourceExist(reference: FhirReference, profiles: Set[String]): Boolean = {
          true
        }
      }

      //Resolving a literal reference
      FhirPathEvaluator(referenceResolver).evaluate("Observation.subject.resolve().gender", observation) mustEqual Seq(FhirPathString("male"))
      FhirPathEvaluator(referenceResolver).evaluate("Observation.subject.resolve().birthDate", observation) mustNotEqual  Seq(FhirPathString("1974-12-26"))
      //Resolving a canonical reference
      val ld = LocalDate.of(2016,11,14)
      FhirPathEvaluator(referenceResolver).evaluate("Questionnaire.derivedFrom.resolve().date", questionnaire) mustEqual Seq(FhirPathDateTime(ld))

      FhirPathEvaluator(referenceResolver).evaluate("Questionnaire.item.extension.where(url = 'http://example.org/additional-information2').valueAttachment.title", questionnaire2)  mustEqual Seq(FhirPathString("Ali"))
      FhirPathEvaluator(referenceResolver).evaluate("Questionnaire.item.extension('http://example.org/additional-information2').valueAttachment.title", questionnaire2) mustEqual Seq(FhirPathString("Ali"))
    }

    "evaluate groupBy and aggregations" in {
      val results = FhirPathEvaluator().evaluate("groupBy(Condition.subject.reference.substring(8),count())", JArray(conditions.toList))
      results.length mustEqual(2)

      val pids = FhirPathEvaluator().evaluateString("groupBy(Condition.subject.reference.substring(8),count()).where(agg >= 2).bucket", JArray(conditions.toList))
      pids mustEqual(Seq("p1"))

      var results2 = FhirPathEvaluator().evaluateNumerical("groupBy($this.notexist, sum($this.valueQuantity.value))[0].agg", JArray(Seq(observation, observation2).toList))
      results2 mustEqual 16.3

      results2 = FhirPathEvaluator().evaluateNumerical("groupBy($this.notexist, min($this.valueQuantity.value))[0].agg", JArray(Seq(observation, observation2).toList))
      results2 mustEqual 6.3

      results2 = FhirPathEvaluator().evaluateNumerical("groupBy($this.notexist, max($this.valueQuantity.value))[0].agg", JArray(Seq(observation, observation2).toList))
      results2 mustEqual 10
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

    "evaluate onfhir added time functions" in {
      var result = FhirPathEvaluator().evaluateNumerical("effectivePeriod.getPeriod(start, @2020-09-07T10:00:00Z, 'years')", observation).toLong
      result mustEqual 7

      result = FhirPathEvaluator().evaluateNumerical("effectivePeriod.getPeriod(start, now(), 'years')", observation).toLong
      result mustEqual 7

      result = FhirPathEvaluator().evaluateNumerical("effectivePeriod.getPeriod(start, @2013-08-07T10:00:00Z, 'months')", observation).toLong
      result mustEqual 4

      result = FhirPathEvaluator().evaluateNumerical("effectivePeriod.getPeriod(start, @2013-04-07T09:30:10+01:00, 'days')", observation).toLong
      result mustEqual 5

      //Due to given zoned date time in resource
      FhirPathEvaluator().evaluateNumerical("effectivePeriod.getPeriod(start, @2020-09-07, 'years')", observation) must throwA[FhirPathException]

      result = FhirPathEvaluator().evaluateNumerical("period.getPeriod(start, end, 'days')", encounter).toLong
      result mustEqual 9
    }

    "evaluate fixed bugs" in {
      var result = FhirPathEvaluator().satisfies("(type='history') or entry.where(fullUrl.exists()).select(fullUrl&resource.meta.versionId).isDistinct()", bundleOp)
      result mustEqual true
    }

  }
}

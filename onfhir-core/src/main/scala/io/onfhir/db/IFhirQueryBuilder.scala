package io.onfhir.db

import io.onfhir.api.FHIR_PREFIXES_MODIFIERS
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

trait IFhirQueryBuilder {
  /**
   *
   * @param queries
   * @return
   */
  def andQueries(queries:Seq[Bson]):Bson = {
    queries match {
      case Seq(single) => single
      case multiple => Filters.and(multiple: _*)
    }
  }

  /**
   * Combine the queries as logical OR
   * @param queries Given queries
   * @return
   */
  def orQueries(queries:Seq[Bson]):Bson = {
    queries match {
      case Seq(single) => single
      case multiple => Filters.or(multiple:_*)
    }
  }

  /**
   *
   * @param queries
   * @return
   */
  def neitherQueries(queries:Seq[Bson]):Bson = {
    queries match {
      case Seq(single) => Filters.not(single)
      case multiple => Filters.and(multiple.map(Filters.not): _*)
    }
  }

  /**
   * Construct final query from given query and elemMatch path (the last array element on the path)
   * @param elemMatchPath The path until (including) the last array element
   * @param query         Query constructed for the search
   * @return
   */
  def getFinalQuery(elemMatchPath:Option[String], query:Bson):Bson = {
    elemMatchPath match {
      case None => query
      case Some(emp) => Filters.elemMatch(emp, query)
    }
  }

  def mergeQueriesFromDifferentPaths(modifier:String, queries:Seq[Bson]):Bson = {
    modifier match {
      //If the modifier is negation, we should and them
      case FHIR_PREFIXES_MODIFIERS.NOT | FHIR_PREFIXES_MODIFIERS.NOT_IN =>
        andQueries(queries)
      //Otherwise use logical OR
      case _ =>
        orQueries(queries)
    }
  }
}

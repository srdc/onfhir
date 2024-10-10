package io.onfhir.db

import com.mongodb.MongoClientSettings
import com.mongodb.bulk.BulkWriteUpsert
import com.mongodb.client.model.UpdateOneModel
import io.onfhir.api.util.FHIRUtil
import io.onfhir.db.DocumentManager.getDocumentHelper
import io.onfhir.exception.NotFoundException
import org.mongodb.scala.model.{Field, Filters, InsertOneModel, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.result.InsertOneResult

import java.time.Instant
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

//import com.mongodb.client.model.{BsonField, Field, InsertOneModel}
import io.onfhir.Onfhir
import io.onfhir.api._
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.{ConflictException, InternalServerException}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonString, BsonValue, _}
import org.mongodb.scala.model.Filters.{and, equal, in, nin, notEqual}
import org.mongodb.scala.model.Projections.{exclude, include}
import org.mongodb.scala.model.Sorts.{ascending, descending}

import com.mongodb.client.model.Updates.setOnInsert
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.util.DateTimeUtil
import org.mongodb.scala.model.{Accumulators, Aggregates,  BulkWriteOptions, Projections, Sorts, UpdateOptions, Updates}
import org.mongodb.scala.{FindObservable, MongoCollection}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

/**
  * Persistency manager for FHIR resources on MongoDB
  */
object DocumentManager {
  //Execution context
  implicit val executionContext:ExecutionContext = Onfhir.actorSystem.dispatchers.lookup("akka.actor.onfhir-blocking-dispatcher")

  /***
    * Query with multiple ids
    * @param rids Resource ids
    * @return
    */
  def ridsQuery(rids:Set[String]):Bson = in(FHIR_COMMON_FIELDS.ID, rids.toSeq:_*)
  /***
    * Resource id query
    * @param rid FHIR Resource identifier (id parameter)
    * @return Query to find the resource
    */
  private def ridQuery(rid:String):Bson = equal(FHIR_COMMON_FIELDS.ID, rid)

  /**
    * Version id query
    * @param vid FHIR resource version id
    * @return
    */
  private def vidQuery(vid:String):Bson = equal(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}", vid)

  // Tuncay - 2024-08-14 - We don't need this any more as now we don't keep deleted versions in current collection
  //private def isActiveQuery:Bson = /*and(isCurrentQuery,*/ notEqual(FHIR_EXTRA_FIELDS.METHOD, FHIR_METHOD_NAMES.METHOD_DELETE)//)

  /**
    * And the BSON queries if there is more than one
    * @param queries Mongo queries
    * @return
    */
  def andQueries(queries:Seq[Bson]):Option[Bson] = queries.length match {
    case 0 => None
    case 1 => Some(queries.head)
    case _ => Some(and(queries:_*))
  }

  /**
    * Returns a specific version of a resource. If version id is not provided then the current version is return
    *
    * @param rtype type of the resource
    * @param id id of the resource
    * @param vid version id of the resource
    * @param includingOrExcludingFields Specifically a set of fields to return  or exclude if exists
    * @param excludeExtraFields if true exclude the fields related with the version control
    * @return a future document for the given resource
    */
  def getDocument(rtype:String, id:String, vid:Option[String] = None, includingOrExcludingFields:Option[(Boolean, Set[String])] = None, excludeExtraFields:Boolean = true)(implicit transactionSession: Option[TransactionSession] = None) : Future[Option[Document]] = {
    //Try to find the version within current documents
    getDocumentHelper(MongoDB.getCollection(rtype), id, vid, includingOrExcludingFields, excludeExtraFields) flatMap {
      case Nil =>
        vid match {
          //If not found and version id is not given, check history it may be deleted
          case None =>
            getDocumentHelper(MongoDB.getCollection(rtype, history = true), id, vid, includingOrExcludingFields, excludeExtraFields) map {
              //If still not found, return none
              case Nil => None
              case history if history.head.getString(FHIR_EXTRA_FIELDS.METHOD) == FHIR_METHOD_NAMES.METHOD_DELETE  =>
                Some(history.head)
              case _ =>
                throw new InternalServerException(s"Invalid state for resource history ($rtype) with id $id in the database, the last version should be a delete if it does not exits in current collection!")
            }
          //Otherwise try to find it within history documents
          case Some(_) =>
            getDocumentHelper(MongoDB.getCollection(rtype, history = true), id, vid, includingOrExcludingFields, excludeExtraFields) map {
              case Nil => None
              case Seq(d) => Some(d)
              case _ =>
                throw new InternalServerException(s"Server responded with the following internal message: Duplicate resource history ($rtype) with id $id and version ${vid.get}  in the database")
            }
        }
      //If one document found, return it
      case Seq(d) => Future.apply(Some(d))
      //If more than one, throw exception
      case _ => throw new InternalServerException(s"Server responded with the following internal message: Duplicate resource ($rtype) with id $id  in the database") //not likely to happen
    }
  }

  /**
    * Helper function to
    * @param coll Collection to look
    * @param id id of the resource
    * @param vid version id of the resource
    * @param includingOrExcludingFields Specifically a set of fields to return  or exclude if exists
    * @param excludeExtraFields if true exclude the fields related with the version control
    * @return a future document for the given resource
    * @return
    */
  private def getDocumentHelper(coll:MongoCollection[Document], id:String, vid:Option[String] = None, includingOrExcludingFields:Option[(Boolean, Set[String])] = None, excludeExtraFields:Boolean = true)(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {
    //Specify the base query
    val baseQuery =
      vid
        .map(vidQuery) //if version exist
        .map(vq => and(ridQuery(id), vq)) //and it with resource id query
        .getOrElse(ridQuery(id))

    var query =
      transactionSession match {
        case None =>coll.find(baseQuery)
        case Some(ts) =>  coll.find(ts.dbSession, baseQuery)
      }

    query = query.sort(Sorts.descending(s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}.__ts"))
    //Handle projection
    query = handleProjection(query, includingOrExcludingFields, excludeExtraFields)

    query.toFuture()
  }

  /**
    * Searches and finds document(s) according to given query, pagination, sorting parameters
    * @param rtype  type of the resource
    * @param filter  query filter for desired resource
    * @param count  limit for # of resources to be returned
    * @param page   page number
    * @param sortingPaths Sorting parameters and sorting direction (negative: descending, positive: ascending)
    * @param includingOrExcludingFields List of to be specifically included or excluded fields in the resulting document, if not given; all document included (true-> include, false -> exclude)
    * @param excludeExtraFields If true exclude extra fields related with version control from the document
    * @return Two sequence of documents (matched, includes); First the matched documents for the main query, Second included resources
    */
  def searchDocuments(rtype:String,
                      filter:Option[Bson],
                      aggFilteringStages:Seq[Bson],
                      count:Int = -1,
                      page:Int= 1,
                      sortingPaths:Seq[(String, Int, Seq[String])] = Seq.empty,
                      includingOrExcludingFields:Option[(Boolean, Set[String])] = None,
                      excludeExtraFields:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {

    val needAggregationPipeline =
      sortingPaths.exists(_._3.length > 1) || //If we have alternative paths for a search parameter, use search by aggregation
        aggFilteringStages.nonEmpty

    if(needAggregationPipeline){
      searchDocumentsByAggregation(rtype, filter,aggFilteringStages, count, page, sortingPaths, includingOrExcludingFields, excludeExtraFields)
    }
    //Otherwise run a normal MongoDB find query
    else {
      //Construct query
      var query = transactionSession match {
        case None => if(filter.isDefined ) MongoDB.getCollection(rtype).find(filter.get) else MongoDB.getCollection(rtype).find()
        case Some(ts) => if(filter.isDefined )  MongoDB.getCollection(rtype).find(ts.dbSession, filter.get) else MongoDB.getCollection(rtype).find(ts.dbSession)
      }

      //Sort by given params + MongoDB _id to ensure uniqueness for pagination
      val sortStmt = (sortingPaths :+ ("_id", 1, Seq("_id"))) match {
        case Seq(sf) => if (sf._2 > 0) ascending(sf._3.head) else descending(sf._3.head)
        case multiple =>
          Sorts.orderBy(
            multiple.map(sf => if (sf._2 > 0) ascending(sf._3.head) else descending(sf._3.head)): _*
          )
      }

      query = query.sort(sortStmt)

      //If count is given limit the search result
      if (count > 0)
        query = query.skip((page - 1) * count).limit(count)

      //Handle projection
      query = handleProjection(query, includingOrExcludingFields, excludeExtraFields)

      //Execute the query
      query.toFuture()
    }
  }

  /**
   * Search and paginate documents via offset based pagination
   *
   * @param rtype                      Type of the FHIR resource
   * @param filter                     query filter for desired resource
   * @param count                      limit for # of resources to be returned
   * @param offset                     Supplied offset (multiple if sorted on multiple fields) and whether to search after this offset (true) or before (false)
   * @param sortingPaths               Sorting parameters and sorting direction (negative: descending, positive: ascending)
   * @param includingOrExcludingFields List of to be specifically included or excluded fields in the resulting document, if not given; all document included (true-> include, false -> exclude)
   * @param excludeExtraFields         If true exclude extra fields related with version control from the document
   * @return                           Offset for search before, Offset for search after, and the result set
   */
  def searchDocumentsWithOffset(rtype: String,
                                filter: Option[Bson],
                                filteringStages:Seq[Bson],
                                count: Int = -1,
                                offset:Option[(Seq[String],Boolean)],
                                sortingPaths: Seq[(String, Int, Seq[String])] = Seq.empty,
                                includingOrExcludingFields: Option[(Boolean, Set[String])] = None,
                                excludeExtraFields: Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[(Seq[String],Seq[String], Seq[Document])] =  {
    //Append the offset filter
    val offsetFilter = offset.map {
      case (Seq(mongoIdOffset), true) =>
        Filters.gt(FHIR_COMMON_FIELDS.MONGO_ID, BsonObjectId(mongoIdOffset))
      case (Seq(mongoIdOffset), false) =>
        Filters.lt(FHIR_COMMON_FIELDS.MONGO_ID, BsonObjectId(mongoIdOffset))
      case _ => throw new NotImplementedError("Pagination with offset is only implemented over MongoDB id of resource!")
    }
    var finalFilter = filter
    offsetFilter.foreach(of =>
      finalFilter = filter.map(f => and(f,of)).orElse(Some(of))
    )

    //We don't allow sorting for cursor based pagination
    if (sortingPaths.nonEmpty) {
      throw new NotImplementedError("Sorting is not implemented for pagination with offset!")
    } else {
      val resultsFuture = {
        //If there are
        if (filteringStages.nonEmpty) {
          searchDocumentsByAggregationForOffsetBasedPagination(rtype, finalFilter, filteringStages, count, includingOrExcludingFields, excludeExtraFields)
        } else {
          //Construct query
          var query = transactionSession match {
            case None => if (finalFilter.isDefined) MongoDB.getCollection(rtype).find(finalFilter.get) else MongoDB.getCollection(rtype).find()
            case Some(ts) => if (finalFilter.isDefined) MongoDB.getCollection(rtype).find(ts.dbSession, finalFilter.get) else MongoDB.getCollection(rtype).find(ts.dbSession)
          }
          //Sort on id
          query =
            query
              .sort(ascending(FHIR_COMMON_FIELDS.MONGO_ID))
              .limit(count)

          //Handle projection
          query = handleProjection(query, includingOrExcludingFields, excludeExtraFields, exceptMongoId = true)

          //Execute the query
          query
            .toFuture()
        }
      }

      resultsFuture
        .map(docs => {
          var finalDocs = if (offset.forall(_._2)) docs else docs.reverse
          val offsetBefore = finalDocs.headOption.map(d => d.getObjectId(FHIR_COMMON_FIELDS.MONGO_ID).toString).toSeq
          val offsetAfter = finalDocs.lastOption.map(d => d.getObjectId(FHIR_COMMON_FIELDS.MONGO_ID).toString).toSeq
          finalDocs = if (excludeExtraFields) finalDocs.map(d => d.filter(_._1 == FHIR_COMMON_FIELDS.MONGO_ID)) else finalDocs

          (offsetBefore, offsetAfter, finalDocs)
        })
    }
  }

  /**
   * Searches and finds document(s) according to given query, pagination, sorting parameters on multiple resource types
   * @param filters                         Mongo Filter and filtering aggregation phases constructed for each resource type
   * @param count                           Limit for # of resources to be returned
   * @param page                            Page number
   * @param sortingPaths                    Sorting parameters and sorting direction (negative: descending, positive: ascending) for each resource type
   * @param includingOrExcludingFields      List of to be specifically included or excluded fields in the resulting document,
   *                                        if not given; all document included (true-> include, false -> exclude) for each resource type
   * @param excludeExtraFields              If true exclude extra fields related with version control from the document
   * @param transactionSession
   * @return
   */
  def searchDocumentsFromMultipleCollection(
                      filters:Map[String, (Option[Bson], Seq[Bson])],
                      count:Int = -1,
                      page:Int= 1,
                      sortingPaths:Map[String, Seq[(String, Int, Seq[String])]] = Map.empty,
                      includingOrExcludingFields:Map[String, Option[(Boolean, Set[String])]] = Map.empty,
                      excludeExtraFields:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {

    //Internal method to construct aggregation pipeline for each resource type
    def constructAggQueryForResourceType(filter:Option[Bson], filteringStages:Seq[Bson], sortingPaths:Seq[(String, Int, Seq[String])], includingOrExcludingFields:Option[(Boolean, Set[String])]):ListBuffer[Bson] = {
      val aggregations = new ListBuffer[Bson]
      //First, append the match query
      if(filter.isDefined)
        aggregations.append(Aggregates.`match`(filter.get))
      //Add the filtering stages
      filteringStages.foreach(s => aggregations.append(s))

      if(sortingPaths.nonEmpty) {
        //Then add common sorting field for sort parameters that has multiple alternative paths
        aggregations.appendAll(sortingPaths.map(sp => addFieldAggregationForParamWithAlternativeSorting(sp)))
      }
      //Handle projections (summary and extra fields)
      handleProjectionForAggregationSearch(includingOrExcludingFields, excludeExtraFields, Set.empty).foreach(prj => aggregations.append(prj))

      aggregations
    }

    //Construct an aggregation pipeline for each resource type
    val aggregatesForEachResourceType =
      filters
      .map(f => f._1 -> constructAggQueryForResourceType(f._2._1, f._2._2, sortingPaths(f._1), includingOrExcludingFields(f._1)))

    //We will start from the first resource type
    val firstAggregation = aggregatesForEachResourceType.head

    //Then call unionWith operator for others
    val unionWithAggregations:Seq[Bson] =
      aggregatesForEachResourceType
        .tail
        .map(a => constructUnionWithExpression(a._1, a._2.map(_.toBsonDocument[BsonDocument](classOf[BsonDocument],  MongoClientSettings.getDefaultCodecRegistry)).toSeq))
        .toSeq
    //Sorting aggregations
    val sortingAggregations = sortingPaths.head._2.map(sp => Aggregates.sort( if(sp._2 > 0) ascending(s"__sort_${sp._1}") else descending(s"__sort_${sp._1}")))
    //Get rid of extra sorting field
    val finalProjectionAggregation = {
      if(sortingPaths.head._2.nonEmpty)
        Seq(Aggregates.project(exclude(sortingPaths.head._2.map(sp => s"__sort_${sp._1}"): _*)))
      else
        Nil
    }

    //Handle paging parameters
    val pagingAggregations =
      if(count != -1){
        Seq(
          Aggregates.skip((page-1) * count),
          Aggregates.limit(count)
        )
      } else Nil

    //Merge aggregations
    val finalAggregations = (firstAggregation._2 ++ unionWithAggregations ++ sortingAggregations ++ pagingAggregations ++ finalProjectionAggregation).toSeq
    //Execute aggregation pipeline
    transactionSession match {
      case None => MongoDB.getCollection(firstAggregation._1).aggregate(finalAggregations).toFuture()
      case Some(ts) =>  MongoDB.getCollection(firstAggregation._1).aggregate(ts.dbSession, finalAggregations).toFuture()
    }
  }


  /**
   * Searching documents by aggregation pipeline to handle some complex sorting
   *
   * @param rtype                      type of the resource
   * @param filter                     query filter for desired resource
   * @param aggPhasesForFiltering      Further aggregation phases for filtering (handling of chaining, reverse chaining or special params like _list)
   * @param count                      limit for # of resources to be returned
   * @param page                       page number
   * @param sortingPaths               Sorting parameters and sorting direction (negative: descending, positive: ascending)
   * @param includingOrExcludingFields List of to be specifically included or excluded fields in the resulting document, if not given; all document included (true-> include, false -> exclude)
   * @param excludeExtraFields         If true exclude extra fields related with version control from the document
   * @return Two sequence of documents (matched, includes); First the matched documents for the main query, Second included resources
   */
  def searchDocumentsByAggregation(rtype: String,
                                   filter: Option[Bson],
                                   aggPhasesForFiltering:Seq[Bson],
                                   count: Int = -1,
                                   page: Int = 1,
                                   sortingPaths: Seq[(String, Int, Seq[String])] = Seq.empty,
                                   includingOrExcludingFields: Option[(Boolean, Set[String])] = None,
                                   excludeExtraFields: Boolean = false)(implicit transactionSession: Option[TransactionSession] = None): Future[Seq[Document]] = {
    val aggregations = new ListBuffer[Bson]

    //First, append the match query
    if (filter.isDefined)
      aggregations.append(Aggregates.`match`(filter.get))

    //Add the phases
    aggPhasesForFiltering.foreach(p => aggregations.append(p))


    //Identify the sorting params that has alternative paths
    val paramsWithAlternativeSorting =
      sortingPaths
        .filter(_._3.length > 1) //Those that have multiple paths

    //Then add common sorting field for sort parameters that has multiple alternative paths
    aggregations.appendAll(paramsWithAlternativeSorting.map(sp => addFieldAggregationForParamWithAlternativeSorting(sp)))

    //Add sorting aggregations
    val sorts =
      (sortingPaths :+ ("_id", 1, Seq("_id"))) //Finally sort on MongoDB _id to ensure uniqueness for pagination
        .map(sp => sp._3.length match {
          //For single alternative path, sort it
          case 1 =>
            if (sp._2 > 0) ascending(sp._3.head) else descending(sp._3.head)
          //For multiple alternatives, sort against the added field which is based on parameter name
          case _ =>
            if (sp._2 > 0) ascending(s"__sort_${sp._1}") else descending(s"__sort_${sp._1}")
        })
    if (sorts.nonEmpty)
      aggregations.append(Aggregates.sort(Sorts.orderBy(sorts: _*)))


    //Handle paging parameters
    if (count != -1) {
      aggregations.append(Aggregates.skip((page - 1) * count))
      aggregations.append(Aggregates.limit(count))
    }

    //Handle projections
    val extraSortingFieldsToExclude = paramsWithAlternativeSorting.map(sp => s"__sort_${sp._1}")
    handleProjectionForAggregationSearch(includingOrExcludingFields, excludeExtraFields, extraSortingFieldsToExclude.toSet)
      .foreach(prj => aggregations.append(prj))

    transactionSession match {
      case None => MongoDB.getCollection(rtype).aggregate(aggregations.toSeq).toFuture()
      case Some(ts) => MongoDB.getCollection(rtype).aggregate(ts.dbSession, aggregations.toSeq).toFuture()
    }
  }

  /**
   * Search by aggregation pipeline for offset based pagination
   *
   * @param rtype                      type of the resource
   * @param filter                     query filter for desired resource
   * @param aggPhasesForFiltering      Further aggregation phases for filtering (handling of chaining, reverse chaining or special params like _list)
   * @param count                      limit for # of resources to be returned
   * @param includingOrExcludingFields List of to be specifically included or excluded fields in the resulting document, if not given; all document included (true-> include, false -> exclude)
   * @param excludeExtraFields         If true exclude extra fields related with version control from the document
   * @param transactionSession
   * @return
   */
  def searchDocumentsByAggregationForOffsetBasedPagination(rtype: String,
                                      filter: Option[Bson],
                                      aggPhasesForFiltering: Seq[Bson],
                                      count: Int = -1,
                                      includingOrExcludingFields: Option[(Boolean, Set[String])] = None,
                                      excludeExtraFields: Boolean = false)(implicit transactionSession: Option[TransactionSession] = None): Future[Seq[Document]] = {
    val aggregations = new ListBuffer[Bson]

    //First, append the match query
    if (filter.isDefined)
      aggregations.append(Aggregates.`match`(filter.get))

    //Add the phases
    aggPhasesForFiltering.foreach(p => aggregations.append(p))

    if(count != -1) {
      aggregations.append(Aggregates.sort(Sorts.ascending("_id")))
      aggregations.append(Aggregates.limit(count))
    }

    transactionSession match {
      case None => MongoDB.getCollection(rtype).aggregate(aggregations.toSeq).toFuture()
      case Some(ts) => MongoDB.getCollection(rtype).aggregate(ts.dbSession, aggregations.toSeq).toFuture()
    }
  }

/*
  /**
    * Searching documents by aggregation pipeline to handle some complex sorting
    * @param rtype  type of the resource
    * @param filter  query filter for desired resource
    * @param count  limit for # of resources to be returned
    * @param page   page number
    * @param sortingPaths Sorting parameters and sorting direction (negative: descending, positive: ascending)
    * @param includingOrExcludingFields List of to be specifically included or excluded fields in the resulting document, if not given; all document included (true-> include, false -> exclude)
    * @param excludeExtraFields If true exclude extra fields related with version control from the document
    * @return Two sequence of documents (matched, includes); First the matched documents for the main query, Second included resources
    */
  private def searchDocumentsByAggregation(rtype:String,
                                   filter:Option[Bson],
                                   count:Int = -1,
                                   page:Int= 1,
                                   sortingPaths:Seq[(String, Int, Seq[String])] = Seq.empty,
                                   includingOrExcludingFields:Option[(Boolean, Set[String])] = None,
                                   excludeExtraFields:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {
    val aggregations = new ListBuffer[Bson]

    //First, append the match query
    if(filter.isDefined)
      aggregations.append(Aggregates.`match`(filter.get))

    //Identify the sorting params that has alternative paths
    val paramsWithAlternativeSorting =
      sortingPaths
      .filter(_._3.length > 1) //Those that have multiple paths

    //Then add common sorting field for sort parameters that has multiple alternative paths
    aggregations.appendAll(paramsWithAlternativeSorting.map(sp => addFieldAggregationForParamWithAlternativeSorting(sp)))

    //Add sorting aggregations
    val sorts =
      (sortingPaths :+ ("_id", 1, Seq("_id"))) //Finally sort on MongoDB _id to ensure uniqueness for pagination
        .map(sp => sp._3.length match {
          //For single alternative path, sort it
          case 1 =>
            if(sp._2 > 0) ascending(sp._3.head) else descending(sp._3.head)
          //For multiple alternatives, sort against the added field which is based on parameter name
          case _ =>
            if(sp._2 > 0) ascending(s"__sort_${sp._1}") else descending(s"__sort_${sp._1}")
        })
    if(sorts.nonEmpty)
      aggregations.append(Aggregates.sort(Sorts.orderBy(sorts:_*)))


    //Handle paging parameters
    if(count != -1){
      aggregations.append(Aggregates.skip((page-1) * count))
      aggregations.append(Aggregates.limit(count))
    }

    //Handle projections
    val extraSortingFieldsToExclude = paramsWithAlternativeSorting.map(sp => s"__sort_${sp._1}")
    aggregations.append(handleProjectionForAggregationSearch(includingOrExcludingFields, excludeExtraFields, extraSortingFieldsToExclude.toSet))

    /*val aggregations = Seq(
      Aggregates.`match`(filter.get),
      Aggregates.addFields(new Field("sort_effective",
        new BsonDocument("$ifNull",
          new BsonArray(
            Seq(
              new BsonString("$effectiveDateTime.__ts"),
              new BsonDocument("$ifNull",
                new BsonArray(Seq(
                  new BsonString("$effectivePeriod.start.__ts"),
                  new BsonString("$effectiveInstant.__ts")).asJava))).asJava)))),
      //Aggregates.addFields(new Field("sort_effective", "$effectivePeriod.start.__ts")),
      //Aggregates.addFields(new Field("sort_effective", "$effectiveInstant.__ts")),
      Aggregates.sort(descending("sort_effective")),
      Aggregates.skip((page-1) * count),
      Aggregates.limit(count),
      Aggregates.project(exclude("sort_effective"))
    )*/

    transactionSession match {
      case None => MongoDB.getCollection(rtype).aggregate(aggregations.toSeq).toFuture()
      case Some(ts) =>  MongoDB.getCollection(rtype).aggregate(ts.dbSession, aggregations.toSeq).toFuture()
    }
  }*/


  /**
    * Construct a Mongo addFields aggregation that add a common field for alternative paths based on existence of the path (with $ifNull )
    * @param sortingParam Sorting params (name, sort direction, paths)
    * @return
    */
  private def addFieldAggregationForParamWithAlternativeSorting(sortingParam:(String, Int, Seq[String])):Bson = {
    val ifNullStatement =
      sortingParam._3.foldRight[BsonValue](null)((path, ifnullDoc) =>
        if(ifnullDoc == null)
          new BsonString("$"+path)
        else {
          new BsonDocument("$ifNull",
            new BsonArray(Seq(
              new BsonString("$"+path),
              ifnullDoc).asJava))
        }
      )
    //Add field with param name
    Aggregates.addFields(new Field(s"__sort_${sortingParam._1}", ifNullStatement))
  }


  /**
   * Construct a Mongo unionWith aggregation operator
   * @param coll      Collection to union with
   * @param pipeline  pipeline for collection
   * @return
   */
  def constructUnionWithExpression(coll:String, pipeline:Seq[BsonValue]):Bson = {
    new BsonDocument("$unionWith",
      new BsonDocument(
        "coll", BsonString(coll))
        .append("pipeline", BsonArray.fromIterable(pipeline))
      )
  }

  /**
    * Handle projection for aggregation search
    * @param includingOrExcludingFields Including /Excluding fields in result
    * @param excludeExtraFields True if excluding extra fields is requested
    * @param addedFields All added fields for aggregation
    * @return
    */
  private def handleProjectionForAggregationSearch(includingOrExcludingFields:Option[(Boolean, Set[String])], excludeExtraFields:Boolean, addedFields:Set[String]):Option[Bson] = {
    includingOrExcludingFields match {
      //Nothing given, so we include all normal FHIR elements
      case None =>
        //If we exclude the extra fields, exclude them
        if(excludeExtraFields)
          Some(Aggregates.project(exclude((ONFHIR_EXTRA_FIELDS ++ addedFields).toSeq:_*)))
        else if(addedFields.nonEmpty)
          Some(Aggregates.project(exclude(addedFields.toSeq: _*)))
        else
          None  //No need for projection
      //Specific inclusion
      case Some((true, fields)) =>
        //If we don't exclude the extra fields, include them to final inclusion set
        val finalIncludes = if(excludeExtraFields) fields ++ FHIR_MANDATORY_ELEMENTS else fields ++ FHIR_MANDATORY_ELEMENTS ++ ONFHIR_EXTRA_FIELDS
        Some(Aggregates.project(include(finalIncludes.toSeq :_*)))

      //Specific exclusion
      case Some((false, fields)) =>
        val finalExcludes = if(excludeExtraFields) fields ++ ONFHIR_EXTRA_FIELDS else fields
        Some(Aggregates.project(exclude((finalExcludes ++ addedFields).toSeq :_*)))
    }
  }
  /**
    * Handle inclusion and exclusion of fields
    * @param query Current query
    * @param includingOrExcludingFields Fields to be included or excluded
    * @param excludeExtraFields If true exclude extra fields related with version control from the document
    * @param exceptMongoId      If true, even if excludeExtraFields is true we include the MongoDB _id field
    * @return Updated query
    */
  private def handleProjection(query:FindObservable[Document],
                               includingOrExcludingFields:Option[(Boolean, Set[String])],
                               excludeExtraFields:Boolean,
                               exceptMongoId:Boolean = false):FindObservable[Document] = {
    val extraFields = if(exceptMongoId) ONFHIR_EXTRA_FIELDS - FHIR_COMMON_FIELDS.MONGO_ID else ONFHIR_EXTRA_FIELDS
    includingOrExcludingFields match {
      //Nothing given, so we include all normal FHIR elements
      case None =>
        //If we exclude the extra fields, exclude them
        if(excludeExtraFields)
          query.projection(exclude(extraFields.toSeq:_*))
        else
          query
      //Specific inclusion
      case Some((true, fields)) =>
        //If we don't exclude the extra fields, include them to final inclusion set
        val finalIncludes = if(excludeExtraFields) fields ++ FHIR_MANDATORY_ELEMENTS else fields ++ FHIR_MANDATORY_ELEMENTS ++ extraFields
        query.projection(include(finalIncludes.toSeq :_*))

      //Specific exclusion
      case Some((false, fields)) =>
        val finalExcludes = if(excludeExtraFields) fields ++ extraFields else fields
        query.projection(exclude(finalExcludes.toSeq :_*))
    }
  }

  /***
    * Return # of active FHIR resources with given type  and other filter
    * @param rtype Resource type filter
    * @param query Mongo query
    * @return
    */
  def countDocuments(rtype:String, query:Option[Bson], filteringStages:Seq[Bson] = Nil)(implicit transactionSession: Option[TransactionSession] = None): Future[Long] = {
    if(filteringStages.isEmpty)
      getCount(
        rtype,
        query
      )
    else
      getCountWithAgg(rtype, query, filteringStages, history = false)
  }

  /**
   * Count documents in multiple resource types
   * @param queries
   * @param transactionSession
   * @return
   */
  def countDocumentsFromMultipleCollections(queries:Map[String, (Option[Bson], Seq[Bson])])(implicit transactionSession: Option[TransactionSession] = None): Future[Long] = {
    Future
      .sequence(queries.map(q => countDocuments(q._1, q._2._1, q._2._2)))
      .map(counts => counts.sum)
  }

  /***
    * Count documents
    * @param filter Mongo Query
    * @return
    */
  private def getCountOfAll(filter:Option[Bson], history:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None): Future[Long]  = {
    MongoDB.listCollections(history).flatMap(rtypes =>
      Future
        .sequence(
          rtypes.map(rtype => getCount(rtype, filter, history)
          )) //get count for
        .map(_.sum) //Sum all counts
    )
  }

  /**
    * Count documents for a resource type given query
    * @param rtype              Resource type
    * @param filter             Filter for query
    * @param history            If this search is on history
    * @param transactionSession Transaction session if exists
    * @return
    */
  private def getCount(rtype:String, filter:Option[Bson], history:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None): Future[Long]  = {
    transactionSession match {
      case None =>
        filter
          .map(MongoDB.getCollection(rtype, history).countDocuments(_).head())
          .getOrElse(MongoDB.getCollection(rtype, history).countDocuments().head())

      case Some(ts) =>
        filter
          .map(MongoDB.getCollection(rtype, history).countDocuments(ts.dbSession, _).head())
          .getOrElse(MongoDB.getCollection(rtype, history).countDocuments(ts.dbSession).head())
    }
  }

  /**
   * Count documents by using MongoDb aggregation pipeline
   *
   * @param rtype              Resource type
   * @param filter             Filter for query
   * @param stages             Further filtering stages
   * @param history            If this search is on history
   * @param transactionSession Transaction session if exists
   * @return
   */
  private def getCountWithAgg(rtype:String, filter:Option[Bson], stages:Seq[Bson], history:Boolean = false)(implicit transactionSession: Option[TransactionSession] = None): Future[Long] = {
    val aggregations = new ListBuffer[Bson]
    //Add filter
    filter.foreach(f => aggregations.append(Aggregates.`match`(f)))
    //Add aggregation stages
    stages.foreach(s => aggregations.append(s))
    //Count the documents
    aggregations.append(Aggregates.count())

    val countResult =
      transactionSession match {
        case None => MongoDB.getCollection(rtype).aggregate(aggregations.toSeq).toFuture()
        case Some(ts) => MongoDB.getCollection(rtype).aggregate(ts.dbSession, aggregations.toSeq).toFuture()
      }
    //Return the result
    countResult.map {
      case Nil => 0L
      case Seq(r) => r.getInteger("count").toLong
    }
  }

  /**
    * Search documents but return their resource ids
    * @param rtype Resource type
    * @param filter Mongo filter
    * @param count Number of docs
    * @param page Page
    * @param sortingPaths Sorting paths
    * @return
    */
  def searchDocumentsReturnIds(rtype:String,
                               filter:Bson,
                               filteringStages:Seq[Bson],
                               count:Int = -1,
                               page:Int= -1,
                               sortingPaths:Seq[(String, Int, Seq[String])] = Seq.empty) (implicit transactionSession: Option[TransactionSession] = None):Future[Seq[String]] = {

    searchDocuments(rtype, Some(filter),filteringStages, count, page, sortingPaths, includingOrExcludingFields = Some(true -> Set(FHIR_COMMON_FIELDS.ID)))
      .map(_.map(_.getString(FHIR_COMMON_FIELDS.ID)))
  }


  /**
   * Search on historic instances and get the highest version that matches the query
   * @param rtype               Resource type
   * @param rid                 Resource id
   * @param filter              Query itself
   * @param count               Number of resources
   * @param skip                Number of records to be skipped
   * @return
   */
  def getHistoricLast(rtype:String,  rid:Option[String], filter:Option[Bson], filteringStages:Seq[Bson], count:Int = -1, skip:Int = -1):Future[Seq[Document]] = {
    val collection = MongoDB.getCollection(rtype, true)

    val finalFilter = andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq)
    val aggregations = new ListBuffer[Bson]

    //Filter with the query
    finalFilter.foreach(ff => aggregations.append(Aggregates.filter(ff)))
    //Add the filtering stages
    filteringStages.foreach(s => aggregations.append(s))

    //Sort according to version in descending order
    aggregations.append(Aggregates.sort(descending("meta.versionId")))
    //Group by resource id and get the first for each group (means the biggest version)
    aggregations.append(Aggregates.group("$id", Accumulators.first("first", "$$CURRENT")))
    //Paging
    if(skip != -1)
      aggregations.append(Aggregates.skip(skip))
    if(count != -1)
      aggregations.append(Aggregates.limit(count))


  collection.aggregate(aggregations.toSeq).toFuture().map(results =>
      results.map(r => Document(r.get("first").get.asDocument()))
    )
  }

  /**
   * Count the instances where one of the versions satisfies the quesy
   * @param rtype               Resource type
   * @param rid                 Resource id
   * @param filter              Query itself
   * @return
   */
  def countHistoricLast(rtype:String,  rid:Option[String], filter:Option[Bson], filteringStages:Seq[Bson]):Future[Long] = {
    val collection = MongoDB.getCollection(rtype, true)

    val finalFilter = andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq)
    val aggregations = new ListBuffer[Bson]

    //Filter with the query
    finalFilter.foreach(ff => aggregations.append(Aggregates.filter(ff)))
    //Add the filtering stages
    filteringStages.foreach(s => aggregations.append(s))

    //Sort according to version in descending order
    aggregations.append(Aggregates.sort(descending("meta.versionId")))
    //Group by resource id and get the first for each group (means the biggest version)
    aggregations.append(Aggregates.group("$id", Accumulators.first("first", "$id")))


    collection.aggregate(aggregations.toSeq).toFuture().map(results =>
      results.length
    )
  }

  /**
   * Handle the history search with _at parameter
   * @param rtype
   * @param rid
   * @param filter
   * @param filteringStages
   * @param count
   * @param page
   * @return
   */
  def searchHistoricDocumentsWithAt(rtype:String,  rid:Option[String], filter:Option[Bson], filteringStages:Seq[Bson], count:Int = -1, page:Int = -1):Future[(Long,Seq[Document])] = {
    def getIdsOfAllCurrents():Future[Seq[String]] = {
      DocumentManager
        .searchDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), filteringStages, count, page, includingOrExcludingFields  = Some(true, Set(FHIR_COMMON_FIELDS.ID)))
        .map(docs => docs.map(d => d.getString(FHIR_COMMON_FIELDS.ID)))
    }

    val totalCurrentFuture = DocumentManager.countDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), filteringStages)

    totalCurrentFuture.flatMap {
      //If all records can be supplied from currents
      case totalCurrent:Long if count * page <= totalCurrent =>
        DocumentManager
          .searchDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), filteringStages, count, page)
          .flatMap(docs => {
            getIdsOfAllCurrents().flatMap(ids =>
              DocumentManager
                .countHistoricLast(rtype, rid,  if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter, filteringStages)
                .map(totalHistory => (totalCurrent + totalHistory) -> docs)
            )
          })
      //If some of them should be from current some from history
      case totalCurrent:Long if count * page > totalCurrent && count * (page-1) < totalCurrent =>
        DocumentManager
          .searchDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), filteringStages, count, page)
          .flatMap(currentDocs => {
            val numOfNeeded = count * page - totalCurrent
            getIdsOfAllCurrents().flatMap(ids =>
              DocumentManager
                .countHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter, filteringStages)
                .flatMap(totalHistory=>
                  DocumentManager.getHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter, filteringStages, numOfNeeded.toInt).map(historicDocs =>
                    (totalCurrent + totalHistory) -> (currentDocs ++ historicDocs)
                  )
                )
            )
          })
      case totalCurrent:Long if count * (page-1) >= totalCurrent =>
        val numToSkip = count * (page-1) - totalCurrent
        getIdsOfAllCurrents().flatMap(ids =>
          DocumentManager
            .countHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter, filteringStages)
            .flatMap(totalHistory =>
            DocumentManager.getHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter, filteringStages, count, numToSkip.toInt).map(historicDocs =>
              (totalCurrent + totalHistory) -> historicDocs
            )
          )
        )
    }
  }


  /**
    * Search on all FHIR resources including the history versions
    * @param rtype  type of the resource
    * @param rid Resource id filter
    * @param filter other filters
    * @param count Pagination count
    * @param page page number
    * @return
    */
  def searchHistoricDocuments(rtype:String,  rid:Option[String], filter:Option[Bson], filteringStages:Seq[Bson], count:Int = -1, page:Int = -1)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long,Seq[Document])]  = {
    val finalFilter = andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq)
    val numOfDocs:Future[(Long, Long)] =
      for {
        totalCurrent <- if(filteringStages.isEmpty) getCount(rtype, finalFilter) else getCountWithAgg(rtype, finalFilter, filteringStages)
        totalHistory <- if(filteringStages.isEmpty)  getCount(rtype, finalFilter, history = true) else getCountWithAgg(rtype, finalFilter, filteringStages, history = true)
      } yield totalCurrent -> totalHistory

    numOfDocs.flatMap {
      //Not found
      case (0L, 0L) if rid.isDefined =>
        throw new NotFoundException(Seq(OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.INVALID,
          None,
          Some(s"Resource history for ${rid.get} is not found!"),
          Nil
        )))
      case (0L, 0L) => Future.apply(0L -> Nil)
      //No current so it must be deleted
      case (0L, totalHistory) =>
        //Skip this number of record
        val skip = count * (page - 1)
        //Otherwise search the history directly
        searchHistoricDocumentsHelper(rtype, history = true, finalFilter, filteringStages, count, skip)
          .map(docs => totalHistory -> docs)
      case (totalCurrent, totalHistory) =>
        val fresults =
          rid match {
            //If resource id exists, we know that historic documents come after current one
            case Some(_) =>
              //If they want page 1, look also to the current collection for resource type
              if(page == 1)
                searchHistoricDocumentsHelper(rtype, history = false, finalFilter,filteringStages, count, 0)
                  .flatMap(currentResults =>
                    //If count is not one or current result is not empty, search also history and merge
                    if(count!=1 || totalCurrent > 0)
                      searchHistoricDocumentsHelper(rtype, history = true, finalFilter, filteringStages, count-1, 0).map(historyResults =>
                        currentResults ++ historyResults
                      )
                    else
                      Future.apply(currentResults)
                  )
              else {
                //Skip this number of record
                val skip = count * (page-1) - 1
                //Otherwise search the history directly
                searchHistoricDocumentsHelper(rtype, history = true, finalFilter,filteringStages, count, skip)
              }
            //If history interaction is executed on type level
            case None =>
              searchHistoricDocumentsHelper(rtype, history = false, finalFilter,filteringStages, count, count * (page-1)).flatMap { cresults =>
                  if(count * page > totalCurrent) {
                    var skip = count * (page-1) - totalCurrent
                    if(skip < 0) skip = 0
                    searchHistoricDocumentsHelper(rtype, history = true, finalFilter,filteringStages, count - cresults.length, skip.toInt).map { hresults =>
                      cresults ++ hresults
                    }
                  } else {
                    Future.apply(cresults)
                  }
              }
        }
        //Return the results
        fresults.map(results => (totalCurrent + totalHistory) -> results)
    }
  }

  /**
    * Helper function to search history on current or history
    * @param rtype         Resource type
    * @param history       Is search on history instances
    * @param finalFilter   Final query if exist
    * @param count         Pagination count
    * @param skip          Skip this number of record
    * @return
    */
  private def searchHistoricDocumentsHelper(rtype:String, history:Boolean,  finalFilter:Option[Bson], filteringStages:Seq[Bson], count:Int, skip:Int)(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {
    if(filteringStages.nonEmpty)
      searchHistoricDocumentsHelperWithAgg(rtype, history, finalFilter, filteringStages, count, skip)
    else {
      val collection = MongoDB.getCollection(rtype, history)
      //Construct query
      var query = transactionSession match {
        case None =>
          finalFilter
            .map(f => collection.find(f))
            .getOrElse(collection.find())
        case Some(ts) =>
          finalFilter
            .map(f => collection.find(ts.dbSession, f))
            .getOrElse(collection.find(ts.dbSession))
      }


      //If count is given limit the search result
      if (count > 0)
        query = query.skip(skip).limit(count)

      //Exclude extra params
      query = query.projection(exclude(FHIR_COMMON_FIELDS.MONGO_ID))

      //Sort by last updated and version id
      val LAST_UPDATED = s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}"
      val VERSION_ID = s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}"
      query = query.sort(descending(LAST_UPDATED, VERSION_ID))

      //Execute the query
      query.toFuture()
    }
  }

  /**
   * Search helper for historic or current via MongoDB aggregation pipeline
   * @param rtype       Resource type
   * @param history     Is search on history instances
   * @param finalFilter Final query if exist
   * @param count       Pagination count
   * @param skip        Skip this number of record
   * @param transactionSession
   * @return
   */
  private def searchHistoricDocumentsHelperWithAgg(rtype:String, history:Boolean,  finalFilter:Option[Bson], filteringStages:Seq[Bson], count:Int, skip:Int)(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {
    val aggregations = new ListBuffer[Bson]
    //Set the query
    finalFilter.foreach(f => aggregations.append(Aggregates.`match`(f)))
    //Add the filtering stages
    filteringStages.foreach(s => aggregations.append(s))

    if(count > 0) {
      aggregations.append(Aggregates.skip(skip))
      aggregations.append(Aggregates.limit(count))
    }
    //Exclude extra params
    aggregations.append(Aggregates.project(Projections.exclude(FHIR_COMMON_FIELDS.MONGO_ID)))

    //Sort by last updated and version id
    val LAST_UPDATED = s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}"
    val VERSION_ID = s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}"
    aggregations.append(Aggregates.sort(Sorts.descending(LAST_UPDATED, VERSION_ID)))

    transactionSession match {
      case None => MongoDB.getCollection(rtype, history).aggregate(aggregations.toSeq).toFuture()
      case Some(ts) =>MongoDB.getCollection(rtype, history).aggregate(ts.dbSession, aggregations.toSeq).toFuture()
    }
  }


  /**
    * Inserts the given document into a appropriate collection
    * @param rtype type of the resource
    * @param document document to be inserted
    * @return a future indicating the completion of insert operation
    */
  def insertDocument(rtype:String, document:Document)(implicit transactionSession: Option[TransactionSession] = None):Future[InsertOneResult] = {
    transactionSession match {
      //Normal execution
      case None =>
        MongoDB
          .getCollection(rtype)
          .insertOne(document)
          .head()
      //Execution as a part of transaction
      case Some(ts) =>
          MongoDB
            .getCollection(rtype)
            .insertOne(ts.dbSession, document)
            .head()
    }
  }

  /**
    * Insert a bulk of same type of documents
    * @param rtype Resource type
    * @param documents Documents to insert
    * @param ordered Option for ordered bulk insertion. See https://docs.mongodb.com/manual/reference/method/Bulk/
    * @return
    */
  def insertDocuments(rtype:String, documents:Seq[Document], ordered:Boolean = false):Future[Seq[BulkWriteUpsert]] = {
    MongoDB
      .getCollection(rtype)
      .bulkWrite(documents.map(d => new InsertOneModel[Document](d)), BulkWriteOptions.apply().ordered(ordered))
      .toFuture()
      .map(br => br.getUpserts.asScala.toSeq)
  }

  /**
   * Insert or update given documents
   * @param rtype       Resource type
   * @param documents   Resource id if upsert and the document
   * @param ordered     Whether order is important
   * @return
   */
  def upsertDocuments(rtype:String, documents:Seq[(Option[String], Document)], ordered:Boolean = false):Future[(Int, Int)] = {
    val operations =
      documents.map {
        case (Some(id), d) => new ReplaceOneModel[Document](Filters.eq("id", id), d, new ReplaceOptions().upsert(true))
        case (None, d) => new InsertOneModel[Document](d)
     }

    MongoDB
      .getCollection(rtype)
      .bulkWrite(operations, BulkWriteOptions.apply().ordered(ordered))
      .toFuture()
      .map(br => br.getInsertedCount -> br.getModifiedCount)
  }

  /**
    * Insert new version of a document while storing previous latest version to history
    * @param rtype Resource type
    * @param rid Resource id
    * @param oldDocument Version number and old document version
    * @param newDocumentOrDeleted New version of document or status code for deletion
    * @return
    */
  def insertNewVersion(rtype:String, rid:String, newDocument:Document, oldDocument:(Long, Document), shardQuery:Option[Bson] = None)(implicit transactionSession: Option[TransactionSession] = None):Future[Unit] = {
    val needTransaction = transactionSession.isEmpty && OnfhirConfig.mongoUseTransaction
    //Create a transaction session if we support it but it is not part of a transaction
    val tempTransaction =
      if(needTransaction) Some(new TransactionSession(UUID.randomUUID().toString)) else transactionSession

     insertOldDocumentToHistory(rtype, rid, oldDocument._1, oldDocument._2)(tempTransaction) //If there is old version, push it to history
        .flatMap {
          case false =>
            //Abort the transaction, if any
            val transactionFinalize =
              if(needTransaction)
                tempTransaction.get.abort()
              else
                Future.apply(())
            transactionFinalize.map(_ => {
              //Schedule a check on persistency in case there is a invalid state, and we don't support Mongo transactions
              if(!OnfhirConfig.mongoUseTransaction)
                DBConflictManager.scheduleCheckAndCleanupForHistory(rtype, rid, "" + oldDocument._1)
              //Throw 409 Conflict exception
              throw new ConflictException(
                OutcomeIssue(
                  FHIRResponse.SEVERITY_CODES.ERROR,
                  FHIRResponse.OUTCOME_CODES.TRANSIENT,
                  None,
                  Some(s"Concurrent update on resource $rid, another interaction is currently overriding the resource! Please try again after a few seconds..."),
                  Nil
                )
              )
            })
          //If everything is fine, upsert the new version to current
          case true =>
             replaceCurrent(rtype, rid, newDocument, shardQuery)(tempTransaction)
              .flatMap( _ =>
                if(needTransaction)
                  tempTransaction.get.commit().map(_ => ())
                else
                  Future.apply(())
              )
        }
  }

  /**
   * Delete the current resource given with id and push the old version and its deleted version (populated with deletion metadata)
   * to history at the same time
   * @param rtype                 FHIR resource type
   * @param rid                   FHIR Resource id
   * @param deleted               Version of current resource, current version of resource, and populated with deleted metadata
   * @param shardQuery            If sharding is enabled the shard query
   * @param transactionSession    Transaction session if this is already part of a transaction
   * @return
   */
  def deleteCurrentAndMoveToHistory(rtype:String, rid:String, deletedVersion:Long, currentVersion:Document, withDeletionMetadata:Document,  shardQuery:Option[Bson] = None)(implicit transactionSession: Option[TransactionSession] = None):Future[Unit] = {
    val needTransaction = transactionSession.isEmpty && OnfhirConfig.mongoUseTransaction
    //Create a transaction session if we support it but it is not part of a transaction
    val tempTransaction =
      if (needTransaction) Some(new TransactionSession(UUID.randomUUID().toString)) else transactionSession

    insertOldDocumentToHistory(rtype, rid, deletedVersion, currentVersion)(tempTransaction) //If there is old version, push it to history
      .flatMap {
        case false => abortTransactionDueToConcurrentAccess(needTransaction, tempTransaction, rtype, rid, deletedVersion)
        //If everything is fine, push also the deleted version to history
        case true =>
          insertOldDocumentToHistory(rtype, rid, deletedVersion + 1, withDeletionMetadata)
            .flatMap {
              case false => abortTransactionDueToConcurrentAccess(needTransaction, tempTransaction, rtype, rid, deletedVersion + 1)
              case true =>
                deleteCurrent(rtype, rid, shardQuery)
            }
            .flatMap(_ =>
              if (needTransaction)
                tempTransaction.get.commit().map(_ => ())
              else
                Future.apply(())
            )
      }
  }

  /**
   * Abort the transaction due to concurrent access
   * @param needTransaction   If we are using MongoDB transaction
   * @param tempTransaction   Transaction session
   * @param rtype             Resource type
   * @param rid               Resource id
   * @param version           Version of resource
   * @return
   */
  private def abortTransactionDueToConcurrentAccess(needTransaction:Boolean, tempTransaction:Option[TransactionSession], rtype:String, rid:String, version:Long) = {
    //Abort the transaction, if any
    val transactionFinalize =
      if (needTransaction)
        tempTransaction.get.abort()
      else
        Future.apply(())
    transactionFinalize.map(_ => {
      //Schedule a check on persistency in case there is a invalid state, and we don't support Mongo transactions
      if (!OnfhirConfig.mongoUseTransaction)
        DBConflictManager.scheduleCheckAndCleanupForHistory(rtype, rid, "" + version)
      //Throw 409 Conflict exception
      throw new ConflictException(
        OutcomeIssue(
          FHIRResponse.SEVERITY_CODES.ERROR,
          FHIRResponse.OUTCOME_CODES.TRANSIENT,
          None,
          Some(s"Concurrent update on resource $rid, another interaction is currently overriding the resource! Please try again after a few seconds..."),
          Nil
        )
      )
    })
  }

  /**
    * Push old version to history collection for resource type if that version not exist
    * @param rtype              Resource type
    * @param rid                Resource id
    * @param previousVersion    Previous version of document
    * @param oldDocument        Old version of document
    * @return
    */
  private def insertOldDocumentToHistory(rtype:String, rid:String, previousVersion:Long, oldDocument:Document)(implicit transactionSession: Option[TransactionSession] = None):Future[Boolean] = {
    val collection = MongoDB.getCollection(rtype, history = true)
    //Try to insert a new version
    val updateOpFuture = transactionSession match {
      case None =>
        collection
          .updateOne(
            and(equal(FHIR_COMMON_FIELDS.ID, rid), equal(FHIR_COMMON_FIELDS.META + "." + FHIR_COMMON_FIELDS.VERSION_ID, ""+previousVersion)),
            setOnInsert(oldDocument),
            UpdateOptions.apply().upsert(true)
          ).toFuture()
      case Some(ts) =>
        collection
          .updateOne(
            ts.dbSession,
            and(equal(FHIR_COMMON_FIELDS.ID, rid), equal(FHIR_COMMON_FIELDS.META + "." + FHIR_COMMON_FIELDS.VERSION_ID, ""+previousVersion)),
            setOnInsert(oldDocument),
            UpdateOptions.apply().upsert(true)
          ).toFuture()
    }

    updateOpFuture.map(uresult => uresult.getMatchedCount == 0) //If there is no match, it is inserted, otherwise not inserted which means there is concurrent write operation on resource instance
  }

  /**
    * Update the current version
    * @param rtype              Resource Type
    * @param rid                Resource id
    * @param newDocument        New version of document
    * @param transactionSession Optional transaction session
    * @return
    */
  def replaceCurrent(rtype:String, rid:String, newDocument:Document, shardQuery:Option[Bson])(implicit transactionSession: Option[TransactionSession] = None):Future[Boolean] = {
    val collection = MongoDB.getCollection(rtype)
    val baseQuery = equal(FHIR_COMMON_FIELDS.ID, rid)
    val query = shardQuery.map(sq => and(sq,  baseQuery)).getOrElse(baseQuery)

    collection.replaceOne(query, newDocument)
      .toFuture()
      .map(updateResult =>
        updateResult.getModifiedCount == 1
      ) //Check if successfully updated
  }

  /**
    * Mark the current version of a resource as deleted
    * @param rtype              Resource type
    * @param rid                Resource id
    * @param transactionSession Transaction session
    * @return
    */
  def deleteCurrent(rtype:String, rid:String, shardQuery:Option[Bson])(implicit transactionSession: Option[TransactionSession] = None):Future[Boolean] = {
    val collection = MongoDB.getCollection(rtype)
    val baseQuery = equal(FHIR_COMMON_FIELDS.ID, rid)
    val query = shardQuery.map(sq => and(sq,  baseQuery)).getOrElse(baseQuery)

    collection
      .deleteOne(
        query
      )
      /*.updateOne(
        query,
        Updates.combine(
          //Set the new version id
          Updates.set(FHIR_COMMON_FIELDS.META + "." +FHIR_COMMON_FIELDS.VERSION_ID, ""+newVersion),
          //Set the last update time
          Updates.set(FHIR_COMMON_FIELDS.META + "." +FHIR_COMMON_FIELDS.LAST_UPDATED,
            OnFhirBsonTransformer.createBsonTimeObject(DateTimeUtil.serializeInstant(lastModified))
          ),
          //Set the status code
          Updates.set(FHIR_EXTRA_FIELDS.STATUS_CODE, sc),
          //Set method as delete
          Updates.set(FHIR_EXTRA_FIELDS.METHOD, FHIR_METHOD_NAMES.METHOD_DELETE),
        )
      )*/
      .toFuture()
      //.map(updateResult => updateResult.getModifiedCount == 1)
      .map(deleteResult => deleteResult.getDeletedCount == 1)
  }
/*
  /**
    * Replace a current resource
    * @param rtype Resource Type
    * @param rid resource id
    * @param document The document for the replacement
    * @return
    */
  def replaceDocument(rtype:String, rid:String, document:Document):Future[Boolean] = {
    val collection = MongoDB.getCollection(rtype)
    collection
      .replaceOne(
        and(
          equal(FHIR_COMMON_FIELDS.ID, rid),
          equal(FHIR_EXTRA_FIELDS.CURRENT, true)
        ), document)
      .toFuture()
      .map(updateResult => updateResult.getModifiedCount == 1) //Check if successfully replaced
  }*/

  /**
    * Remove the resources from the persistence (actual delete)
    * IMPORTANT !!! Never use this for FHIR, this is only used for test setups to clean data for patient accounts without deleting patient
    * @param rtype FHIR Resource type
    * @param query Mongo Query
    * @return
    */
  def removeDocuments(rtype:String, query:Bson):Future[Long] = {
    MongoDB
      .getCollection(rtype)
      .deleteMany(query)
      .toFuture()
      .map(dr => dr.getDeletedCount)
  }


  /**
   *
   * @param rtype                         Resource type
   * @param filter                        Query to filter the results
   * @param lastOrFirstN                  Number of last/first results to return for each group;
   *                                        - negative value means last e.g -2 --> last 2
   *                                        - positive value means first e.g. 3 --> first 3
   * @param sortingPaths                  Query params and its alternative paths for sorting the results (sorting is default in ascending)
   *                                      e.g.for Observations  date -> Seq(effectiveDateTime, effectivePeriod.start, etc)
   * @param groupByExpressions            Search parameters to execute group by on and the Mongo expression for them to group by
   *                                      e.g. for Observations  code -> ($concat(code.coding.system, code.coding.code) ....
   * @param includingOrExcludingFields    List of to be specifically included or excluded fields in the resulting document, if not given;
   *                                      all document included (true-> include, false -> exclude)
   * @param excludeExtraFields            If true exclude extra fields related with version control from the document
   * @return                              Sequence of JObject including bucket keys, Resources belong to that bucket
   */
  def searchLastOrFirstNByAggregation(rtype:String,
                               filter:Option[Bson],
                               filteringStages:Seq[Bson],
                               lastOrFirstN:Int = -1,
                               sortingPaths:Seq[(String, Seq[String])] = Seq.empty,
                               groupByExpressions:Seq[(String, BsonValue)],
                               includingOrExcludingFields:Option[(Boolean, Set[String])] = None,
                               excludeExtraFields:Boolean = false):Future[Seq[(Document, Seq[Document])]] = {

    val aggregations = new ListBuffer[Bson]
    //First, append the match query
    if(filter.isDefined)
      aggregations.append(Aggregates.`match`(filter.get))
    //Add the further filtering stages if exist
    filteringStages.foreach(s => aggregations.append(s))

    val spaths = sortingPaths.map(s => (s._1, if(lastOrFirstN < 0) -1 else 1, s._2))

    //Identify the sorting params that has alternative paths
    val paramsWithAlternativeSorting = spaths.filter(_._3.length > 1) //Those that have multiple paths

    //Then add common sorting field for sort parameters that has multiple alternative paths
    aggregations.appendAll(paramsWithAlternativeSorting.map(sp => addFieldAggregationForParamWithAlternativeSorting(sp)))

    //Add sorting aggregations
    spaths.foreach(sp => sp._3.length match {
      //For single alternative path, sort it
      case 1 =>
        aggregations.append(Aggregates.sort( if(sp._2 > 0) ascending(sp._3.head) else descending(sp._3.head)))
      //For multiple alternatives, sort against the added field which is based on parameter name
      case _ =>
        aggregations.append(Aggregates.sort( if(sp._2 > 0) ascending(s"__sort_${sp._1}") else descending(s"__sort_${sp._1}")))
    })

    //Handle projections
    val extraSortingFieldsToExclude = paramsWithAlternativeSorting.map(sp => s"__sort_${sp._1}")
    handleProjectionForAggregationSearch(includingOrExcludingFields, excludeExtraFields, extraSortingFieldsToExclude.toSet)
      .foreach(prj => aggregations.append(prj))

    //Merge the expressions into single groupBy expression
    val groupByExpr:Document = Document.apply(groupByExpressions)
    //based on the n value
    lastOrFirstN match {
      //last one or first one (we already sort based on the sign if last one it is sorted in descending)
      case -1 | 1 =>
        aggregations.append(Aggregates.group(groupByExpr, Accumulators.first("first", "$$CURRENT")))
      //first or last n
      case o =>
        aggregations.append(Aggregates.group(groupByExpr, Accumulators.push("docs", "$$CURRENT")))
        aggregations.append(Aggregates.project(Projections.computed("bucket", AggregationUtil.constructSliceExpression("docs", Math.abs(o)))))
    }

    MongoDB
      .getCollection(rtype)
      .aggregate(aggregations.toSeq)  //run the aggregation pipeline
      .allowDiskUse(true) //these aggregations may not fit into memory restrictions defined by mongo
      .toFuture()
      .map(results =>
        results
          .map(d =>
            if(Math.abs(lastOrFirstN)> 1)
              Document(d.get[BsonDocument]("_id").get) ->
                d.get[BsonArray]("bucket").get.getValues.asScala.map(v => Document(v.asInstanceOf[BsonDocument])).toSeq
            else
              Document(d.get[BsonDocument]("_id").get) -> d.get[BsonDocument]("first").toSeq.map(Document(_))
          )
      )
  }
}

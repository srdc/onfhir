package io.onfhir.db

import java.time.Instant
import java.util.UUID

import com.mongodb.bulk.BulkWriteUpsert
import com.mongodb.client.model.{BsonField, Field, InsertOneModel}
import io.onfhir.Onfhir
import io.onfhir.api._
import io.onfhir.config.OnfhirConfig
import io.onfhir.exception.{ConflictException, InternalServerException}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonDocument, BsonString, BsonValue}
import org.mongodb.scala.model.Filters.{and, equal, in, nin, notEqual}
import org.mongodb.scala.model.Projections.{exclude, include}
import org.mongodb.scala.model.Sorts.{ascending, descending}
import com.mongodb.client.model.Updates.setOnInsert
import io.onfhir.api.model.{FHIRResponse, OutcomeIssue}
import io.onfhir.util.DateTimeUtil
import org.bson.BsonValue
import org.json4s.JValue
import org.mongodb.scala.model.{Accumulators, Aggregates, BsonField, BulkWriteOptions, Filters, Projections, UpdateOptions, Updates}
import org.mongodb.scala.{Completed, FindObservable, MongoCollection, bson}
import org.mongodb.scala.bson._

import scala.collection.JavaConverters._
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

  /***
    * Query to get latest version documents
    * @return
    */
  //private def isCurrentQuery:Bson = equal(FHIR_EXTRA_FIELDS.CURRENT, true)

  /***
    * Query to get the documents that are the latest version and active (not deleted)
    * @return
    */
  private def isActiveQuery:Bson = /*and(isCurrentQuery,*/ notEqual(FHIR_EXTRA_FIELDS.METHOD, FHIR_METHOD_NAMES.METHOD_DELETE)//)

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
        //If not found and version id is not given, return None
        vid match {
          case None => Future.apply(None)
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
    * @param excludeDeleted If deleted resources are not taken into accouunt in search or not
    * @return Two sequence of documents (matched, includes); First the matched documents for the main query, Second included resources
    */
  def searchDocuments(rtype:String,
                      filter:Option[Bson],
                      count:Int = -1,
                      page:Int= 1,
                      sortingPaths:Seq[(String, Int, Seq[String])] = Seq.empty,
                      includingOrExcludingFields:Option[(Boolean, Set[String])] = None,
                      excludeExtraFields:Boolean = false,
                      excludeDeleted:Boolean = true )(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {

    //If we have alternative paths for a search parameter, use search by aggregation
    if(sortingPaths.exists(_._3.length > 1)){
      searchDocumentsByAggregation(rtype, filter, count, page, sortingPaths, includingOrExcludingFields, excludeExtraFields)
    }
    //Otherwise run a normal MongoDB find query
    else {
      val finalFilter:Option[Bson] = filter match {
        case None => if(excludeDeleted) Some(isActiveQuery) else None
        case Some(sfilter) => if(excludeDeleted) Some(and(isActiveQuery, sfilter)) else Some(sfilter)
      }
      //Construct query
      var query = transactionSession match {
        case None => if(finalFilter.isDefined ) MongoDB.getCollection(rtype).find(finalFilter.get) else MongoDB.getCollection(rtype).find()
        case Some(ts) => if(finalFilter.isDefined )  MongoDB.getCollection(rtype).find(ts.dbSession, finalFilter.get) else MongoDB.getCollection(rtype).find(ts.dbSession)
      }

      //Handle sorting
      sortingPaths
        .foreach(sf =>
          query = query.sort(if (sf._2 > 0) ascending(sf._3.head) else descending(sf._3.head))
        )

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
    * Searching documents by aggreation pipelin to handle some complex sorting
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
    aggregations.append(paramsWithAlternativeSorting.map(sp => addFieldAggregationForParamWithAlternativeSorting(sp)) : _*)

    //Add sorting aggregations
    sortingPaths.foreach(sp => sp._3.length match {
      //For single alternative path, sort it
      case 1 =>
        aggregations.append(Aggregates.sort( if(sp._2 > 0) ascending(sp._3.head) else descending(sp._3.head)))
      //For multiple alternatives, sort against the added field which is based on parameter name
      case _ =>
        aggregations.append(Aggregates.sort( if(sp._2 > 0) ascending(s"__sort_${sp._1}") else descending(s"__sort_${sp._1}")))
    })

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
      case None => MongoDB.getCollection(rtype).aggregate(aggregations).toFuture()
      case Some(ts) =>  MongoDB.getCollection(rtype).aggregate(ts.dbSession, aggregations).toFuture()
    }
  }

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
    * Handle projection for aggregation search
    * @param includingOrExcludingFields Including /Excluding fields in result
    * @param excludeExtraFields True if excluding extra fields is requested
    * @param addedFields All added fields for aggregation
    * @return
    */
  private def handleProjectionForAggregationSearch(includingOrExcludingFields:Option[(Boolean, Set[String])], excludeExtraFields:Boolean, addedFields:Set[String]):Bson = {
    includingOrExcludingFields match {
      //Nothing given, so we include all normal FHIR elements
      case None =>
        //If we exclude the extra fields, exclude them
        if(excludeExtraFields)
          Aggregates.project(exclude((ONFHIR_EXTRA_FIELDS ++ addedFields).toSeq:_*))
        else
          Aggregates.project(exclude(addedFields.toSeq: _*))
      //Specific inclusion
      case Some((true, fields)) =>
        //If we don't exclude the extra fields, include them to final inclusion set
        val finalIncludes = if(excludeExtraFields) fields ++ FHIR_MANDATORY_ELEMENTS else fields ++ FHIR_MANDATORY_ELEMENTS ++ ONFHIR_EXTRA_FIELDS
        Aggregates.project(include(finalIncludes.toSeq :_*))

      //Specific exclusion
      case Some((false, fields)) =>
        val finalExcludes = if(excludeExtraFields) fields ++ ONFHIR_EXTRA_FIELDS else fields
        Aggregates.project(exclude((finalExcludes ++ addedFields).toSeq :_*))
    }
  }
  /**
    * Handle inclusion and exclusion of fields
    * @param query Current query
    * @param includingOrExcludingFields Fields to be included or excluded
    * @param excludeExtraFields If true exclude extra fields related with version control from the document
    * @return Updated query
    */
  private def handleProjection(query:FindObservable[Document], includingOrExcludingFields:Option[(Boolean, Set[String])], excludeExtraFields:Boolean):FindObservable[Document] = {
    includingOrExcludingFields match {
      //Nothing given, so we include all normal FHIR elements
      case None =>
        //If we exclude the extra fields, exclude them
        if(excludeExtraFields)
          query.projection(exclude(ONFHIR_EXTRA_FIELDS.toSeq:_*))
        else
          query
      //Specific inclusion
      case Some((true, fields)) =>
        //If we don't exclude the extra fields, include them to final inclusion set
        val finalIncludes = if(excludeExtraFields) fields ++ FHIR_MANDATORY_ELEMENTS else fields ++ FHIR_MANDATORY_ELEMENTS ++ ONFHIR_EXTRA_FIELDS
        query.projection(include(finalIncludes.toSeq :_*))

      //Specific exclusion
      case Some((false, fields)) =>
        val finalExcludes = if(excludeExtraFields) fields ++ ONFHIR_EXTRA_FIELDS else fields
        query.projection(exclude(finalExcludes.toSeq :_*))
    }
  }

  /***
    * Return # of active FHIR resources with given type  and other filter
    * @param rtype Resource type filter
    * @param query Mongo query
    * @return
    */
  def countDocuments(rtype:String, query:Option[Bson], excludeDeleted:Boolean = true)(implicit transactionSession: Option[TransactionSession] = None): Future[Long] = {
    getCount(
      rtype,
      query
          .map(q => if(excludeDeleted) and(isActiveQuery, q) else q) match {
        case None => if(excludeDeleted) Some(isActiveQuery) else None
        case oth => oth
      }
    )
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
                               count:Int = -1,
                               page:Int= -1,
                               sortingPaths:Seq[(String, Int, Seq[String])] = Seq.empty) (implicit transactionSession: Option[TransactionSession] = None):Future[Seq[String]] = {

    searchDocuments(rtype, Some(filter), count, page, sortingPaths, includingOrExcludingFields = Some(true -> Set(FHIR_COMMON_FIELDS.ID)))
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
  def getHistoricLast(rtype:String,  rid:Option[String], filter:Option[Bson], count:Int = -1, skip:Int = -1):Future[Seq[Document]] = {
    val collection = MongoDB.getCollection(rtype, true)

    val finalFilter = andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq)
    val aggregations = new ListBuffer[Bson]

    //Filter with the query
    finalFilter.foreach(ff => aggregations.append(Aggregates.filter(ff)))
    //Sort according to version in descending order
    aggregations.append(Aggregates.sort(descending("meta.versionId")))
    //Group by resource id and get the first for each group (means the biggest version)
    aggregations.append(Aggregates.group("$id", Accumulators.first("first", "$$CURRENT")))
    //Paging
    if(skip != -1)
      aggregations.append(Aggregates.skip(skip))
    if(count != -1)
      aggregations.append(Aggregates.limit(count))


  collection.aggregate(aggregations).toFuture().map(results =>
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
  def countHistoricLast(rtype:String,  rid:Option[String], filter:Option[Bson]):Future[Long] = {
    val collection = MongoDB.getCollection(rtype, true)

    val finalFilter = andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq)
    val aggregations = new ListBuffer[Bson]

    //Filter with the query
    finalFilter.foreach(ff => aggregations.append(Aggregates.filter(ff)))
    //Sort according to version in descending order
    aggregations.append(Aggregates.sort(descending("meta.versionId")))
    //Group by resource id and get the first for each group (means the biggest version)
    aggregations.append(Aggregates.group("$id", Accumulators.first("first", "$id")))


    collection.aggregate(aggregations).toFuture().map(results =>
      results.length
    )
  }

  def searchHistoricDocumentsWithAt(rtype:String,  rid:Option[String], filter:Option[Bson], count:Int = -1, page:Int = -1):Future[(Long,Seq[Document])] = {
    def getIdsOfAllCurrents():Future[Seq[String]] = {
      DocumentManager
        .searchDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), count, page, includingOrExcludingFields  = Some(true, Set(FHIR_COMMON_FIELDS.ID)), excludeDeleted = false)
        .map(docs => docs.map(d => d.getString(FHIR_COMMON_FIELDS.ID)))
    }

    val totalCurrentFuture = DocumentManager.countDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), excludeDeleted = false)

    totalCurrentFuture.flatMap {
      //If all records can be supplied from currents
      case totalCurrent:Long if count * page <= totalCurrent =>
        DocumentManager
          .searchDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), count, page, excludeDeleted = false)
          .flatMap(docs => {
            getIdsOfAllCurrents().flatMap(ids =>
              DocumentManager
                .countHistoricLast(rtype, rid,  if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter)
                .map(totalHistory => (totalCurrent + totalHistory) -> docs)
            )
          })
      //If some of them should be from current some from history
      case totalCurrent:Long if count * page > totalCurrent && count * (page-1) < totalCurrent =>
        DocumentManager
          .searchDocuments(rtype, DocumentManager.andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq), count, page, excludeDeleted = false)
          .flatMap(currentDocs => {
            val numOfNeeded = count * page - totalCurrent
            getIdsOfAllCurrents().flatMap(ids =>
              DocumentManager
                .countHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter)
                .flatMap(totalHistory=>
                  DocumentManager.getHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter, numOfNeeded.toInt).map(historicDocs =>
                    (totalCurrent + totalHistory) -> (currentDocs ++ historicDocs)
                  )
                )
            )
          })
      case totalCurrent:Long if count * (page-1) >= totalCurrent =>
        val numToSkip = count * (page-1) - totalCurrent
        getIdsOfAllCurrents().flatMap(ids =>
          DocumentManager
            .countHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter)
            .flatMap(totalHistory =>
            DocumentManager.getHistoricLast(rtype, rid, if(ids.nonEmpty) DocumentManager.andQueries(filter.toSeq :+ nin(FHIR_COMMON_FIELDS.ID, ids:_*)) else  filter, count, numToSkip.toInt).map(historicDocs =>
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
  def searchHistoricDocuments(rtype:String,  rid:Option[String], filter:Option[Bson], count:Int = -1, page:Int = -1)(implicit transactionSession: Option[TransactionSession] = None):Future[(Long,Seq[Document])]  = {
    val finalFilter = andQueries(rid.map(ridQuery).toSeq ++ filter.toSeq)
    val numOfDocs:Future[(Long, Long)] =
      for {
        totalCurrent <- getCount(rtype, finalFilter)
        totalHistory <- getCount(rtype, finalFilter, history = true)
      } yield totalCurrent -> totalHistory

    numOfDocs.flatMap {
      case (totalCurrent, totalHistory) =>
        val fresults =
          rid match {
            //If resource id exists, we know that historic documents come after current one
            case Some(_) =>
              //If they want page 1, look also to the current collection for resource type
              if(page == 1)
                searchHistoricDocumentsHelper(rtype, history = false, finalFilter, count, 0)
                  .flatMap(currentResults =>
                    //If count is not one or current result is not empty, search also history and merge
                    if(count!=1 || totalCurrent > 0)
                      searchHistoricDocumentsHelper(rtype, history = true, finalFilter, count-1, 0).map(historyResults =>
                        currentResults ++ historyResults
                      )
                    else
                      Future.apply(currentResults)
                  )
              else {
                //Skip this number of record
                val skip = count * (page-1) - 1
                //Otherwise search the history directly
                searchHistoricDocumentsHelper(rtype, history = true, finalFilter, count, skip)
              }
            //If history interaction is executed on type level
            case None =>
              searchHistoricDocumentsHelper(rtype, history = false, finalFilter, count, count * (page-1)).flatMap { cresults =>
                  if(count * page > totalCurrent) {
                    var skip = count * (page-1) - totalCurrent
                    if(skip < 0) skip = 0
                    searchHistoricDocumentsHelper(rtype, history = true, finalFilter, count - cresults.length, skip.toInt).map { hresults =>
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
  private def searchHistoricDocumentsHelper(rtype:String, history:Boolean,  finalFilter:Option[Bson], count:Int, skip:Int)(implicit transactionSession: Option[TransactionSession] = None):Future[Seq[Document]] = {
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
    if(count > 0)
      query = query.skip(skip).limit(count)

    //Exclude extra params
    query = query.projection(exclude(FHIR_COMMON_FIELDS.MONGO_ID))

    //Sort by last updated and version id
    val LAST_UPDATED = s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.LAST_UPDATED}"
    val VERSION_ID   = s"${FHIR_COMMON_FIELDS.META}.${FHIR_COMMON_FIELDS.VERSION_ID}"
    query = query.sort(descending(LAST_UPDATED, VERSION_ID))

    //Execute the query
    query.toFuture()
  }

  /**
    * Returns a boolean future value for the existence of a resource with given id and type
    *
    * @param rtype type of the resource
    * @param id id of the resource
    * @return a future boolean value for the existence of the resource
    */
  def documentExists(rtype:String, id:String) : Future[Boolean] = {
    val collection = MongoDB.getCollection(rtype)
    collection
      .countDocuments(and(ridQuery(id), isActiveQuery))
      .head()
      .map(count => count > 0) //check if count of the given resource is greater than 0
  }

  /**
    * Inserts the given document into a appropriate collection
    * @param rtype type of the resource
    * @param document document to be inserted
    * @return a future indicating the completion of insert operation
    */
  def insertDocument(rtype:String, document:Document)(implicit transactionSession: Option[TransactionSession] = None):Future[Completed] = {
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
    MongoDB.
      getCollection(rtype)
      .bulkWrite(documents.map(d => new InsertOneModel[Document](d)), BulkWriteOptions.apply().ordered(ordered))
      .toFuture()
      .map(br => br.getUpserts.asScala)
  }

  /**
    * Insert new version of a document while storing previous latest version to history
    * @param rtype Resource type
    * @param rid Resource id
    * @param oldDocument Version number and old document version
    * @param newDocumentOrDeleted New version of document or status code for deletion
    * @return
    */
  def insertNewVersion(rtype:String, rid:String, newDocumentOrDeleted:Either[Document, (String, Instant)], oldDocument:(Long, Document), shardQuery:Option[Bson] = None)(implicit transactionSession: Option[TransactionSession] = None):Future[Completed] = {
    val needTransaction = transactionSession.isEmpty && OnfhirConfig.mongoUseTransaction
    //Create a transaction session if we support it but it is not part of a transaction
    val tempTransaction =
      if(needTransaction) Some(new TransactionSession(UUID.randomUUID().toString)) else transactionSession

     insertOldDocumentToHistory(rtype, rid, oldDocument._1, oldDocument._2)(tempTransaction) //If there is old version, push it to history
        .flatMap {
          case false =>
            //Abort the transaction, if any
            val transactionFinalize = if(needTransaction)
              tempTransaction.get.abort()
            else
              Future.apply(Completed())
            transactionFinalize.map(_ => {
              //Schedule a check on persistency in case there is a invalid state, and we don't support Mongo transactions
              if(!OnfhirConfig.mongoUseTransaction)
                DBConflictManager.scheduleCheckAndCleanupForHistory(rtype, rid, "" + oldDocument._1)
              //Throw 409 Conflict exception
              throw throw new ConflictException(
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
            (newDocumentOrDeleted match {
              case Left(newDocument) => replaceCurrent(rtype, rid, newDocument, shardQuery)(tempTransaction)
              case Right((sc, lastModified)) => deleteCurrent(rtype, rid, sc, oldDocument._1 + 1, lastModified, shardQuery)
            }).flatMap( _ =>
              if(needTransaction)
                tempTransaction.get.commit()
              else
                Future.apply(Completed())
            )
        }
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
    * @param sc                 Status code for deletion
    * @param newVersion         New version number for deletion
    * @param transactionSession Transaction session
    * @return
    */
  def deleteCurrent(rtype:String, rid:String, sc:String, newVersion:Long, lastModified:Instant, shardQuery:Option[Bson])(implicit transactionSession: Option[TransactionSession] = None):Future[Boolean] = {
    val collection = MongoDB.getCollection(rtype)
    val baseQuery = equal(FHIR_COMMON_FIELDS.ID, rid)
    val query = shardQuery.map(sq => and(sq,  baseQuery)).getOrElse(baseQuery)

    collection
      .updateOne(
        query,
        Updates.combine(
          //Set the new version id
          Updates.set(FHIR_COMMON_FIELDS.META + "." +FHIR_COMMON_FIELDS.VERSION_ID, ""+newVersion),
          //Set the last update time
          Updates.set(FHIR_COMMON_FIELDS.META + "." +FHIR_COMMON_FIELDS.LAST_UPDATED,
            BsonTransformer.createBsonTimeObject(DateTimeUtil.serializeInstant(lastModified))
          ),
          //Set the status code
          Updates.set(FHIR_EXTRA_FIELDS.STATUS_CODE, sc),
          //Set method as delete
          Updates.set(FHIR_EXTRA_FIELDS.METHOD, FHIR_METHOD_NAMES.METHOD_DELETE),
        )
      )
      .toFuture()
      .map(updateResult => updateResult.getModifiedCount == 1)
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
                               lastOrFirstN:Int = -1,
                               sortingPaths:Seq[(String, Seq[String])] = Seq.empty,
                               groupByExpressions:Seq[(String, BsonValue)],
                               includingOrExcludingFields:Option[(Boolean, Set[String])] = None,
                               excludeExtraFields:Boolean = false):Future[Seq[(Document, Seq[Document])]] = {

    val aggregations = new ListBuffer[Bson]
    //First, append the match query
    if(filter.isDefined)
      aggregations.append(Aggregates.`match`(filter.get))

    val spaths = sortingPaths.map(s => (s._1, if(lastOrFirstN < 0) -1 else 1, s._2))

    //Identify the sorting params that has alternative paths
    val paramsWithAlternativeSorting = spaths.filter(_._3.length > 1) //Those that have multiple paths

    //Then add common sorting field for sort parameters that has multiple alternative paths
    aggregations.append(paramsWithAlternativeSorting.map(sp => addFieldAggregationForParamWithAlternativeSorting(sp)) : _*)

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
    aggregations.append(handleProjectionForAggregationSearch(includingOrExcludingFields, excludeExtraFields, extraSortingFieldsToExclude.toSet))

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
      .aggregate(aggregations)  //run the aggregation pipeline
      .allowDiskUse(true) //these aggregations may not fit into memory restrictions defined by mongo
      .toFuture()
      .map(results =>
        results
          .map(d =>
            if(Math.abs(lastOrFirstN)> 1)
              Document(d.get[BsonDocument]("_id").get) ->
                d.get[BsonArray]("bucket").get.getValues.asScala.map(v => Document(v.asInstanceOf[BsonDocument]))
            else
              Document(d.get[BsonDocument]("_id").get) -> d.get[BsonDocument]("first").toSeq.map(Document(_))
          )
      )
  }
}

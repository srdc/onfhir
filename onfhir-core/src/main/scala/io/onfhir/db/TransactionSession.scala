package io.onfhir.db

import java.util.concurrent.TimeUnit
import io.onfhir.config.OnfhirConfig
import org.mongodb.scala.{MongoException, ScalaClientSession, SingleObservable}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}

/**
  * Transaction Session object for FHIR transactions
  */
class TransactionSession(val transactionId:String) {

  /**
    * Mongo Database transaction session
    */
  val dbSession:org.mongodb.scala.ClientSession = {
    val clientSession = Await.result(MongoDB.createSession().head(), FiniteDuration.apply(OnfhirConfig.fhirRequestTimeout.toMinutes, TimeUnit.MINUTES))
    clientSession.startTransaction(MongoDB.transactionOptions)
    clientSession
  }

  /**
    * Run and commit the transaction
    * @return
    */
  def commit():Future[Unit] = {
    val commitTransactionObservable: SingleObservable[Void] = new ScalaClientSession(dbSession).commitTransaction()
    val commitAndRetryObservable: SingleObservable[Void] = commitAndRetry(commitTransactionObservable)
    runTransactionAndRetry(commitAndRetryObservable).head().map(_ => ())
  }

  /**
    * Abort the transaction
    * @return
    */
  def abort():Future[Unit] = {
    //IMPORTANT: Their future implementation has a problem for ScalaClientSession so we just abort here without waiting
    dbSession.abortTransaction()
    Future.apply(())
  }

  /**
    * Commit transaction and retry if not successfull
    * @param observable
    * @return
    */
  private def commitAndRetry(observable: SingleObservable[Void]): SingleObservable[Void] = {
    observable.recoverWith({
      case e: MongoException if e.hasErrorLabel(MongoException.UNKNOWN_TRANSACTION_COMMIT_RESULT_LABEL) => {
        println("UnknownTransactionCommitResult, retrying commit operation ...")
        commitAndRetry(observable)
      }
      case e: Exception => {
        println(s"Exception during commit ...: $e")
        throw e
      }
    })
  }

  private def runTransactionAndRetry(observable: SingleObservable[Void]): SingleObservable[Void] = {
    observable.recoverWith({
      case e: MongoException if e.hasErrorLabel(MongoException.TRANSIENT_TRANSACTION_ERROR_LABEL) => {
        println("TransientTransactionError, aborting transaction and retrying ...")
        runTransactionAndRetry(observable)
      }
    })
  }
}

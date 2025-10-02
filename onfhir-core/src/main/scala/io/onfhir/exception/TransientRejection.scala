package io.onfhir.exception

import akka.http.scaladsl.server.Rejection

/**
 * Transient rejections due to internal error
 * @param msg Message indicating the phase the problem occurs
 * @param t   Exception thrown
 */
case class TransientRejection(msg:String, t:Throwable) extends Rejection

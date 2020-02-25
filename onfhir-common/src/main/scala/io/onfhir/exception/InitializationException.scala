package io.onfhir.exception

/**
  * Created by tuncay on 11/15/2016.
  */
class InitializationException (reason:String, innerException:Option[Exception] = None) extends Exception(reason:String) {

}


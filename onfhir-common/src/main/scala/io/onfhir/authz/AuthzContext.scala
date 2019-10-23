package io.onfhir.authz

import java.util.Date

import net.minidev.json.JSONArray

/**
  * Created by tuncay on 2/27/2017.
  * * Authorization Context for onFhir
  *
  * @param isActive         If the token is active or not
  * @param clientId         Identifier of client that the user is accessing from
  * @param scopes           Authorization scopes given to the user
  * @param expirationTime   Expiration time of this context (scopes, etc)
  * @param aud              Audiences of the token
  * @param sub              Identifier of the user who is trying to access
  * @param furtherParams    All other parameters related with user and client
  * @param username         Human-readable identifier for the user who is trying to access
  * @param reasonNotActive
  */
case class AuthzContext(
                         isActive:Boolean,
                         clientId:Option[String]=None, //
                         scopes:Seq[String]=Seq.empty, //
                         expirationTime:Option[Date]=None, //
                         aud:Seq[String]=Seq.empty, //
                         sub:Option[String]=None, //
                         furtherParams:Map[String, Any] = Map.empty, //
                         username:Option[String] = None, //
                         reasonNotActive:Option[String] = None) {

  /**
    * If authorization is expired
    * @return
    */
  def isExpired:Boolean = {
    (isActive && (expirationTime.nonEmpty && expirationTime.get.getTime < new Date().getTime)) ||
      (!isActive && expirationTime.isEmpty)
  }

  /**
    * Get a simple parameter from further parameters
    * @param pname  Name of parameter
    * @tparam T     Type of parameter
    * @return
    */
  def getSimpleParam[T](pname:String):Option[T] = {
      furtherParams.get(pname).map(_.asInstanceOf[T])
  }

  /**
    * Get simple list parameter from further parameters
    * @param pname  Name of parameter
    * @tparam T     Type of parameter
    * @return
    */
  def getListParam[T](pname:String):Option[List[T]] = {
    furtherParams.get(pname).map {
      case jsonArray:JSONArray => jsonArray.toArray.toList.asInstanceOf[List[T]]
      case listOfT:List[Any] => listOfT.asInstanceOf[List[T]]
      case t:Any => List(t.asInstanceOf[T])
      case _ => List.empty
    }
  }
}

package io.onfhir.authz

/**
  * Created by tuncay on 5/11/2017.
  * Authentication context
  * @param accessToken      Access token for the request
  * @param networkAddress   Network address of client that sends the request
  */
case class AuthContext(accessToken:Option[String], networkAddress:String)

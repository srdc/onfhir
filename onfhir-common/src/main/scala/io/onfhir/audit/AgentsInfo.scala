package io.onfhir.audit

/**
  * Internal model for Audit Agents (parties that take role)
  * @param userId                 User identifier accessing the user
  * @param refToIdentityResource  Reference to the Identity resource corresponding to user e.g. Practitioner/....
  * @param roles                  Roles of the user (system,code)
  * @param userName               Name of the user
  * @param clientId               Identifier for the client system
  * @param clientName             Name of the client system
  * @param networkAddress         Network IP address where the user is accessing to the resources
  */
case class AgentsInfo(
                       userId:Option[String], //
                       refToIdentityResource:Option[String], //
                       roles:Seq[(Option[String], String)], //
                       userName:Option[String], //
                       clientId:Option[String], //
                       clientName:Option[String], //
                       networkAddress:String) //
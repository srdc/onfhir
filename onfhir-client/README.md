# onfhir-client
This module a provides a Scala library that you can use as a FHIR client to easily construct and send FHIR requests 
(search, CRUD, operations) to a configured FHIR server (similar Firely .Net FHIR client). 

You can use the library over the wrapper [io.onfhir.client.OnFhirNetworkClient](./src/main/scala/io/onfhir/client/OnFhirNetworkClient.scala).
There are two type of constructors for the OnFhirNetworkClient. The first one gets simply the base URL for FHIR endpoint. 
This can be used for FHIR servers that does not apply any authentication mechanism. Both constructors need akka ActorSystem
implicitly as the library is based on Akka HTTP. If you are already using akka actors in your code you can use the ActorSystem 
you have created for your system. Otherwise, you can simply construct an ActorSystem and OnFhirNetworkClient instance 
and share and use it everywhere you try to access target FHIR API.

The following snippet shows a simple usage of the library. 

```
import io.onfhir.util.JsonFormatter._
import akka.actor.ActorSystem
import io.onfhir.api.Resource
import io.onfhir.client._

# An implicit akka ActorSystem is required as module is based on Akka Http
implicit val actorSystem: ActorSystem = ActorSystem("OnFhirClientExample")

# Constuct the client wrapper
val fhirClient: OnFhirNetworkClient = OnFhirNetworkClient.apply("http://127.0.0.1:8080/fhir")

# Creating a FHIR resource
val patient1: Resource =  Source.fromFile(...).mkString.parseJson
var persistedPatient1 = Await.result(fhirClient.create(patient1), 2 seconds)

# Updating a FHIR resource
... //Update some elements of the persistedPatient1 resource
persistedPatient1 = Await.result(fhirClient.update(persistedPatient1), 2 seconds)

# Searching for FHIR resources
val bundle:FHIRSearchSetBundle = 
    Await.result(
            fhirClient
                .search("Patient")
                .where("gender", "male"), 
            2 seconds)    
  
bundle.entries.map(e => ...) //Access to entries in the FHIR search-set bundle
```
## Constructing FHIR requests
You can start to construct your FHIR request by using one of the methods given in the io.onfhir.api.client.BaseFhirClient 
given in onfhir-common module. 

| Method signature                                        | FHIR interaction | Description/Example                                                                                                                                                                                                                                                                                                                                                                                                          |
|---------------------------------------------------------|------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
 | create(r:Resource)                                      | create           | Creating given FHIR resource.                                                                                                                                                                                                                                                                                                                                                                                                |
 | read(rype:String, rid:String)                           | read             | Reading a resource by giving FHIR resurce type and identifier. e.g. _.read("Patient", "p1")                                                                                                                                                                                                                                                                                                                                  |
 | update(r:Resource, forceVersionControl:Boolean = false) | update           | Updating a FHIR resource. The given FHIR resource should have an resource identifier (Resource.id). If you set forceVersionControl to true (See related [FHIR API documentation](https://hl7.org/fhir/R5/http.html#concurrency)), the client uses If-Match header (as described in FHIR standard) with the current version of the resource obtained from given resource content to execute update if current version matches |
| delete(rype:String, rid:String)                         | delete           | Deleting the FHIR resource given by resource type and resource identifier                                                                                                                                                                                                                                                                                                                                                    |
| delete(r:Resource)                                      | delete           | Deleting the given FHIR resource where the resource type and resource identifier is extracted from the content                                                                                                                                                                                                                                                                                                               |
| delete(rtype:String)                                    | delete           | Used when you want to construct a conditional delete request by giving the resource type                                                                                                                                                                                                                                                                                                                                     |
 | vread(rype:String, rid:String, vid:String)              | vread            | For reading a specific version of a FHIR resource e.g. vread("Encounter", "e1", "3")                                                                                                                                                                                                                                                                                                                                         |
| patch(rype:String, rid:String)                          | patch            | Initiating a FHIR patch request for the given FHIR resource type and identifier                                                                                                                                                                                                                                                                                                                                              |
| patch(r:Resource)                                       | patch            | Initiating a FHIR patch request for the given resource where resource type and identifier will be extracted from the given content                                                                                                                                                                                                                                                                                           |
| patch(rtype:String)                                     | patch            | Initiating a conditional FHIR path request for the given FHIR resource type                                                                                                                                                                                                                                                                                                                                                  |
| history(rtype:String, rid:String)                       | history          | For reading the history of a specific FHIR resource given by FHIR resource type and identifier                                                                                                                                                                                                                                                                                                                               |
| history(rtype:String, rid:String, count:Int)            | history          | For reading the history of a specific FHIR resource with pagination parameter count                                                                                                                                                                                                                                                                                                                                          |
 | history(r:Resource)                                     | history          | For reading the history of given FHIR resource where resource type and identifier are extracted from content                                                                                                                                                                                                                                                                                                                 |
| history(r:Resource, count:Int)                          | history | For reading the history of given FHIR resource with given pagination parameter where resource type and identifier are extracted from content                                                                                                                                                                                                                                                                                 |
| history(rtype:String)                                   | history | For initiating type-level history request with given FHIR resource type                                                                                                                                                                                                                                                                                                                                                      |
| history(rtype:String, count:Int)                        | history | or initiating type-level history request with given FHIR resource type and pagination parameter                                                                                                                                                                                                                                                                                                                              |
| search(rtype:String)                                    | search | For initiating a FHIR search request on given FHIR resource type with default pagination                                                                                                                                                                                                                                                                                                                                     |
| search(rype:String, count:Int)                          | search | For initiating a FHIR search request on given FHIR resource type with given pagination parameter                                                                                                                                                                                                                                                                                                                             |
| search(rype:String, count:Int, page:Int)                | search | For initiating a FHIR search request on given FHIR resource type with given pagination parameters                                                                                                                                                                                                                                                                                                                            |
 | operation(opName:String) | operation | For initiating a specific FHIR Operation request with given operation name                                                                                                                                                                                                                                                                                                                                                   |
| batch() | batch | For initiating a FHIR batch request                                                                                                                                                                                                                                                                                                                                                                                          |
| transaction() | transaction | For initiating a FHIR transaction request                                                                                                                                                                                                                                                                                                                                                                                    |

After calling this initial method, a request builder specific to the FHIR interaction is returned which you can continue 
on to set further settings if necessary.

### FHIR search requests
For search requests you can use the following methods to construct your final request.
* forCompartment(ctype:String, cid:String) : If you want compartment search, you can provide compartment type and identifier. e.g. _.forCompartment("Patient", "p1")
* where(param:String, value:String) : With this method you can add search parameter statement to your query. e.g. _.where("gender", "male") --> ?gender=male
* byHttpPost(): If you use this, the search request will be send as HTTP Post rather than default HTTP Get.
* sortOnAsc(params:String*): You can provide the parameter names in order to sort the result set accordingly in ascending order.
* sortOnDesc(params:String*): You can provide the parameter names in order to sort the result set accordingly in descending order.
* strictHandling(): Set the strict handling for search (Prefer: handling=strict)
* lenientHandling(): Set the lenient handling for search (Prefer: handling=lenient)

```
    # Searching blood pressure measurements of patient with id 'p1' after January 1 2024 sorted descending on date
    fhirClient
        .search("Observation")
        .forCompartment("Patient", "p1")
        .where("code", "http://loinc.org|85354-9")
        .where("date", "ge2024-01-01")
        .sortOnDesc("date")
```

### FHIR CRUD requests
The followings are further options that can be used while constructing create, update, patch and batch/transaction requests. 
See [related section](https://hl7.org/fhir/R5/http.html#ops) in FHIR standard for details.  
* returnMinimal(): Used for create/update/patch/transaction requests to indicate that FHIR Response can be minimal as 
client does not need it (i.e. adding HTTP Header Prefer: return=minimal). If used, the returned FHIR response does not 
include the content for example created/updated FHIR resource but only related headers. 
* returnOperationOutcome(): Used for create/update/patch/transaction requests to ask the server to return an 
OperationOutcome resource containing hints and warnings about the operation rather than the full resource. 

For FHIR read requests, the following options can be used for conditional read. See [FHIR standard](https://hl7.org/fhir/R5/http.html#cread) details.
* ifModifiedSince(since:DateTime): To indicate the resource is requested if it is modified after given time. 
* ifNoneMatch(version:Long): To indicate the resource is requested if a newer version is available.

For conditional create, update, delete, patch requests, you can use the following option as in the search to provide the conditional statement.
* where(param:String, value:String)

```
    # Updating the observation that the identifier is matching or creating the resource if not exist. 
    fhirClient
        .update(updatedObservation)
        .where("identifier", "http://my-lab-system|123")
```

For patch requests, you can use the following options to choose the patch type.
* patchContent(patch:JValue): Either you can directly provide the FHIR patch or Json patch content you want to use in 
which case the request builder parses and uses it.
* fhirPathPatch(): Initiates a [FHIR Path patch](https://hl7.org/fhir/R5/fhirpatch.html) request 
* jsonPatch(): Initiates a [JSON Patch](https://tools.ietf.org/html/rfc6902) request

For FHIR Path patch, you can use the following options.
* patchAdd(path:String, name:String, value:(String, JValue)): Patching a FHIR element on a specific path with given name, value and FHIR data type.
* patchDelete(path:String): Deleting a FHIR element on given path.
* patchInsert(path:String, index:Int, value:(String, JValue)): Inserting a content into an array on given path and index.
* patchMove(path:String, source:Int, destination:Int): Move an element within an array on given path from soure index to destination index.
* patchReplace(path:String,value:(String, JValue)): Replace the content on given path.

```
  # Update the status of Observation resource with identifier 'obs1' to final if its status is preliminary. 
  fhirClient
     .patch("Observation","obs1")
     .where("status", "preliminary")
     .fhirPathPatch()
     .patchReplace("Observation.status", "code" ->  JString("final"))
  
   # Updating the active HbA1c Goal of patient p1 as achieved by also setting the date of update and the Observation 
   #that causes this achievement
   val achieved = 
        JObject(
            "coding" -> JArray(List(
                JObject(
                    "system" -> JString("http://terminology.hl7.org/CodeSystem/goal-achievement")
                    "code" -> JString("achieved)
                    )
            ))
        )
   fhirClient
     .patch("Goal")
     .where("subject", "Patient/p1")
     .where("target-measure", "http://loinc.org|4548-4")
     .where("lifecycle-status", "active")
     .fhirPathPatch()
     .patchAdd("Goal", "achievementStatus",  "CodeableConcept" ->   achieved)
     .patchReplace("Goal.statusDate", "date" -> JString("2024-06-14"))
     .patchInsert("Goal.outcome", 0, "CodeableReference" -> JObject("reference" -> JObject("reference" -> Jstring("Observation/obs1")))        
```
Similarly, for JSON Patch, you can use the following options.
* patchAdd(path:String, value:JValue): Add the JSON value to the given JSON path.
* patchCopy(from:String, path:String): Copy the JSON content given in from as JSON path to the given path
* patchMove(from:String, path:String): Move the JSON content from one path to another.
* patchRemove(path:String): Delete the content given in the path.
* patchReplace(path:String, value:JValue): Replate the JSON content given in the path with the given content
```
    #Update the status of observiation as preliminary and add a new Coding to the first component's code in position 1
    fhirClient
      .patch("Observation","obs1")
      .jsonPatch()
      .patchReplace("status", JString("preliminary"))
      .patchAdd("/component/0/code/coding/1", JObject("system" -> JString("test"), "code" -> JString("test")))
```

### FHIR batch/transaction requests
For batch/transaction requests you can use the following options to add new child requests.
* entry(rbFunction:IOnFhirClient => FhirRequestBuilder): You can provide a function returning a request builder given fhir client. 
* entry(fullUrlUuid:String, rbFunction:IOnFhirClient => FhirRequestBuilder): This provides the same, but it additionally assigns an UUID 
to the child request which will be used in Bundle.entry.fullUrl for the child request. This is used when you want to 
match the responses with the requests.

```
   # Execute multiple requests in a batch 
   fhirClient
     .batch()
     .entry(_.create(diagnosticReport))
     .entry(_.create(bloodGlucoseOb))
     .entry(_.update(bloodGlucoseGoal))
     .entry(
        _
         .delete("AdverseEvent")
         .where("patient", "Patient/p1")
     )
     
   # Execute multiple requests in a transaction by assigning a uuid to requests
   fhirClient
     .transaction()
     .entry(bloodClucodeUuid, _.create(bloodGlucoseOb))
     .entry(diagnosticReportUuid, _.create(diagnosticReport))
     .entry(bloodGlucoseGoalUuid,_.update(bloodGlucoseGoal))
```

## Handling authentication process
The OnFhirNetworkClient provides a constructor where you can supply interceptor(s) that can add necessary HTTP headers for 
the configured authentication protocol and custom requirements of target FHIR server. In default, this module provides 
the following interceptors that can be useful for certain authentication mechanisms.

### [BasicAuthenticationInterceptor](./src/main/scala/io/onfhir/client/intrcp/BasicAuthenticationInterceptor.scala)
You can use this interceptor if target FHIR API service supports HTTP Basic Authentication. As shown below its constructor 
just gets the username and password that is given to your system for FHIR API access.

```
val authInterceptor = new BasicAuthenticationInterceptor("myuser", "mypassword")
val fhirClient = OnFhirNetworkClient.apply("http://127.0.0.1:8080/fhir", authInterceptor)
...
```
### [FixedBearerTokenInterceptor](./src/main/scala/io/onfhir/client/intrcp/FixedBearerTokenInterceptor.scala)
If the target FHIR server supports Bearer authentication (i.e. HTTP Header -> Authorization = Bearer .... ), and if 
your system is given a fixed bearer token or you want to get the token yourself, you can simply use this interceptor.
```
val fixedToken = ...
val authInterceptor = new FixedBearerTokenInterceptor(fixedToken)
val fhirClient = OnFhirNetworkClient.apply("http://127.0.0.1:8080/fhir", authInterceptor)
...
```
### [BearerTokenInterceptorFromTokenEndpoint](./src/main/scala/io/onfhir/client/intrcp/BearerTokenInterceptorFromTokenEndpoint.scala)
If the target FHIR server supports Bearer authentication and if your system is expected to get the token from a token 
endpoint (Smart-on-fhir authentication; OpenID Connect token endpoint) with client credentials assigned to your system you can use this interceptor.

Interceptor has constructor with the following parameters.
* clientId : Assigned client identifier by the authorization server for your system
* client_secret: Client secret given by the authorization server for your system
* requiredScopes: Set of scopes you need in the token to access the intended FHIR resources
* token_endpoint: URL of the OAuth2.0/OpenID Token endpoint of the authorization server
* token_endpoint_auth_method: Client authentication method for the token request. Please check [OpenID Client Authentication section](https://openid.net/specs/openid-connect-core-1_0.html#ClientAuthentication)  
Only the following authentication methods are supported by this interceptor.
    * 'client_secret_basic'
    * 'client_secret_post'
    * 'client_secret_jwt' 

```
val authInterceptor = 
    BearerTokenInterceptorFromTokenEndpointa
        .apply(
            clientId = "myclient", 
            clientSecret = "mysecret",
            requiredScopes = Seq("patient/Observation.rs", "patient/Condition.rs"),
            authzServerTokenEndpoint = "http://authzserver.com/token",
            clientAuthenticationMethod = "client_secret_basic"    
        )
val fhirClient = OnFhirNetworkClient.apply("http://127.0.0.1:8080/fhir", authInterceptor)
...
```
You can also configure the OpenID/OAuth related parameters in your configuration file (Typesafe config) under 'onfhir.client.authz' as shown in followings.
```
# Your <application.conf> file 

onfhir.client {
  authz {
    # Assigned client identifier by the authorization server for your client
    client_id = "myclient"
    # Client secret given by the authorization server for your client
    client_secret="mysecret"
    # URL of the OAuth2.0 Token endpoint of the authorization server
    token_endpoint="http://authzserver.com/token"
    # Client authentication method; use either 'client_secret_basic', 'client_secret_post' or 'client_secret_jwt'
    token_endpoint_auth_method ="client_secret_basic"
  }
}
```
Then you can call the other constructor as shown in the following snippet. 
```
val config:Config = ... //Load your config file
val authInterceptor = 
    BearerTokenInterceptorFromTokenEndpointa
        .getFromConfig(config, requiredScopes = Seq("patient/*.rs")) //Assume we need to access all resources for the patient
        
val fhirClient = OnFhirNetworkClient.apply("http://127.0.0.1:8080/fhir", authInterceptor)
...
```

## Handling responses
The library provides several alternative formats to get the FHIR response for most of the interactions. The basic one is 
where you want to get the whole FHIR response details with [io.onfhir.api.model.FHIRResponse](../onfhir-common/src/main/scala/io/onfhir/api/model/FHIRResponse.scala) by calling execute() method.
This is illustrated in the following example.

```
val fhirResponse:Future[FHIRResponse] = 
    fhirClient
      .create(observationResource)
      .execute()
      
      
fhirResponse
   .foreach {
      //When interaction is rejected due to an error
      case r: FHIRResponse if r.isError =>
         //You can access the parsed outcome issues for error responses
         logger.error(s"Observation resource cannot be created. HttpStatus: ${r.httpStatus.intValue()} Issues: ${r.outcomeIssues}")
      //Otherwise, it is successfull. You can access the identifier of the resource from the Location header as indicated in the standard
      case r:FHIRResponse =>  
         //This will print for example '... at location http://localhost:8080/fhir/Observation/21325325'   
         logger.debug(s"Observation is created successfully at location ${r.location.get}")
         //You can access the created resource if the request is configured to return the created resource
         val createdResource = r.responseBody.get
         ...
   }      

```
Other than this basic option, there are two further options that can be useful for FHIR CRUD interactions where response returns optional FHIR resource.
* executeAndReturnResource() : Returns directly the FHIR resource created/updated/patched/read if interaction is
successfull and a resource is returned. Otherwise, if the interaction is failed with other status codes, it throws FhirClientException which also includes the FHIRResponse itself.
* executeAndReturnResourceOption() : Returns the optional FHIR resource created/updated/patched/read if interaction is successfull and returns None otherwise.

```
 try {
   val createdObservation: Future[Resource] =
       fhirClient
         .create(patientWithoutId)
         .executeAndReturnResource()
    ...     
 } catch {
        case fce:FhirClientException =>
          logger.error(fce.msg, fce)
 }
 
  //For optional resource return
  fhirClient
     .read("Patient", "123")
     .executeAndReturnResourceOption()
     .flatMap {
        //No such patient
        case None => ...
        case Some(patientResource) => ...
     }
```

For search like operations (FHIR search and history) there are also further options.
* executeAndReturnBundle(): This option returns future of FHIRSearchSetBundle where you can easily access entries and included resources as well as make another call for next page (pagination of search). 
* toIterator(): This returns an iterator where you can iterate over the search results via the specified pagination.  
* executeAndMergeBundle(): This return a merged FHIR Bundle including all the search results by handling the pagination internally by repeatedly calling the new page.

The following snippet shows an example usage for the first option.
```
      var resultSetBundle: FHIRSearchSetBundle = null
      
      do {
        //For the first time, search Observations and include patients
        if(resultSetBundle == null)
          resultSetBundle = 
            Await.result(
              fhirClient
                .search("Observation", count = 50)
                .where("code", "http://loinc.org|5014-4")
                .where("_include", "Observation:patient")
                .executeAndReturnBundle(),
              1 seconds)
        else 
          //For further iteration, just call the next page over the bundle   
          resultSetBundle = Await.result(fhirClient.next(resultSetBundle), 1 seconds)

        //Access to returned Observations and patients that own them
        val observations = resultSetBundle.searchResults
        observations.map(obs => {
          val patientId = FhirPathEvaluator().evaluateOptionalString("Observation.subject.reference", obs).get
          val patient = resultSetBundle.includedResults.get(patientId)
          
          ...
        })
      } while(resultSetBundle.hasNext()) 
```
The following snippet shows second option with iterator usage.
```
  val resultSetItr: Iterator[Future[FHIRSearchSetBundle]] =
     onFhirClient
       .search("Observation", count = 50)
       .where("code", "http://loinc.org|5014-4")
       .where("_include", "Observation:patient")
       .toIterator()
        
  while(resultSetItr.hasNext) {
     val resultSetBundle = Await.result(resultSetItr.next(), 1 seconds)
     val observations = resultSetBundle.searchResults
        ...
  }
```
Finally, you can access the whole result set by using the third option.
```
  val allResultsBundle =
       Await.result(
         onFhirClient
          .search("Observation", count = 50)
          .forCompartment("Patient", "123")
          .executeAndMergeBundle(), 1 seconds)

  val allObservationsOfPatient = allResultsBundle.searchResults
  ...
```

Please also note that as for all of these options, an implicit method is added to FHIRRequestBuilder, you may just omit 
this execution method from your code and the implicit conversion can handle it while assigning the result to the variable 
according to type of the variable. The following illustrates this.
```
  //No need to call executeAndReturnBundle as it is implicity handled if you assing it to the correct typed variable          
  var resultSetBundle: FHIRSearchSetBundle =
      Await.result(fhirClient
          .search("Observation", count = 50)
          .where("code", "http://loinc.org|5014-4"), 1 seconds)
                  
```

## Easy mutation of FHIR Content
This module also provides a utility class, [io.onfhir.client.util.FhirResourceMutator](./src/main/scala/io/onfhir/client/util/FhirResourceMutator.scala) 
to use for mutation of FHIR content parsed into Json4s objects. This wrapper provides several methods to add,update or remove elements to 
your FHIR resources that you retrieved by using OnFhir client library. These methods provide a similar mechanism with FHIR Path Patch interaction.  
You only need to import "io.onfhir.client.util.FhirResourceMutator._" to implicitly access these methods on io.onfhir.api.Resource (JObject type). 
This is illustrated in the following example. Check the details in the documentation of methods in class definition.

```
import io.onfhir.client.util.FhirResourceMutator._

...

val episodeOfCare = Await.result(fhirClient.read("EpisodeOfCare", "eps1"), 1 seconds)
//Make changes on the EpisodeOfCare resource
val updatedEpisode =
     episodeOfCare
       .addElement("EpisodeOfCare", "status", JString("finished")) //Set status as finished
       .addElement("EpisodeOfCare.period", "end", JString(LocalDate.now().toString())) //set the end time
//Call FHIR update
fhirClient.update(updatedEpisode)
```

# onFHIR FHIR Repository

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.onfhir/onfhir-core/badge.svg)](https://search.maven.org/search?q=io.onfhir)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

[onFHIR](http://onfhir.io) is a FHIR compliant secure health data repository that you can use as a central data service for your FHIR compliant healthcare applications. 
You can use it as a standalone server, or you can extend it with your further custom FHIR Operations to build your own application layer in addition to having standard FHIR repository capabilities. 
onFHIR.io is using FHIR Infrastructure Resource definitions (CapabilityStatement, StructureDefinition, SearchParameter, etc.) to tailor 
the FHIR server to your specific FHIR capabilities you required; resource profiles, search parameters, FHIR interactions you wanted to support.     

It is implemented with Scala, based on Akka and MongoDB.

Stable versions are released in [Maven Central](https://search.maven.org/search?q=io.onfhir) and snapshot versions in [Sonatype Snapshot Repository](https://oss.sonatype.org/content/repositories/snapshots/io/onfhir/).

## Basic Configuration
You can copy and update **onfhir-core/src/main/resources/application.conf** file, which is the entrypoint configuration to configure onFHIR repository based on your needs.

For logger configurations, check **onfhir-core/src/main/resources/logback.xml**

For configuration of the FHIR API to be provided, you need to provide the followings;
* A file providing your **Conformance statement** (FHIR Capability Statement - See http://hl7.org/fhir/capabilitystatement.html) that describes the capabilities of the FHIR server you want to provide
* A folder including all your **Profile definitions** (FHIR StructureDefinition - See http://hl7.org/fhir/structuredefinition.html) including resource, data type and extension definitions that will be used in the FHIR server you want to provide
* A folder including all your **Compartment definitions** (FHIR CompartmentDefinition - See http://hl7.org/fhir/compartmentdefinition.html) for all compartments that you want to support for search
* A folder including all your **Search parameter definitions** (FHIR SearchParameter - See http://hl7.org/fhir/searchparameter.html) for all extra search parameters (apart from what is available from the base FHIR standard) that you define and support for your resources
* A folder including all your **Value sets** (FHIR ValueSet - See http://hl7.org/fhir/valueset.html) that you define and refer in your resource profiles
* A folder including all your **Operation definitions** (FHIR OperationDefinition - http://hl7.org/fhir/operationdefinition.html) that you define and refer from capability statement in operations part (For your OperationDefinitions write the full class path of your implementation of operation in OperationDefinition.name)

You can also provide the zip file for FHIR base definitions (Validation package - 'validation-min.xml.zip') that you want to support specifically. 
onFHIR supports all stable and build versions of HL7 FHIR. In this project, we provide modules for the last 3 main versions that are configured automatically with standard definitions and special configurators: 
* R5    >> onfhir-server-r5
* R4    >> onfhir-server-r4
* STU3  >> onfhir-server-stu3

## Prerequisites
onFHIR requires a MongoDB database up and running. If you do not use the given docker containers, the MongoDB configuration parameters (host, port, dbname etc.)
should be passed to onFHIR through either application.conf file or as runtime parameters. The parameter names can be seen in the provided application.conf file.

## Build & Run

You need to run the below command to build fhir-repository. This will compile 
your code, execute unit tests and create a single standalone jar with all the dependencies:
```
$ mvn package
```

Unit tests may take some time, so you can add **-DskipTests=true** command line parameter 
to the above command to skip the test execution, but it is **not recommended**:
```
$ mvn package -DskipTests=true
```

Executable standalone jars **target/onfhir-server-standalone.jar** will be created under each onfhir-server for 
different FHIR version. Executing the following command will run the onRHI server for that version with nearly whole FHIR 
capabilities.
```
$ java -jar target/onfhir-server-standalone.jar
```

You can override in-app configurations by supplying an external application.conf file or JAVA arguments
using the following commands:
```
$ java -Dconfig.file={path-to-application.conf} -jar target/onfhir-server-standalone.jar
$ java -Dserver.port=9999 -Dserver.host=172.17.0.1 -jar target/onfhir-server-standalone.jar
```

### Extensibility
You can develop your own FHIR compliant backend application based on onFHIR. In order to do this you can import the 
corresponding server module as a dependency to your project and write a scala App (Boot) that initiates onFHIR with a 
custom configuration. **Onfhir.scala** is the main entrypoint to the project. The following is the default server Boot 
configuration for onfhir-server-r4. It initiates a FHIR R4 server with the given configurations. 
```
object Boot extends App {
  //Initialize onfhir for R4
  var onfhir = Onfhir.apply(new FhirR4Configurator())
  //Start it
  onfhir.start
}
```
You can extend the onFHIR by implementing certain custom mechanisms; 
* Custom Authorizer (Implementing **io.onfhir.authz.IAAuthorizer** interface): In default(if you configure), onFHIR 
supports the authorization mechanism defined in [SmartOnFhir](https://docs.smarthealthit.org/authorization/) initiative 
which is based on OAuth2.0 Bearer Token based authorization. If you need a custom authorization mechanism with different set of 
scopes (permissions), you can implement a authorizer module and register it to onFHIR. 
* Custom Token Resolver (Implementing **io.onfhir.authz.ITokenResolver** interface): onFHIR supports two default token 
resolution methods; Signed JWT tokens and OAuth2.0 Token Introspection. You can use them by configurations or implement a new module. 
* Custom Audit Handler (Implementing **io.onfhir.audit.ICustomAuditHandler**): In default, you can configure onFHIR 
to store FHIR AuditEvent records to its own local repository, or a remote FHIR server running as a seperate audit repository. 
If you want to create audit events/logs in different format and send them to a custom audit repository (ElasticSearch+Kibana, etc),
you can extend this interface with your module and register it.
* Further FHIR Operations: You can implement custom FHIR Operations by extending **io.onfhir.api.service.FHIROperationHandlerService** and 
preparing an OperationDefinition file describing the input and output parameters of the operation. You then need to provide a Map[String, String]
of the (operation URL -> the class name of the module) that you implemented by extending FHIROperationHandlerService.  
* External Akka Routes: You can also implement non-FHIR REST services for your server and register them to onFHIR. 

```
object Boot extends App {
  //Initialize onfhir for R4
  var onfhir = 
     Onfhir.apply(
        fhirConfigurator = new FhirR4Configurator(),
        fhirOperationImplms = myFhirOperations,
        customAuthorizer = new MyAuthorizer(),
        customAuditHandler = new MyAuditHandler(),
        externalRoutes = ...my non-fhir routes 
     )
  //Start it
  onfhir.start
}
```
      
### Docker
We also provide a simple docker setup for onFHIR under 'docker' folder. It provides a docker-compose file with 
two containers; one for MongoDB database and one for onFHIR application. You can run it with our sample onFHIR setup given with 'sample-setup' directory.
You can copy the 'onfhir-server-standalone.jar' file to this sample-setup directory and run the sample setup as it is with the following command;  

```
$ cd docker
$ cp ../onfhir-server-r4/target/onfhir-server-standalone.jar ./sample-setup/.
$ docker-compose -f docker-compose.yml -p onfhir up -d
```

Then you will be able to send requests to this running instance over your docker machine. The following will return the CapabilityStatement
```
$ curl http://127.0.0.1:8080/fhir/metadata
```

## Tests 

fhir-repository uses **specs2** for unit testing. To execute tests for each build with 
the following command:
```
$ mvn test
```

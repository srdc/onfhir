# onFHIR.io Subscription Engine
onFHIR.io Subscription Engine is a module of onFHIR.io that handles FHIR Subscription mechanism as defined in the standard (See https://www.hl7.org/fhir/subscription.html).
When subscription mechanism is enabled in onFHIR.io repository, it streams the created and updated FHIR resources to a configured Kafka cluster.
Furthermore, onFHIR.io repository also handles the FHIR Subscription resource management, and publishes new and updated subscription events over a special internal topic to the same Kafka cluster.

This module works on these data and subscription streams and send notifications to the given endpoints in the FHIR Subscription resources, if a new or updated resource satisfies the criteria again given in the FHIR Subscription resource.
Currently only 'rest-hook' (sending notification to a RESTfull endpoint) and 'websocket' (sending notification over web socket connections) channels are supported for notifications.
onFHIR.io Subscription Engine also utilizes an internal REST api provided by onFHIR.io repository. This api is used for
* Retrieving required search parameter configurations for the evaluation of the criteria statements (FHIR query statements) given in FHIR Subscriptions 
* Updating the status of FHIR Subscription resources i) set it to active when new subscription is received, and processed successfully ii) set it to error or off if there is an error (or 3 successive errors in later case) while sending the notification   

This module can be executed as a cluster. It is implemented by using Akka Cluster and its sharding mechanims to execute the jobs in parallel on 1 or more nodes. 

## Basic Configuration
You can configure the module by providing an application.conf file where default file is given under src/resources folder.
onFHIR related configurations are under 'onfhir' tag. The followings are the summary of the important configurations;
* **onfhir.hosts** You should provide the list of network addresses for internal API opened by onFHIR.io Repository instances
* **onfhir.subscription.kafka.bootstrap-servers** Provide the addresses of Kafka brokers
* **onfhir.websocket.port** Web socket port that the onFHIR.io Subscription Engine node will listen for web socket notifications

If you are running the module as cluster (multiple nodes), then you should also configure the followings for each node;
* **akka.remote-artery.canonical** This provides the internal network address (host and port) used by node for Akka node-to-node communications
* **akka.cluster.seed-nodes** List of Akka Cluster seed nodes which should be the addresses of the initial nodes that you will start in your cluster. So they should be same for all nodes.
* **akka.cluster.roles** If you are starting the onFHIR.io Subscription Engine cluster from scratch and if there are already created FHIR subscriptions in onFHIR.io repository that you also want to process, 'initializer' role should be given to the first node that you will start. 
For all other nodes, do not use any role. For a running cluster,  if the oldest node is stopped and want to be restarted, the role should not be given in the configuration. 

## Enabling subscription mechanism in onFHIR.io Repository

The following configuration parameters should be set within onFHIR.io repository (via application.conf or passing JAVA properties) to enable FHIR subscription handling;
* **fhir.subscription.active** should be set to true
* **fhir.subscriotion.allowed-resources** provide the list of resource types that you want to restrict the subscription mechanism for. If you want to enable subscription mechanism for all resources that you support within the onFHIR repository instance, comment the line
* **kafka.enabled** should be set to true
* **kafka.bootstrap-servers** should be set to the list of Kafka brokers addresses 
      
## Prerequisites
* onFHIR.io repository (single or cluster) should be running and configured to enable subscription. 
* Target Kafka cluster should be up and running.   

## Build & Run

You need to run the below command to build onfhir-subscription. This will compile 
your code, execute unit tests and create a single standalone jar with all the dependencies:
```
$ mvn package
```

Executable standalone jars **target/onfhir-subscription-standalone.jar**  will be created under onfhir-subscription module. Executing the following command will run the engine; 
capabilities.
```
$ java -jar target/onfhir-subscription-standalone.jar
```

You can override in-app configurations by supplying an external application.conf file or JAVA arguments
using the following commands:
```
$ java -Dconfig.file={path-to-application.conf} -jar target/onfhir-subscription-standalone.jar
```
or 
```
$ java -Donfhir.hosts.0={onfhir-repo-1} -Donfhir.hosts.1={onfhir-repo-2} -Donfhir.subscription.kafka.bootstrap-servers.0={kafka-broker-1} -Dakka.cluster.roles.0=initializer -Donfhir.subscription.websocket.port=8082  -jar target/onfhir-subscription-standalone.jar
```

## onFHIR.io Subscription Engine Web Socket channel handling 
onFHIR.io Subscription Engine opens a web socket endpoint at http://<host>:<websocket-port>/fhir-wss for each node. Clients after creating FHIR Subscription on onFHIR.io Repository, can connect to the endpoint of any node and then send a message as follows with the id of the FHIR subscription resource.

```
bind <subscriptionid>
``` 

onFHIR.io Subscription Engine responds with the following message if binding is successful. 
```
bound <subscriptionid>
```
If not it sends an error message in the following format (This is not described in FHIR standard) 
```
error <subscriptionid> '<Error message>'
```

If bind is successful, for each notification (when the criteria of the bound subscription is satisfied) a ping message is sent back to the client in the following format;
```
ping <subscriptionid>
```

## Simple Integration Test Setup
This section describes a simple integration test setup for onFHIR.io Subscription Engine for local environment.

Run a kafka cluster on your local setup. You can use a docker setup (like https://hub.docker.com/r/wurstmeister/kafka/) that you can interact from outside for this.

Then run a onFHIR.io repository in local setting.
```
$ java -Dkafka.enabled=true -Dkafka.bootstrap-servers.0=<kafka-address> -jar target/onfhir-server-standalone.jar
```



```
java -Dakka.cluster.roles.0=initializer -Donfhir.subscription.kafka.bootstrap-servers.0=localhost:9092 -jar target/onfhir-subscription-standalone.jar 
```
 



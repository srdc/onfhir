#!/bin/bash

JAVA_CMD="java -Xms256m -Xmx3g -jar "

# Configure FHIR repository ROOT URL (after deployment and proxies)
if [ ! -z "$FHIR_ROOT_URL" ]; then
    JAVA_CMD+="-Dfhir.root-url=$FHIR_ROOT_URL "
fi

# Configure FHIR repository binding host
if [ ! -z "$FHIR_HOST" ]; then
    JAVA_CMD+="-Dserver.host=$FHIR_HOST "
fi

# Configure Kafka broker host
if [ ! -z "$KAFKA_HOST" ]; then
    JAVA_CMD+="-Dkafka.host=$KAFKA_HOST "
    JAVA_CMD+="-Dkafka.enabled=true "
fi

# Configure Kafka broker port
if [ ! -z "$KAFKA_PORT" ]; then
    JAVA_CMD+="-Dkafka.port=$KAFKA_PORT "
    JAVA_CMD+="-Dkafka.enabled=true "
fi

# Configure SSL
if [ ! -z "$USE_SSL" ]; then
    JAVA_CMD+="-Dspray.can.server.ssl-encryption=on "
    JAVA_CMD+="-Dserver.ssl.keystore=/pds/ssl/keystore.jks "
    JAVA_CMD+="-Dserver.ssl.password=fhir-repository "
fi

# Configure application.conf path
if [ ! -z "$APP_CONF_FILE" ]; then
    JAVA_CMD+="-Dconfig.file=$APP_CONF_FILE "
fi

# Configure application.conf path
if [ ! -z "$LOGBACK_CONF_FILE" ]; then
    JAVA_CMD+="-Dlogback.configurationFile=$LOGBACK_CONF_FILE "
fi

# Configure MongoDB Host name
#if [ ! -z "$DB_HOST" ]; then
#    JAVA_CMD+="-Dmongodb.host=['$DB_HOST'] "
#fi

# Configure Auditing
if [ ! -z "$AUDIT_SERVER_ROOT_URL" ]; then
    JAVA_CMD+="-Dfhir.auditing.repository='remote' "
    JAVA_CMD+="-Dfhir.auditing.repository-url=$AUDIT_SERVER_ROOT_URL "
    JAVA_CMD+="-Dfhir.auditing.is-secure=true "
fi

# Configure Authz Server URL
if [ ! -z "$AUTHZ_SERVER_ROOT_URL" ]; then
    JAVA_CMD+="-Dfhir.authorization.authz-server-url=$AUTHZ_SERVER_ROOT_URL "
fi

# Delay the execution for this amount of seconds
if [ ! -z "$DELAY_EXECUTION" ]; then
    sleep $DELAY_EXECUTION
fi

# Finally, tell which jar to run
JAVA_CMD+="/fhir/onfhir-server-standalone.jar"

eval $JAVA_CMD

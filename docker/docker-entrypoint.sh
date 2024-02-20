#!/usr/bin/env bash

JAVA_CMD="java -Xms256m -Xmx3g -jar "

# Configure application.conf path
if [ ! -z "$APP_CONF_FILE" ]; then
    JAVA_CMD+="-Dconfig.file=$APP_CONF_FILE "
fi

# Configure FHIR repository server binding host
if [ ! -z "$SERVER_HOST" ]; then
    JAVA_CMD+="-Dserver.host=$SERVER_HOST "
fi
if [ ! -z "$SERVER_PORT" ]; then
    JAVA_CMD+="-Dserver.port=$SERVER_PORT "
fi
if [ ! -z "$SERVER_BASE_URI" ]; then
    JAVA_CMD+="-Dserver.base-uri=$SERVER_BASE_URI "
fi

# Configure FHIR repository ROOT URL (after deployment and proxies)
if [ ! -z "$FHIR_INIT" ]; then
    JAVA_CMD+="-Dfhir.initialize=$FHIR_INIT "
fi
if [ ! -z "$FHIR_ROOT_URL" ]; then
    JAVA_CMD+="-Dfhir.root-url=$FHIR_ROOT_URL "
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

# Configure MongoDB
if [ ! -z "$DB_EMBEDDED" ]; then
    JAVA_CMD+="-Dmongodb.embedded=$DB_EMBEDDED "
fi
if [ ! -z "$DB_HOST" ]; then
    JAVA_CMD+="-Dmongodb.host=$DB_HOST "
fi
if [ ! -z "$DB_NAME" ]; then
    JAVA_CMD+="-Dmongodb.db=$DB_NAME "
fi

if [ ! -z "$DB_SHARDING" ]; then
    JAVA_CMD+="-Dmongodb.sharding=$DB_SHARDING"
fi

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
JAVA_CMD+="onfhir-server-standalone.jar"

eval $JAVA_CMD

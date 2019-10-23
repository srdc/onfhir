#!/bin/bash

JAVA_CMD="java -Xms512m -Xmx4g -jar "

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
if "$USE_SSL"; then
    JAVA_CMD+="-Dspray.can.server.ssl-encryption=on "
    JAVA_CMD+="-Dserver.ssl.keystore=/fhir/ssl/keystore.jks "
    JAVA_CMD+="-Dserver.ssl.password=fhir-repository "
fi

# Configure application.conf path
if [ ! -z "$APP_CONF_FILE" ]; then
    JAVA_CMD+="-Dconfig.file=$APP_CONF_FILE "
fi

# Finally, tell which jar to run
JAVA_CMD+="/fhir/onfhir-standalone.jar"

mongod &
eval $JAVA_CMD

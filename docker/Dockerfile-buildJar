# Stage 0
FROM maven:3.6-openjdk-11 AS builder

ENV ONFHIR_HOME /usr/local/onfhir
RUN mkdir -p "$ONFHIR_HOME"
WORKDIR $ONFHIR_HOME

COPY . ./

# Build without running the unit and integration tests
RUN mvn package -Pxtest

# Stage 1
FROM openjdk:11-jre-slim

ENV ONFHIR_HOME /usr/local/onfhir
RUN mkdir -p "$ONFHIR_HOME"
WORKDIR $ONFHIR_HOME

COPY --from=builder $ONFHIR_HOME/onfhir-server-r4/target/onfhir-server-standalone.jar .

COPY docker/docker-entrypoint.sh .
RUN chmod +x docker-entrypoint.sh

ENTRYPOINT ["./docker-entrypoint.sh"]

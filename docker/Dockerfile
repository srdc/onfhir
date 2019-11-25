FROM srdc/java:oraclejdk-8

# Create folders
RUN mkdir -p /fhir
ADD docker-entrypoint.sh /fhir
ADD ../onfhir-server-r4/target/onfhir-standalone.jar /fhir

RUN chown root:root /fhir/docker-entrypoint.sh
RUN chmod +x /fhir/docker-entrypoint.sh

EXPOSE 8080
ENTRYPOINT ["/fhir/docker-entrypoint.sh"]

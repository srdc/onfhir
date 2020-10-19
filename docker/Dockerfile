FROM openjdk:11

# Create folders
RUN mkdir -p /onfhir
ADD docker-entrypoint.sh /onfhir

RUN chown root:root /onfhir/docker-entrypoint.sh
RUN chmod +x /onfhir/docker-entrypoint.sh

EXPOSE 8080
ENTRYPOINT ["/onfhir/docker-entrypoint.sh"]

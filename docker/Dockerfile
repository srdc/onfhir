FROM srdc/java:oraclejdk-8

# Get MongoDB
RUN \
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927 && \
  echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.2 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-3.2.list && \
  apt-get update && \
  apt-get install -y mongodb-org && \
  rm -rf /var/lib/apt/lists/*

# Create volume for MongoDB
VOLUME ["/data/db"]

# Install Java Cryptography Extension (JCE) to be able to use SSL encryption
RUN cd /tmp/ && \
    curl -LO "http://download.oracle.com/otn-pub/java/jce/8/jce_policy-8.zip" -H 'Cookie: oraclelicense=accept-securebackup-cookie' && \
    unzip jce_policy-8.zip && \
    rm jce_policy-8.zip && \
    yes |cp -v /tmp/UnlimitedJCEPolicyJDK8/*.jar /usr/lib/jvm/java-8-oracle/jre/lib/security/

# Create folders
RUN mkdir -p /fhir
RUN mkdir -p /fhir/ssl

# Add Files
ADD onfhir-standalone.jar /fhir
ADD docker-entrypoint.sh /fhir
ADD keystore.jks /fhir/ssl/

RUN chown root:root /fhir/docker-entrypoint.sh
RUN chmod +x /fhir/docker-entrypoint.sh


EXPOSE 8080
ENTRYPOINT ["/fhir/docker-entrypoint.sh"]
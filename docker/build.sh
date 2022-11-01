# Execute one of the following commands from the project.root.directory (../)

docker build -f docker/Dockerfile-addJar -t srdc/onfhir:r4 .
docker build -f docker/Dockerfile-buildJar -t srdc/onfhir:r4 .


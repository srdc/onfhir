# Execute one of the following commands from the project.root.directory (../)

docker build -f docker/Dockerfile-addJar-r4 -t srdc/onfhir:r4 .
docker build -f docker/Dockerfile-addJar-r5 -t srdc/onfhir:r5 .
docker build -f docker\Dockerfile-addJar-r4 -t srdc/onfhir:r4 .

docker build -f docker/Dockerfile-buildJar-r4 -t srdc/onfhir:r4 .
docker build -f docker\Dockerfile-buildJar-r4 -t srdc/onfhir:r4 .


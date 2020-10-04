#!/bin/bash
set -x #echo on

APPDIR=`dirname $0`

set -ex
export AWS_REGION=us-east-1
export AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID
export AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY
export PORT=$PORT
#export JAVA_OPTS="-Xms50m -Xmx100m"

if [ "$1" = "start" ]
then
    exec java -jar $APPDIR/app.jar
fi

if [ "$1" = "quarkus-mvn-create" ]
then
    exec mvn io.quarkus:quarkus-maven-plugin:1.8.1.Final:create \
            -DprojectGroupId=com.kamestery.datakam \
            -DprojectArtifactId=datakam \
            -DclassName="com.kamestery.security.Auth" \
            -Dpath="/" \
            -Dextensions="kotlin,resteasy-jsonb"
fi

if [ "$1" = "quarkus-add-extensions" ]
then
    ./mvnw quarkus:add-extension -Dextensions="spring-di"
    ./mvnw quarkus:add-extension -Dextensions="graphql"
    ./mvnw quarkus:add-extension -Dextensions="jwt"
    ./mvnw quarkus:add-extension -Dextensions="amazon-dynamodb"
    ./mvnw quarkus:add-extension -Dextensions="resteasy-mutiny"
    ./mvnw quarkus:add-extension -Dextensions="quarkus-smallrye-openapi"
fi

if [ "$1" = "quarkus-list-extensions" ]
then
    ./mvnw quarkus:list-extensions
fi

exec "$@"

exit 0


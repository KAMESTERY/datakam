#!/bin/bash
set -x #echo on

set -ex
export AWS_REGION=us-east-1
export AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID
export AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY
export PORT=$PORT
#export JAVA_OPTS="-Xms50m -Xmx100m"

if [ "$1" = "start" ]
then
    exec java -jar app.jar
fi

exec "$@"

exit 0


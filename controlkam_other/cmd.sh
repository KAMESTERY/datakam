#!/bin/bash

BASEDIR=`pwd`

case $1 in
    gen.code)
        echo "Generating Go..."
        protoc -I svc/ svc/ctrlkm.proto --go_out=plugins=grpc:grpc
        echo "Generating TypeScript..."
        docker run -v $PWD:/app grpcweb/common protoc -I /app/svc/ /app/svc/ctrlkm.proto \
        --js_out=import_style=commonjs:/app/ui-src/javascript/grpc \
        --grpc-web_out=import_style=commonjs+dts,mode=grpcwebtext:/app/ui-src/javascript/grpc
        ;;
    clean)
        echo "Implement this!"
        ;;
    esac
exit 0

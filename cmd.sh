#!/bin/bash

BASEDIR=`pwd`
unamestr=`uname`
RUSTY_WORKER=worker-fn
RUSTY_WORKER_RPC=worker-rpc
RUSTY_LIBWORKEREXT=workerext
RUSTY_WORKER_LIB=worker-lib

genRsa() {
    openssl genrsa -out $BASEDIR/$RUSTY_WORKER_LIB/src/private_rsa_key.pem 4096
    openssl rsa -in $BASEDIR/$RUSTY_WORKER_LIB/src/private_rsa_key.pem -outform DER -out $BASEDIR/$RUSTY_WORKER_LIB/src/private_rsa_key.der
    openssl rsa -in $BASEDIR/$RUSTY_WORKER_LIB/src/private_rsa_key.der -inform DER -RSAPublicKey_out -outform DER -out $BASEDIR/$RUSTY_WORKER_LIB/src/public_key.der
}

case $1 in
    devops.init)
        rm -rf .terraform
        terraform init infrastructure
        ;;
    gen.rsa)
        genRsa
        ;;
    sub.update)
        git submodule update --recursive --remote
        ;;
    build.workers)
        docker run --rm -v $BASEDIR/cargo:/home/cargo -e CARGO_HOME='/home/cargo' -v $BASEDIR:/code -w /code og-rust-lambda:latest cargo build --release
        ;;
    build.webkam)
        cd $BASEDIR/webkam; make build-ui; GOOS=linux GOARCH=amd64 go build -o server *.go
        ;;
    deploy.function)
        cd $2; up $3
        ;;
    dyna.tbl.create)
        aws dynamodb create-table --table-name $2 --attribute-definitions AttributeName=id,AttributeType=S --key-schema AttributeName=id,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5 --endpoint-url $3
        ;;
    dyna.tbl.list)
        aws dynamodb list-tables --endpoint-url $2 --output json
        ;;
    aws-delete-alarms)
        aws cloudwatch delete-alarms --alarm-names $2
        ;;
    build_wasm_fn)
        rustup override set nightly
        cargo +nightly build --target wasm32-unknown-unknown --release
        mkdir -p worker-fn/wasm
        wasm-bindgen target/wasm32-unknown-unknown/debug/hello_world_wasm.wasm --nodejs --out-dir worker-fn/wasm
        ;;
    esac
exit 0

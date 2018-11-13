#!/bin/bash
set -x #echo on

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
        rm -rf $BASEDIR/webkam $BASEDIR/controlkam $BASEDIR/worker-rpc/svc $BASEDIR/worker-fn/svc
        git config -f .gitmodules --get-regexp '^submodule\..*\.path$' |
        while read path_key path
        do
            url_key=$(echo $path_key | sed 's/\.path/.url/')
            url=$(git config -f .gitmodules --get "$url_key")
            git submodule add --force $url $path
            cd $path && git reset master@{upstream} && git pull && cd ..
        done
        ;;
    build.workers.lambda)
        docker run --rm -v $BASEDIR/cargo:/home/cargo -e CARGO_HOME='/home/cargo' -v $BASEDIR:/code -w /code outcastgeek/rust-lambda:0.1 cargo build --release
        ;;
    build.workers.ubuntu)
        docker build -t worker_rpc-rust-ubuntu:0.1 -f $BASEDIR/infrastructure/Dockerfile.ubuntu $BASEDIR
        ;;
    build.webapp)
        cd $2; make build-ui; GOOS=linux GOARCH=amd64 go build -o server main.go
        # cd $2; GOOS=linux GOARCH=amd64 go build -o server main.go
        # cd $2; GOOS=linux GOARCH=amd64 go build -o server *.go
        # cd $2; make build-ui; GOOS=linux GOARCH=amd64 go build -o server *.go
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

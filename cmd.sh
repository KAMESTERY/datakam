#!/bin/bash

BASEDIR=`pwd`
ENV_NAME=SLAPENV

upgrade_all_deps() {
    pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U
}

clean_all() {
  rm -rf **/*.pyc
}

create_py3env() {
    conda create -n $ENV_NAME python=3 -y
}

activate_env() {
    source activate $ENV_NAME
}

install_openssl_dev() {
    curl -O https://www.openssl.org/source/openssl-1.1.0e.tar.gz
    tar xf openssl-1.1.0e.tar.gz
    cd openssl-1.1.0e
    export CC=gcc
    ./Configure --prefix=`pwd` linux-x86_64 -fPIC
    make -j$(nproc)
    sudo make install
}

case $1 in
    py3.create)
        create_py3env
        ;;
    py.delete)
        conda env remove -n $ENV_NAME -y
        ;;
    py.activate)
        activate_env
        ;;
    deps.upgrade)
        activate_env
        upgrade_all_deps
        ;;
    deps.list)
        activate_env
        pip list
        ;;
    deps.outdated)
        activate_env
        pip list --outdated
        ;;
    deps.freeze)
        activate_env
        pip freeze > $2/requirements.txt
        ;;
    deps.install)
        activate_env
        pip install -r $2/requirements.txt -q --upgrade
        ;;
    deps.deploy)
        activate_env
        pip install -r $2/requirements.txt -q -t $2/lib
        ;;
    clean)
        clean_all
        ;;
    install_openssl_dev)
        install_openssl_dev
        ;;
    dyna.tbl.create)
        aws dynamodb create-table --table-name $2 --attribute-definitions AttributeName=id,AttributeType=S --key-schema AttributeName=id,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5 --endpoint-url $3
        ;;
    dyna.tbl.list)
        aws dynamodb list-tables --endpoint-url $2 --output json
        ;;
    esac
exit 0

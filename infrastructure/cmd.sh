#!/bin/bash
set -x #echo on

BASEDIR=`pwd`
unamestr=`uname`
SVC_NAME=smooth_sailing

case $1 in
    stack.deploy)
        docker stack deploy --compose-file $BASEDIR/docker-compose.yml --with-registry-auth $SVC_NAME
        ;;
    deploy)
        docker service create --detach=true --name worker_rpc_kam --constraint node.role==worker --replicas 3 --publish 9099:80 kamestery/worker_rpc:0.1
        ;;
    status)
        docker stack services --format "{{.ID}}: {{.Mode}} {{.Replicas}}" $SVC_NAME
        ;;
    shutdown)
        docker stack rm $SVC_NAME
        ;;
    esac
exit 0

#!/bin/bash
#set -x #echo on

BASEDIR=`pwd`
unamestr=`uname`

awsLambda() {
    echo "https://github.com/aws/aws-extensions-for-dotnet-cli"
    $DOTNET new -i Amazon.Lambda.Template
    $DOTNET tool install -g Amazon.Lambda.Tools
    $DOTNET tool install -g Amazon.ECS.Tools
    $DOTNET tool install -g Amazon.ElasticBeanstalk.Tools
}

case $1 in
    dotnet.info)
        which $DOTNET
        $DOTNET --version
        ;;
    aws.lambda.setup)
        awsLambda
        ;;
    aws.serverless.deploy)
        $DOTNET lambda deploy-serverless -pl $2
        ;;
    aws.delete.serverless)
        $DOTNET lambda delete-serverless -pl $2
        ;;

    esac
exit 0

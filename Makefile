# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)
RUSTY_WORKER=worker-fn
RUSTY_WORKER_RPC=worker-rpc
RUSTY_LIBWORKEREXT=workerext
RUSTY_WORKER_LIB=worker-lib
INFRASTRUCTURE=$(BASEDIR)/infrastructure

# These are the values we want to pass for VERSION and BUILD
VERSION=0.0.1
REVISION=$(shell echo `git rev-parse HEAD`)

.PHONY: deploy-functions

# DEVOPS

devops-init:
	rm -rf .terraform
	terraform init infrastructure
	brew update && brew install terraform kops kubernetes-cli kubernetes-helm
	go get -u github.com/kubernetes/kompose
	go get -u -v github.com/heptio/authenticator/...
#	cp $(GOPATH)/bin/aws-iam-authenticator $(GOPATH)/bin/heptio-authenticator-aws

lightsail_keys:
	rm -rf $(INFRASTRUCTURE)/lightsail_keys/*
	ssh-keygen -f $(INFRASTRUCTURE)/lightsail_keys/grpc_vps_servers_key -t rsa -b 4096 -C ubuntu
#	ssh-keygen -f $(INFRASTRUCTURE)/lightsail_keys/grpc_vps_servers_key -t rsa -b 4096 -C vpskam@kamestery.com # for the first time
	chmod 400 $(INFRASTRUCTURE)/lightsail_keys/*

deploy:
	terraform fmt $(INFRASTRUCTURE)
	terraform get $(INFRASTRUCTURE)
	terraform plan $(INFRASTRUCTURE)
	terraform apply -auto-approve $(INFRASTRUCTURE)

deploy-functions: deploy
	cd $(BASEDIR)/$(RUSTY_WORKER)/ && up data_dev -v
	cd $(BASEDIR)/$(RUSTY_WORKER_RPC)/ && up rpc_dev -v

update-external-functions:
	git submodule foreach git pull origin master

# Crypto

rsa:
	openssl genrsa -out $(BASEDIR)/$(RUSTY_WORKER_LIB)/src/private_rsa_key.pem 4096
	openssl rsa -in $(BASEDIR)/$(RUSTY_WORKER_LIB)/src/private_rsa_key.pem -outform DER -out $(BASEDIR)/$(RUSTY_WORKER_LIB)/src/private_rsa_key.der
	openssl rsa -in $(BASEDIR)/$(RUSTY_WORKER_LIB)/src/private_rsa_key.der -inform DER -RSAPublicKey_out -outform DER -out $(BASEDIR)/$(RUSTY_WORKER_LIB)/src/public_key.der

build-workers: rsa
	cargo build --release
	cp $(BASEDIR)/target/debug/$(RUSTY_WORKER) $(BASEDIR)/lambda/worker/

prod-build-workers: rsa lambda-binary-builder
	mkdir -p $(BASEDIR)/{cargo,target/$(RUSTY_WORKER)_lambda}
	docker run --rm -v $(BASEDIR)/cargo:/home/cargo -e CARGO_HOME='/home/cargo' -v `pwd`:/code -w /code og-rust-lambda:latest cargo build --release
	cp $(BASEDIR)/target/release/$(RUSTY_WORKER) $(BASEDIR)/$(RUSTY_WORKER)/server
	cp $(BASEDIR)/target/release/$(RUSTY_WORKER_RPC) $(BASEDIR)/$(RUSTY_WORKER_RPC)/server
#	cp $(BASEDIR)/target/release/lib$(RUSTY_LIBWORKEREXT).so $(BASEDIR)/lambda/worker/lib$(RUSTY_LIBWORKEREXT).so

rusty-binary-builder:
	docker build -t og-rust-lambda:0.1 -f $(INFRASTRUCTURE)/Dockerfile.lambda $(INFRASTRUCTURE)
	docker build -t og-rust-ubuntu:0.1 -f $(INFRASTRUCTURE)/Dockerfile.ubuntu $(INFRASTRUCTURE)

# CLEAN

clean:
	@rm -rf infrastructure/*.zip

# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)

.PHONY: prod-deploy prod-url

BASEDIR = $(shell pwd)

CGO_ENABLED=0
WORKER=slapman_worker

# TOOLING
tools:
	go get -u github.com/kardianos/govendor
	ln -s $(BASEDIR)/$(WORKER) $(GOPATH)/src/
	cd $(GOPATH)/src/$(WORKER); govendor sync

vendor-status:
	cd $(GOPATH)/src/$(WORKER); govendor status

vendor-fetch:
	cd $(GOPATH)/src/$(WORKER); govendor fetch -tree github.com/graphql-go/graphql
	cd $(GOPATH)/src/$(WORKER); govendor fetch -tree github.com/aws/aws-sdk-go

vendor-init:
	cd $(GOPATH)/src/$(WORKER); govendor init

build-worker:
	cd $(GOPATH)/src/$(WORKER); go build -buildmode=c-shared -o $(BASEDIR)/lambda/worker/worker.so $(WORKER)

prod-build-worker:
	GOOS=linux GOARCH=amd64 cd $(GOPATH)/src/$(WORKER); go build -buildmode=c-shared -o $(BASEDIR)/lambda/worker/worker.so $(WORKER)

# DEVOPS

prod-deploy: clean tools prod-build-worker build-lambda deploy
	@echo "Completed Production Deployment! :-)"

deploy:
	terraform get infrastructure
	terraform plan infrastructure
	terraform apply infrastructure

prod-url:
	terraform show | grep invoke_url

build-lambda: deps-deploy
	cd $(BASEDIR)/lambda && zip -9 -rq $(BASEDIR)/infrastructure/slapalicious.zip .

# PY3.6

CMD = $(BASEDIR)/cmd.sh

create-env:
	$(CMD) py3.create

destroy-env:
	$(CMD) py.delete

deps-deploy:
	$(CMD) deps.deploy lambda

deps-install:
	$(CMD) deps.install lambda

deps-freeze:
	$(CMD) deps.freeze lambda

# CLEAN

clean:
	@rm -rf infrastructure/*.zip lambda/lib $(GOPATH)/src/$(WORKER)


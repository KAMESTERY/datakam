# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)

# These are the values we want to pass for VERSION and BUILD
VERSION=0.0.1
REVISION=$(shell echo `git rev-parse HEAD`)

.PHONY: prod-deploy prod-url

# DEVOPS

prod-deploy: clean build-lambda deploy
	@echo "Completed Production Deployment! :-)"

deploy:
	terraform get infrastructure
	terraform plan infrastructure
	terraform apply infrastructure

prod-url:
	terraform show | grep invoke_url

build-lambda: deps-deploy build-worker package-lambda
	@echo "Completed Building Lambda"

package-lambda:
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

deps-upgrade:
	$(CMD) deps.upgrade

# Golang

WORKER=slapman
OS := $(shell uname)
# Setup the -ldflags option for go build here, interpolate the variable values
LDFLAGS=-ldflags '-s -w -X "main.Version=${VERSION}" -X "main.Revision=${REVISION}" -linkmode "internal" -extldflags "-static"'

build-worker: worker-link worker-fmt
	cd $(GOPATH)/src/$(WORKER)/cmd/$(WORKER) && go build $(LDFLAGS) -v -o $(BASEDIR)/lambda/worker/$(WORKER)

prod-build-worker: worker-link worker-fmt
	GOOS=linux GOARCH=amd64 cd $(GOPATH)/src/$(WORKER)/cmd/$(WORKER) && go build $(LDFLAGS) -v -o $(BASEDIR)/lambda/worker/$(WORKER)

# prod-build-worker: worker-link
# 	cd $(GOPATH)/src/$(WORKER)/cmd/web && go build -v -o $(BASEDIR)/lambda/worker/$(WORKER)

#LDFLAGS=-linkmode external -extldflags -static
#prod-build-worker: worker-link
#	docker run --rm -v $(BASEDIR)/$(WORKER):/go/src/$(WORKER) -w /go/src golang:1.9.0 go build -ldflags="$(LDFLAGS)" -v -x -o /go/src/$(WORKER)/$(WORKER) $(WORKER)
#	mv $(BASEDIR)/$(WORKER)/$(WORKER) $(BASEDIR)/lambda/worker/$(WORKER)

worker-link:
	rm -rf $(GOPATH)/src/$(WORKER)
	ln -s $(BASEDIR)/$(WORKER) $(GOPATH)/src/

# TOOLING
tools: worker-link
	go get -u github.com/golang/dep/cmd/dep
	go get -u github.com/mitchellh/gox

vendor-status: worker-link
	cd $(GOPATH)/src/$(WORKER); dep status

vendor-fetch: worker-link
	cd $(GOPATH)/src/$(WORKER); dep ensure

vendor-update: worker-link
	cd $(GOPATH)/src/$(WORKER); dep ensure -update

vendor-init: worker-link
	cd $(GOPATH)/src/$(WORKER); dep init

worker-fmt:
	gofmt -l -s -w $(BASEDIR)/$(WORKER)

# CLEAN

clean:
	@rm -rf infrastructure/*.zip lambda/lib $(GOPATH)/src/$(WORKER)

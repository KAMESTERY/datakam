# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)

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

build-worker: worker-link
	cd $(GOPATH)/src/cmd/slapman && go build -v -o $(BASEDIR)/lambda/worker/$(WORKER) $(WORKER)

prod-build-worker: worker-link
	$(BASEDIR)/cmd.sh worker.build.prod

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

vendor-status:
	cd $(GOPATH)/src/$(WORKER); dep status

vendor-update:
	cd $(GOPATH)/src/$(WORKER); dep ensure -update

vendor-init:
	cd $(GOPATH)/src/$(WORKER); dep init

# CLEAN

clean:
	@rm -rf infrastructure/*.zip lambda/lib $(GOPATH)/src/$(WORKER)

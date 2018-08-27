# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)
WORKER=slapman
WEBSITE=slapman-web
CRYPTO_OUTPUT_DIR=$(BASEDIR)/$(WORKER)/keys
OUTPUT_DIR=$(BASEDIR)/bin

# These are the values we want to pass for VERSION and BUILD
VERSION=0.0.1
REVISION=$(shell echo `git rev-parse HEAD`)

.PHONY: prod-deploy publish-website deploy-everything prod-url

# DEVOPS

devops-init:
	rm -rf .terraform
	terraform init infrastructure

deploy-everything: prod-deploy publish-website
	@echo "All Done!"

prod-deploy: clean build-lambda deploy
	@echo "Completed the Production Deployment of Lambda [$(WORKER)]! :-)"

deploy:
	terraform fmt infrastructure
	terraform get infrastructure
	terraform plan infrastructure
	terraform apply -auto-approve infrastructure

prod-url:
	terraform show | grep invoke_url
	terraform show | grep $(WORKER)-api-url
	terraform show | grep $(WEBSITE)-url

publish-website: deploy
	lein do clean, cljsbuild once min
	aws s3 sync --acl public-read $(BASEDIR)/public s3://$(WEBSITE)
	@echo "Completed Publishing [$(WEBSITE)] to Production! :-)"

#build-lambda: deps-deploy package-lambda prod-build-worker-rusty
#build-lambda: deps-deploy package-lambda
build-lambda: deps-deploy prod-build-worker prod-build-worker-rusty package-lambda
	@echo "Completed Building Lambda"

package-lambda:
	cd $(BASEDIR)/lambda && zip -9 -rq $(BASEDIR)/infrastructure/slapalicious-web.zip .
	cd $(OUTPUT_DIR) && zip -9 -rq $(BASEDIR)/infrastructure/slapalicious-api.zip .

# Crypto

rsa:
	mkdir -p $(CRYPTO_OUTPUT_DIR)
	openssl genrsa -out $(CRYPTO_OUTPUT_DIR)/$(WORKER).rsa 1024
	openssl rsa -in $(CRYPTO_OUTPUT_DIR)/$(WORKER).rsa -pubout > $(CRYPTO_OUTPUT_DIR)/$(WORKER).rsa.pub

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

PROJ_GOPATH = $(BASEDIR)/build
OS := $(shell uname)
# Setup the -ldflags option for go build here, interpolate the variable values
LDFLAGS=-ldflags '-s -w -X "main.Version=${VERSION}" -X "main.Revision=${REVISION}" -X "main.CryptoRsa=${WORKER}" -linkmode "internal" -extldflags "-static"'

build-worker-rusty:
	cargo build --release
	cp $(BASEDIR)/target/lambda_fn $(OUTPUT_DIR)/

prod-build-worker-rusty:
	docker run --rm -it -v $(BASEDIR):/home/rust/src ekidd/rust-musl-builder cargo build --release
	cp $(BASEDIR)/target/lambda_fn $(OUTPUT_DIR)/

pack-assets:
	packr -i $(BASEDIR)/$(WORKER)

build-worker: rsa worker-link pack-assets go-fmt
	cd $(GOPATH)/src/$(WORKER) && go build $(LDFLAGS) -v -o $(OUTPUT_DIR)/$(WORKER)

prod-build-worker: rsa worker-link pack-assets go-fmt go-lint
	cd $(GOPATH)/src/$(WORKER) && GOOS=linux GOARCH=amd64 go build $(LDFLAGS) -v -o $(OUTPUT_DIR)/$(WORKER)

# prod-build-worker: worker-link
# 	cd $(GOPATH)/src/$(WORKER)/cmd/web && go build -v -o $(BASEDIR)/lambda/worker/$(WORKER)

test-worker: rsa worker-link go-fmt
	LOG_LEVEL=debug cd $(GOPATH)/src/$(WORKER) && go test -v -cover ./...

#LDFLAGS=-linkmode external -extldflags -static
#prod-build-worker: worker-link
#	docker run --rm -v $(BASEDIR)/$(WORKER):/go/src/$(WORKER) -w /go/src golang:1.9.0 go build -ldflags="$(LDFLAGS)" -v -x -o /go/src/$(WORKER)/$(WORKER) $(WORKER)
#	mv $(BASEDIR)/$(WORKER)/$(WORKER) $(BASEDIR)/lambda/worker/$(WORKER)

worker-link:
	rm -rf $(GOPATH)/src/$(WORKER)
	ln -s $(BASEDIR)/$(WORKER) $(GOPATH)/src/

vendor-link:
	rm -rf $(PROJ_GOPATH)
	mkdir -p $(PROJ_GOPATH)/src
	for d in $(BASEDIR)/$(WORKER)/vendor/*; do echo "Linking $$d" && ln -fs $$d $(PROJ_GOPATH)/src/; done

#ln -s $(BASEDIR)/$(WORKER)/vendor/* $(GOPATH)/src/

#for d in $(BASEDIR)/$(WORKER)/vendor; do cd $$d && mkdir foo && cd ..; done

# TOOLING
tools: worker-link
	go get -u github.com/gobuffalo/packr/...
	go get -u github.com/golang/dep/cmd/dep
	go get -u github.com/mitchellh/gox
	go get -u github.com/nsf/gocode
	go get -u github.com/rogpeppe/godef
	go get -u golang.org/x/tools/cmd/guru
	go get -u golang.org/x/tools/cmd/gorename
	go get -u golang.org/x/tools/cmd/goimports
	go get -u github.com/alecthomas/gometalinter
	gometalinter --install --update

vendor-status: worker-link
	cd $(GOPATH)/src/$(WORKER); dep status

vendor-fetch: worker-link
	cd $(GOPATH)/src/$(WORKER); dep ensure

vendor-update: worker-link
	cd $(GOPATH)/src/$(WORKER); dep ensure -update

vendor-init: worker-link
	cd $(GOPATH)/src/$(WORKER); dep init

go-fmt:
	gofmt -l -s -w $(BASEDIR)/$(WORKER)

go-lint:
	gometalinter --deadline 120s --vendored-linters $(WORKER)

# CLEAN

clean:
	@rm -rf $(OUTPUT_DIR) $(CRYPTO_OUTPUT_DIR) infrastructure/*.zip lambda/lib $(GOPATH)/src/$(WORKER) && packr clean

# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)

.PHONY: invoke-hello run-sbcl run-ccl deploy-sbcl deploy-ccl stage-sbcl stage-ccl prod-deploy prod-url test-sbcl test-ccl file-check

BASEDIR = $(shell pwd)

prod-deploy: build-lambda
	terraform get infrastructure
	terraform plan infrastructure
	terraform apply infrastructure
	@echo "Completed Production Deployment! :-)"

prod-url:
	terraform show | grep invoke_url

build-lambda:
	lein with-profile lambda uberjar

clean:
	@rm -rf target infrastructure/*.jar


# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)

.PHONY: prod-deploy prod-url

BASEDIR = $(shell pwd)

# DEVOPS

prod-deploy: clean build-lambda
	terraform get infrastructure
	terraform plan infrastructure
	terraform apply infrastructure
	@echo "Completed Production Deployment! :-)"

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
	@rm -rf infrastructure/*.zip lambda/lib


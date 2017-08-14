# Slapman Build Build

BASEDIR := $(shell pwd)
UNAME_S := $(shell uname -s)

.PHONY: prod-deploy prod-url

BASEDIR = $(shell pwd)

# DEVOPS

prod-deploy: clean build-lambda deploy
	@echo "Completed Production Deployment! :-)"

deploy:
	terraform get infrastructure
	terraform plan infrastructure
	terraform apply infrastructure

prod-url:
	terraform show | grep invoke_url

build-lambda: deps-deploy build-lib-prod
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

# Rust

RUST_VERSION = 1.19.0

build-lib-prod:
	docker run --rm --user `id -u`:`id -g` -v `pwd`:/usr/src/myapp -w /usr/src/myapp rust:$(RUST_VERSION) cargo build --release
	cp target/release/libslapman.so lambda/worker/slapman.so

build-lib-macos:
	cargo build --release
	cp target/release/libslapman.dylib lambda/worker/slapman.so

build-lib-linux:
	cargo build --release
	cp target/release/libslapman.so lambda/worker/slapman.so

# CLEAN

clean:
	cargo clean
	@rm -rf infrastructure/*.zip lambda/lib

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

deps-upgrade:
	$(CMD) deps.upgrade

# Nim and Rust

OSARCH = x86_64-unknown-linux-musl

test-lib: build-lib
	docker run --rm -v `pwd`:/usr/src/app -w /usr/src/app nimlang/nim:alpine nim c -r -d:release --passL:-static --passL:target/x86_64-unknown-linux-musl/release/libslapman.a test.nim && strip test
	#nim c -r -d:release --passL:target/$(OSARCH)/release/libslapman.a test.nim

build-lib: conan-install
	PKG_CONFIG_ALLOW_CROSS=1 cargo build --release --target=$(OSARCH)

conan-install:
	conan install .

# CLEAN

clean:
	cargo clean
	@rm -rf infrastructure/*.zip lambda/lib test nimcache


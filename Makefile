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

# Rust

RUST_VERSION = 1.19.0
OSARCH = x86_64-unknown-linux-musl
WORKER = slapman

build-worker: prod-conan-install
	OPENSSL_DIR=`pwd` OPENSSL_STATIC=1 PKG_CONFIG_ALLOW_CROSS=1 cargo build --release --target=$(OSARCH)
	cp $(BASEDIR)/target/$(OSARCH)/release/$(WORKER) $(BASEDIR)/lambda/worker

prod-conan-install:
	conan install . --build OpenSSL --profile dev_to_prod

local-worker: conan-install
	 OPENSSL_DIR=`pwd` OPENSSL_STATIC=1 RUST_LOG=warn cargo build -j 1 --release

run-worker: conan-install
	 OPENSSL_DIR=`pwd` OPENSSL_STATIC=1 RUST_LOG=warn cargo run

test-worker: conan-install
	 OPENSSL_DIR=`pwd` OPENSSL_STATIC=1 RUST_LOG=warn cargo test

conan-install:
	conan install . --build OpenSSL

# CLEAN

clean:
	cargo clean
	@rm -rf infrastructure/*.zip lambda/lib lib include

BASEDIR=$(PWD)
APPNAME=datakam
PROJECTID=kamestery
REGION=us-east1

system-prep:
	curl -O https://download.clojure.org/install/linux-install-1.10.1.469.sh
	chmod +x linux-install-1.10.1.469.sh
	./linux-install-1.10.1.469.sh
	rm ./linux-install-1.10.1.469.sh
	curl -sL https://deb.nodesource.com/setup_13.x | bash -
	apt-get install build-essential nodejs -y
	curl https://sh.rustup.rs -sSf | bash -s -- -y
	npm install --global neon-cli

gen-rsa:
	# Generating params file
	openssl ecparam -name brainpoolP512t1 -out $(BASEDIR)/resources/ecparams.pem
	# Generate a private key from params file
	openssl ecparam -in $(BASEDIR)/resources/ecparams.pem -genkey -noout -out $(BASEDIR)/resources/ecprivkey.pem
	# Generate a public key from private key
	openssl ec -in $(BASEDIR)/resources/ecprivkey.pem -pubout -out $(BASEDIR)/resources/ecpubkey.pem

upgrade-node-deps:
	npx npm-check-updates -u

deps:
	rm -rf $(PWD)/node_modules; npm i

compile:
	npx shadow-cljs compile app

watch-cljs:
	npm run start-dev

release:
	npx shadow-cljs release app


container: gen-rsa
	gcloud builds submit --tag gcr.io/$(PROJECTID)/$(APPNAME)

deploy: container
	gcloud beta run deploy $(APPNAME) --image gcr.io/$(PROJECTID)/$(APPNAME) --platform managed --region $(REGION)


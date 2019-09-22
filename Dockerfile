FROM clojure:openjdk-8-lein-2.9.1 AS builder

RUN mkdir $HOME/.aws
RUN echo "[default]" > $HOME/.aws/config
RUN echo "region=us-east-1" >> $HOME/.aws/config

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY project.clj /usr/src/app/
RUN lein deps
ADD . /usr/src/app
RUN openssl ecparam -name brainpoolP512t1 -out ./resources/ecparams.pem
RUN openssl ecparam -in ./resources/ecparams.pem -genkey -noout -out ./resources/ecprivkey.pem
RUN openssl ec -in ./resources/ecprivkey.pem -pubout -out ./resources/ecpubkey.pem
RUN lein uberjar


FROM openjdk:8-alpine
MAINTAINER outcastgeek <outcastgeek+git@gmail.com>

RUN mkdir /datakam
COPY --from=builder /usr/src/app/cmd.sh /datakam/cmd.sh
COPY --from=builder /usr/src/app/resources /datakam/resources
COPY --from=builder /usr/src/app/target/datakam-0.0.1-SNAPSHOT-standalone.jar /datakam/app.jar

ENV AWS_REGION=us-east-1 \
    AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID} \
    AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY} \
    PORT=${PORT}

EXPOSE ${PORT}

WORKDIR /datakam
CMD ["java", "-Dserver.port=${PORT}", "-jar", "/datakam/app.jar"]


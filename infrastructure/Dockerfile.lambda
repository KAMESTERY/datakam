
FROM lambci/lambda:build-go1.x

## Install sudo
#RUN yum install -y sudo
#RUN yum install -y sudo --nogpgcheck #Is this recommended? Probably not and does not work anyway
# The following gets rid of the Checksum Errors for some reason; refer to this: https://github.com/CentOS/sig-cloud-instance-images/issues/15
RUN yum install -y sudo; yum clean all

## install Rust from rustup
RUN curl -s https://static.rust-lang.org/rustup.sh | sh -s --

## install Protocol Buffers
ARG PROTOC_ZIP=protoc-3.3.0-linux-x86_64.zip
RUN curl -OL https://github.com/google/protobuf/releases/download/v3.3.0/$PROTOC_ZIP
RUN unzip -o $PROTOC_ZIP -d /tmp/protoc3
RUN mv /tmp/protoc3/bin/* /usr/local/bin/ && \
        mv /tmp/protoc3/include/* /usr/local/include/ && \
        rm -rf /tmp/protoc3
RUN rm -f $PROTOC_ZIP


FROM lambci/lambda:build-go1.x
## Install sudo
#RUN yum install -y sudo
#RUN yum install -y sudo --nogpgcheck #Is this recommended? Probably not and does not work anyway
# The following gets rid of the Checksum Errors for some reason; refer to this: https://github.com/CentOS/sig-cloud-instance-images/issues/15
RUN yum install -y sudo; yum clean all
## install Rust from rustup
RUN curl -s https://static.rust-lang.org/rustup.sh | sh -s --

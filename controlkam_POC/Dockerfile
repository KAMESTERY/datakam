# Dockerfile

FROM lambci/lambda:build-go1.x
## Install sudo
#RUN yum install -y sudo
#RUN yum install -y sudo --nogpgcheck #Is this recommended? Probably not and does not work anyway
# The following gets rid of the Checksum Errors for some reason; refer to this: https://github.com/CentOS/sig-cloud-instance-images/issues/15
RUN yum install -y subversion libssl-dev wget bzip2 gnupg2 rlwrap; yum clean all

MAINTAINER outcastgeek <outcastgeek+docker@gmail.com>

ENV CCL_VERSION 1.11
ENV SBCL_VERSION 1.4.12

## Install CCL
RUN svn co http://svn.clozure.com/publicsvn/openmcl/release/$CCL_VERSION/linuxx86/ccl /opt/ccl && \
    ln -s /opt/ccl/lx86cl64 /usr/bin/ccl

## Install SBCL
RUN wget https://prdownloads.sourceforge.net/sbcl/$SBCL_VERSION/sbcl-$SBCL_VERSION-x86-64-linux-binary.tar.bz2 -O /tmp/sbcl.tar.bz2 && \
    mkdir /opt/sbcl && \
    tar jxvf /tmp/sbcl.tar.bz2 --strip-components=1 -C /opt/sbcl/ && \
    ls -l /opt/sbcl && \
    ln -s /opt/sbcl/run-sbcl.sh /usr/bin/sbcl
RUN wget -O /tmp/quicklisp.lisp "https://beta.quicklisp.org/quicklisp.lisp" && \
    wget -O /tmp/quicklisp.lisp.asc "https://beta.quicklisp.org/quicklisp.lisp.asc" && \
    #gpg --verify /tmp/quicklisp.lisp.asc /tmp/quicklisp.lisp && \
    sbcl --no-sysinit --no-userinit --non-interactive \
         #--security-opt=seccomp=unconfined \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         #--eval '(ql::without-prompting (ql:add-to-init-file))' \
         --eval '(ql::without-prompting (dolist (imp `(:sbcl :ccl :abcl :ecl)) (ql:add-to-init-file imp)))' \
         --eval '(quit)'

VOLUME /app

WORKDIR /app

CMD [ "rlwrap", "sbcl" ]


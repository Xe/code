FROM xena/elemental-ircd:alpine
MAINTAINER Xena <xena@yolo-swag.com>

USER root

# We need testing.
RUN echo "http://repos.lax-noc.com/alpine/edge/testing" >> /etc/apk/repositories
RUN apk update

RUN apk add python
RUN apk add git
RUN apk add py-pip py-setuptools
RUN apk add openssh
RUN apk add wget ca-certificates

# Pip dependencies
RUN pip install ircmess

WORKDIR /home/ircd

ADD adjunct.sh /home/ircd/adjunct.sh
ADD shadownet.sslconf /home/ircd/shadownet.sslconf

RUN chmod -R 777 /home/ircd/*

USER ircd

CMD ["/bin/sh", "/home/ircd/adjunct.sh"]


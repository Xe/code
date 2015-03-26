FROM uggedal/alpine-3.0

# Upgrade stuff
RUN apk update
RUN apk upgrade

# Install dependencies for build
RUN apk add flex bison openssl-dev gcc make git musl-dev libc-dev
RUN apk add libgcc

# Download and build ircd
RUN adduser ircd -D -S -h /home/ircd -g "ircd"
RUN chmod 777 /home/ircd
RUN chown ircd /home/ircd

WORKDIR /home/ircd

USER ircd

RUN git clone http://github.com/Elemental-IRCd/elemental-ircd elemental
RUN cd elemental ; ./configure --prefix=/home/ircd/ircd
RUN cd elemental ; make
RUN cd elemental ; make install

# Seed ircd.conf
ADD ircd.conf /home/ircd/ircd/etc/ircd.conf

EXPOSE 6667 6697

USER root

RUN chmod -R 777 /home/ircd/ircd

USER ircd

CMD ["/home/ircd/ircd/bin/ircd", "-foreground"]


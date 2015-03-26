from ubuntu:14.04

RUN apt-get update
RUN apt-get -y upgrade

RUN apt-get -y install wget

RUN useradd -d /home/ircd -m ircd
RUN chown ircd /home/ircd

USER ircd
WORKDIR /home/ircd

RUN wget http://cyka.yolo-swag.com/ircd.tar.gz
RUN tar xf ircd.tar.gz

ADD ircd.conf /home/ircd/ircd/etc/ircd.conf

USER ircd

EXPOSE 6667

CMD ["/home/ircd/ircd/bin/ircd", "-foreground"]
